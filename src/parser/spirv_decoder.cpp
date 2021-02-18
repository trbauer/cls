#include "../system.hpp"
#include "../text.hpp"
#include "decoder.hpp"
#include "kargs.hpp"
#include "spirv_decoder.hpp"

#include <iomanip>
#include <map>
#include <sstream>
#include <string>
#include <utility>
#include <unordered_map>

using namespace cls;
using namespace cls::k;


struct kndr {uint32_t dim[3];};

struct spv_decoder: decoder {
  program_info &pi;
  int verbosity;
  bool endian_flip_needed;

  // per inst decode state
  uint32_t curr_inst_offset = 0;
  uint16_t curr_inst_words = 0;
  const char *curr_inst_opname = nullptr;
  kernel_info *last_kernel = nullptr;
  int function_params_left = 0;

  spv_decoder(
    int _verbosity,
    diagnostics &_diags,
    loc at, const uint8_t *bits, size_t bits_length,
    program_info &_pi)
    : decoder(_diags, at, bits, bits_length)
    , pi(_pi)
    , verbosity(_verbosity)
  {
    bool binary_little_endian = peek<uint8_t>() == (SPIRV_MAGIC & 0xFF);
    union {uint32_t i; char c[4];} bint = {0x01020304};
    bool machine_endian_little = bint.c[0] == 0x4;
    endian_flip_needed =
      binary_little_endian != machine_endian_little;
  }

  uint32_t decodeWord() {
    auto x = peekWord();
    skip(sizeof(x));
    return x;
  }
  uint32_t peekWord() const {
    auto x = peek<uint32_t>();
    if (endian_flip_needed) {
      x = swapByteOrder(x);
    }
    return x;
  }

  std::string decodeString() {
    // no endian conversion needed for SPV inline strings
    std::stringstream ss;

    // the reference disassembler uses the operand count to deduce the
    // string length, but the spec promises to use a NUL terminator byte;
    // hence, we take the easy path and stop when we hit a NUL byte
    uint32_t ws[2] = {decodeWord(),0x0};
    const char *str = (const char *)&ws[0];
    ss << str;
    while (strlen(str) == 4) {
      ws[0] = decodeWord();
      ss << str;
    }
    return ss.str();
  }

  template <typename...Ts>
  void debug(Ts...ts) {
    if (debugging()) {
      std::cout << text::format(ts...);
    }
  }

  bool debugging() const {
    return verbosity > 1;
  }

  ////////////////////////////////////////////////
  // per instruction methods
  template <typename...Ts>
  [[noreturn]]
  void fatalAtInst(Ts...ts) {
    fatalAt(
      curr_inst_offset,
      "in SPIR-V binary inst ",
      currInstOpName(), ": ", ts...);
  }

  const char *currInstOpName() {
    return curr_inst_opname ? curr_inst_opname : "???";
  }

  // int instWordsLeft() const {
  //  return (int)curr_inst_words - ((int)offset() - (int)curr_inst_offset)/4;
  // }

  //
  std::map<uint32_t, const type *> builtin_types; // e.g. int, int2, ...
  std::map<uint32_t, const type *> custom_types; // e.g. struct foo {...}
  const type *findType(uint32_t tid){
    auto itr = builtin_types.find(tid);
    if (itr == builtin_types.end()) {
      itr = custom_types.find(tid);
      if (itr == custom_types.end()) {
        return nullptr;
      }
    }
    return itr->second;
  }
  const type *getType(uint32_t tid) {
    const type *t = findType(tid);
    if (t == nullptr)
      fatalAtInst("%", tid, ": cannot find type id in SPIR-V binary");
    return t;
  };
  const type *getBuiltinType(std::string tnm) {
    const type *t = lookupBuiltinType(tnm, pi.pointer_size);
    if (t == nullptr)
      fatalAtInst(tnm, ": cannot find builtin type");
    return t;
  };

  // kernels by id, we build this based on the kernel ids we see
  std::map<uint32_t, kernel_info> ks;
  kernel_info *findKernel(uint32_t id) {
    auto itr = ks.find(id);
    if (itr == ks.end()) {
      return nullptr;
    }
    return &itr->second;
  }
  kernel_info *getKernel(uint32_t id) {
    auto *ki = findKernel(id);
    if (ki == nullptr) {
      ks[id] = kernel_info();
      return &ks[id];
    }
    return ki;
  }

  std::unordered_map<uint32_t, std::string> names;
  const std::string *findName(uint32_t id) {
    auto itr = names.find(id);
    if (itr == names.end()) {
      // fatal("cannot find name id in SPIR-V binary");
      return nullptr;
    }
    return &itr->second;
  }
  //
  std::unordered_map<uint32_t, int64_t> integer_constants;
  int64_t getIntegerConstant(uint32_t id) {
    auto itr = integer_constants.find(id);
    if (itr == integer_constants.end()) {
      fatalAtInst("%", id, ": unable to find integer constant");
    }
    return itr->second;
  };
  //
  struct ftype {
    const type *ret;
    std::vector<const type *> args;
    ftype() : ret(nullptr) {}
    ftype(const type *_ret) : ret(_ret) {}
  };
  std::unordered_map<uint32_t, ftype> function_types;
  ftype &getFunctionType(uint32_t id) {
    auto itr = function_types.find(id);
    if (itr == function_types.end()) {
      fatalAtInst("%", id, " unable to find function type");
    }
    return itr->second;
  }

  // Certain argument information such as access qualifiers and whatnot
  // are littered between types and decorations. E.g.
  //  - address qualifiers are part of OpTypePointer
  //  - access qualifiers for images like read_only are part of OpTypeImage
  //  - type qualifiers like "const" are in OpDecorate
  struct arg_attrs {
    cl_kernel_arg_address_qualifier  addr_qual = 0;
    cl_kernel_arg_access_qualifier   accs_qual = CL_KERNEL_ARG_ACCESS_NONE;
    cl_kernel_arg_type_qualifier     type_qual = CL_KERNEL_ARG_TYPE_NONE;
    // other decorators?
  };
  std::unordered_map<uint32_t, arg_attrs> arg_attributes;
  arg_attrs *findArgAttrs(uint32_t tid) {
    auto itr = arg_attributes.find(tid);
    if (itr == arg_attributes.end()) {
      return nullptr;
    }
    return &itr->second;
  };
  arg_attrs &getArgAttrs(uint32_t tid) {
    auto *aas = findArgAttrs(tid);
    if (!aas) {
      arg_attributes[tid] = arg_attrs();
      return arg_attributes[tid];
    } else {
      return *aas;
    }
  }

  /////////////////////////////////////////////////////////////////////////////
  /////////////////////////////////////////////////////////////////////////////
  //
  void decodeBinary() {
    // https://www.khronos.org/registry/spir-v/specs/1.0/SPIRV.html#Binary
    if (bytes_left() < 5*4)
      fatal("invalid SPIR-V binary (truncated header)");
    if (bytes_left() % 4 != 0)
      fatal("invalid SPIR-V binary (ragged binary)");

    uint32_t magic = SPIRV_MAGIC;
    if (endian_flip_needed)
      magic = swapByteOrder(magic);
    decodeEq(magic, "expected SPIR-V file input (bad magic)");

    // spv_header_t
    // c.f. https://github.com/KhronosGroup/SPIRV-Tools/blob/master/source/spirv_definition.h
    auto version = decodeWord();
    debug("version: ",
      (int)(version>>16) & 0xFF, ".", (int)(version>>8) & 0xFF, "\n");
    auto generator = decodeWord();
    // https://www.khronos.org/registry/spir-v/api/spir-v.xml
    debug("generator: ", generator, "\n");
    auto bound = decodeWord();
    debug("bound: ", bound, "\n"); // all ids are less than this
    auto schema = decodeWord();
    debug("schema: ", schema, "\n");

    // instructions
    // spv_parsed_instruction_t
    // c.f. https://github.com/KhronosGroup/SPIRV-Tools/blob/master/include/spirv-tools/libspirv.h
    while (bytes_left() > 0) {
      curr_inst_opname = "???";
      curr_inst_offset = (uint32_t)offset();
      uint32_t inst_header = decodeWord();
      curr_inst_words = (uint16_t)(inst_header >> 16);
      uint16_t opcode = (uint16_t)(inst_header & 0xFFFF);
      if (curr_inst_words == 0) { // to prevent skip by 0xFFFFF...
        fatalAtInst("invalid SPIR-V instruction (invalid word count)");
      } else if (4*((size_t)curr_inst_words - 1) > bytes_left()) {
        fatalAtInst("invalid SPIR-V instruction (truncated instruction)");
      }
      decodeInst(opcode);
    } // while inst

    // now copy out the kernels
    pi.kernels.reserve(ks.size());
    for (const auto &k : ks) {
      pi.kernels.push_back(k.second);
    }
    for (const auto &ti : custom_types)
      pi.types.push_back(ti.second);
  }

  void decodeInst(uint16_t opcode) {
    bool emit_inst = true;
    //
    constexpr uint16_t OpName          = 0x05;
    // ...
    constexpr uint16_t OpMemoryModel   = 0x0E;
    constexpr uint16_t OpEntryPoint    = 0x0F;
    constexpr uint16_t OpExecutionMode = 0x10;
    constexpr uint16_t OpCapability    = 0x11;
    // ...
    constexpr uint16_t OpTypeVoid      = 0x13;
    constexpr uint16_t OpTypeBool      = 0x14;
    constexpr uint16_t OpTypeInt       = 0x15;
    constexpr uint16_t OpTypeFloat     = 0x16;
    constexpr uint16_t OpTypeVector    = 0x17; // e.g. float2
    //
    constexpr uint16_t OpTypeImage     = 0x19;
    constexpr uint16_t OpTypeSampler   = 0x1A;
    // ...
    constexpr uint16_t OpTypeArray     = 0x1C; // e.g. local float tile[2];
    // ...
    constexpr uint16_t OpTypeStruct    = 0x1E; // e.g. struct foo
    constexpr uint16_t OpTypePointer   = 0x20; // float *...
    constexpr uint16_t OpTypeFunction  = 0x21;
    // ...
    constexpr uint16_t OpConstant      = 0x2B; // float *...
    //
    constexpr uint16_t OpFunction      = 0x36;
    constexpr uint16_t OpFunctionParameter = 0x37;
    //
    constexpr uint16_t OpDecorate = 0x47;
    //
    if (opcode == OpName) {
      curr_inst_opname = "OpName";
      uint32_t target_id = decodeWord();
      names[target_id] = decodeString();
      //
    } else if (opcode == OpMemoryModel) {
      curr_inst_opname = "OpMemoryModel";
      constexpr uint32_t Logical = 0x0;
      constexpr uint32_t Physical32 = 0x1;
      constexpr uint32_t Physical64 = 0x2;
      uint32_t addr_model = decodeWord();
      (void)decodeWord(); // memory model
      pi.pointer_size =
        addr_model == Physical32 ? 4 :
        addr_model == Physical64 ? 8 : 0;
      //
    } else if (opcode == OpEntryPoint) {
      curr_inst_opname = "OpEntryPoint";
      uint32_t model = decodeWord();
      uint32_t id = decodeWord();
      constexpr uint32_t Kernel = 6;
      if (model == Kernel) {
        auto *ki = getKernel(id);
        ki->name = decodeString();
      } // else skip
      //
    } else if (opcode == OpExecutionMode) {
      curr_inst_opname = "OpExecutionMode";
      uint32_t id = decodeWord();
      uint32_t mode = decodeWord();
      constexpr uint32_t LocalSize = 0x11;
      if (mode == LocalSize) {
        auto *ki = getKernel(id);
        ki->reqd_word_group_size[0] = decodeWord();
        ki->reqd_word_group_size[1] = decodeWord();
        ki->reqd_word_group_size[2] = decodeWord();
      } // else: ignore it
      //
    } else if (opcode == OpTypeInt) {
      curr_inst_opname = "OpTypeInt";
      uint32_t id = decodeWord();
      uint32_t bits = decodeWord();
      uint32_t sign = decodeWord();
      switch (bits) {
      case 8:
        builtin_types[id] = sign ? &cls::CHAR() : &cls::UCHAR();
        break;
      case 16:
        builtin_types[id] = sign ? &cls::SHORT() : &cls::USHORT();
        break;
      case 32:
        builtin_types[id] = sign ? &cls::INT() : &cls::UINT();
        break;
      case 64:
        builtin_types[id] = sign ? &cls::LONG() : &cls::ULONG();
        break;
      }
      //
    } else if (opcode == OpTypeVoid) {
      curr_inst_opname = "OpTypeVoid";
      uint32_t id = decodeWord();
      builtin_types[id] = &cls::VOID();
      //
    } else if (opcode == OpTypeFloat) {
      curr_inst_opname = "OpTypeFloat";
      uint32_t id = decodeWord();
      uint32_t bits = decodeWord();
      switch (bits) {
      case 16: builtin_types[id] = &cls::HALF(); break;
      case 32: builtin_types[id] = &cls::FLOAT(); break;
      case 64: builtin_types[id] = &cls::DOUBLE(); break;
      }
      //
    } else if (opcode == OpTypeVector) {
      curr_inst_opname = "OpTypeVector";
      uint32_t id = decodeWord();
      uint32_t elem_type = decodeWord();
      uint32_t elem_count = decodeWord();
      const type *et = getType(elem_type);
      //
      std::stringstream ss;
      ss << et->syntax() << elem_count;
      //
      const type *vt = getBuiltinType(ss.str());
      if (!vt) {
        fatalAtInst("OpTypeVector with non-builtin type");
      }
      builtin_types[id] = vt;
      //
    } else if (opcode == OpTypeImage) {
      curr_inst_opname = "OpTypeImage";
      uint32_t rid = decodeWord(); // result id
      uint32_t stid = decodeWord(); // sampled type
      uint32_t dim = decodeWord(); // 1D, 2D, 3D, Cube, Rect, Buffer, SubpassData
      //
      uint32_t depth = decodeWord(); // 0 (no), 1 (yes), or 2 (unknown)
      uint32_t arrayed = decodeWord(); // 0 or 1
      uint32_t msaa = decodeWord(); // multi sampled
      uint32_t is_sampled = decodeWord(); // 0 (runtime sampler), 1 (yes), 2 (no)
      //
      uint32_t format = decodeWord(); // probably Unknown
      uint32_t aqbits = decodeWord();
      //
      // use the following:
      // https://www.khronos.org/registry/OpenCL/sdk/2.1/docs/man/xhtml/otherDataTypes.html
      std::stringstream ss;
      ss << "image";
      const uint32_t Dim_1D = 0;
      const uint32_t Dim_2D = 1;
      const uint32_t Dim_3D = 2;
      const uint32_t Dim_Buffer = 5;
      switch (dim) {
      case Dim_1D: ss << "1d"; break;
      case Dim_2D: ss << "2d"; break;
      case Dim_3D: ss << "3d"; break;
      case Dim_Buffer: ss << "1d_buffer"; break; // image1d_array_t
      // TODO: image_buffer
      default: fatalAtInst("OpTypeImage with unsupported Dim");
      }
      //
      if (arrayed) {
        ss << "_array"; // e.g. image*_array_*_t
      }
      if (msaa)
        ss << "_msaa"; // e.g. image2d_depth_t o image2d_array_depth_t
      if (depth)
        ss << "_depth"; // e.g. image2d_depth_t o image2d_array_depth_t
      //
      ss << "_t";
      builtin_types[rid] = getBuiltinType(ss.str());
      //
      cl_kernel_arg_address_qualifier aq = CL_KERNEL_ARG_ACCESS_NONE;
      if (aqbits == 0) {
        aq = CL_KERNEL_ARG_ACCESS_READ_ONLY;
      } else if (aqbits == 1) {
        aq = CL_KERNEL_ARG_ACCESS_WRITE_ONLY;
      } else if (aqbits == 2) {
        aq = CL_KERNEL_ARG_ACCESS_READ_WRITE;
      } else {
        fatalAtInst("OpTypeImage invalid access qualifier");
      }
      // the image built-in type has already been mapped to the correct id
      // we need only associate this type id with the image access qualifier
      getArgAttrs(rid).accs_qual = aq;
      //
    } else if (opcode == OpTypeSampler) {
      curr_inst_opname = "OpTypeSampler";
      uint32_t id = decodeWord();
      builtin_types[id] = getBuiltinType("sampler_t");
      //
    } else if (opcode == OpTypeArray) {
      curr_inst_opname = "OpTypeArray";
      uint32_t id = decodeWord();
      const type *et = getType(decodeWord());
      int64_t ic = getIntegerConstant(decodeWord());
      if (ic < 0)
        fatalAtInst("OpTypeArray with negative size");
      builtin_types[id] = new type(type_array(et,(size_t)ic));
      //
    } else if (opcode == OpTypeStruct) {
      curr_inst_opname = "OpTypeStruct";
      auto id = decodeWord();
      std::vector<const type *> elems;
      size_t total_size = 0;
      for (uint16_t ix = 0; ix < curr_inst_words - 2; ix++) {
        const type *t = getType(decodeWord());
        while (total_size % t->size() != 0)
          total_size++; // natural alignment
        if (!t)
          fatalAtInst("OpTypeStruct with unknown type");
        elems.push_back(t);
      }
      while (total_size % 4 == 0)
        total_size++; // natural alignment: pad out struct

      // construct a dynamically allocated name for this structure
      // program_info::~program_info cleans this up
      std::string dummy;
      const std::string *nm = findName(id);
      if (!nm) {
        // not sure if a struct is promised to have a named id, but we'll
        // support the omission of that case
        std::stringstream ss;
        ss << "spv$anon$" << custom_types.size();
        dummy = ss.str();
        nm = &dummy;
      } else {
        // struct.cmplx
        if (nm->substr(0, 7) == "struct.") {
          dummy = "struct " + nm->substr(7);
          nm = &dummy;
        }
      }
      //
      // TODO: figure out reasonable alignment
      // (find any annotations and learn the default)
      type_struct *ts = new type_struct(nm->c_str(), false, 16, elems);
      type *t = new type(*ts);
      custom_types[id] = t;
      //
    } else if (opcode == OpTypePointer) {
      curr_inst_opname = "OpTypePointer";
      uint32_t tid = decodeWord();
      // https://www.khronos.org/registry/spir-v/specs/1.0/SPIRV.html#Storage_Class
      uint32_t storage_class = decodeWord();
      uint32_t element_type_id = decodeWord();
      const type *element_type = findType(element_type_id);
      if (element_type == nullptr) {
        fatalAtInst("OpTypePointer points to unknown type");
      }
      auto addPointerType = [&](cl_kernel_arg_address_qualifier aq) {
        custom_types[tid] =
          new type(type_ptr(element_type, pi.pointer_size));
        getArgAttrs(tid).addr_qual = aq;
      };
      //
      static const uint32_t UniformConstant  =  0;
      static const uint32_t Input            =  1;
      static const uint32_t Uniform          =  2;
      static const uint32_t Output           =  3;
      static const uint32_t Workgroup        =  4;
      static const uint32_t CrossWorkgroup   =  5;
      static const uint32_t Private          =  6;
      static const uint32_t Function         =  7;
      static const uint32_t Generic          =  8;
      static const uint32_t PushConstant     =  9;
      static const uint32_t AtomicCounter    = 10;
      static const uint32_t Image            = 11;
      static const uint32_t StorageBuffer    = 12;
      //
      switch (storage_class) {
      case UniformConstant: // constant T *
        // e.g.
        //   %v3ulong = OpTypeVector %ulong 3 ; 0x00000860
        //   %_ptr_UniformConstant_v3ulong = OpTypePointer UniformConstant %v3ulong; 0x00000870
        addPointerType(CL_KERNEL_ARG_ADDRESS_CONSTANT);
        break;
      case Workgroup: // local T *
        // custom_types[id] =
        //  new type(type_ptr(element_type, pi.pointer_size));
        addPointerType(CL_KERNEL_ARG_ADDRESS_LOCAL);
        break;
        //
      case CrossWorkgroup: // global T *
        addPointerType(CL_KERNEL_ARG_ADDRESS_GLOBAL);
        break;
      case Function:
        // e.g. a *uniform* structure passed by value manifests itself as
        //      a pointer (C semantics)
        //   struct point {int x, y;};
        //   kernel void test(struct point x);
        addPointerType(CL_KERNEL_ARG_ADDRESS_PRIVATE);
        break;
        //
      case Input:
      case Uniform:
      case Output:
      // case Private:
      case Generic:
      case PushConstant:
      case AtomicCounter:
      case Image:
      case StorageBuffer:
      default:
        fatalAtInst("unhandled storage class ", storage_class);
      }
      //
    } else if (opcode == OpTypeFunction) {
      curr_inst_opname = "OpTypeFunction";
      uint32_t ftid = decodeWord();
      uint32_t rtid = decodeWord();
      const type *rt = getType(rtid);
      function_types[ftid] = ftype(rt);
      ftype &ft = function_types[ftid];
      for (int i = 0; i < curr_inst_words - 3; i++) {
        ft.args.push_back(getType(decodeWord()));
      }
      //
    } else if (opcode == OpConstant) {
      // we need these for static array arguments
      // e.g. local int[256];
      curr_inst_opname = "OpConstant";
      uint32_t tid = decodeWord();
      uint32_t cid = decodeWord();
      const type *t = findType(tid);
      if (t->is<type_num>()) {
        const type_num &tn = t->as<type_num>();
        if (tn.skind == type_num::UNSIGNED || tn.skind == type_num::SIGNED) {
          int64_t val = (int64_t)decodeWord();
          if (tn.size() == 8) {
            val |= (int64_t)decodeWord() << 32;
          }
          integer_constants[cid] = val;
        } // else float: skip it
      }
      //
    } else if (opcode == OpFunction) {
      curr_inst_opname = "OpFunction";
      uint32_t fid = decodeWord(); // result type id
      uint32_t rid = decodeWord(); // result function id
      uint32_t fctl = decodeWord(); // function control
      (void)fctl; // ignore
      uint32_t ftid = decodeWord(); // function type id
      const ftype &ft = getFunctionType(ftid);
      function_params_left = (int)ft.args.size();
      if (kernel_info *ki = findKernel(rid)) {
        ki->args.reserve(ft.args.size());
        last_kernel = ki;
      } else {
        // non-kernel function
        last_kernel = nullptr;
      }
      //
    } else if (opcode == OpFunctionParameter) {
      curr_inst_opname = "OpFunctionParameter";
      uint32_t rtid = decodeWord(); // result type id
      const type *t = getType(rtid);
      uint32_t rid = decodeWord(); // result id
      // %8 = OpFunctionParameter %5; 0x00000118
      //  ^ rid                   ^ rtid

      // name that matches rtid
      if (function_params_left == 0)
        fatalAtInst("OpFunctionParameter without OpFunction");
      if (last_kernel) {
        const std::string *arg = findName(rid);
        if (arg == nullptr) {
          fatalAtInst("unable to OpFunctionParameter name");
        }
        last_kernel->args.emplace_back();
        arg_info &ai = last_kernel->args.back();
        ai.name = *arg;
        ai.arg_type = t;
        if (t->is<type_ptr>()) {
          const arg_attrs *param_attrs = findArgAttrs(rid);
          if (param_attrs) {
            // const, restrict, ... come from OpDecorator
            ai.type_qual = param_attrs->type_qual;
          }
          const arg_attrs *type_attrs = findArgAttrs(rtid);
          if (type_attrs) {
            // private, local, global, ... come from OpPointerType
            ai.addr_qual = type_attrs->addr_qual;
          }

        } else if (t->is<type_builtin>()) {
          // access qualifiers are a property of the image type
          const arg_attrs *type_attrs = findArgAttrs(rtid);
          if (type_attrs) {
            ai.accs_qual = type_attrs->accs_qual;
          }
        } else {
          // e.g. int, float4, structure
          ai.addr_qual = CL_KERNEL_ARG_ADDRESS_PRIVATE;
        }
      }
      //
    } else if (opcode == OpDecorate) {
      curr_inst_opname = "OpDecorate";
      uint32_t tid = decodeWord(); // target id
      uint32_t dec = decodeWord();
      //
      // static const uint32_t CPacked = 10;
      static const uint32_t BuiltIn = 11;
      static const uint32_t Restrict = 19;
      // static const uint32_t Aliased = 20;
      static const uint32_t Volatile = 21;
      static const uint32_t Constant = 22;
      static const uint32_t FuncParamAttr = 38;
      switch (dec) {
      case Restrict:
        // Intel appears to send this via FuncParamAttr:NoAlias
        getArgAttrs(tid).type_qual |= CL_KERNEL_ARG_TYPE_RESTRICT;
        break;
      case Volatile:
        getArgAttrs(tid).type_qual |= CL_KERNEL_ARG_TYPE_VOLATILE;
        break;
      case Constant:
        // Intel appears to send this via FuncParamAttr:NoWrite
        getArgAttrs(tid).type_qual |= CL_KERNEL_ARG_TYPE_CONST;
        break;
      case FuncParamAttr:
      {
        static const uint32_t NoAlias = 4;
        static const uint32_t NoWrite = 6;
        uint32_t fpa = decodeWord(); // func param attr
        switch (fpa) {
        case NoWrite:
          getArgAttrs(tid).type_qual |= CL_KERNEL_ARG_TYPE_CONST;
          break;
        case NoAlias:
          getArgAttrs(tid).type_qual |= CL_KERNEL_ARG_TYPE_RESTRICT;
          break;
        default:
          warningAt(curr_inst_offset, "SPIRV ignoring decorator ", dec);
        }
        break;
      }
      case BuiltIn: //
        // normal cases that we can avoid silently GlobalInvocationId
        break;
      default:
        warningAt(curr_inst_offset, "SPIRV ignoring decorator ", dec);
      }
      //
    } else { // else: opcode == ?
      // skip it (unrecognized)
      emit_inst = false;
    }
    // some instructions above only partially decode; so we just move to
    // the next based on operand count
    seek(curr_inst_offset + 4*curr_inst_words);
    //
    if (debugging() && emit_inst) {
      std::cout << text::fmt_hex(curr_inst_offset) << ": ";
      std::cout << "0x" << text::fmt_hex(opcode);
      std::stringstream ss;
      ss << " (" << currInstOpName() << ") ";
      std::cout << std::left << std::setw(24) << ss.str();
      for (uint16_t op_ix = 1; op_ix < curr_inst_words; op_ix++) {
        std::cout << "  0x" <<
          text::fmt_hex(peek<uint32_t>(curr_inst_offset + 4*op_ix));
      }
      std::cout << "\n";
    }
  } // decodeInst
};

cls::k::program_info *cls::parseProgramInfoBinarySPIRV(
  const opts &os,
  diagnostics &ds, loc at,
  const std::string &path)
{
  program_info *pi = new program_info();
  auto bits = sys::read_file_binary(path);
  spv_decoder sd(os.verbosity, ds, at, bits.data(), bits.size(), *pi);
  sd.decodeBinary();
  return pi;
}