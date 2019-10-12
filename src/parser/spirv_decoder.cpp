#include "../system.hpp"
#include "../text.hpp"
#include "decoder.hpp"
#include "kargs.hpp"
#include "spirv_decoder.hpp"

#include <iomanip>
#include <map>
#include <sstream>
#include <string>
#include <unordered_map>

using namespace cls;
using namespace cls::k;

// info on a kernel
struct kndr { uint32_t dim[3]; };

struct spv_decoder: decoder {
  program_info &pi;
  int verbosity;
  bool endian_flip_needed;

  spv_decoder(
    int _verbosity,
    fatal_handler &fh,
    loc at, const uint8_t *bits, size_t bits_length,
    program_info &_pi)
    : decoder(fh, at, bits, bits_length)
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
  uint32_t peekWord() {
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

  void run() {
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

    // kernels by id, we build this based on the kernel ids we see
    std::map<uint32_t, kernel_info> ks;
    auto getKernel = [&](uint32_t id) {
      auto itr = ks.find(id);
      if (itr == ks.end()) {
        ks[id] = kernel_info();
        return &ks[id];
      }
      return &itr->second;
    };
    //
    std::map<uint32_t, const type *> builtin_types; // e.g. int, int2, ...
    std::map<uint32_t, const type *> custom_types; // e.g. struct foo {...}
    auto getType = [&](uint32_t id) {
      auto itr = builtin_types.find(id);
      if (itr == builtin_types.end()) {
        itr = custom_types.find(id);
        if (itr == custom_types.end()) {
          fatal("cannot find type id in SPIR-V binary");
        }
      }
      return itr->second;
    };
    //
    std::unordered_map<uint32_t, std::string> names;
    auto findName = [&](uint32_t id) -> const std::string* {
      auto itr = names.find(id);
      if (itr == names.end()) {
        // fatal("cannot find name id in SPIR-V binary");
        return nullptr;
      }
      return &itr->second;
    };

    // instructions
    // spv_parsed_instruction_t
    // c.f. https://github.com/KhronosGroup/SPIRV-Tools/blob/master/include/spirv-tools/libspirv.h
    while (bytes_left() > 0) {
      bool emit_inst = true;
      const char *opname_str = "???";
      uint32_t inst_offset = (uint32_t)offset();
      uint32_t inst_header = decodeWord();
      uint16_t word_count = (uint16_t)(inst_header >> 16);
      uint16_t opcode = (uint16_t)(inst_header & 0xFFFF);
      if (word_count == 0) { // to prevent skip by 0xFFFFF...
        fatal("invalid SPIR-V instruction (invalid word count)");
      } else if (4*(word_count - 1) > bytes_left()) {
        fatal("invalid SPIR-V instruction (truncated instruction)");
      }
      //
      constexpr uint16_t OpName          = 0x05;
      //
      constexpr uint16_t OpMemoryModel   = 0x0E;
      constexpr uint16_t OpEntryPoint    = 0x0F;
      constexpr uint16_t OpExecutionMode = 0x10;
      constexpr uint16_t OpCapability    = 0x11;
      //
      constexpr uint16_t OpTypeVoid      = 0x13;
      constexpr uint16_t OpTypeBool      = 0x14;
      constexpr uint16_t OpTypeInt       = 0x15;
      constexpr uint16_t OpTypeFloat     = 0x16;
      constexpr uint16_t OpTypeVector    = 0x17; // e.g. float2
      //
      constexpr uint16_t OpTypeStruct    = 0x1E;
      //
      if (opcode == OpName) {
        opname_str = "OpName";
        uint32_t target_id = decodeWord();
        names[target_id] = decodeString();
        //
      } else if (opcode == OpMemoryModel) {
        opname_str = "OpMemoryModel";
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
        opname_str = "OpEntryPoint";
        uint32_t model = decodeWord();
        uint32_t id = decodeWord();
        constexpr uint32_t Kernel = 6;
        if (model == Kernel) {
          auto *ki = getKernel(id);
          ki->name = decodeString();
        } // else skip
        //
      } else if (opcode == OpExecutionMode) {
        opname_str = "OpExecutionMode";
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
        opname_str = "OpTypeInt";
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
      } else if (opcode == OpTypeFloat) {
        opname_str = "OpTypeFloat";
        uint32_t id = decodeWord();
        uint32_t bits = decodeWord();
        switch (bits) {
        case 16: builtin_types[id] = &cls::HALF(); break;
        case 32: builtin_types[id] = &cls::FLOAT(); break;
        case 64: builtin_types[id] = &cls::DOUBLE(); break;
        }
        //
      } else if (opcode == OpTypeVector) {
        opname_str = "OpTypeVector";
        uint32_t id = decodeWord();
        uint32_t elem_type = decodeWord();
        uint32_t elem_count = decodeWord();
        const type *et = getType(elem_type);

        std::stringstream ss;
        ss << et->syntax() << elem_count;
        //
        const size_t PTRSIZE_DOESNT_MATTER = 8;
        const type *vt = lookupBuiltinType(ss.str(), PTRSIZE_DOESNT_MATTER);
        if (!vt) {
          fatalAt(inst_offset, "OpTypeVector with non-builtin type");
        }
        builtin_types[id] = vt;
        //
      } else if (opcode == OpTypeStruct) {
        opname_str = "OpTypeStruct";
        auto id = decodeWord();
        std::vector<const type *> elems;
        size_t total_size = 0;
        for (uint16_t ix = 0; ix < word_count - 2; ix++) {
          const type *t = getType(decodeWord());
          while (total_size % t->size() != 0)
            total_size++; // natural alignment
          if (!t)
            fatalAt(inst_offset, "OpTypeStruct with unknown type");
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
      } else {
        // skip it (unrecognized)
        emit_inst = false;
      }
      // some instructions above only partially decode; so we just move to
      // the next based on operand count
      seek(inst_offset + 4*word_count);
      //
      if (debugging() && emit_inst) {
        std::cout << text::fmtHex(inst_offset) << ": ";
        std::cout << "0x" << text::fmtHex(opcode) <<
          " (" << opname_str << ") ";
        for (uint16_t op_ix = 0; op_ix < word_count; op_ix++) {
          std::cout << "  0x" <<
            text::fmtHex(peek<uint32_t>(inst_offset + 4*op_ix));
        }
        std::cout << "\n";
      }
    } // while

    // now copy out the kernels
    pi.kernels.reserve(ks.size());
    for (const auto &k : ks) {
      pi.kernels.push_back(k.second);
      // TODO: need to figure out all the arguments
      //  1. Resolve OpTypePointer
      //      %_ptr_CrossWorkgroup_v2float = OpTypePointer CrossWorkgroup %v2float ; 0x00000898
      //     creates type_ptr for float2
      //  2. Resolve OpTypeFunction
      //      %49 = OpTypeFunction %void %_ptr_CrossWorkgroup_v2float ...
      //  3. Resolve OpFunctionParameter following OpFunction
      //      %50 = OpFunction %void None %49 ; 0x00000c34
      //      %dst_0 = OpFunctionParameter %_ptr_CrossWorkgroup_v2float; 0x00000c48
      //      ...
      //    find this via %50 in
      //      OpEntryPoint Kernel %50 "mulcs" ; 0x00000070
    }
    for (const auto &ti : custom_types)
      pi.types.push_back(ti.second);
  }
};

cls::k::program_info *cls::parseProgramInfoBinarySPIRV(
  const opts &os,
  fatal_handler *fh, loc at,
  const std::string &path)
{
  program_info *pi = new program_info();
  auto bits = sys::read_file_binary(path);
  spv_decoder sd(os.verbosity, *fh, at, bits.data(), bits.size(), *pi);
  sd.run();
  return pi;
}