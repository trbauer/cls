#include "../cl_headers.hpp"
#include "../half.hpp"
#include "../ir/types.hpp"
#include "../devices.hpp"
#include "../system.hpp"
#include "../text.hpp"
#include "intel_gen_binary_decoder.hpp"
#include "kargs.hpp"
#include "parser.hpp"
#include "spirv_decoder.hpp"

#include <fstream>
#include <sstream>
#include <unordered_map>

using namespace cls::k;
using namespace cls;


std::string cls::k::arg_info::typeSyntax() const
{
  std::stringstream ss;
  const char *sep = "";

  if (type_qual & CL_KERNEL_ARG_TYPE_CONST) {
    ss << sep << "const";
    sep = " ";
  }
  if (type_qual & CL_KERNEL_ARG_TYPE_VOLATILE) {
    ss << sep << "volatile";
    sep = " ";
  }
  if (type_qual & CL_KERNEL_ARG_TYPE_PIPE) {
    ss << sep << "pipe";
    sep = " ";
  }

  switch (addr_qual) {
  case CL_KERNEL_ARG_ADDRESS_GLOBAL:     ss << sep << "global"; break;
  case CL_KERNEL_ARG_ADDRESS_LOCAL:      ss << sep << "local"; break;
  case CL_KERNEL_ARG_ADDRESS_CONSTANT:   ss << sep << "constant"; break;
  case CL_KERNEL_ARG_ADDRESS_PRIVATE:    ss << sep << "private"; break;
  }
  if (addr_qual != 0 && accs_qual != CL_KERNEL_ARG_ACCESS_NONE) {
    ss << " ";
  }
  switch (accs_qual) {
  case CL_KERNEL_ARG_ACCESS_READ_ONLY:   ss << "read_only"; break;
  case CL_KERNEL_ARG_ACCESS_WRITE_ONLY:  ss << "write_only"; break;
  case CL_KERNEL_ARG_ACCESS_READ_WRITE:  ss << "read_write"; break;
  case CL_KERNEL_ARG_ACCESS_NONE: break;
  }
  ss << " " << arg_type->syntax();

  if (type_qual & CL_KERNEL_ARG_TYPE_RESTRICT) {
    // restrict applies to the pointer
    // global int * restrict foo, ...
    ss << " restrict";
  }

  return ss.str();
}

program_info::~program_info()
{
  for (const type *t : types) {
    if (t->is<type_struct>()) {
      auto &ts = t->as<type_struct>();
      if (ts.needs_cleanup())
        ts.cleanup();
    }
    delete t;
  }
}

const type &program_info::pointerTo(const type &t_ref, size_t ptr_size)
{
  for (const type *t : types) {
    if (t->is<type_ptr>() && t->as<type_ptr>().element_type == &t_ref) {
      return *t;
    }
  }
  types.push_back(new type(type_ptr(&t_ref, ptr_size)));
  return *types.back();
}


struct karg_parser : cls::parser
{
  program_info  &pi;
  size_t         bytes_per_addr;
  std::unordered_map<std::string,const cls::type *> typedefs;

  karg_parser(
    diagnostics &ds,
    const std::string &inp,
    program_info  &_p,
    size_t _bytes_per_addr)
    : cls::parser(ds, inp, true)
    , pi(_p)
    , bytes_per_addr(_bytes_per_addr)
  {
  }
  // TODO: how do we chain this
  // want diganostic to be properly chained
  //
  // 1.2: failed to parse kernel arguments
  // #1`foo.cl[-opts]`kernel<...>(...)
  //                  ^^^^^^
  //
  // in [CPP(foo.cl,-D... )] => cpp-foo.cl
  // kernel void foo(...........)
  //                 ^^^^
  //
  // also handle simple typedefs
  // typedef int s32;
  void parse() {
    while (!endOfFile()) {
      if (lookingAtIdentEq("typedef")) {
        skip();
        parseTypedef();
      }

      if (lookingAtIdentEq("__kernel") || lookingAtIdentEq("kernel")) {
        parseKernel();
      } else {
        skip();
      }
    }
  }

  // look up a typedef'd or built-in type
  const cls::type *findType(std::string type_name) const {
    auto itr = typedefs.find(type_name);
    if (itr != typedefs.end())
      return itr->second;
    return lookupBuiltinType(type_name, bytes_per_addr);
  }

  //
  // typedef int s32;
  //         ^ we start here
  //
  // We don't handle more complicated stuff
  //   typedef int* myintptr_t;
  //   typedef int (*round_function)(float x);
  void parseTypedef() {
    if (!lookingAtIdent()) {
      return; // something too hard
    }
    auto id = consumeIdent();
    if (id == "struct" || id == "union" || id == "class") {
      // TODO: handle structures and unions
      // typedef struct {int x, y;} point_t;
      return;
    }
    const type *existing = findType(id);
    if (existing == nullptr) { // unknown type
      return;
    }
    if (!lookingAtIdent()) {
      return;
    }
    auto eq = consumeIdent();
    if (!lookingAt(SEMI)) {
      return;
    }
    typedefs[eq] = existing;
  }

  void parseKernel() {
    skip(); // kernel

    pi.kernels.emplace_back();
    kernel_info &k = pi.kernels.back();

    consumeIdentEq("void","void");
    k.name = consumeIdent("kernel name");
    skipPreprocessorLineDirectives();
    consume(LPAREN);
    skipPreprocessorLineDirectives();
    while (!lookingAt(RPAREN)) {
      skipPreprocessorLineDirectives();
      parseArg(k);
      if (!consumeIf(COMMA)) {
        break;
      }
    }
    consume(RPAREN);
  }

  int parseTypeQualsOpt() {
    int qs = CL_KERNEL_ARG_TYPE_NONE;
    while (true) {
      if (consumeIfIdentEq("const")) {
        qs |= CL_KERNEL_ARG_TYPE_CONST;
      } else if (consumeIfIdentEq("restrict")) {
        qs |= CL_KERNEL_ARG_TYPE_RESTRICT;
      } else if (consumeIfIdentEq("volatile")) {
        qs |= CL_KERNEL_ARG_TYPE_VOLATILE;
      } else if (consumeIfIdentEq("pipe")) {
        qs |= CL_KERNEL_ARG_TYPE_PIPE;
      } else {
        break;
      }
    }
    return qs;
  }

  void skipPreprocessorLineDirectives() {
    // e.g. #line 1082 "convolution_gpu_bfyx_f16_1x1_264.cl"
    auto loc = nextLoc();
    if (loc.column == 1 && lookingAt(HASH)) {
      skip();
      auto loc2 = nextLoc();
      while (!endOfFile() && loc2.line == loc.line) {
        skip();
        loc2 = nextLoc();
      }
    }
  }

  void parseArg(kernel_info &k) {
    k.args.emplace_back();
    arg_info &a = k.args.back();

    bool is_const = false;
    bool is_volatile = false;
    bool is_restrict = false;
    bool is_pipe = false;

    a.type_qual |= parseTypeQualsOpt();

    if (consumeIfIdentEq("global","__global")) {
      a.addr_qual = CL_KERNEL_ARG_ADDRESS_GLOBAL;
    } else if (consumeIfIdentEq("constant","__constant")) {
      a.addr_qual = CL_KERNEL_ARG_ADDRESS_CONSTANT;
    } else if (consumeIfIdentEq("local","__local")) {
      a.addr_qual = CL_KERNEL_ARG_ADDRESS_LOCAL;
    } else if (consumeIfIdentEq("private","__private")) {
      a.addr_qual = CL_KERNEL_ARG_ADDRESS_PRIVATE;
    } else {
      a.addr_qual = CL_KERNEL_ARG_ADDRESS_PRIVATE;
    }

    a.type_qual |= parseTypeQualsOpt();

    if (consumeIfIdentEq("read_only","__read_only")) {
      a.accs_qual = CL_KERNEL_ARG_ACCESS_READ_ONLY;
    } else if (consumeIfIdentEq("write_only","__write_only")) {
      a.accs_qual = CL_KERNEL_ARG_ACCESS_WRITE_ONLY;
    } else if (consumeIfIdentEq("read_write","__read_write")) {
      a.accs_qual = CL_KERNEL_ARG_ACCESS_READ_WRITE;
    } else {
      a.accs_qual = CL_KERNEL_ARG_ACCESS_NONE;
    }

    loc type_loc = nextLoc();
    bool explicit_signed_or_unsigned = false;

    auto type_name = consumeIdent("qualifier or type");
    if ((type_name == "unsigned" || type_name == "signed")) {
      if (lookingAtIdentEq("char") || lookingAtIdentEq("short") ||
        lookingAtIdentEq("int") || lookingAtIdentEq("long"))
      {
        // e.g. unsigned int
        type_name += ' ';
        type_name += consumeIdent("type");
      }
    }
    auto t = findType(type_name);
    if (t == nullptr) {
      fatalAt(type_loc,"unrecognized type");
    }
    a.arg_type = t;
    while (consumeIf(MUL)) {
      a.arg_type = &pi.pointerTo(*a.arg_type, bytes_per_addr);
      // this allows (global char *const *name)
      //                           ^^^^^
      // maybe useful for SVM
      //
      // we discard it because it's not an attribute of the outermost pointer
      (void)parseTypeQualsOpt();
    }

    // omit address qualifier for images
    if (a.arg_type->is<type_builtin>() && type_name.substr(0, 5) == "image") {
      a.addr_qual = 0;
      if (a.accs_qual == CL_KERNEL_ARG_ACCESS_NONE) {
        // image without an access qualifier is implicitly read_only
        a.accs_qual = CL_KERNEL_ARG_ACCESS_READ_ONLY;
      }
    }

    if (lookingAtIdent()) {
      a.name = consumeIdent();
    }
    if (!lookingAt(RPAREN) && !lookingAt(COMMA)) {
      fatal("syntax in argument");
    }
  }
};

static cls::k::program_info *parseProgramInfoText(
  const cls::opts &os,
  cls::diagnostics &ds, cls::loc at,
  const cls::program_source &src,
  size_t bytes_per_addr)
{
  std::stringstream ss_opts;
  size_t off = src.build_opts.find("-D",0);
  while (off < src.build_opts.size()) {
    if ((size_t)ss_opts.tellp() != 0) // separate arguments
      ss_opts << " ";

    // consume the -D option
    //
    // -Dfoo
    // -Dfoo=bar
    // -D foo
    // -D foo=bar
    size_t d_start = off;
    ss_opts << "-D";
    off += 2;
    while (off < src.build_opts.size() && ::isspace(src.build_opts[off])) {
      // deals with separate tokens: e.g. "-D" "foo=bar"
      ss_opts << src.build_opts[off++];
    }
    while (off < src.build_opts.size() && !::isspace(src.build_opts[off])) {
      ss_opts << src.build_opts[off++];
    }

    // next iteration
    off = src.build_opts.find("-D", off);
  }

  text::cpp_result cpp;
  if (os.cpp_override_path.empty()) {
    cpp = text::load_c_preprocessed(src.path, ss_opts.str());
  } else {
    if (!sys::file_exists(os.cpp_override_path))
      ds.fatalAt(at,
        "unable to find C Preprocessor from command line option "
        "for kernel analysis");
    cpp = text::load_c_preprocessed_using(
      os.cpp_override_path, src.path, ss_opts.str());
  }
  if (!cpp.succeeded()) {
    ds.fatalAt(at,
      "C preprocessor ", cpp.status_to_string(), ":\n",
      "CPP: ", cpp.cpp_path, "\n", cpp.output);
  } else if (os.verbosity >= 2) {
    ds.debugAt(at, "used preprocessor: ", cpp.cpp_path);
  }
  // suffix the build options so that line numbers all map correctly
  // (when we decide to support #line directives)
  std::string cpp_inp =
    text::format(
      cpp.output,
      "\n\n"
      "// CPP OPTIONS: ", ss_opts.str(), "\n"
      "// CPP PATH: ", cpp.cpp_path, "\n");
  if (os.save_preprocessed) {
    // auto ppc_path =
    //  fs::path(".") / fs::path(src.path).filename().replace_extension(".ppcl");
    // std::ofstream of(ppc_path.string());
    std::string ppc_path = ".";
    ppc_path += sys::path_separator;
    ppc_path += sys::replace_extension(src.path, ".pp.cl");
    std::ofstream of(ppc_path);
    ds.verboseAt(at, ppc_path, ": dumping preprocessed");
    of << cpp_inp;
  }

  program_info *pi = new program_info();
  karg_parser kp(ds, cpp_inp, *pi, bytes_per_addr);
  try {
    kp.parse();
  } catch (const diagnostic &d) {
    std::ofstream fs("debug-out.cl");
    fs << cpp_inp;

    // create a dummy diagnostic that obeys that uses our input
    std::stringstream ss;
    d.str(ss, cpp_inp);
    ss << "\n";
    ss << "SEE: debug-out.cl";
    ds.fatalAt(at,ss.str());
  }
  return pi;
}

static program_info *parseProgramInfoBinary(
  const opts &os,
  diagnostics &ds, loc at,
  const std::string &path,
  cl_device_id dev_id)
{
  // auto bits = sys::read_file_binary(path);
  auto ma = getDeviceMicroArchitecture(dev_id);
  if (isIntelGEN(ma)) {
    return parseProgramInfoBinaryGEN(os, ds, at, path);
  } else {
    // TODO: would love to handle more binary formats
    // PTX won't work because it lacks type information we need unify
    // arg initializers with arguments
    ds.fatalAt(at,
      "parseProgramInfoBinary: "
      "argument info from binaries not supported yet on this device");
    return nullptr;
  }
}

program_info *cls::k::parseProgramInfo(
  const opts &os,
  diagnostics &ds, loc at,
  const program_source &src,
  cl_device_id dev_id)
{
  if (src.kind == program_source::BINARY) {
    return parseProgramInfoBinary(os, ds, at, src.path, dev_id);
  } else if (src.kind == program_source::SPIRV) {
    return parseProgramInfoBinarySPIRV(os, ds, at, src.path);
  } else { // TEXT
    // loading from binary gives us a pointer size to sanity check,
    // for here we just trust the OpenCL C runtime compiler to tell the truth
    // (i.e. there's nothing to check); however, we do store the value
    // in the program_info structure for consistency
    cl_uint bytes_per_addr;
    if (getDeviceInfo(
      dev_id, CL_DEVICE_ADDRESS_BITS, bytes_per_addr) != CL_SUCCESS)
    {
      ds.fatalAt(at, "clGetDeviceInfo(CL_DEVICE_ADDRESS_BITS)");
    }
    bytes_per_addr /= 8;
    auto *pi = parseProgramInfoText(os, ds, at, src, bytes_per_addr);
    pi->pointer_size = bytes_per_addr;
    return pi;
  }
}

program_info *cls::k::parseProgramInfoFromAPI(
  const cls::opts &os,
  cls::diagnostics &ds, cls::loc at,
  cl_program program,
  cl_device_id dev_id)
{
  cl_uint ks_len = 0;
  auto err = clCreateKernelsInProgram(program, 0, nullptr, &ks_len);
  if (err != CL_SUCCESS) {
    ds.fatalAt(at,
      "failed to parse program info program: clCreateKernelsInProgram: ",
      status_to_symbol(err));
  }
  cl_kernel *ks = (cl_kernel*)alloca(sizeof(*ks) * ks_len);
  err = clCreateKernelsInProgram(program, ks_len, ks, nullptr);
  if (err != CL_SUCCESS) {
    ds.fatalAt(at,
      "failed to parse program info program: clCreateKernelsInProgram: ",
      status_to_symbol(err));
  }

  program_info *pi = new program_info();
  pi->kernels.reserve(ks_len);

  cl_uint bytes_per_addr;
  if (getDeviceInfo(
    dev_id, CL_DEVICE_ADDRESS_BITS, bytes_per_addr) != CL_SUCCESS)
  {
    ds.fatalAt(at, "clGetDeviceInfo(CL_DEVICE_ADDRESS_BITS)");
  }
  bytes_per_addr /= 8;
  pi->pointer_size = bytes_per_addr;

  for (cl_uint k_ix = 0; k_ix < ks_len; k_ix++) {
    char knm_buf_static[256];
    pi->kernels.emplace_back();
    kernel_info &ki = pi->kernels.back();

    size_t knm_len = 0;
    err = clGetKernelInfo(
      ks[k_ix],
      CL_KERNEL_FUNCTION_NAME,
      0,
      nullptr,
      &knm_len);
    if (err != CL_SUCCESS) {
      ds.fatalAt(at,
        "failed to parse program info program: "
        "clGetKernelInfo(CL_KERNEL_FUNCTION_NAME): ",
        status_to_symbol(err));
    }
    char *knm =
      (knm_len + 1 < sizeof(knm_buf_static)) ?
        &knm_buf_static[0] : (char *)alloca(knm_len + 1);
    memset(knm,0,knm_len+1);

    err = clGetKernelInfo(
      ks[k_ix],
      CL_KERNEL_FUNCTION_NAME,
      knm_len,
      knm,
      nullptr);
    if (err != CL_SUCCESS) {
      ds.fatalAt(at,
        "failed to parse program info program: "
        "clGetKernelInfo(CL_KERNEL_FUNCTION_NAME): ",
        status_to_symbol(err));
    }
    ki.name = knm;

    memset(ki.reqd_word_group_size,0,sizeof(ki.reqd_word_group_size));
    err = clGetKernelWorkGroupInfo(
      ks[k_ix],
      dev_id,
      CL_KERNEL_COMPILE_WORK_GROUP_SIZE,
      sizeof(ki.reqd_word_group_size),
      &ki.reqd_word_group_size[0],
      nullptr);
    if (err != CL_SUCCESS) {
      ds.fatalAt(at,
        "failed to parse program info program: "
        "clGetKernelWorkGroupInfo(CL_KERNEL_GLOBAL_WORK_SIZE): ",
        status_to_symbol(err));
    }

    cl_uint ka_len = 0;
    err = clGetKernelInfo(
      ks[k_ix],
      CL_KERNEL_NUM_ARGS,
      sizeof(ka_len),
      &ka_len,
      nullptr);
    if (err != CL_SUCCESS) {
      ds.fatalAt(at,
        "failed to parse program info program: "
        "clGetKernelInfo(CL_KERNEL_NUM_ARGS): ",
        status_to_symbol(err));
    }

    // FIXME: vector resize leads to corruption
    // (I can't find the stale reference)
    ki.args.reserve(ka_len);

    for (cl_uint ka_ix = 0; ka_ix < ka_len; ka_ix++) {
      // could detect and be nice about CL_KERNEL_ARG_INFO_NOT_AVAILABLE
      // bins may not ...
      ki.args.emplace_back();
      arg_info &ai = ki.args.back();

      auto getStringParam = [&] (cl_int param, const char *param_name) {
        cl_int err = 0;
        size_t len = 0;
        err = clGetKernelArgInfo(
          ks[k_ix],
          ka_ix,
          param,
          0,
          nullptr,
          &len);
        if (err != CL_SUCCESS) {
          ds.fatalAt(at,
            "failed to parse program info program object: "
            "clGetKernelArgInfo(", param_name, "): ",
            status_to_symbol(err));
        }
        char *buf = (char *)alloca(len + 1);
        err = clGetKernelArgInfo(
          ks[k_ix],
          ka_ix,
          param,
          len,
          buf,
          nullptr);
        if (err != CL_SUCCESS) {
          ds.fatalAt(at,
            "failed to parse program info program object: "
            "clGetKernelArgInfo(", param_name, "): ",
            status_to_symbol(err));
        }
        buf[len] = 0;
        return std::string(buf);
      };

      ai.name = getStringParam(CL_KERNEL_ARG_NAME,"CL_KERNEL_ARG_NAME");
      auto full_type_name =
        getStringParam(CL_KERNEL_ARG_TYPE_NAME,"CL_KERNEL_ARG_TYPE_NAME");
      // e.g. float*
      bool is_pointer = false;
      auto star = full_type_name.find("*");
      std::string type_name = full_type_name.substr(0,star);

      const type *t = lookupBuiltinType(type_name, bytes_per_addr);
      if (t == nullptr) {
        ds.fatalAt(at,
          "failed to parse program info program: "
          "unable to lookup type ", type_name, " from program object");
      }
      ai.arg_type = t;
      for (size_t i = star; i < full_type_name.size(); i++) {
        if (full_type_name[i] == '*')
          ai.arg_type = &pi->pointerTo(*ai.arg_type, bytes_per_addr);
      }

      err = clGetKernelArgInfo(
        ks[k_ix],
        ka_ix,
        CL_KERNEL_ARG_ADDRESS_QUALIFIER,
        sizeof(ai.addr_qual),
        &ai.addr_qual,
        nullptr);
      if (err != CL_SUCCESS) {
        ds.fatalAt(at,
          "failed to parse program info program object: "
          "clGetKernelArgInfo(CL_KERNEL_ARG_ADDRESS_QUALIFIER): ",
          status_to_symbol(err));
      }

      err = clGetKernelArgInfo(
        ks[k_ix],
        ka_ix,
        CL_KERNEL_ARG_ACCESS_QUALIFIER,
        sizeof(ai.accs_qual),
        &ai.accs_qual,
        nullptr);
      if (err != CL_SUCCESS) {
        ds.fatalAt(at,
          "failed to parse program info program: "
          "clGetKernelArgInfo(CL_KERNEL_ARG_ACCESS_QUALIFIER): ",
          status_to_symbol(err));
      }

      err = clGetKernelArgInfo(
        ks[k_ix],
        ka_ix,
        CL_KERNEL_ARG_TYPE_QUALIFIER,
        sizeof(ai.type_qual),
        &ai.type_qual,
        nullptr);
      if (err != CL_SUCCESS) {
        ds.fatalAt(at,
          "failed to parse program info program: "
          "clGetKernelArgInfo(CL_KERNEL_ARG_TYPE_QUALIFIER): ",
          status_to_symbol(err));
      }
    } // for kernel args
    err = clReleaseKernel(ks[k_ix]);
    if (err != CL_SUCCESS) {
      ds.fatalAt(at,
        "failed to parse program info program: clReleaseKernel(): ",
        status_to_symbol(err));
    }
  } // for kernels

  return pi;
}
