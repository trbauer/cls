#include "intel_gen_binary_decoder.hpp"
#include "kargs.hpp"
#include "parser.hpp"
#include "../cl_headers.hpp"
#include "../half.hpp"
#include "../ir/types.hpp"
#include "../devices.hpp"
#include "../system.hpp"
#include "../text.hpp"

#include <fstream>
#include <sstream>

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
  if (type_qual & CL_KERNEL_ARG_TYPE_RESTRICT) {
    ss << sep << "restrict";
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
  switch (accs_qual) {
  case CL_KERNEL_ARG_ACCESS_READ_ONLY:   ss << " read_only"; break;
  case CL_KERNEL_ARG_ACCESS_WRITE_ONLY:  ss << " write_only"; break;
  case CL_KERNEL_ARG_ACCESS_READ_WRITE:  ss << " read_write"; break;
  case CL_KERNEL_ARG_ACCESS_NONE: break;
  }
  ss << " " << type.syntax();

  return ss.str();
}

program_info::~program_info()
{
  for (type *t : types)
    delete t;
  for (type_ptr *pt : pointer_types)
    delete pt;
}

const type &program_info::pointerTo(const type &t_ref, size_t ptr_size)
{
  for (const type *t : types) {
    if (t->is<type_ptr>() && t->as<type_ptr>().element_type == &t_ref) {
      return *t;
    }
  }
  pointer_types.push_back(new type_ptr(&t_ref,ptr_size));
  types.push_back(new type(*pointer_types.back()));
  return *types.back();
}


struct karg_parser : cls::parser
{
  program_info  &p;
  size_t         bytes_per_addr;

  karg_parser(
    const std::string &inp,
    program_info  &_p,
    size_t _bytes_per_addr)
    : cls::parser(inp,true)
    , p(_p)
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
  void parse() {
    while (!endOfFile()) {
      if (lookingAtIdentEq("__kernel") || lookingAtIdentEq("kernel")) {
        parseKernel();
      } else {
        skip();
      }
    }
  }

  void parseKernel() {
    skip(); // kernel

    p.kernels.emplace_back();
    kernel_info &k = p.kernels.back();

    consumeIdentEq("void","void");
    k.name = consumeIdent("kernel name");
    consume(LPAREN);
    while (!lookingAt(RPAREN)) {
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
    auto t = lookupBuiltinType(type_name,bytes_per_addr);
    if (t == nullptr) {
      fatalAt(type_loc,"unrecognized type");
    }
    a.type = *t;
    while (consumeIf(MUL)) {
      a.type = p.pointerTo(*t,bytes_per_addr);
      // this allows (global char *const *name)
      //                           ^^^^^
      // maybe useful for SVM
      //
      // we discard it because it's not an attribute of the outermost pointer
      (void)parseTypeQualsOpt();
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
  const cls::fatal_handler *fh, cls::loc at,
  const cls::program_source &src,
  size_t bytes_per_addr)
{
  std::stringstream ss;
  size_t off = src.build_opts.find("-D",0);
  while (off < src.build_opts.size()) {
    if ((size_t)ss.tellp() != 0) // separate arguments
      ss << " ";

    // consume the -D option
    //
    // -Dfoo
    // -Dfoo=bar
    // -D foo
    // -D foo=bar
    size_t d_start = off;
    ss << "-D";
    off += 2;
    while (off < src.build_opts.size() && ::isspace(src.build_opts[off])) {
      // deals with separate tokens: e.g. "-D" "foo=bar"
      ss << src.build_opts[off++];
    }
    while (off < src.build_opts.size() && !::isspace(src.build_opts[off])) {
      ss << src.build_opts[off++];
    }

    // next iteration
    off = src.build_opts.find("-D",off);
  }

  std::string cpp_inp;
  if (os.cpp_override_path.empty()) {
    cpp_inp = text::load_c_preprocessed(src.path,ss.str());
  } else {
    if (!sys::file_exists(os.cpp_override_path))
      fh->fatalAt(at,
        "unable to find C Preprocessor from command line option "
        "for kernel analysis");
    cpp_inp =
      text::load_c_preprocessed_using(os.cpp_override_path,src.path,ss.str());
  }
  // suffix the build options so that line numbers all map correctly
  // (when we decide to support #line directives)
  cpp_inp +=
    "\n\n"
    "// CPP OPTIONS: " + ss.str();
  if (os.save_preprocessed) {
    // auto ppc_path =
    //  fs::path(".") / fs::path(src.path).filename().replace_extension(".ppcl");
    // std::ofstream of(ppc_path.string());
    std::string ppc_path = ".";
    ppc_path += sys::path_separator;
    ppc_path += sys::replace_extension(src.path, ".ppcl");
    std::ofstream of(ppc_path);
    os.verbose() << "dumping preprocessed " << ppc_path << "\n";
    of << cpp_inp;
  }

  program_info *pi = new program_info();
  karg_parser kp(cpp_inp, *pi, bytes_per_addr);
  try {
    kp.parse();
  } catch (const diagnostic &d) {
    std::ofstream fs("debug-out.cl");
    fs << cpp_inp;

    std::stringstream ss;
    d.str(ss);
    ss << "\n";
    ss << "SEE: debug-out.cl";
    fh->fatalAt(at,ss.str());
  }
  return pi;
}

static program_info *parseProgramInfoBinary(
  const opts &os,
  const fatal_handler *fh, loc at,
  const std::string &path,
  cl_device_id dev_id)
{
  // auto bits = sys::read_file_binary(path);
  // TODO: parse PTX or GEN binary
  auto ma = getDeviceMicroArchitecture(dev_id);
  if (isIntelGEN(ma)) {
    return parseProgramInfoBinaryGEN(os, fh, at, path);
  } else {
    fh->fatalAt(at,
      "parseProgramInfoBinary: "
      "argument info from binaries not supported yet on this device");
    return nullptr;
  }
}

program_info *cls::k::parseProgramInfo(
  const opts &os,
  const fatal_handler *fh, loc at,
  const program_source &src,
  cl_device_id dev_id)
{

  if (src.is_binary) {
    return parseProgramInfoBinary(os, fh, at, src.path, dev_id);
  } else {
    cl_uint bytes_per_addr;
    if (getDeviceInfo(
      dev_id, CL_DEVICE_ADDRESS_BITS, bytes_per_addr) != CL_SUCCESS)
    {
      fh->fatalAt(at, "clGetDeviceInfo(CL_DEVICE_ADDRESS_BITS)");
    }
    bytes_per_addr /= 8;
    return parseProgramInfoText(os, fh, at, src, bytes_per_addr);
  }
}

program_info *cls::k::parseProgramInfoFromAPI(
  const cls::opts &os,
  const cls::fatal_handler *fh, cls::loc at,
  cl_program program,
  cl_device_id dev_id)
{
  cl_uint ks_len = 0;
  auto err = clCreateKernelsInProgram(program, 0, nullptr, &ks_len);
  if (err != CL_SUCCESS) {
    fh->fatalAt(at,
      "failed to parse program info program: clCreateKernelsInProgram: ",
      status_to_symbol(err));
  }
  cl_kernel *ks = (cl_kernel*)alloca(sizeof(*ks) * ks_len);
  err = clCreateKernelsInProgram(program, ks_len, ks, nullptr);
  if (err != CL_SUCCESS) {
    fh->fatalAt(at,
      "failed to parse program info program: clCreateKernelsInProgram: ",
      status_to_symbol(err));
  }

  program_info *pi = new program_info();
  pi->kernels.reserve(ks_len);

  cl_uint bytes_per_addr;
  if (getDeviceInfo(
    dev_id, CL_DEVICE_ADDRESS_BITS, bytes_per_addr) != CL_SUCCESS)
  {
    fh->fatalAt(at, "clGetDeviceInfo(CL_DEVICE_ADDRESS_BITS)");
  }
  bytes_per_addr /= 8;

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
      fh->fatalAt(at,
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
      fh->fatalAt(at,
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
      fh->fatalAt(at,
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
      fh->fatalAt(at,
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
          fh->fatalAt(at,
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
          fh->fatalAt(at,
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
        fh->fatalAt(at,
          "failed to parse program info program: "
          "unable to lookup type ", type_name, " from program object");
      }
      ai.type = *t;
      for (size_t i = star; i < full_type_name.size(); i++) {
        if (full_type_name[i] == '*')
          ai.type = pi->pointerTo(ai.type, bytes_per_addr);
      }

      err = clGetKernelArgInfo(
        ks[k_ix],
        ka_ix,
        CL_KERNEL_ARG_ADDRESS_QUALIFIER,
        sizeof(ai.addr_qual),
        &ai.addr_qual,
        nullptr);
      if (err != CL_SUCCESS) {
        fh->fatalAt(at,
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
        fh->fatalAt(at,
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
        fh->fatalAt(at,
          "failed to parse program info program: "
          "clGetKernelArgInfo(CL_KERNEL_ARG_TYPE_QUALIFIER): ",
          status_to_symbol(err));
      }
    } // for kernel args
    err = clReleaseKernel(ks[k_ix]);
    if (err != CL_SUCCESS) {
      fh->fatalAt(at,
        "failed to parse program info program: clReleaseKernel(): ",
        status_to_symbol(err));
    }
  } // for kernels

  return pi;
}
