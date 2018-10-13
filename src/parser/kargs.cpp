#include "kargs.hpp"
#include "parser.hpp"
#include "../cl_headers.hpp"
#include "../half.hpp"
#include "../ir/types.hpp"
#include "../devices.hpp"
#include "../system.hpp"
#include "../text.hpp"

#if __has_include(<filesystem>)
#include <filesystem>
namespace fs = std::filesystem;
#elif __has_include(<experimental/filesystem>)
#include <experimental/filesystem>
namespace fs = std::experimental::filesystem;
#else
#error "#include <filesystem> not found"
#endif
#include <fstream>

using namespace cls::k;
using namespace cls;

/*
struct child_a {
  int x;
//  child_a(int _x) : x(_x) { }
};
struct child_b {
  int x;
  const char *str;
  constexpr child_b(const char *_str, int _x) : x(_x), str(_str) { }
};
struct example {
  const char *name;
  std::variant<child_a,child_b> v;
//  constexpr example(int x) : v(x) { }
//  constexpr example(const char *_name, child_a a) : name(_name), v(a) { }
  constexpr example(const char *_name, child_b b) : name(_name), v(b) { }
};
constexpr static example X{"foo",child_b("foo",3)};
*/


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

  void parseArg(kernel_info &k) {
    k.args.emplace_back();
    arg_info &a = k.args.back();

    a.is_const|= consumeIfIdentEq("const");

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

    a.is_const |= consumeIfIdentEq("const");

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
    auto t = lookupPrimtiveType(type_name);
    if (t == nullptr) {
      if (type_name == "size_t" || type_name == "uintptr_t")
        t = lookupPrimtiveType(bytes_per_addr == 4 ? "uint" : "ulong");
      else if (type_name == "intptr_t" || type_name == "ptrdiff_t")
        t = lookupPrimtiveType(bytes_per_addr == 4 ? "int" : "long");
      else
        fatalAt(type_loc,"unrecognized type");
    }
    a.type = *t;
    if (consumeIf(MUL)) {
      p.pointer_types.emplace_back(t,bytes_per_addr);
      a.type = type(p.pointer_types.back());
    }
    while (true) {
      // int
      if (consumeIfIdentEq("const")) {
        a.addr_qual |= CL_KERNEL_ARG_TYPE_CONST;
      } else if (consumeIfIdentEq("restrict")) {
        a.addr_qual |= CL_KERNEL_ARG_TYPE_RESTRICT;
      } else if (consumeIfIdentEq("volatile")) {
        a.addr_qual |= CL_KERNEL_ARG_TYPE_VOLATILE;
      } else if (consumeIfIdentEq("pipe")) {
        a.addr_qual |= CL_KERNEL_ARG_TYPE_PIPE;
      } else {
        break;
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

static cls::k::program_info parseProgramInfoText(
  const cls::opts &os,
  const cls::fatal_handler *fh, cls::loc at,
  const cls::program_source &src,
  size_t bytes_per_addr)
{
  std::stringstream ss;
  size_t off = src.build_opts.find("-D",0);
  while (off < src.build_opts.size()) {
    if (ss.tellp() != 0) // separate arguments
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
  std::string cpp_inp = text::load_c_preprocessed(src.path,ss.str());
  // suffix the build options so that line numbers all map correctly
  // (when we decide to support #line directives)
  cpp_inp +=
    "\n\n"
    "// CPP OPTIONS: " + ss.str();
  if (os.save_preprocessed) {
    auto ppc_path =
      fs::path(".") / fs::path(src.path).filename().replace_extension(".ppcl");
    std::ofstream of(ppc_path.string());
    os.verbose() << "dumping preprocessed " << ppc_path << "\n";
    of << cpp_inp;
  }

  program_info p;
  karg_parser kp(cpp_inp, p, bytes_per_addr);
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
  return p;
}

static cls::k::program_info parseProgramInfoBinary(
  const cls::opts &os,
  const cls::fatal_handler *fh, cls::loc at,
  const std::string &path)
{
  auto bits = sys::read_file_binary(path);
  // TODO: parse PTX or GEN binary
  fh->fatalAt(at,
    "parseProgramInfoBinary: "
    "argument info from binaries not supported yet");
  return program_info();
}

cls::k::program_info cls::k::parseProgramInfo(
  const cls::opts &os,
  const cls::fatal_handler *fh, cls::loc at,
  const cls::program_source &src,
  size_t bytes_per_addr)
{
  if (src.is_binary) {
    return parseProgramInfoBinary(os, fh, at, src.path);
  } else {
    return parseProgramInfoText(os, fh, at, src, bytes_per_addr);
  }
}
