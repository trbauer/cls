#include "kargs.hpp"
#include "parser.hpp"
#include "../cl_headers.hpp"
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


#define CAT2(X,Y) X ## Y
#define CAT(X,Y) CAT2(X,Y)

#define DEFINE_PRIM_TYPE_VEC(N,IDENT,NAME,CATEGORY,SIZE) \
  constexpr static type_struct ST_ ## IDENT ## N{NAME #N,SIZE,&IDENT,N}; \
  constexpr static type IDENT ## N{ST_ ## IDENT ## N};

#define DEFINE_PRIM_TYPE(IDENT,NAME,CATEGORY,SIZE) \
  constexpr static type IDENT{type_num{NAME,type_num::CATEGORY,SIZE}}; \
  DEFINE_PRIM_TYPE_VEC(2,IDENT,NAME,CATEGORY,SIZE) \
  DEFINE_PRIM_TYPE_VEC(3,IDENT,NAME,CATEGORY,SIZE) \
  DEFINE_PRIM_TYPE_VEC(4,IDENT,NAME,CATEGORY,SIZE) \
  DEFINE_PRIM_TYPE_VEC(8,IDENT,NAME,CATEGORY,SIZE) \
  DEFINE_PRIM_TYPE_VEC(16,IDENT,NAME,CATEGORY,SIZE)

/*
DEFINE_PRIM_TYPE(CHAR,"char",SIGNED,1);
constexpr static type_struct ST_CHAR2{"char2",sizeof(char),&CHAR,2};
constexpr static type CHAR2{ST_CHAR2};
DEFINE_PRIM_TYPE(INT,"int",SIGNED,4);
*/

DEFINE_PRIM_TYPE(FLOAT,"float",FLOATING,4);

// constexpr static type FLOAT{type_num{"float",type_num::FLOATING,4}};
// constexpr static type_struct ST_FLOAT2{"float2",sizeof(float),&FLOAT,2};
// constexpr static type FLOAT2{ST_FLOAT2};
// ...


// constexpr static type CHAR{type_num("char",type_num::SIGNED,1)};
// constexpr static type_struct TS2 = type_struct{&CHAR};
// constexpr static type CHAR2{TS2};

static const type *lookupPrimtiveType(std::string name)
{
  /*
  if (name == "char" || name == "signed char") {
    return &CHAR;
  }
  if (name == "char2") {
    return &CHAR2;
  }
  */

  if (name == "float") {
    return &FLOAT;
  }
  if (name == "float2") {
    return &FLOAT2;
  }

  // if (name == "uchar" || name == "unsigned char") {
  // }
  return nullptr;
}


struct karg_parser : cls::parser
{
  program_info  &p;

  karg_parser(const std::string &inp, program_info  &_p)
    : cls::parser(inp,true)
    , p(_p)
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
    auto knm = consumeIdent("kernel name");
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

    a.is_const|= consumeIfIdentEq("const");

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

    auto type = consumeIdent("qualifier or type");
    if ((type == "unsigned" || type == "signed") && lookingAtIdent()) {
      explicit_signed_or_unsigned = true;
      type += ' ' + consumeIdent("type");
    }
    a.type = lookupPrimtiveType(type);
//    if (consumeIf(MUL))
//      a.type = pointerTo(a.type);
    if (a.type == nullptr) {
      fatalAt(type_loc,"unrecognized type");
    }
    /*
    if (type == "char" ||
    } else type == "uchar" ||
      type == "unsigned char")
    {
      a.type =
    } else if (ka.elem_name == "short" ||
      ka.elem_name == "ushort" ||
      ka.elem_name == "unsigned short")
    {
      ka.elem_class = INTEGRAL;
      ka.elem_size = 2;
      ka.elem_signed = ka.elem_name[0] != 'u';
    } else if (ka.elem_name == "int" ||
      ka.elem_name == "uint" ||
      ka.elem_name == "unsigned int")
    {
      ka.elem_class = INTEGRAL;
      ka.elem_size = 4;
      ka.elem_signed = ka.elem_name[0] != 'u';
    } else if (ka.elem_name == "long" ||
      ka.elem_name == "ulong" ||
      ka.elem_name == "unsigned long")
    {
      ka.elem_class = INTEGRAL;
      ka.elem_size = 8;
      ka.elem_signed = ka.elem_name[0] != 'u';
    } else if (ka.elem_name == "float") {
      ka.elem_class = FLOATING;
      ka.elem_size = 4;
    } else if (ka.elem_name == "double") {
      ka.elem_class = FLOATING;
      ka.elem_size = 8;
    } else if (ka.elem_name == "half") {
      ka.elem_class = FLOATING;
      ka.elem_size = 2;
    } else if (ka.elem_name == "size_t" ||
      ka.elem_name == "ptrdiff_t" ||
      ka.elem_name == "intptr_t" ||
      ka.elem_name == "uintptr_t")
    }
    */
  }
};

static cls::k::program_info parseProgramInfoText(
  const cls::opts &os,
  const cls::fatal_handler *fh, cls::loc at,
  const cls::program_source &src)
{
  std::stringstream ss;
  size_t off = src.build_opts.find("-D",0);
  while (off < src.build_opts.size()) {
    if (ss.gcount() != 0) // is this right?
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
  karg_parser kp(cpp_inp, p);
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
  fh->fatalAt(at,"parseProgramInfoBinary: argument info from binaries not supported yet");
  return program_info();
}

cls::k::program_info cls::k::parseProgramInfo(
  const cls::opts &os,
  const cls::fatal_handler *fh, cls::loc at,
  const cls::program_source &src)
{
  if (src.is_binary) {
    return parseProgramInfoBinary(os, fh, at, src.path);
  } else {
    return parseProgramInfoText(os, fh, at, src);
  }
}