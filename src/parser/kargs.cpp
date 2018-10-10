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

DEFINE_PRIM_TYPE(HALF,   "half",   FLOATING, 2);
DEFINE_PRIM_TYPE(FLOAT,  "float",  FLOATING, 4);
DEFINE_PRIM_TYPE(DOUBLE, "double", FLOATING, 8);

DEFINE_PRIM_TYPE(CHAR,   "char",   SIGNED,   1);
DEFINE_PRIM_TYPE(UCHAR,  "uchar",  UNSIGNED, 1);
DEFINE_PRIM_TYPE(SHORT,  "short",  SIGNED,   2);
DEFINE_PRIM_TYPE(USHORT, "ushort", UNSIGNED, 2);
DEFINE_PRIM_TYPE(INT,    "int",    SIGNED,   4);
DEFINE_PRIM_TYPE(UINT,   "uint",   UNSIGNED, 4);
DEFINE_PRIM_TYPE(LONG,   "long",   SIGNED,   8);
DEFINE_PRIM_TYPE(ULONG,  "ulong",  UNSIGNED, 8);

// constexpr static type FLOAT{type_num{"float",type_num::FLOATING,4}};
// constexpr static type_struct ST_FLOAT2{"float2",sizeof(float),&FLOAT,2};
// constexpr static type FLOAT2{ST_FLOAT2};
// ...


// constexpr static type CHAR{type_num("char",type_num::SIGNED,1)};
// constexpr static type_struct TS2 = type_struct{&CHAR};
// constexpr static type CHAR2{TS2};

static const type *lookupPrimtiveType(std::string name)
{
  // normalize primitive names (e.g. unsigned int -> uint)
  if (name == "signed") {
    name = "int";
  } else if (name == "unsigned") {
    name = "uint";
  } else {
    auto pos = name.find(' ');
    if (pos != std::string::npos) {
      auto tk1 = name.substr(0,pos);
      auto tk2 = name.substr(pos);
      if (tk2 == "char" || tk2 == "short" || tk2 == "int" || tk2 == "long") {
        if (tk1 == "signed") {
          name = tk2;
        } else if (tk1 == "unsigned") {
          name = "u" + tk2; //
        }
      }
    }
  }

#define MATCH_PRIM_TYPE(T_ID,T_STR) \
  do { \
    if (name == (T_STR)) return &(T_ID); \
    if (name == (T_STR "2")) return &(CAT(T_ID,2)); \
    if (name == (T_STR "3")) return &(CAT(T_ID,3)); \
    if (name == (T_STR "4")) return &(CAT(T_ID,4)); \
    if (name == (T_STR "8")) return &(CAT(T_ID,8)); \
    if (name == (T_STR "16")) return &(CAT(T_ID,16)); \
  } while (0)

  MATCH_PRIM_TYPE(HALF,"half");
  MATCH_PRIM_TYPE(FLOAT,"float");
  MATCH_PRIM_TYPE(DOUBLE,"double");

  MATCH_PRIM_TYPE(INT,"char");
  MATCH_PRIM_TYPE(UINT,"uchar");
  MATCH_PRIM_TYPE(SHORT,"short");
  MATCH_PRIM_TYPE(USHORT,"ushort");
  MATCH_PRIM_TYPE(INT,"int");
  MATCH_PRIM_TYPE(UINT,"uint");
  MATCH_PRIM_TYPE(LONG,"long");
  MATCH_PRIM_TYPE(ULONG,"ulong");

  return nullptr;
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
        type_name += ' ';
        type_name += consumeIdent("type");
      }
    }
    auto t = lookupPrimtiveType(type_name);
    if (t == nullptr) {
      if (type_name == "size_t" || type_name == "uintptr_t")
        t = (bytes_per_addr == 4) ? &UINT : &ULONG;
      else if (type_name == "intptr_t" || type_name == "ptrdiff_t")
        t = (bytes_per_addr == 4) ? &INT : &LONG;
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

template <typename T>
static T read_unaligned(const void *buf)
{
  T val;
  memcpy(&val,buf,sizeof(val));
  return val;
}

void formatBufferElement(
  std::ostream &os,
  const type &t,
  const void *ptr)
{
  if (std::holds_alternative<type_num>(t.var)) {
    type_num tn = std::get<type_num>(t.var);
    switch (tn.kind) {
    case type_num::FLOATING:
      switch (tn.size()) {
      case 2:
        os << std::setw(8) << std::fixed << std::setprecision(3) <<
          half_to_float(read_unaligned<cl_half>(ptr));
        break;
      case 4:
        os << std::setw(10) << std::fixed << std::setprecision(5) <<
          read_unaligned<float>(ptr);
        break;
      case 8:
        os << std::setw(12) << std::fixed << std::setprecision(8) <<
          read_unaligned<double>(ptr);
        break;
      }
      break;
    case type_num::SIGNED:
      switch (tn.size()) {
      case 1:
        os << std::setw(4) << read_unaligned<int8_t>(ptr);
        break;
      case 2:
        os << std::setw(6) << read_unaligned<int16_t>(ptr);
        break;
      case 4:
        os << std::setw(12) << read_unaligned<int32_t>(ptr);
        break;
      case 8:
        os << std::setw(22) << read_unaligned<int64_t>(ptr);
        break;
      }
      break;
    case type_num::UNSIGNED:
      switch (tn.size()) {
      case 1:
        os << "0x" << std::setw(2) << std::setfill('0') <<
          read_unaligned<uint8_t>(ptr);
        break;
      case 2:
        os << "0x" << std::setw(4) << std::setfill('0') <<
          read_unaligned<uint16_t>(ptr);
        break;
      case 4:
        os << "0x" << std::setw(8) << std::setfill('0') <<
          read_unaligned<uint32_t>(ptr);
        break;
      case 8:
        os << "0x" << std::setw(16) << std::setfill('0') <<
          read_unaligned<uint64_t>(ptr);
        break;
      }
      break;
    }
  // } else if (std::holds_alternative<type_enum>(t.var)) {
    // type_enum te = std::get<type_enum>(t.var);
    // ss << std::setw(12) << read_unaligned<int32_t>(ptr);
  } else if (std::holds_alternative<type_builtin>(t.var)) {
    const type_builtin &tbi = std::get<type_builtin>(t.var);
    if (std::get<type_ptr>(t.var).size() == 4) {
      os << "0x" << std::setw(8) << std::setfill('0') <<
        read_unaligned<uint32_t>(ptr);
    } else {
      os << "0x" << std::setw(16) << std::setfill('0') <<
        read_unaligned<uint64_t>(ptr);
    }
  } else if (std::holds_alternative<type_struct>(t.var)) {
    const type_struct &ts = std::get<type_struct>(t.var);
    os << "{";
    const uint8_t *struct_ptr = (const uint8_t *)ptr;
    for (size_t i = 0; i < ts.elements_length; i++) {
      if (i > 0)
        os << ",";
      formatBufferElement(os, *ts.elements[i], struct_ptr);
      struct_ptr += ts.elements[i]->size();
    }
    os << "}";
  } else if (std::holds_alternative<type_union>(t.var)) {
    const type_union &tu = std::get<type_union>(t.var);
    os << "{";
    for (size_t i = 0; i < tu.elements_length; i++) {
      if (i > 0)
        os << "|";
      formatBufferElement(os, *tu.elements[i], ptr);
    }
    os << "}";
  } else if (std::holds_alternative<type_ptr>(t.var)) {
    if (std::get<type_ptr>(t.var).size() == 4) {
      os << "0x" << std::setw(8) << std::setfill('0') <<
        read_unaligned<uint32_t>(ptr);
    } else {
      os << "0x" << std::setw(16) << std::setfill('0') <<
        read_unaligned<uint64_t>(ptr);
    }
  } else {
    os << "formatBufferElement<" << t.syntax() << ">?";
  }
}

void cls::k::formatBuffer(
  std::ostream &os,
  const void *buffer,
  size_t buffer_length_in_bytes,
  const type &buffer_type)
{
  const size_t max_cols = sys::get_terminal_width();
  size_t curr_col = 1;
  const uint8_t *base = (const uint8_t *)buffer;
  const uint8_t *curr = base;

  auto startNewLine = [&] {
    os << std::setw(5) << std::setfill('0') << std::hex << (curr - base);
    os << ": ";
    curr_col = 7;
  };
  auto fmtElem = [&] (const type &t) {
    std::stringstream ss;
    formatBufferElement(ss, t, curr);
    if (curr_col + ss.tellp() > max_cols) {
      startNewLine();
    }
    os << ss.str();
    curr += t.size();
  };

  if (std::holds_alternative<type_ptr>(buffer_type.var)) {
    while (curr < base + buffer_length_in_bytes) {
      if (curr_col > max_cols) {
        os << "\n";
        startNewLine();
      }
      fmtElem(*std::get<type_ptr>(buffer_type.var).element_type);
    }
  } else {
    fmtElem(buffer_type);
  }
}