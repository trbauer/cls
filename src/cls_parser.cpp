#include "cls.hpp"
#include "svm.hpp"
#include "system.hpp"
#include "text.hpp"

#define YY_DECL cls::Lexeme yylex (yyscan_t yyscanner, unsigned &inp_off, unsigned &strlit_off)
#ifndef YY_NO_UNISTD_H
#define YY_NO_UNISTD_H
#endif
#include "cls_lex.yy.hpp"
YY_DECL;

#include <sstream>
#include <iostream>
// #include <filesystem>
// using namespace std::tr2::sys;
// namespace fs = std::tr2::sys;
#include <experimental/filesystem>
namespace fs = std::experimental::filesystem;

using namespace cls;

loc loc::INVALID(0, 0, 0, 0);

static const char *LexemeString(const Lexeme &l)
{
#define CLS_LEXEME_TOKEN(T) case Lexeme::T: return #T
  switch (l) {
    CLS_LEXEME_TOKEN(LEXICAL_ERROR);
    CLS_LEXEME_TOKEN(NEWLINE);

    CLS_LEXEME_TOKEN(LANGLE); // <
    CLS_LEXEME_TOKEN(RANGLE); // >
    CLS_LEXEME_TOKEN(LBRACK); // [
    CLS_LEXEME_TOKEN(RBRACK); // ]
    CLS_LEXEME_TOKEN(LBRACE); // {
    CLS_LEXEME_TOKEN(RBRACE); // }
    CLS_LEXEME_TOKEN(LPAREN); // (
    CLS_LEXEME_TOKEN(RPAREN); // )

    CLS_LEXEME_TOKEN(AMP);    // &
    CLS_LEXEME_TOKEN(PIPE);   // |
    CLS_LEXEME_TOKEN(DOT);    // .
    CLS_LEXEME_TOKEN(COMMA);  // ,
    CLS_LEXEME_TOKEN(SEMI);   // ;
    CLS_LEXEME_TOKEN(COLON);  // :

    CLS_LEXEME_TOKEN(TILDE);  // ~

    CLS_LEXEME_TOKEN(BANG);   // !
    CLS_LEXEME_TOKEN(AT);     // @
    CLS_LEXEME_TOKEN(HASH);   // #
    CLS_LEXEME_TOKEN(EQ);     // =

    CLS_LEXEME_TOKEN(MUL);    // *
    CLS_LEXEME_TOKEN(DIV);    // /
    CLS_LEXEME_TOKEN(MOD);    // %
    CLS_LEXEME_TOKEN(ADD);    // +
    CLS_LEXEME_TOKEN(SUB);    // -
    CLS_LEXEME_TOKEN(LSH);    // <<
    CLS_LEXEME_TOKEN(RSH);    // >>

    CLS_LEXEME_TOKEN(IDENT);  // [_a-zA-Z][_a-zA-Z0-9]*
    CLS_LEXEME_TOKEN(INTLIT02); // an integral pattern
    CLS_LEXEME_TOKEN(INTLIT10); // an integral pattern
    CLS_LEXEME_TOKEN(INTLIT16); // an integral pattern
    CLS_LEXEME_TOKEN(FLTLIT); // a floating point pattern

    CLS_LEXEME_TOKEN(END_OF_FILE);
#undef CLS_LEXEME_TOKEN
  default: return "?";
    //  default: {
    //    static char buf[16];
    //    sprintf(buf,"%d?",(int)l);
    //    return buf;
    //    }
  }
}

static const char *LexemeSyntax(const Lexeme &l)
{
#define CLS_LEXEME_TOKEN(T,S) case Lexeme::T: return S;
  switch (l) {
  CLS_LEXEME_TOKEN(LEXICAL_ERROR,"ERROR");
  CLS_LEXEME_TOKEN(NEWLINE,"newline");

  CLS_LEXEME_TOKEN(LANGLE,"<"); // <
  CLS_LEXEME_TOKEN(RANGLE,">"); // >
  CLS_LEXEME_TOKEN(LBRACK,"["); // [
  CLS_LEXEME_TOKEN(RBRACK,"]"); // ]
  CLS_LEXEME_TOKEN(LBRACE,"{"); // {
  CLS_LEXEME_TOKEN(RBRACE,"}"); // }
  CLS_LEXEME_TOKEN(LPAREN,"("); // (
  CLS_LEXEME_TOKEN(RPAREN,")"); // )

  CLS_LEXEME_TOKEN(AMP,"&");    // &
  CLS_LEXEME_TOKEN(PIPE,"|");   // |
  CLS_LEXEME_TOKEN(DOT,".");    // .
  CLS_LEXEME_TOKEN(COMMA,",");  // ,
  CLS_LEXEME_TOKEN(SEMI,";");   // ;
  CLS_LEXEME_TOKEN(COLON,":");  // :

  CLS_LEXEME_TOKEN(TILDE,"~");  // ~

  CLS_LEXEME_TOKEN(BANG,"!");   // !
  CLS_LEXEME_TOKEN(AT,"@");     // @
  CLS_LEXEME_TOKEN(HASH,"#");   // #
  CLS_LEXEME_TOKEN(EQ,"=");     // =

  CLS_LEXEME_TOKEN(MUL,"*");    // *
  CLS_LEXEME_TOKEN(DIV,"/");    // /
  CLS_LEXEME_TOKEN(MOD,"%");    // %
  CLS_LEXEME_TOKEN(ADD,"+");    // +
  CLS_LEXEME_TOKEN(SUB,"-");    // -
  CLS_LEXEME_TOKEN(LSH,"<<");    // <<
  CLS_LEXEME_TOKEN(RSH,">>");    // >>

  CLS_LEXEME_TOKEN(IDENT,"identifier");  // [_a-zA-Z][_a-zA-Z0-9]*
  CLS_LEXEME_TOKEN(INTLIT02,"int"); // an integral pattern
  CLS_LEXEME_TOKEN(INTLIT10,"int"); // an integral pattern
  CLS_LEXEME_TOKEN(INTLIT16,"int"); // an integral pattern
  CLS_LEXEME_TOKEN(FLTLIT,"float"); // a floating point pattern

  CLS_LEXEME_TOKEN(END_OF_FILE,"<<EOF>>");
#undef CLS_LEXEME_TOKEN
  default: return "?";
    //  default: {
    //    static char buf[16];
    //    sprintf(buf,"%d?",(int)l);
    //    return buf;
    //    }
  }
}

std::string diag::toString() const
{
  std::stringstream ss;
  if (location != loc::INVALID) {
    ss << location.line << "." << location.col << ": ";
  }
  ss << message << "\n";

  size_t off = location.offset - (location.col - 1);
  while (off < input.length() && input[off] != '\n' && input[off] != '\r') {
    ss << input[off++];
  }
  ss << "\n";
  if (location.col > 0) {
    for (size_t i = 0; i < location.col - 1; i++) {
      ss << ' ';
    }
  }
  ss << "^\n";
  return ss.str();
}

struct Token {
  Lexeme lexeme;
  loc    loc;

  Token() : lexeme(Lexeme::LEXICAL_ERROR) { }
  Token(
    const Lexeme &lxm,
    uint32_t ln,
    uint32_t cl,
    uint32_t off,
    uint32_t len) : lexeme(lxm), loc(ln, cl, off, len)
  {
  }
};

class Parser {
  std::vector<Token>  m_tokens;
  size_t              m_offset;
  Token               m_eof;
  std::string         m_input;
public:
  Parser(const std::string &inp)
    : m_offset(0)
    , m_eof(Lexeme::END_OF_FILE, 0, 0, 0, 0)
    , m_input(inp)
  {
    yyscan_t yy;

    yylex_init(&yy);
    yy_scan_string(inp.c_str(), yy);
    yyset_lineno(1, yy);
    yyset_column(1, yy);

    unsigned inpOff = 0, bolOff = 0;
    unsigned strLitOff;

    Lexeme lxm;
    while (true) {
      lxm = yylex(yy, inpOff, strLitOff);
      uint32_t lno = (uint32_t)yyget_lineno(yy);
      uint32_t len =
        (lxm == cls::Lexeme::STRLIT) ?
          inpOff - strLitOff : (uint32_t)yyget_leng(yy);
      uint32_t col = (uint32_t)yyget_column(yy) - len;
      uint32_t off =
        (lxm == cls::Lexeme::STRLIT) ?
          strLitOff : (uint32_t)inpOff;
      if (lxm == cls::Lexeme::LEXICAL_ERROR) {
        fatalAt(loc(lno,col,off,len), "lexical error");
      }

      if (lxm == Lexeme::NEWLINE) {
        // flex increments yylineno and clear's column before this
        // we fix this by backing up the newline for that case
        // and inferring the final column from the beginning of
        // the last line
        lno--;
        col = inpOff - bolOff + 1;
        bolOff = inpOff;
      }
      // const char *str = yyget_text(yy);
      // printf("AT %u.%u(%u:%u:\"%s\"): %s\n",
      //  lno,col,off,len,str,LexemeString(lxm));
      // struct Loc loc(lno,col,off,len);
      // ShowToken(inp,loc,std::cout);

      if (lxm == Lexeme::END_OF_FILE) {
        m_eof = Token(lxm, lno, col, off, len); // update EOF w/ loc
        m_tokens.push_back(m_eof);
        break;
      }
      m_tokens.emplace_back(lxm, lno, col, off, len);
      if (lxm != cls::Lexeme::STRLIT)
        inpOff += len;
    }

    yylex_destroy(yy);
  } // CLSParser::CLSParser

  template <typename...Ts>
  void fatal(Ts... ts) {
    fatalAt(nextLoc(),ts...);
  }
  template <typename...Ts>
  void fatalAt(loc loc, Ts... ts) {
    std::stringstream ss;
    text::format_to(ss, ts...);
    throw diag(loc, ss.str(), m_input);
  }

  /*
  template <typename T1>
  void fatal(const T1 &t1) {
    fatalAt(nextLoc(),t1);
  }
  template <typename T1,typename T2>
  void fatal(const T1 &t1,const T2 &t2) {
    fatalAt(nextLoc(),t1,t2);
  }
  template <typename T1,typename T2,typename T3>
  void fatal(const T1 &t1,const T2 &t2,const T3 &t3) {
    fatalAt(nextLoc(),t1,t2,t3);
  }
  template <typename T1,typename T2,typename T3,typename T4>
  void fatal(const T1 &t1,const T2 &t2,const T3 &t3,const T4 &t4) {
    fatalAt(nextLoc(),t1,t2,t3,t4);
  }
  template <typename T1,typename T2,typename T3,typename T4,typename T5>
  void fatal(const T1 &t1,const T2 &t2,const T3 &t3,const T4 &t4,const T5 &t5) {
    fatalAt(nextLoc(),t1,t2,t3,t4,t5);
  }

  template <typename T, typename...Ts>
  void appendAll(std::stringstream &ss, T t, Ts...ts) {
    ss << t;
    appendAll(ss,ts...);
  }
  template <typename T, typename...Ts>
  void appendAll(std::stringstream &ss, T t) {
    ss << t;
  }
  template <typename T1>
  void fatalAt(loc loc, const T1 &t1) {
    std::stringstream ss;
    ss << t1;
    throw diag(loc, ss.str(), m_input);
  }
  template <typename T1,typename T2>
  void fatalAt(loc loc, const T1 &t1, const T2 &t2) {
    std::stringstream ss;
    ss << t1;
    ss << t2;
    throw diag(loc, ss.str(), m_input);
  }
  template <typename T1,typename T2,typename T3>
  void fatalAt(loc loc, const T1 &t1, const T2 &t2, const T3 &t3) {
    std::stringstream ss;
    ss << t1;
    ss << t2;
    ss << t3;
    throw diag(loc, ss.str(), m_input);
  }
  template <typename T1,typename T2,typename T3,typename T4>
  void fatalAt(loc loc, const T1 &t1, const T2 &t2, const T3 &t3, const T4 &t4) {
    std::stringstream ss;
    ss << t1;
    ss << t2;
    ss << t3;
    ss << t4;
    throw diag(loc, ss.str(),m_input);
  }
  template <typename T1,typename T2,typename T3,typename T4,typename T5>
  void fatalAt(loc loc, const T1 &t1, const T2 &t2, const T3 &t3, const T4 &t4, const T5 &t5) {
    std::stringstream ss;
    ss << t1;
    ss << t2;
    ss << t3;
    ss << t4;
    ss << t5;
    throw diag(loc, ss.str(), m_input);
  }
  */

  const std::string &input() const {return m_input;}

  bool endOfFile() const {
    return m_tokens[m_offset].lexeme == Lexeme::END_OF_FILE;
  }
  std::string tokenString() const {
    auto loc = nextLoc();
    return input().substr(loc.offset, loc.extent);
  }
  std::string tokenStringLiteral() const {
    if (!lookingAt(STRLIT)) {
      FATAL("INTERNAL ERROR: need to be looking at strlit\n");
    }
    auto str = tokenString();
    std::stringstream ss;
    for (size_t i = 1; i < str.length() - 1; i++) {
      if (str[i] == '\\') {
        ss << str[i + 1];
        i++;
      } else {
        ss << str[i];
      }
    }
    return ss.str();
  }
  const Token &next(int i = 0) const {
    int k = (int)m_offset + i;
    if (k < 0 || k >= (int)m_tokens.size()) {
      return m_eof;
    } else {
      return m_tokens[k];
    }
  }
  const loc &nextLoc(int i = 0) const {
    return next(i).loc;
  }
  bool skip(int i = 1) {
    int k = (int)m_offset + i;
    if (k < 0 || k >= (int)m_tokens.size()) {
      return false;
    }
    m_offset = (size_t)k;
    return true;
  }
  bool lookingAt(Lexeme lx, int i = 0) const {
    return next(i).lexeme == lx;
  }
  bool lookingAtIdent(int i = 0) const {
    return next(i).lexeme == IDENT;
  }
  bool lookingAtIdent(const char *v, int i = 0) {
    if (!lookingAt(IDENT)) {
      return false;
    }
    auto loc = nextLoc();
    auto slen = strlen(v);
    if (loc.extent > slen)
      return false;
    auto str = input().c_str() + loc.offset;
    if (strncmp(str,v,slen) != 0) {
      return false;
    }
    return true;
  }
  bool lookingAtIdent(const char *v1, const char *v2, int i = 0) {
    return lookingAtIdent(v1, i) || lookingAtIdent(v2, i);
  }
  bool lookingAtInt() const {
    return lookingAt(INTLIT02) || lookingAt(INTLIT10) || lookingAt(INTLIT16);
  }
  bool lookingAtFloat() const {
    return lookingAt(FLTLIT);
  }

  void consume(Lexeme lx) {
    if (lookingAt(lx)) {
      (void)skip();
    } else {
      fatal("expected ", LexemeSyntax(lx));
    }
  }
  bool consumeIf(Lexeme lx) {
    if (lookingAt(lx)) {
      (void)skip();
      return true;
    }
    return false;
  }
  void consumeOneOf(const char *expected, Lexeme lx1, Lexeme lx2) {
    consumeOneOf(expected, lx1, lx2, lx2);
  }
  void consumeOneOf(const char *expected, Lexeme lx1, Lexeme lx2, Lexeme lx3) {
    if (lookingAt(lx1) || lookingAt(lx2) || lookingAt(lx3)) {
      (void)skip();
    } else {
      std::string str;
      if (expected == nullptr) {
        std::stringstream ss;
        ss << "expected " << LexemeString(lx1) << ", " <<
          LexemeString(lx2) << " or " << LexemeString(lx3);
        str = ss.str();
      } else {
        str = expected;
      }
      fatal(str.c_str());
    }
  }
  std::string consumeIdentStringAs(const char *group = "identifier") {
    if (!lookingAt(IDENT)) {
      fatal("expected ", group);
    }
    auto s = tokenString();
    skip();
    return s;
  }
  void consumeIdentAs(const char *kw, const char *group = "identifier") {
    if (!lookingAt(IDENT)) {
      fatal("expected \"", group, "\"");
    }
    auto loc = nextLoc();
    auto str = input().c_str() + loc.offset;
    if (strncmp(str,kw,strlen(kw)) != 0) {
      fatal("expected \"", group, "\"");
    }
    skip();
  }
  bool consumeIfIdent(const char *ident) {
    if(lookingAtIdent(ident)) {
      skip();
      return true;
    }
    return false;
  }
  bool consumeIfIdent(const char *ident1, const char *ident2) {
    if(lookingAtIdent(ident1,ident2)) {
      skip();
      return true;
    }
    return false;
  }
  int64_t consumeInt() {
    if (!lookingAtInt()) {
      fatal("expected int");
    }
    std::stringstream ss(tokenString());
    int64_t x;
    ss >> x;
    skip();
    return x;
  }
  double consumeFloat() {
    if (!lookingAtFloat()) {
      fatal("expected float");
    }
    std::stringstream ss(tokenString());
    double x;
    ss >> x;
    skip();
    return x;
  }
}; // class Parser

std::string arg::toSyntax() const {
  std::stringstream ss;
  if (has_const) {
    ss << "const ";
  }
  switch (addr_qual) {
  case CL_KERNEL_ARG_ADDRESS_GLOBAL:
    ss << "global ";  break;
  case CL_KERNEL_ARG_ADDRESS_LOCAL:
    ss << "local ";  break;
  case CL_KERNEL_ARG_ADDRESS_CONSTANT:
    ss << "constant ";  break;
  case CL_KERNEL_ARG_ADDRESS_PRIVATE:
    break;
  }
  switch (accs_qual) {
  case CL_KERNEL_ARG_ACCESS_READ_ONLY:
    ss << "read_only "; break;
  case CL_KERNEL_ARG_ACCESS_WRITE_ONLY:
    ss << "write_only "; break;
  case CL_KERNEL_ARG_ACCESS_READ_WRITE:
    ss << "read_write "; break;
  case CL_KERNEL_ARG_ACCESS_NONE:
    break;
  }
  ss << type_name;
  if (has_restrict) {
    ss << " restrict";
  }
  ss << " " << name;
  return ss.str();
}


// used to parse raw opencl code
struct CLParser : Parser {
  ndr &clc;
  const size_t device_ptr_size;
  CLParser(ndr &_clc, size_t ptr_size)
    : Parser(_clc.prg.source), clc(_clc), device_ptr_size(ptr_size)
  {
  }

  void parse() {
    while (!endOfFile()) {
      findNextKernel();
    }
  }
  void findNextKernel() {
    // TODO: handle preprocessor line adjustments so we can report good
    // errors on pre-processed results
    // e.g.
    // # 1 "<built-in>" 2
    // # 1 "..\\..\\..\\tests\\pp.cl" 2
    while (!lookingAtIdent("__kernel","kernel")) {
      if (lookingAt(END_OF_FILE)) {
        return;
      }
      skip(); // skip until kernel
    }

    loc kloc = nextLoc();
    clc.prg.kernels.emplace_back(kloc,&clc.prg);
    kernel &k = clc.prg.kernels.back();
    skip(); // kernel
    consumeIdentAs("void","void");
    k.name = consumeIdentStringAs("kernel name");
    consume(LPAREN);
    if (!lookingAt(RPAREN)) {
      parseArg(k);
      while (consumeIf(COMMA)) {
        parseArg(k);
      }
    }
    consume(RPAREN);
  }

  void parseArg(kernel &k) {
    k.args.emplace_back(nextLoc());
    arg &ka = k.args.back();

    ka.has_const = consumeIfIdent("const");

    if (consumeIfIdent("global","__global")) {
      ka.addr_qual = CL_KERNEL_ARG_ADDRESS_GLOBAL;
    } else if (consumeIfIdent("constant","__constant")) {
      ka.addr_qual = CL_KERNEL_ARG_ADDRESS_CONSTANT;
    } else if (consumeIfIdent("local","__local")) {
      ka.addr_qual = CL_KERNEL_ARG_ADDRESS_LOCAL;
    } else if (consumeIfIdent("private","__private")) {
      ka.addr_qual = CL_KERNEL_ARG_ADDRESS_LOCAL;
    } else {
      ka.addr_qual = CL_KERNEL_ARG_ADDRESS_PRIVATE;
    }

    ka.has_const |= consumeIfIdent("const");

    if (consumeIfIdent("read_only") || consumeIfIdent("__read_only")) {
      ka.accs_qual = CL_KERNEL_ARG_ACCESS_READ_ONLY;
    } else if (consumeIfIdent("write_only") || consumeIfIdent("__write_only")) {
      ka.accs_qual = CL_KERNEL_ARG_ACCESS_WRITE_ONLY;
    } else if (consumeIfIdent("read_write") || consumeIfIdent("__read_write")) {
      ka.accs_qual = CL_KERNEL_ARG_ACCESS_READ_WRITE;
    } else {
      ka.accs_qual = CL_KERNEL_ARG_ACCESS_NONE;
    }

    bool pointer = false;
    ka.elem_name = consumeIdentStringAs("qualifier or type");
    bool explicit_signed_or_unsigned = false;
    if (ka.elem_name == "unsigned" || ka.elem_name == "signed") {
      explicit_signed_or_unsigned = true;
      std::string type = consumeIdentStringAs("type");
      ka.elem_name += ' ';
      ka.elem_name += type;
    }

    ka.elem_class = INVALID;
    ka.vec_width = 1;
    ka.elem_signed = false;
    if (ka.elem_name == "bool") {
      ka.elem_class = OTHER;
    } else if (ka.elem_name == "char" ||
      ka.elem_name == "uchar" ||
      ka.elem_name == "unsigned char")
    {
      ka.elem_class = INTEGRAL;
      ka.elem_size = 1;
      ka.elem_signed = ka.elem_name[0] != 'u';
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
    {
      ka.elem_class = INTEGRAL;
      ka.elem_size = device_ptr_size;
      ka.elem_signed =
        ka.elem_name[0] != 's' && ka.elem_name[0] != 'u';
    } else if (ka.elem_name == "void") {
      ka.elem_class = OTHER;
      ka.elem_size = 0;
    } else if (ka.elem_name == "image2d_t") {
      ka.elem_class = IMAGE2D;
      ka.elem_size = 0;
    } else if (ka.elem_name == "image3d_t") {
      ka.elem_class = IMAGE3D;
      ka.elem_size = 0;
    } else if (ka.elem_name == "image2d_array_t") {
      ka.elem_class = IMAGE2D_ARRAY;
      ka.elem_size = 0;
    } else if (ka.elem_name == "image1d_t") {
      ka.elem_class = IMAGE1D;
      ka.elem_size = 0;
    } else if (ka.elem_name == "image1d_buffer_t") {
      ka.elem_class = IMAGE1D_BUFFER;
      ka.elem_size = 0;
    } else if (ka.elem_name == "image1d_array_t") {
      ka.elem_class = IMAGE1D_ARRAY;
      ka.elem_size = 0;
    } else if (ka.elem_name == "sampler_t") {
      ka.elem_class = SAMPLER;
      ka.elem_size = sizeof(cl_sampler);
    } else if (ka.elem_name == "event_t") {
      ka.elem_class = EVENT;
      ka.elem_size = sizeof(cl_event);
    } else {
      // Try it as a vector type (e.g. uint4)
      if (!explicit_signed_or_unsigned) {
        std::string vectype;
        size_t toff = 0;
        while (toff < ka.elem_name.size() && isalpha(ka.elem_name[toff])) {
          vectype += ka.elem_name[toff++];
        }
        size_t width = 0;
        while (toff < ka.elem_name.size() && isdigit(ka.elem_name[toff])) {
          width = width * 10 + ka.elem_name[toff++] - '0';
        }

        ka.elem_size = 0;
        bool is_vec_type = true;
        if (width == 2 || width == 3 || width == 4 || width == 8 || width == 16) {
          if (vectype == "char" || vectype == "uchar") {
            ka.elem_class = INTEGRAL;
            ka.type_class = VECTOR;
            ka.elem_size = 1;
          } else if (vectype == "short" || vectype == "ushort") {
            ka.elem_class = INTEGRAL;
            ka.elem_size = 2;
          } else if (vectype == "int" || vectype == "uint") {
            ka.elem_class = INTEGRAL;
            ka.elem_size = 4;
          } else if (vectype == "long" || vectype == "ulong") {
            ka.elem_class = INTEGRAL;
            ka.elem_size = 8;
          } else if (vectype == "half") {
            ka.elem_class = FLOATING;
            ka.elem_size = 2;
          } else if (vectype == "float") {
            ka.elem_class = FLOATING;
            ka.elem_size = 4;
          } else if (vectype == "double") {
            ka.elem_class = FLOATING;
            ka.elem_size = 8;
          } else {
            // some other type (e.g. foo8)
            ka.elem_class = STRUCT;
            ka.elem_size = 0;
            is_vec_type = false;
            fatalAt(ka.location, "user struct type unsupported");
          }
        } else {
          // user-define struct type because of invalid vec size E.g. "uint7"
          ka.elem_size = 0;
          is_vec_type = false;
          fatalAt(ka.location, "user struct type unsupported");
        }
        if (is_vec_type) {
          ka.type_class = VECTOR;
          if (sizeof(cl_uint3) == sizeof(cl_uint4) && width == 3)
            width = 4; // vec3 == really 4 wide
          ka.elem_signed = vectype[0] != 'u';
          ka.vec_width = width;
        } else {
          ka.type_class = STRUCT;
          ka.vec_width = 1;
        }
      } // vector type
    } // else: type
    ka.type_class = ka.elem_class;
    ka.type_name = ka.elem_name;

    if (consumeIf(MUL)) { // *
      pointer = true;
      ka.type_name += '*';
      ka.type_class = BUFFER;
    } // if '*'

    ka.name = consumeIdentStringAs("argument name");
    // global const uchar4 *restrict input
    if (ka.name == "restrict") {
      ka.has_restrict = true;
      ka.name = consumeIdentStringAs("argument name");
    } else {
      ka.has_restrict = false;
    }

    if (ka.elem_size == 0 && ka.type_class != IMAGE2D) {
      fatalAt(ka.location, "INTERNAL ERROR: unable to parse non-zero type size");
    }
  } // parseArg
};


static const char *FindClangExe()
{
  static const char *paths [] = {
    "C:\\Progra~2\\LLVM\\bin\\clang.exe", // 32-bit Program Files (x86)
    "C:\\Progra~1\\LLVM\\bin\\clang.exe", // 64-bit
  };
  static const char *good_clang;
  if (good_clang == nullptr) {
    for (size_t i = 0; i < sizeof(paths)/sizeof(paths[0]); i++) {
      if (sys::file_exists(paths[i])) {
        good_clang = paths[i];
        break;
      }
    }
    if (good_clang == nullptr) {
      good_clang = "clang";
    }
  }

  return good_clang;
}

#if 0
std::string PreProcessPath(
  const std::string &inp_path,
  const std::string &args)
{
  // use clang.exe
  // text::ReadFileToString
  std::string temp_out_path = sys::get_temp_path("pp.cl");
  auto clang_exe = FindClangExe();
  std::stringstream ss;
  ss << clang_exe << " -E -o " <<
    temp_out_path << " " << args << " " << inp_path;
  int e = system(ss.str().c_str());
  std::string outp;
  if (e != 0) {
    WARNING("PreProcessPath: unable to preprocess with clang.exe\n");
    // punt, and just return the .cl contents
    outp = sys::read_file_text(inp_path);
  } else {
    outp = sys::read_file_text(temp_out_path);
  }
  (void)remove(temp_out_path.c_str());
  return outp;
}
#endif


// used to parse cl scripts
struct CLSParser : Parser {
  ndr *clc = nullptr;
  const cl::Device &device;
  cl::Context &context;

  CLSParser(cl::Context &c, const cl::Device &d, const std::string &inp)
    : Parser(inp), context(c), device(d)
  { }

  // foo-bar/baz.cl[-DT=int]`kernel<...>(...)
  // baz.bin`kernel<...>(...)
  ndr *parse() {
    clc = nullptr;
    try {
      auto cl_call_start = nextLoc();
      clc = new ndr;
      // cl_call_start
      clc->source = input();
      fs::path prog_path = parsePath(BACKTICK);
      clc->prg.build_opts = parseOpts();
      consume(BACKTICK);
      auto kLoc = nextLoc();
      auto knm = consumeIdentStringAs("kernel name");
      loadProgram(cl_call_start, *clc, prog_path, kLoc, knm);

      // dimension
      consume(LANGLE);
      clc->global_size = parseNDRange();
      if (consumeIf(COMMA)) {
        if (consumeIfIdent("nullptr") ||
          consumeIfIdent("NULL"))
        {
          clc->local_size = cl::NullRange;
        } else {
          clc->local_size = parseNDRange();
        }
      } else {
        clc->local_size = cl::NullRange;
      }
      consume(RANGLE);
      // arguments
      consume(LPAREN);
      int arg_ix = 0;
      if (!lookingAt(RPAREN)) {
        parseInit(*clc, arg_ix++);
        while (consumeIf(COMMA)) {
          parseInit(*clc, arg_ix++);
        }
      }
      consume(RPAREN);

      if (clc->inits.size() < clc->entry->args.size()) {
        fatal("expected more initializers");
      } else if(clc->inits.size() > clc->entry->args.size()) {
        fatal("too many initializers for kernel arguments");
      } // else: just right

      setKernelArgs(*clc);
    } catch (...) {
      if (clc) {
        delete clc;
        clc = nullptr;
      }
      throw;
    }
    return clc;
  }
 private:
  void loadProgram(
    loc ndr_loc,
    ndr &clc,
    const fs::path &prog_path,
    loc kLoc,
    const std::string &kName)
  {
    auto st = fs::status(prog_path);
    if (!fs::exists(st)) {
      fatalAt(ndr_loc, prog_path.string(), ": file not found");
    } else if (!is_regular_file(st)) {
      fatalAt(ndr_loc, prog_path.string(), ": not a regular file");
    }
    clc.prg.source = text::load_c_preprocessed(
      prog_path.string(),
      filterDefines(clc.prg.build_opts));
    try {
      CLParser p(clc, device.getInfo<CL_DEVICE_ADDRESS_BITS>()/8);
      p.parse();
      clc.prg.cl_program = new cl::Program(context, clc.prg.source);
      try {
        clc.prg.cl_program->build(clc.prg.build_opts.c_str());
        auto log = clc.prg.cl_program->getBuildInfo<CL_PROGRAM_BUILD_LOG>(device);
        VERBOSE("BUILD LOG:\n%s\n",log.c_str());
      } catch (const cl::Error &) {
        auto log = clc.prg.cl_program->getBuildInfo<CL_PROGRAM_BUILD_LOG>(device);
        fatalAt(ndr_loc, "failed to compile program:\n", log);
      }
      clc.prg.cl_program->createKernels(&clc.prg.cl_kernels);
      for (auto &k : clc.prg.kernels) {
        bool found = false;
        for (auto &cl_k : clc.prg.cl_kernels) {
          std::string knm = cl_k.getInfo<CL_KERNEL_FUNCTION_NAME>().c_str(); // WA for cl.hpp bug
          if (knm == k.name) {
            found = true;
            k.cl_kernel = &cl_k;
            if (knm == kName) {
              clc.entry = &k;
            }
            break;
          }
        }
        if (!found) {
          fatalAt(kLoc, "could not find kernel ", k.name, " in program");
        }
      }
    } catch (const diag &d) {
      std::cerr << d.toString() << "\n";
      fatalAt(ndr_loc, prog_path.string().c_str(), " error reading program");
    }
    if (!clc.entry) {
      fatalAt(kLoc, "could not find entry kernel ", kName);
    }
  }

  std::string filterDefines(const std::string &bos) const
  {
    std::stringstream ss;
    size_t i = 0, slen = bos.size();
    while (i < slen) {
      // -cl-fast-relaxed-math -D USE_UCHAR4 -D FILTER_RAD=2 -D FILTER_SIZE=5
      if (i < slen - 1 && bos[i] == '-' && bos[i+1] == 'D') {
        // -D USE_UCHAR4
        // or
        // -DFILTER_RAD=2
        if (ss.tellp() > 0) {
          ss << ' ';
        }
        ss << "-D";
        i += 2;
        while (i < slen && isspace(bos[i]))
          i++;
        while (i < slen && !isspace(bos[i]))
          ss << bos[i++];
      } else {
        i++;
      }
    }
    return ss.str();
  }

  // e.g. "foo-bar/baz.cl/"
  // or "../foo/bar/baz.cl"
  // or "/foo/bar/baz"
  std::string parsePath(cls::Lexeme endLxm) {
    if (lookingAt(STRLIT)) {
      return tokenStringLiteral();
    } else {
      // must be looking at identifiers or .
      // ./foo/bar
      //
      auto start = nextLoc();
      auto end = nextLoc();
      consumeOneOf("program (path or identifier)",IDENT,DOT,DIV);
      while (!lookingAt(endLxm) && !lookingAt(LBRACK)) {
        if (endOfFile()) {
          fatalAt(start, "unable to find end of path: `");
        }
        skip();
        end = nextLoc();
      }
      return input().substr(start.offset, end.offset - start.offset);
    }
  }

  std::string parseOpts() {
    auto start = nextLoc();
    if (consumeIf(LBRACK)) {
      auto end = nextLoc();
      while (!consumeIf(RBRACK)) {
        if (endOfFile()) {
          fatalAt(start, "unclosed build options [");
        }
        skip();
        end = nextLoc();
      }
      auto opts = input().substr(
        (start.offset + 1),
        end.offset - start.offset - 1);
      return opts;
    } else {
      return "";
    }
  }
  // 1024x1024
  // 1024 x 1024
  cl::NDRange parseNDRange() {
    size_t dims[3] = {0,0,0};

    auto xDimStr = tokenString();
    consume(INTLIT10);
    dims[0] = atoi(xDimStr.c_str());

    int rank = 1;
    while (rank < 3) {
      if (consumeIfIdent("x") && lookingAt(INTLIT10)) {
        dims[rank++] = atoi(tokenString().c_str());
        skip();
        // 1024 x 1024
        //      ^ ident x
      } else if (lookingAt(IDENT)) {
        // could be 1024x1024
        //              ^^^^^
        // strip the x off and evaluate 1024
        std::string str = tokenString();
        skip();
        if (str.size() > 0 && str[0] == 'x') {
          for (size_t i = 1; i < str.size(); i++) {
            if (!isdigit(str[i])) {
              fatal("expected y dimention value");
            }
          }
          dims[rank++] = atoi(str.substr(1).c_str());
        } else {
          fatal("expected y dimention value");
        }
      } else {
        break;
      }
    } // while

    if (rank == 3) {
      return cl::NDRange(dims[0], dims[1], dims[2]);
    } else if (rank == 2) {
      return cl::NDRange(dims[0], dims[1]);
    } else {
      return cl::NDRange(dims[0]);
    }
  }


  // expr = term (('+'|'-') term)*
  // term = fact (('*'|'/'|'%') fact)*
  // fact = ('-')? prim
  // prim = ... see below ...
  //
  void parseInit(ndr &c, size_t arg_ix) {
    if (arg_ix >= c.entry->args.size()) {
      fatal("kernel prototype only defines ", c.entry->args.size(),
        " arguments");
    }
    arg &a = c.entry->args[arg_ix];
    init *i = parseExpr(c,a);
    c.inits.push_back(i);
    if (consumeIf(COLON)) {
      // :rwp
      auto location = nextLoc();
      if (consumeIf(LBRACK)) {
        // :[1024*1024]r
        //  ^
        auto sz = parseIntegralExpr(c);
        if (sz <= 0) {
          fatalAt(location, "memory size expression must be positive");
        }
        i->explicit_element_count = (size_t)sz;
        consume(RBRACK);
      }
      auto attrsLoc = nextLoc();
      if (consumeIf(IDENT)) {
        const char *astr = input().c_str() + attrsLoc.offset;
        for (size_t ci = 0; ci < attrsLoc.extent; ci++) {
          switch (astr[ci]) {
          case 'r':
            i->buffer_r = true;
            break;
          case 'w':
            i->buffer_w = true;
            break;
          case 'p':
            i->display_pre = true;
            if (ci + 1 < attrsLoc.extent && astr[ci + 1] == 'x') {
              ++ci;
              i->display_pst_as_hex = true;
            }
            break;
          case 'P':
            i->display_pst = true;
            if (ci + 1 < attrsLoc.extent && astr[ci + 1] == 'x') {
              ++ci;
              i->display_pre_as_hex = true;
            }
            break;
          case 'S':
            i->save_post = true;
            break;
          case 'm':
            i->transfer = init::TRANS_MAP;
            break;
          case 'c':
            i->transfer = init::TRANS_COPY;
            break;
          case 'v':
            i->transfer = init::TRANS_SVM;
            if (ci + 1 < attrsLoc.extent && astr[ci + 1] == 'f') {
              ci++;
              i->use_svm_fine_grained = true;
            }
            if (ci + 1 < attrsLoc.extent && astr[ci + 1] == 'a') {
              ci++;
              i->use_svm_atomics = true;
            }
            break;
          default:
            fatalAt(
              loc(attrsLoc.line,
                attrsLoc.col + (uint32_t)ci, attrsLoc.offset +  (uint32_t)ci, 1),
              "invalid buffer attribute");
          }  // for switch
        } // for attr chars
        if (i->transfer == init::TRANS_INVALID) {
          // default to map :m
          i->transfer = init::TRANS_MAP;
        }
      }
    }
  }
  int64_t parseIntegralExpr(const ndr &c) {
    auto e = parseIntegralTerm(c);
    while (true) {
      if (consumeIf(ADD)) {
        e += parseIntegralTerm(c);
      } else if (consumeIf(SUB)) {
        e -= parseIntegralTerm(c);
      } else {
        break;
      }
    }
    return e;
  }
  int64_t parseIntegralTerm(const ndr &c) {
    auto e = parseIntegralFactor(c);
    while (true) {
      auto loc = nextLoc();
      if (consumeIf(MUL)) {
        e *= parseIntegralTerm(c);
      } else if (consumeIf(DIV)) {
        auto rhs = parseIntegralFactor(c);
        if (rhs == 0) {
          fatalAt(loc, "division by 0");
        }
        e /= parseIntegralTerm(c);
      } else if (consumeIf(MOD)) {
        auto rhs = parseIntegralFactor(c);
        if (rhs == 0) {
          fatalAt(loc, "mod by 0");
        }
        e %= parseIntegralFactor(c);
      } else {
        break;
      }
    }
    return e;
  }
  int64_t parseIntegralFactor(const ndr &c) {
    bool neg = consumeIf(SUB);
    auto e = parseIntegralAtom(c);
    if (neg) {
      e = -e;
    }
    return e;
  }
  int64_t parseIntegralAtom(const ndr &c) {
    if (lookingAtInt()) {
      return consumeInt();
    } else if (consumeIfIdent("g")) {
      return parseNDRange(c.global_size);
    } else if (consumeIfIdent("l")) {
      return parseNDRange(c.local_size);
    } else {
      fatal("unbound symbol");
      return 0;
    }
  }
  int64_t parseNDRange(const cl::NDRange &ndr) {
    consume(DOT);
    auto loc = nextLoc();
    if (consumeIfIdent("x")) {
      if (ndr.dimensions() <= 0) {
        fatalAt(loc, "not enough dimensions for .x");
      }
      return (int64_t)ndr[0];
    } else if (consumeIfIdent("y")) {
      if (ndr.dimensions() <= 1) {
        fatalAt(loc, "not enough dimensions for .y");
      }
      return (int64_t)ndr[1];
    } else if (consumeIfIdent("z")) {
      if (ndr.dimensions() <= 2) {
        fatalAt(loc, "not enough dimensions for .z");
      }
      return (int64_t)ndr[2];
    } else {
      fatalAt(loc, "invalid dimension name for ndrange");
      return 0;
    }
  }
  void toFloat(init *ci) {
    if (ci->type == init::LIT_FLT) {
      return;
    }
    if (ci->type != init::LIT_INT) {
      fatalAt(ci->location, "cannot promote expression to floating point");
    }
    ci->type = init::LIT_FLT;
    ci->fltval = (double)ci->intval;
  }

  void toVector(init *ci, size_t elems) {
    for (size_t k = 0; k < elems; k++) {
      auto ptr = new init(ci->location);
      ptr->type = ci->type;
      if (ci->type == ci->LIT_FLT) {
        ptr->fltval = ci->fltval;
      } else if (ci->type == ci->LIT_INT) {
        ptr->intval = ci->intval;
      }
      ci->children.push_back(ptr);
    }
    ci->type = init::LIT_VEC;
  }

  void applyBinary(const Token &op, init *lhs, init *rhs) {
    if (lhs->type == init::LIT_VEC || rhs->type == init::LIT_VEC) {
      // ensure both are vectors, promote if needed
      if (lhs->type != init::LIT_VEC) {
        toVector(lhs,rhs->children.size());
      } else if (rhs->type != init::LIT_VEC) {
        toVector(rhs,lhs->children.size());
      } else if (lhs->children.size() != rhs->children.size()) {
        fatalAt(op.loc, "vectors operands different sizes");
      } // else: length matches
      for (size_t i = 0; i < lhs->children.size(); i++) {
        applyBinary(op, lhs->children[i], rhs->children[i]);
      }
    }  else if (lhs->type == init::LIT_FLT || rhs->type == init::LIT_FLT) {
      // floating point scalar op
      toFloat(lhs);
      toFloat(rhs);
      switch (op.lexeme) {
      case ADD: lhs->fltval += rhs->fltval; break;
      case SUB: lhs->fltval -= rhs->fltval; break;
      case MUL: lhs->fltval *= rhs->fltval; break;
      case DIV: lhs->fltval /= rhs->fltval; break;
      case MOD: fatalAt(op.loc, "cannot apply mod to floating point operands");
      default:
        fatalAt(op.loc, "unsupported binary operator");
      }
    } else if (lhs->type == init::LIT_INT && rhs->type == init::LIT_INT) {
      switch (op.lexeme) {
      case ADD: lhs->intval += rhs->intval; break;
      case SUB: lhs->intval -= rhs->intval; break;
      case MUL: lhs->intval *= rhs->intval; break;
      case DIV:
        if (rhs->intval == 0) {
          fatalAt(op.loc, "division by zero");
        }
        lhs->intval /= rhs->intval;
        break;
      case MOD:
        if (rhs->intval == 0) {
          fatalAt(op.loc, "mod by zero");
        }
        lhs->intval %= rhs->intval;
        break;
      default:
        fatalAt(op.loc, "unsupported binary operator");
      }
    } else {
      // unsupported type
      fatalAt(op.loc, "unsupported types for operator");
    }
  }

  init *parseExpr(const ndr &c, arg &a) {
    init *lhs = parseTerm(c,a);
    while (true) {
      auto op = next();
      if (consumeIf(ADD) || consumeIf(SUB)) {
        init *rhs = parseTerm(c,a);
        applyBinary(op, lhs, rhs);
        delete rhs;
      } else {
        break;
      }
    }
    return lhs;
  }
  init *parseTerm(const ndr &c, arg &a) {
    init *lhs = parseFact(c,a);
    while (true) {
      auto op = next();
      if (consumeIf(MUL) || consumeIf(DIV) || consumeIf(MOD)) {
        init *rhs = parseFact(c,a);
        applyBinary(op, lhs, rhs);
        delete rhs;
      } else {
        break;
      }
    }
    return lhs;
  }
  init *parseFact(const ndr &c, arg &a) {
    auto loc = nextLoc();
    bool neg = consumeIf(SUB);
    init *e = parsePrim(c, a);
    if (neg) {
      init negOne(loc);
      negOne.intval = -1;
      negOne.type = init::LIT_INT;
      if (e->type == init::LIT_VEC) {
        // e.g. -(1,2) == (-1,-2)
        // need to treat this is -1 * (-1,-2)
        // so the type of -1 must be LIT_INT or LIT_FLT
        negOne.type = e->children.front()->type;
        if (negOne.type == init::LIT_FLT) {
          negOne.fltval = -1.0f;
        }
        toVector(&negOne, e->children.size());
      }
      Token t(MUL, loc.line, loc.col, loc.offset, loc.extent);
      applyBinary(t, e, &negOne);
    }
    return e;
  }
  // prim = ilit
  //      | flit
  //      | '{' expr (',' expr)* '}'            -- struct
  //      | '(' expr ')'                        -- group
  //      | '(' expr (',' expr)* ')'            -- vector
  //      | func '(' (expr (',' expr)* )* ')'   -- function call
  //      | ('g'|'l') '.' ('x'|'y'|'z')         -- dimension
  init *parsePrim(const ndr &c, arg &a) {
    auto loc = nextLoc();
    if (lookingAtFloat()) {
      init *ci = new init(loc);
      ci->type = init::LIT_FLT;
      ci->fltval = consumeFloat();
      return ci;
    } else if (lookingAtInt()) {
      init *ci = new init(loc);
      ci->type = init::LIT_INT;
      ci->intval = consumeInt();
      return ci;
    } else if (consumeIf(LPAREN)) {
      // either grouping or vector
      init *ci_arg0 = parseExpr(c, a);
      if (consumeIf(RPAREN)) {
        // grouping expression
        return ci_arg0;
      } else if (!lookingAt(COMMA)) {
        fatal("expected , or )");
      }
      init *ci = new init(loc);
      ci->type = init::LIT_VEC;
      ci->children.push_back(ci_arg0);
      while (consumeIf(COMMA)) {
        ci->children.push_back(parseExpr(c, a));
      }
      consume(RPAREN);
      return ci;
    } else if (consumeIfIdent("g")) {
      init *ci = new init(loc);
      ci->type = init::LIT_INT;
      ci->intval = parseNDRange(c.global_size);
      return ci;
    } else if (consumeIfIdent("l")) {
      init *ci = new init(loc);
      ci->type = init::LIT_INT;
      ci->intval = parseNDRange(c.local_size);
      return ci;
    } else if (consumeIfIdent("rnd") || consumeIfIdent("rand")) {
      init *ci = new init(loc);
      ci->type = init::LIT_RND;
      if (consumeIf(LPAREN)) {
        if (lookingAtInt()) {
          ci->rand_seed = (int32_t)parseIntegralExpr(c);
        }
        consume(RPAREN);
      } else {
        ci->rand_seed = 0;
      }
      return ci;
    } else if (consumeIfIdent("cyc")) {
      init *ci = new init(loc);
      ci->type = init::LIT_CYC;
      consume(LPAREN);
      ci->cycle_values.push_back(parseIntegralExpr(c));
      while (consumeIf(COMMA)) {
        ci->cycle_values.push_back(parseIntegralExpr(c));
      }
      consume(RPAREN);
      return ci;
    } else if (consumeIfIdent("seq")) {
      init *ci = new init(loc);
      ci->type = init::LIT_SEQ;
      ci->seq_start = 0;
      ci->seq_delta = 1;
      if (consumeIf(LPAREN)) {
        ci->seq_start = parseIntegralExpr(c);
        if (consumeIf(COMMA)) {
          ci->seq_delta = parseIntegralExpr(c);
        }
        consume(RPAREN);
      }
      return ci;
    } else if (lookingAt(STRLIT)) {
      init *ci = new init(loc);
      ci->type = init::LIT_FILE;
      ci->fileval = parsePath(COLON);
      skip();
      return ci;
    } else {
      fatal("syntax error in argument initializer");
      return nullptr;
    }
  } // parsePrim

  void setKernelArgs(ndr &c) {
    kernel &k = *c.entry;
    for (cl_uint arg_ix = 0, len = (cl_uint)k.args.size();
      arg_ix < len;
      arg_ix++)
    {
      arg &a = k.args[arg_ix];
      init &i = *c.inits[arg_ix];
      ErrorHandler eh = [&](const char *msg) {
        fatalAt(i.location, "setting kernel arg: ", msg);
      };
      i.setKernelArg(c, k, a, arg_ix, &context, eh);
    } // for arg_ix
  } // setKernelArgs
};

ndr *cls::ParseCLS(
  cl::Context &ctx,
  const cl::Device &dev,
  const std::string& inp)
{
  CLSParser p(ctx, dev, inp);
  return p.parse();
}

