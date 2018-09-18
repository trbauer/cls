#ifndef PARSER_PARSER_HPP
#define PARSER_PARSER_HPP

#include "lexemes.hpp"
#include "../fatal.hpp"

#define YY_DECL \
  cls::lexemes::lexeme yylex (yyscan_t yyscanner, unsigned &inp_off, unsigned &strlit_off)
#ifndef YY_NO_UNISTD_H
#define YY_NO_UNISTD_H
#endif
#include "cls_lex.yy.hpp"
YY_DECL;

#include <vector>

using namespace cls::lexemes;

namespace cls {
  const char *to_symbol(lexeme l);
  const char *to_syntax(lexeme l);

  struct token {
    lexeme lexeme;
    loc    loc;

    token() : lexeme(lexeme::LEXICAL_ERROR) { }
    token(
      lexemes::lexeme lxm,
      uint32_t ln,
      uint32_t cl,
      uint32_t off,
      uint32_t len) : lexeme(lxm), loc(ln, cl, off, len) { }
  };

  class parser : public cls::fatal_handler {
    std::vector<token>  m_tokens;
    size_t              m_offset;
    token               m_eof;
  public:
    parser(const std::string &input)
      : fatal_handler(input)
      , m_offset(0)
      , m_eof(lexemes::END_OF_FILE, 0, 0, 0, 0)
    {
      yyscan_t yy;

      yylex_init(&yy);
      yy_scan_string(input.c_str(), yy);
      yyset_lineno(1, yy);
      yyset_column(1, yy);

      unsigned inpOff = 0, bolOff = 0;
      unsigned strLitOff;

      lexemes::lexeme lxm;
      while (true) {
        lxm = yylex(yy, inpOff, strLitOff);
        uint32_t lno = (uint32_t)yyget_lineno(yy);
        uint32_t len =
          (lxm == lexemes::STRLIT) ?
            inpOff - strLitOff : (uint32_t)yyget_leng(yy);
        uint32_t col = (uint32_t)yyget_column(yy) - len;
        uint32_t off =
          (lxm == lexemes::STRLIT) ?
            strLitOff : (uint32_t)inpOff;
        if (lxm == lexeme::LEXICAL_ERROR) {
          fatalAt(cls::loc(lno,col,off,len), "lexical error");
        }

        if (lxm == lexeme::NEWLINE) {
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

        if (lxm == END_OF_FILE) {
          m_eof = cls::token(lxm, lno, col, off, len); // update EOF w/ loc
          m_tokens.push_back(m_eof);
          break;
        }
        m_tokens.emplace_back(lxm, lno, col, off, len);
        if (lxm != STRLIT)
          inpOff += len;
      }

      yylex_destroy(yy);
    } // Parser::Parser
    parser(const parser &p) = delete;
    parser& operator=(parser const&) = delete;
    parser() = delete;

    template <typename...Ts>
    void fatal(Ts... ts) const {
      fatalAt(nextLoc(),ts...);
    }

    bool endOfFile() const {
      return m_tokens[m_offset].lexeme == END_OF_FILE;
    }
    std::string tokenString() const {
      auto loc = nextLoc();
      return input().substr(loc.offset, loc.extent);
    }
    std::string tokenStringLiteral() const {
      if (!lookingAt(STRLIT)) {
        fatal("INTERNAL ERROR: Parser::tokenStringLiteral: "
          "need to be looking at STRLIT\n");
      }
      auto str = tokenString();
      std::stringstream ss;
      for (size_t i = 1; i < str.length() - 1; i++) {
        if (str[i] == '\\') {
          switch (str[i + 1]) {
          case '\\': ss << '\\'; break;
          case 'n': ss << '\n'; break;
          case 't': ss << '\t'; break;
          default: {
            auto loc = nextLoc();
            loc.column += (uint32_t)i;
            loc.offset += (uint32_t)i;
            fatalAt(loc, "invalid escape sequence in string literal");
          }
          }
          i++;
        } else {
          ss << str[i];
        }
      }
      return ss.str();
    }
    const token &next(int i = 0) const {
      int k = (int)m_offset + i;
      if (k < 0 || k >= (int)m_tokens.size()) {
        return m_eof;
      } else {
        return m_tokens[k];
      }
    }
    const cls::loc &nextLoc(int i = 0) const {
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
    bool lookingAt(lexeme lx, int i = 0) const {
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

    void consume(lexeme lx) {
      if (lookingAt(lx)) {
        (void)skip();
      } else {
        fatal("expected ", to_syntax(lx));
      }
    }
    bool consumeIf(lexeme lx) {
      if (lookingAt(lx)) {
        (void)skip();
        return true;
      }
      return false;
    }
    void consumeOneOf(const char *expected, lexeme lx1, lexeme lx2) {
      consumeOneOf(expected, lx1, lx2, lx2);
    }
    void consumeOneOf(
      const char *expected,
      lexeme lx1,
      lexeme lx2,
      lexeme lx3)
    {
      if (lookingAt(lx1) || lookingAt(lx2) || lookingAt(lx3)) {
        (void)skip();
      } else {
        std::string str;
        if (expected == nullptr) {
          std::stringstream ss;
          ss << "expected " << to_syntax(lx1) << ", " <<
            to_syntax(lx2) << " or " << to_syntax(lx3);
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
    int64_t consumeInt(const char *what = "int") {
      if (!lookingAtInt()) {
        fatal("expected ",what);
      }
      std::stringstream ss(tokenString());
      int64_t x;
      ss >> x;
      skip();
      return x;
    }
    double consumeFloat(const char *what = "float") {
      if (!lookingAtFloat()) {
        fatal("expected ",what);
      }
      std::stringstream ss(tokenString());
      double x;
      ss >> x;
      skip();
      return x;
    }
  }; // class Parser

} // namespace

#endif