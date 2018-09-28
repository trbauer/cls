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
  private:
    template <typename...Ts>
    bool lookingAtSeqHelper(int ix) const {
      return true;
    }
    template <typename...Ts>
    bool lookingAtSeqHelper(int ix, lexeme lxm, Ts...ts) const {
      return lookingAt(lxm,ix) && lookingAtSeqHelper(ix+1,ts...);
    }
     template <typename...Ts>
    bool lookingAtSeqHelper(int ix, const char *lxm, Ts...ts) const {
      return lookingAtIdentEq(lxm,ix) && lookingAtSeqHelper(ix+1,ts...);
    }
  public:
    parser(const std::string &input, bool omit_newlines = false)
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

      lexeme lxm;
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

        if (lxm != lexeme::NEWLINE || !omit_newlines)
          m_tokens.emplace_back(lxm, lno, col, off, len);
        if (lxm != STRLIT)
          inpOff += len;
      } // while

      yylex_destroy(yy);
    } // Parser::Parser
    parser(const parser &p) = delete;
    parser& operator=(parser const&) = delete;
    parser() = delete;

    template <typename...Ts>
    void fatal(Ts... ts) const {
      fatalAt(nextLoc(),ts...);
    }
    template <typename...Ts>
    void warning(Ts... ts) const {
      warningAt(nextLoc(),ts...);
    }

    bool endOfFile() const {
      return m_tokens[m_offset].lexeme == END_OF_FILE;
    }
    size_t tokensLeft() const {
      return m_tokens.size() - m_offset;
    }

    std::string tokenString() const {
      auto loc = nextLoc();
      return input().substr(loc.offset, loc.extent);
    }
    std::string tokenStringLiteral() const {
      if (!lookingAt(STRLIT)) {
        fatal("expected string literal");
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
            loc.extent = 2;
            fatalAt(loc, "invalid escape sequence in string literal");
          }
          }
          i++;
        } else if (str[i] == '\n') {
            fatal("newline in string literal");
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
    template <typename...Ts>
    bool lookingAtSeq(Ts...ts) const {
      return lookingAtSeqHelper(0,ts...);
    }
    bool lookingAtSymbol(const char *sym) const {
      return tokenString() == sym;
    }
    bool lookingAtIdent(int i = 0) const {
      return next(i).lexeme == IDENT;
    }
    bool lookingAtIdentEq(const char *v, int i = 0) {
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
    bool lookingAtIdentEq(const char *v1, const char *v2, int i = 0) {
      return lookingAtIdentEq(v1, i) || lookingAtIdentEq(v2, i);
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
    std::string consumeIdent(const char *group = "identifier") {
      if (!lookingAt(IDENT)) {
        fatal("expected ", group);
      }
      auto s = tokenString();
      skip();
      return s;
    }
    void consumeIdentEq(const char *kw, const char *group = "identifier") {
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

    bool consumeIfIdentEq(const char *ident) {
      if(lookingAtIdentEq(ident)) {
        skip();
        return true;
      }
      return false;
    }
    bool consumeIfIdentEq(const char *ident1, const char *ident2) {
      if(lookingAtIdentEq(ident1) || lookingAtIdentEq(ident2)) {
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