#ifndef PARSER_PARSER_HPP
#define PARSER_PARSER_HPP

#include "lexemes.hpp"
#include "../fatal.hpp"

#define YY_DECL \
  cls::lexemes::lexeme yylex(\
    yyscan_t yyscanner, unsigned &inp_off, unsigned &strlit_off)
#ifndef YY_NO_UNISTD_H
#define YY_NO_UNISTD_H
#endif
#include "cls_lex.yy.hpp"
YY_DECL;

#include <string>
#include <vector>

using namespace cls::lexemes;

namespace cls {
  const char *to_symbol(lexeme l);
  const char *to_syntax(lexeme l);

  struct token {
    lexeme kind;
    loc    at;

    token() : kind(lexeme::LEXICAL_ERROR) { }
    token(
      lexemes::lexeme lxm,
      uint32_t ln, uint32_t cl, uint32_t off, uint32_t len)
      : kind(lxm), at(ln, cl, off, len) { }
  };

  // These have to be functions for some reason unknown to me
  // (MinGW GCC 7.2 rejects as method, VS 2017 accepts)
  //
  // I was using I token_string() within the body.
  template<typename T>
  void parse_integral_body(std::string str, int base, T &val);
  template<>
  inline void parse_integral_body(std::string str, int base, int64_t &val)
  {
    val = std::stoll(str, nullptr, base);
  }
  template<>
  inline void parse_integral_body(std::string str, int base, uint64_t &val)
  {
    val = std::stoull(str, nullptr, base);
  }
  template<>
  inline void parse_integral_body(std::string str, int base, int32_t &val)
  {
    val = std::stol(str, nullptr, base);
  }
  template<>
  inline void parse_integral_body(std::string str, int base, uint32_t &val)
  {
    val = std::stoul(str, nullptr, base);
  }

  class parser {
    cls::diagnostics   &m_diagnostics;
    const std::string  &m_input;
    std::vector<token>  m_tokens;
    size_t              m_offset;
    token               m_eof;
  private:
    // template <typename...Ts> bool looking_at_seq_helper(int ix) const {
    //  return true;
    // }
    // GCC 7.2 rules require non-empty unpack
    template <typename...Ts> bool looking_at_seq_helper(
      int ix, lexeme lxm) const
    {
      return looking_at(lxm,ix);
    }
    template <typename...Ts> bool looking_at_seq_helper(
      int ix, const char *lxm) const
    {
      return looking_at_ident_eq(lxm,ix);
    }
    //
    template <typename...Ts> bool looking_at_seq_helper(
      int ix, lexeme lxm, Ts...ts) const
    {
      return looking_at(lxm,ix) && looking_at_seq_helper(ix + 1, ts...);
    }
    template <typename...Ts> bool looking_at_seq_helper(
      int ix, const char *lxm, Ts...ts) const
    {
      return looking_at_ident_eq(lxm, ix) &&
             looking_at_seq_helper(ix + 1, ts...);
    }
  public:
    parser(
      cls::diagnostics &diags,
      const std::string &input,
      bool omit_newlines = false)
      : m_diagnostics(diags)
      , m_input(input)
      , m_offset(0)
      , m_eof(lexemes::END_OF_FILE, 0, 0, 0, 0)
    {
      yyscan_t yy;

      yylex_init(&yy);
      yy_scan_string(input.c_str(), yy);
      yyset_lineno(1, yy);
      yyset_column(1, yy);

      unsigned inp_off = 0, bol_off = 0;
      unsigned str_lit_off;

      lexeme lxm;
      while (true) {
        lxm = yylex(yy, inp_off, str_lit_off);
        uint32_t lno = (uint32_t)yyget_lineno(yy);
        uint32_t len =
          (lxm == lexemes::STRLIT) ?
            inp_off - str_lit_off : (uint32_t)yyget_leng(yy);
        uint32_t col = (uint32_t)yyget_column(yy) - len;
        uint32_t off =
          (lxm == lexemes::STRLIT) ?
            str_lit_off : (uint32_t)inp_off;
        if (lxm == lexeme::LEXICAL_ERROR) {
          fatal_at(cls::loc(lno,col,off,len), "lexical error");
        }

        if (lxm == lexeme::NEWLINE) {
          // flex increments yylineno and clear's column before this
          // we fix this by backing up the newline for that case
          // and inferring the final column from the beginning of
          // the last line
          lno--;
          col = inp_off - bol_off + 1;
          bol_off = inp_off;
        }
        // const char *str = yyget_text(yy);
        // printf("AT %u.%u(%u:%u:\"%s\"): %s\n",
        //  lno, col, off, len,str, lexeme_string(lxm));
        // struct loc at{lno, col, off, len};
        // show_token(inp, at, std::cout);

        if (lxm == END_OF_FILE) {
          m_eof = cls::token(lxm, lno, col, off, len); // update EOF w/ loc
          m_tokens.push_back(m_eof);
          break;
        }

        if (lxm != lexeme::NEWLINE || !omit_newlines)
          m_tokens.emplace_back(lxm, lno, col, off, len);
        if (lxm != STRLIT)
          inp_off += len;
      } // while

      yylex_destroy(yy);
    } // Parser::Parser
    parser(const parser &p) = delete;
    parser& operator=(parser const&) = delete;
    parser() = delete;

    // generate fatal_at, warning_at, ...
    DIAGNOSTIC_MIXIN_MEMBERS(m_diagnostics, next_loc());

    const std::string &input() const {return m_input;}

    bool end_of_file() const {
      return m_tokens[m_offset].kind == END_OF_FILE;
    }
    size_t tokens_left() const {
      return m_tokens.size() - m_offset;
    }

    std::string token_string() const {
      auto loc = next_loc();
      return input().substr(loc.offset, loc.extent);
    }
    std::string token_string_literal() const {
      if (!looking_at(STRLIT)) {
        fatal("expected string literal");
      }
      auto str = token_string();
      std::stringstream ss;
      for (size_t i = 1; i < str.length() - 1; i++) {
        if (str[i] == '\\') {
          switch (str[i + 1]) {
          case '\\': ss << '\\'; break;
          case 'n': ss << '\n'; break;
          case 't': ss << '\t'; break;
          default: {
            auto loc = next_loc();
            loc.column += (uint32_t)i;
            loc.offset += (uint32_t)i;
            loc.extent = 2;
            fatal_at(loc, "invalid escape sequence in string literal");
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
    const cls::loc &next_loc(int i = 0) const {
      return next(i).at;
    }
    bool skip(int i = 1) {
      int k = (int)m_offset + i;
      if (k < 0 || k >= (int)m_tokens.size()) {
        return false;
      }
      m_offset = (size_t)k;
      return true;
    }
    bool looking_at(lexeme lx, int i = 0) const {
      return next(i).kind == lx;
    }
    template <typename...Ts>
    bool looking_at_seq(Ts...ts) const {
      return looking_at_seq_helper(0,ts...);
    }
    bool looking_at_symbol(const char *sym) const {
      return token_string() == sym;
    }
    bool looking_at_ident(int i = 0) const {
      return next(i).kind == IDENT;
    }
    bool looking_at_ident_eq(const char *v, int i = 0) const {
      if (!looking_at(IDENT)) {
        return false;
      }
      auto loc = next_loc();
      auto slen = strlen(v);
      if (loc.extent > slen)
        return false;
      auto str = input().c_str() + loc.offset;
      if (strncmp(str,v,slen) != 0) {
        return false;
      }
      return true;
    }
    bool looking_at_ident_eq(const char *v1, const char *v2, int i = 0) {
      return looking_at_ident_eq(v1, i) || looking_at_ident_eq(v2, i);
    }
    bool looking_at_int() const {
      return looking_at(INTLIT02) || looking_at(INTLIT10) ||
             looking_at(INTLIT16);
    }
    bool looking_at_float() const {
      return looking_at(FLTLIT);
    }

    void consume(lexeme lx) {
      if (looking_at(lx)) {
        (void)skip();
      } else {
        fatal("expected ", to_syntax(lx));
      }
    }
    bool consume_if(lexeme lx) {
      if (looking_at(lx)) {
        (void)skip();
        return true;
      }
      return false;
    }
    std::string consume_ident(const char *group = "identifier") {
      if (!looking_at(IDENT)) {
        fatal("expected ", group);
      }
      auto s = token_string();
      skip();
      return s;
    }
    void consume_ident_eq(const char *kw, const char *group = "identifier") {
      if (!looking_at(IDENT)) {
        fatal("expected \"", group, "\"");
      }
      auto at = next_loc();
      if (at.extent != strlen(kw))
        fatal("expected \"", group, "\"");
      auto str = input().c_str() + at.offset;
      if (strncmp(str, kw, at.extent) != 0) {
        fatal("expected \"", group, "\"");
      }
      skip();
    }

    bool consume_if_ident_eq(const char *ident) {
      if(looking_at_ident_eq(ident)) {
        skip();
        return true;
      }
      return false;
    }
    bool consume_if_ident_eq(const char *ident1, const char *ident2) {
      if(looking_at_ident_eq(ident1) || looking_at_ident_eq(ident2)) {
        skip();
        return true;
      }
      return false;
    }

    template <typename T>
    T consume_integral(const char *what = "int") noexcept {
      T x = 0;
      try {
        if (!looking_at_int()) {
          fatal("expected ", what);
        }
        int base =
          looking_at(INTLIT10) ? 10 :
          looking_at(INTLIT16) ? 16 :
          looking_at(INTLIT02) ? 2 :
          0; // looking_at_int() => unreachable; use 0
        parse_integral_body<T>(token_string(), base, x);
      } catch (std::invalid_argument &) {
        fatal("expected ", what);
      } catch (std::out_of_range &) {
        fatal("literal out of range");
      }
      skip();
      return x;
    }
    double consume_float(const char *what = "float") {
      if (!looking_at_float()) {
        fatal("expected ",what);
      }
      double x = 0.0;
      try {
        auto str = token_string();
        char sfx = str.empty() ? 0 : str[str.size()-1];
        if (sfx == 'f' || sfx == 'F')
          x = std::stof(str.substr(0, str.size()-1));
        else
          x = std::stod(str);
      } catch (std::invalid_argument &) {
        fatal("expected ",what);
      } catch (std::out_of_range &) {
        fatal("literal out of range");
      }
      skip();
      return x;
    }
  }; // class parser
} // namespace

#endif