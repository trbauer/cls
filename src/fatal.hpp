#ifndef FATAL_HPP
#define FATAL_HPP

#include "text.hpp"

#include <cstdint>
#include <exception>
#include <iostream>
#include <ostream>
#include <sstream>
#include <string>
#include <tuple>
#include <vector>

#define EXIT_INTERNAL_ERROR 2

namespace cls
{
  struct loc {
    uint32_t line, column, offset, extent;
    constexpr loc() : line(0), column(0), offset(0), extent(0) {}
    constexpr loc(uint32_t ln, uint32_t col, uint32_t off, uint32_t len)
      : line(ln), column(col), offset(off), extent(len) { }

    std::string str() const {
      std::stringstream ss;
      ss << line << "." << column;
      return ss.str();
    }

    void extend_to(loc end) {extent = end.offset - offset;}
    void extend_past(loc end) {extent = end.offset - offset + end.extent;}
  }; // loc

  void formatMessageWithContextImpl(
    std::ostream &os,
    const loc &at,
    const text::ansi_literal *highlight,
    const std::string &input,
    const std::string &message);

  template <typename...Ts>
  void formatMessageWithContext(
    std::ostream &os,
    const loc &at,
    const text::ansi_literal *highlight,
    const std::string &input,
    Ts... ts)
  {
    std::stringstream ss;
    text::format_to(ss, ts...);
    formatMessageWithContextImpl(os, at, highlight, input, ss.str());
  }

  struct diagnostic : std::exception {
    loc           at;
    std::string   message;
    std::string   input;
    bool          internal_error;

    diagnostic(
      loc _at,
      const std::string &m,
      const std::string &inp,
      bool _internal_error = false)
      : at(_at)
      , message(m)
      , input(inp)
      , internal_error(_internal_error)
    { }

    void          str(std::ostream &os) const;
    std::string   str() const;

    // default handler emits to std::cerr and exits with EXIT_INTERNAL_ERROR
    [[noreturn]]
    void          default_exit() const;
  }; // diagnostic

  using warning_list = std::vector<std::tuple<loc,std::string>>;
  class fatal_handler {
    const std::string                          &m_input;
    warning_list                                m_warnings;
  public:
    fatal_handler(const std::string &input) : m_input(input) { }

    const std::string &input() const {return m_input;}
    const warning_list &warnings() const {return m_warnings;}

    template <typename...Ts>
    [[noreturn]]
    void internalAt(const loc &at, Ts... ts) const {
      std::stringstream ss;
      ss << "INTERNAL ERROR: ";
      text::format_to(ss, ts...);
      throw diagnostic(at, ss.str(), m_input, true);
    }
    template <typename...Ts>
    [[noreturn]]
    void fatalAt(const loc &at, Ts... ts) const {
      std::stringstream ss;
      text::format_to(ss, ts...);
      throw diagnostic(at, ss.str(), m_input);
    }
    template <typename...Ts>
    void warningAt(const loc &at, Ts... ts) {
      std::stringstream ss;
      text::format_to(ss, ts...);
      m_warnings.emplace_back(at,ss.str());
    }
    template <typename...Ts>
    void debugAt(const loc &at, Ts... ts) const {
      formatMessageWithContext(std::cout, at, input(), ts...);
    }
  }; // fatal_handler
} // namespace cls

#endif