#ifndef FATAL_HPP
#define FATAL_HPP

#include "text.hpp"

#include <cstdint>
#include <exception>
#include <ostream>
#include <sstream>
#include <string>
#include <tuple>
#include <vector>

namespace cls
{
  struct loc {
    uint32_t line, column, offset, extent;
    loc() {}
    constexpr loc(uint32_t ln, uint32_t col, uint32_t off, uint32_t len)
      : line(ln), column(col), offset(off), extent(len) { }

    std::string str() const {
      std::stringstream ss;
      ss << line << "." << column;
      return ss.str();
    }

    void extend_to(loc end) {
      extent = end.offset - offset;
    }
  }; // loc

  void formatMessageWithContextImpl(
    std::ostream &os,
    loc at,
    const std::string &input,
    const std::string &message);

  template <typename...Ts>
  void formatMessageWithContext(
    std::ostream &os, loc at, const std::string &input, Ts... ts)
  {
    std::stringstream ss;
    text::format_to(ss, ts...);
    formatMessageWithContextImpl(os, at, input, ss.str());
  }

  struct diagnostic : std::exception {
    loc           location;
    std::string   message;
    std::string   input;

    diagnostic(loc l, const std::string &m, const std::string &inp)
      : location(l), message(m), input(inp) { }

    void          str(std::ostream &os) const;
    std::string   str() const;
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
    void fatalAt(loc loc, Ts... ts) const {
      std::stringstream ss;
      text::format_to(ss, ts...);
      throw diagnostic(loc, ss.str(), m_input);
    }
    template <typename...Ts>
    void warningAt(loc loc, Ts... ts) {
      std::stringstream ss;
      text::format_to(ss, ts...);
      m_warnings.emplace_back(loc,ss.str());
    }
  }; // fatal_handler
} // namespace cls

#endif