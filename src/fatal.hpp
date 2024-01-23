#ifndef FATAL_HPP
#define FATAL_HPP

#include "text.hpp"

#include <cstdint>
#include <exception>
#include <iostream>
#include <ostream>
#include <string>
#include <vector>

#define EXIT_INTERNAL_ERROR 2

namespace cls
{
  [[noreturn]]
  void fatal_message_and_exit(int code, std::string s);

  template <typename...Ts>
  [[noreturn]]
  void fatal(Ts... ts) {
    cls::fatal_message_and_exit(EXIT_FAILURE, text::format(ts...));
  }
  template <typename...Ts>
  [[noreturn]]
  void fatal_internal(Ts... ts) {
    cls::fatal_message_and_exit(EXIT_INTERNAL_ERROR, text::format(ts...));
  }

  struct loc {
    uint32_t line, column, offset, extent;
    constexpr loc() : line(0), column(0), offset(0), extent(0) {}
    constexpr loc(uint32_t ln, uint32_t col, uint32_t off, uint32_t len)
      : line(ln), column(col), offset(off), extent(len) { }

    std::string str() const {return text::format(line, ".", column);}

    void extend_to(loc end) {extent = end.offset - offset;}
    void extend_past(loc end) {extent = end.offset - offset + end.extent;}

  }; // loc
  static constexpr loc NO_LOC {0,0,0,0};

  void format_message_with_context_impl(
    std::ostream &os,
    const loc &at,
    const text::ansi_literal *highlight,
    const text::ansi_literal *message_color,
    const std::string &input,
    const std::string &message);

  template <typename...Ts>
  void format_message_with_context(
    std::ostream &os,
    const loc &at,
    const text::ansi_literal *highlight,
    const text::ansi_literal *message_color,
    const std::string &input,
    Ts... ts)
  {
    std::stringstream ss;
    text::format_to(ss, ts...);
    format_message_with_context_impl(
      os, at, highlight, message_color, input, ss.str());
  }

  struct diagnostic : std::exception {
    enum error_level {WARNING, ERROR, INTERNAL} level = ERROR;
    loc           at;
    std::string   message;
    std::string   input;

    diagnostic(
      enum error_level lvl,
      loc _at,
      const std::string &m,
      const std::string &inp)
      : at(_at)
      , level(lvl)
      , message(m)
      , input(inp)
    { }

    // emits the diagnostic to an output stream
    void          str(std::ostream &os) const;
    // allows us to override the input
    void          str(std::ostream &os, const std::string &inp) const;
    // returns a new string
    std::string   str() const;

    // default handler emits to std::cerr and exits with EXIT_INTERNAL_ERROR
    [[noreturn]]
    void          emit_and_exit_with_error() const;
  }; // diagnostic




  using warning_list = std::vector<diagnostic>;

  class diagnostics {
    int                                         m_verbosity;
    const std::string                          &m_input;
    warning_list                                m_warnings;
  public:
    diagnostics(
      int verbosity,
      const std::string &input)
      : m_verbosity(verbosity), m_input(input)
    { }

    const std::string &input() const {return m_input;}
    int verbosity() const {return m_verbosity;}

    bool is_verbose() const {return verbosity() > 0;}
    bool is_debug() const { return verbosity() > 1; }
    bool is_quiet() const { return verbosity() < 0; }

    // const warning_list &warnings() const {return m_warnings; }

    void flush_warnings(std::ostream &os) {
      for (const auto &w : m_warnings) {
        w.str(os);
      }
      m_warnings.clear();
    }

    template <typename...Ts>
    [[noreturn]]
    void internal_at(const loc &at, Ts... ts) const {
      throw diagnostic(
        diagnostic::INTERNAL,
        at,
        text::format("INTERNAL ERROR: ", ts...),
        m_input);
    }
    template <typename...Ts>
    [[noreturn]]
    void fatal_at(const loc &at, Ts... ts) const {
      throw diagnostic(diagnostic::ERROR, at, text::format(ts...), m_input);
    }
    template <typename...Ts>
    void warning_at(const loc &at, Ts... ts) {
      m_warnings.emplace_back(
        diagnostic::WARNING, at, text::format(ts...), m_input);
    }
    template <typename...Ts>
    void verbose_at(const loc &at, Ts... ts) const {
      if (m_verbosity > 0)
        format_message_with_context(
          std::cout, at,
          &text::ANSI_GREEN, &text::ANSI_FADED,
          input(), ts...);
    }
    template <typename...Ts>
    void debug_at(const loc &at, Ts... ts) const {
      if (m_verbosity > 1)
        format_message_with_context(
          std::cout, at,
          &text::ANSI_FADED_YELLOW, &text::ANSI_FADED,
          input(), ts...);
    }
  }; // fatal_handler

  // This allows any class composing a diagnostics to generate member
  // functions to redirect to the diagnostics object.
#define DIAGNOSTIC_MIXIN_MEMBERS(DIAGS_MEMBER, DFT_AT)\
  template <typename...Ts> \
  [[noreturn]] void internal_at(const loc &at, Ts... ts) const {\
    (DIAGS_MEMBER).internal_at(at, ts...);\
  }\
  template <typename...Ts>\
  [[noreturn]] void internal(Ts... ts) const {\
    (DIAGS_MEMBER).internal_at(DFT_AT, ts...);\
  }\
  template <typename...Ts>\
  [[noreturn]] void fatal_at(const loc &at, Ts... ts) const {\
    (DIAGS_MEMBER).fatal_at(at, ts...);\
  }\
  template <typename...Ts>\
  [[noreturn]] void fatal(Ts... ts) const {\
    (DIAGS_MEMBER).fatal_at(DFT_AT, ts...);\
  }\
  template <typename...Ts> void warning_at(const loc &at, Ts... ts) const {\
    (DIAGS_MEMBER).warning_at(at, ts...);\
  }\
  template <typename...Ts> void warning(Ts... ts) const {\
    (DIAGS_MEMBER).warning_at(DFT_AT, ts...);\
  }\
  template <typename...Ts> void verbose_at(const loc &at, Ts... ts) const {\
    (DIAGS_MEMBER).verbose_at(at, ts...);\
  }\
  template <typename...Ts> void verbose(Ts... ts) const {\
    (DIAGS_MEMBER).verbose_at(DFT_AT, ts...);\
  }\
  template <typename...Ts> void debug_at(const loc &at, Ts... ts) const {\
    (DIAGS_MEMBER).debug_at(at, ts...);\
  }\
  template <typename...Ts> void debug(Ts... ts) const {\
    (DIAGS_MEMBER).debug_at(DFT_AT, ts...);\
  }\
  bool is_quiet() const {return (DIAGS_MEMBER).is_quiet();}\
  bool is_verbose() const {return (DIAGS_MEMBER).is_verbose();}\
  bool is_debug() const {return (DIAGS_MEMBER).is_debug();}

} // namespace cls

#endif