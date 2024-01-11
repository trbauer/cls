#include "fatal.hpp"

#include <cstdlib>
#include <iostream>

using namespace cls;

std::string diagnostic::str() const
{
  std::stringstream ss;
  str(ss);
  return ss.str();
}

void cls::format_message_with_context_impl(
  std::ostream &os,
  const cls::loc &at,
  const text::ansi_literal *highlight,
  const text::ansi_literal *message_color,
  const std::string &input,
  const std::string &message)
{
  if (message_color)
    os << *message_color;

  if (at.line > 0 && at.column > 0) {
    os << at.line << "." << at.column << ": ";
  }
  os << message << "\n";

  if (at.line > 0 && at.column > 0) {
    size_t off = at.offset - (at.column - 1);
    while (off < input.length() && input[off] != '\n' && input[off] != '\r') {
      if (highlight && off == at.offset && at.extent > 0)
        os << *highlight;
      os << input[off++];
      if (highlight && off == at.offset + at.extent) {
        os << (message_color ? *message_color : text::ANSI_RESET);
      }
    }
    os << "\n";
    if (at.column > 0) {
      for (size_t i = 0; i < at.column - 1; i++) {
        os << ' ';
      }
    }
    os << "^";
    for (size_t ext = 1; ext < at.extent; ext++)
      os << "^";
    os << "\n";
  }
  os << text::ANSI_RESET;
}


void diagnostic::str(std::ostream &os) const {
  str(os, input);
}
void diagnostic::str(std::ostream &os, const std::string &inp) const {
  format_message_with_context_impl(
    os,
    at,
    level == ERROR ? &text::ANSI_RED : &text::ANSI_YELLOW,
    nullptr,
    inp,
    message);
}
void diagnostic::emit_and_exit_with_error() const {
  str(std::cerr);
  if (level == INTERNAL) {
    exit(EXIT_INTERNAL_ERROR);
  } else {
    exit(EXIT_FAILURE);
  }
}
