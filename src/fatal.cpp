#include "fatal.hpp"

std::string cls::diagnostic::str() const
{
  std::stringstream ss;
  str(ss);
  return ss.str();
}

void cls::formatMessageWithContextImpl(
  std::ostream &os,
  cls::loc location,
  const text::ansi_literal *highlight,
  const std::string &input,
  const std::string &message)
{
  if (location.line > 0 && location.column > 0) {
    os << location.line << "." << location.column << ": ";
  }
  os << message << "\n";

  size_t off = location.offset - (location.column - 1);
  while (off < input.length() && input[off] != '\n' && input[off] != '\r') {
    if (highlight && off == location.offset && location.extent > 0)
      os << *highlight;
    os << input[off++];
    if (highlight && off == location.offset + location.extent)
      os << text::ANSI_RESET;
  }
  os << "\n";
  if (location.column > 0) {
    for (size_t i = 0; i < location.column - 1; i++) {
      os << ' ';
    }
  }
  os << "^";
  for (size_t ext = 1; ext < location.extent; ext++)
    os << "^";
  os << "\n";
}

void cls::diagnostic::str(std::ostream &os) const {
  cls::formatMessageWithContextImpl(
    os,
    location,
    &text::ANSI_RED,
    input,
    message);
}