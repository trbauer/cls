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
  const std::string &input,
  const std::string &message)
{
  if (location.line > 0 && location.column > 0) {
    os << location.line << "." << location.column << ": ";
  }
  os << message << "\n";

  size_t off = location.offset - (location.column - 1);
  while (off < input.length() && input[off] != '\n' && input[off] != '\r') {
    os << input[off++];
  }
  os << "\n";
  if (location.column > 0) {
    for (size_t i = 0; i < location.column - 1; i++) {
      os << ' ';
    }
  }
  os << "^\n";
}