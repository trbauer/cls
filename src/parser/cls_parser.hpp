#ifndef CLS_PARSER_HPP
#define CLS_PARSER_HPP

#include "../cl_headers.hpp"
#include "../cls_opts.hpp"
#include "../ir/cls_ir.hpp"

#include <ostream>
#include <tuple>
#include <vector>

namespace cls
{
  extern const char *CLS_SYNTAX;

  void parse_script(
    const opts &os,
    const std::string &input,
    const std::string &filename,
    cls::script &s,
    diagnostics &ds);
}

#endif