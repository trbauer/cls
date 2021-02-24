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
  std::string CLS_SYNTAX_ALL();

  std::string CLS_SYN_SC();
  std::string CLS_SYN_ST();

  std::string CLS_SYN_PEX(); // primitive expression initializers
  std::string CLS_SYN_SEX(); // surface initializer expressions

  std::string expand_input_variables(
    const opts &os,
    const std::string &input,
    diagnostics &ds);

  void parse_script(
    const opts &os,
    const std::string &input,
    const std::string &filename,
    cls::script &s,
    diagnostics &ds);
}

#endif