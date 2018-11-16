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
  void parse_script(
    const opts &os,
    const std::string &input,
    const std::string &filename,
    cls::script &s,
    warning_list &wl);
}

#endif