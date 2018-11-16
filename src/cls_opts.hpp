#ifndef CLS_OPTS_HPP
#define CLS_OPTS_HPP

#include "cl_headers.hpp"
#include "text.hpp"

#include <iostream>
#include <ostream>
#include <string>
#include <vector>

namespace cls
{
struct filtered_stream {
  bool            should_log;
  std::ostream   &underlying_stream;

  filtered_stream(
    bool _should_log,
    std::ostream &_underlying_stream = std::cout)
    : should_log(_should_log)
    , underlying_stream(_underlying_stream)
  {
  }

  template <typename T>
  filtered_stream& operator<<(const T &v) {
    if (should_log) {
      underlying_stream << v;
    }
    return *this;
  }
};

struct opts {
  int                         verbosity = 0;
  std::vector<std::string>    input_files; // regular arg
  std::string                 input_expr; // -e
  bool                        list_devices = false;
  std::vector<cl::Device>     list_devices_specific;
  int                         iterations = 1;
  bool                        wall_time = false;
  bool                        parse_only = false;
  bool                        prof_time = false;
  bool                        save_preprocessed = false;
  bool                        save_binaries = false;
  bool                        use_kernel_arg_info = false; // OpenCL 1.2+

  filtered_stream warning() const{
    std::cout << text::ANSI_YELLOW << "WARNING:" << text::ANSI_RESET << " ";
    return filtered_stream(true);
  }
  filtered_stream always() const{return filtered_stream(true);}
  filtered_stream normal() const{return filtered_stream(verbosity >= 0);}
  filtered_stream verbose() const{return filtered_stream(verbosity >= 1);}
  filtered_stream debug() const{return filtered_stream(verbosity >= 2);}
};

} // namespace cls

#endif