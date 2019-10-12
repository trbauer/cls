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
  std::string                 cpp_override_path;
  std::vector<std::string>    input_files; // regular arg
  std::string                 input_expr; // -e
  int                         iterations = 1;
  bool                        list_devices = false;
  std::vector<cl_device_id>   list_devices_specific;
  bool                        no_exit_on_diff_fail = false;
  bool                        parse_only = false;
  bool                        prof_time = false;
  bool                        save_preprocessed = false;
  bool                        save_binaries = false;
  bool                        show_init_times = false;
  bool                        use_kernel_arg_info = false; // OpenCL 1.2+
  int                         verbosity = 0;
  bool                        wall_time = false;

  filtered_stream warning() const{
    std::cout << text::ANSI_YELLOW << "WARNING:" << text::ANSI_RESET << " ";
    return filtered_stream(true);
  }
  filtered_stream always() const{return filtered_stream(true);}
  filtered_stream normal() const{return filtered_stream(verbosity >= 0);}
  filtered_stream verbose() const{return filtered_stream(verbose_enabled());}
  filtered_stream debug() const{return filtered_stream(debug_enabled());}

  bool debug_enabled() const { return verbosity >= 2; }
  bool verbose_enabled() const { return verbosity >= 1; }
};

} // namespace cls

#endif