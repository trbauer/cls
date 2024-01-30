#ifndef CLS_OPTS_HPP
#define CLS_OPTS_HPP

#include "cl_headers.hpp"
#include "text.hpp"

#include <iostream>
#include <ostream>
#include <string>
#include <utility>
#include <vector>

namespace cls
{
struct opts {
  std::string                 cpp_override_path;
  std::vector<std::string>    input_files; // regular arg
  std::vector<std::pair<std::string,std::string>> input_vars; // -DK=V => $K
  std::string                 input_expr; // -e
  int                         iterations = 1;
  bool                        list_devices = false;
  std::vector<cl_device_id>   list_devices_specific;
  std::vector<std::string>    list_metrics; // -lm
  std::vector<std::string>    list_sysinfo; // -ls
  enum {NAT,TRANS,CSV}        metric_format = TRANS;
  bool                        no_cleanup = false;
  bool                        no_exit_on_diff_fail = false;
  bool                        no_device_check = false;
  bool                        parse_only = false;
  bool                        prof_time = false;
  std::string                 metric_counter_set;
  bool                        save_preprocessed = false;
  bool                        save_binaries = false;
  bool                        show_all_times = false;
  bool                        use_kernel_arg_info = false; // OpenCL 1.2+
  int                         verbosity = 0;
  bool                        wall_time = false;
  bool                        warn_old_dipatch_syntax = false; // old dispatch

  opts() = default;

  bool debug_enabled() const {return verbosity >= 2;}
  bool verbose_enabled() const {return verbosity >= 1;}
};

} // namespace cls

#endif