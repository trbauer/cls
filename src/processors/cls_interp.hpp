#ifndef PROCESSORS_CLS_INTERP_HPP
#define PROCESSORS_CLS_INTERP_HPP

#include "../cls_opts.hpp"
#include "../ir/cls_ir.hpp"
#include "../stats.hpp"

#include <tuple>
#include <vector>

namespace cls
{
  using disp_times = std::vector<std::tuple<const dispatch_spec*,sampler>>;
  using init_times = std::vector<std::tuple<const init_spec_mem*,sampler>>;

  struct compiled_script {
    // various CL objects
    void *impl;

    // intializer buffer values from whatever backing store
    void execute(int iteration);

    disp_times get_wall_times() const;
    disp_times get_prof_times() const;
    init_times get_init_times() const;
  };

  // compiles programs
  compiled_script compile(
    const cls::opts &os,
    const cls::script &s,
    diagnostics &ds);

  // return 0 if unsupported or invalid input
  size_t channels_per_pixel(cl_channel_order co);
  size_t bytes_per_channel(cl_channel_type ct);

} // namespace cls

#endif