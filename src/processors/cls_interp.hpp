#ifndef PROCESSORS_CLS_INTERP_HPP
#define PROCESSORS_CLS_INTERP_HPP

#include "../cls_opts.hpp"
#include "../ir/cls_ir.hpp"
#include "../stats.hpp"

#include <tuple>
#include <vector>

namespace cls
{
  using times = std::vector<std::tuple<const cls::dispatch_spec*,sampler>>;

  struct compiled_script {
    // various CL objects
    void *impl;

    // intializer buffer values from whatever backing store
    void execute(int iteration);

    times get_wall_times() const;
    times get_prof_times() const;
  };

  // compiles programs
  compiled_script compile(const cls::opts &os, const cls::script &s);
} // namespace cls

#endif