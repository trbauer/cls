#ifndef PROCESSORS_CLS_INTERP_HPP
#define PROCESSORS_CLS_INTERP_HPP

#include "../ir/cls_ir.hpp"
#include "../cls.hpp"
#include "../cls_opts.hpp"
#include "../stats.hpp"

#include <tuple>
#include <vector>

namespace cls
{
  using times = std::vector<std::tuple<const cls::dispatch_spec*,sampler>>;

  struct compiled_script {
    // various CL objects
    void *impl;

    // intializer buffer values from whatever backing stores exist
    void setup(const cls::opts &os, int iteration);

    // intializer buffer values from whatever backing store
    void execute(const cls::opts &os, int iteration);

    times get_times() const;
  };

  // compiles programs
  compiled_script compile(const cls::opts &os, const cls::script &s);
} // namespace cls

#endif