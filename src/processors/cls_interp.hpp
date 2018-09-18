#ifndef PROCESSORS_CLS_INTERP_HPP
#define PROCESSORS_CLS_INTERP_HPP

#include "../ir/cls_ir.hpp"
#include "../cls.hpp"
#include "../cls_opts.hpp"

namespace cls
{
  struct compiled_script {
    // various CL objects
    void *state;

    // intializer buffer values from whatever backing stores exist
    void setup(const cls::Opts &os, int iteration);

    // intializer buffer values from whatever backing store
    void execute(const cls::Opts &os, int iteration);
  };

  // compiles programs
  compiled_script compile(const cls::Opts &os, const cls::script &s);
} // namespace cls

#endif