#include "cls_interp_internal.hpp"
#include "../text.hpp"

void compiled_script::setup(const Opts &os, int)
{
  std::cout << "compiled_script::setup\n";
}

void compiled_script::execute(const Opts &os, int)
{
  std::cout << "compiled_script::execute\n";
  compiled_script_impl *csi = (compiled_script_impl *)impl;
  for (const dispatch_command &dc : csi->dispatches) {
    std::cout << "  we would execute " <<
      text::str_extract(*dc.ds) << "\n";
  }
}
