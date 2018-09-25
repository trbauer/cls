#include "cls_interp.hpp"
#include "cls_interp_internal.hpp"
#include "../devices.hpp"
#include "../system.hpp"
#include "../text.hpp"

#include <functional>
#include <map>
#include <tuple>

using namespace cls;


/*
====
#0`prog1.cl`kernel<...>(...)
let X=#0`prog2.cl`kernel2
#2`prog1.cl
#GTX`prog1.cl...
barrier()
#0`prog1.cl...
print(...)
======
== construct contexts and command queues for each unique device in the system
#0`prog1.cl`kernel<...>(...)
 ^  device_state(0)
let X=#0`prog2.cl`kernel2
      ^  device_state(0)
#2`prog1.cl
#GTX`prog1.cl...
barrier()
#0`prog1.cl...
print(...)
=========
 */



times compiled_script::get_times() const
{
  times ts;
  const compiled_script_impl *csi = (const compiled_script_impl *)impl;
  for (const dispatch_command &dc : csi->dispatches)
    ts.emplace_back(dc.ds,dc.times);
  return ts;
}
