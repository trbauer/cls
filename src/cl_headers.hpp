#ifndef _CL_HEADERS_
#define _CL_HEADERS_

#include <ostream>
#include <string>
#include <vector>
#define CL_HPP_ENABLE_EXCEPTIONS
#define CL_TARGET_OPENCL_VERSION 220
#define CL_HPP_TARGET_OPENCL_VERSION 200
// cl2.hpp rejects higher values than 200
#include "CL/cl2.hpp"

// OpenCL headers typedefs cl_half to unsigned __int16 already which
// is also cl_ushort, but we wanted it to be a distinct type so that we
// can use it in templates (e.g. for std::ostream(<<)).
// Hence, we create cls_half.
//
// TODO: this may not be used anymore...
struct cls_half {cl_ushort bits;};
static_assert(sizeof(cls_half) == sizeof(cl_half),"wrong size for cls_half");

// define an ostream operator

namespace cls
{
  std::string     status_to_symbol(cl_int error);

  // TODO: make a quick test to verify these
  float           half_to_float(cl_half);
  static
  float           half_to_float(cls_half h) {return half_to_float(h.bits);}
  cl_half         float_to_half(float);

  std::string     fmtNDRange(const cl::NDRange &ndr);
} // namespace cls

static std::ostream &operator <<(std::ostream &os, const cls_half &h) {
  // os << "0x" << std::hex << h.bits;
  os << cls::half_to_float(h);
  return os;
}
#endif