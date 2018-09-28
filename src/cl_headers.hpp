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

// OpenCL headers typedef the half data type to unsigned __int16, so we
// create a distinct type that we can use for templates etc...
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
} // namespace cls

static std::ostream &operator <<(std::ostream &os, const cls_half &h) {
  // os << "0x" << std::hex << h.bits;
  os << cls::half_to_float(h);
  return os;
}
#endif