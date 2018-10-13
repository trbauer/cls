#ifndef _CL_HEADERS_
#define _CL_HEADERS_

#include <cstdint>
#include <ostream>
#include <string>
#include <sstream>
#include <vector>
#define CL_HPP_ENABLE_EXCEPTIONS
#define CL_TARGET_OPENCL_VERSION 220
#define CL_HPP_TARGET_OPENCL_VERSION 200
// cl2.hpp rejects higher values than 200
#include "CL/cl2.hpp"



// define an ostream operator

namespace cls
{
  std::string     status_to_symbol(cl_int error);

  std::string     fmtNDRange(const cl::NDRange &ndr);
} // namespace cls

#endif