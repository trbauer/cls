#ifndef CLS_CL_HEADERS
#define CLS_CL_HEADERS

#define CL_TARGET_OPENCL_VERSION 220
#define CL_USE_DEPRECATED_OPENCL_1_2_APIS
#include "CL/cl.h"
#include "cl_exts.hpp"

#include <string>

namespace cls
{
  std::string     status_to_symbol(cl_int error);
} // namespace cls

#endif // CLS_CL_HEADERS