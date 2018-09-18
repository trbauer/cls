#ifndef _CL_HEADERS_
#define _CL_HEADERS_

#include <string>
#include <vector>
#define CL_HPP_ENABLE_EXCEPTIONS
#define CL_TARGET_OPENCL_VERSION 220
#define CL_HPP_TARGET_OPENCL_VERSION 200
// cl2.hpp rejects higher values than 200
#include "CL/cl2.hpp"

#endif