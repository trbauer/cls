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

  struct ndr {
    size_t num_dims;
    size_t dims[3];

    ndr(size_t x = 0, size_t y = 0, size_t z = 0) {
      dims[0] = x; dims[1] = y; dims[2] = z;
      num_dims = 3;
      if (z == 0)
        num_dims--;
      if (y == 0)
        num_dims--;
      if (x == 0)
        num_dims--;
    }
    // ndr(const ndr &) = default;

    const size_t *get() const {return dims;}

    size_t rank() const {return num_dims;}

    size_t product() const {
      size_t p = 0;
      for (int i = 0; i < sizeof(dims)/sizeof(dims[0]) && dims[i] != 0; i++)
        p *= dims[i];
      return p;
    }

    std::string str() const {std::stringstream ss; str(ss); return ss.str();}
    void str(std::ostream &os) const {
      for (int i = 0; i < sizeof(dims)/sizeof(dims[0]) && dims[i] != 0; i++) {
        if (i > 0) os << "x";
        os << dims[i];
      }
    }
  };
} // namespace cls

#endif