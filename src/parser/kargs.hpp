#ifndef PARSER_KARGS
#define PARSER_KARGS

#include "../cl_headers.hpp"
#include "../opts.hpp"

#include <string>
#include <vector>

namespace cls
{
  struct karg {
    std::string name;
  };

  std::vector<karg> parseKernelArgumentInfo(cl::Device dev, cl::Kernel &k);

}



#endif