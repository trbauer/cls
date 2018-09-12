#include "cls.hpp"

#include <string>

cl::Device        getDeviceByName(
                      const cls::Opts &opts,
                      cl_device_type dt,
                      std::string substr);
cl::Device        getDeviceByIndex(
                      const cls::Opts &opts,
                      cl_device_type dt,
                      int dev_ix);
void              listDeviceInfo(
                      const cls::Opts &opts);