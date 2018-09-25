#include "cls_opts.hpp"
#include "cl_headers.hpp"

#include <string>

bool              getDeviceByName(const cls::Opts &opts, std::string substr, cl::Device &out_dev, std::string &err_msg);
cl::Device        getDeviceByName(const cls::Opts &opts, std::string substr);
bool              getDeviceByIndex(const cls::Opts &opts, int dev_ix, cl::Device &out_dev);
cl::Device        getDeviceByIndex(const cls::Opts &opts, int dev_ix);
cl::Device        getDeviceDefault(const cls::Opts &opts);
void              listDeviceInfo(const cls::Opts &opts);


enum class cl_spec {
  CL_1_0 = 100,
  CL_1_1 = 110,
  CL_1_2 = 120,
  CL_2_0 = 200,
  CL_2_1 = 210,
  CL_2_2 = 220,
};
cl_spec           getDeviceSpec(const cl::Device &dev);

enum class cl_vendor {
  CL_AMD    = 0x2222,
  CL_INTEL  = 0x8086,
  CL_NVIDIA = 0x4444,
  CL_OTHER  = 0xFFFF,
};
cl_vendor        getDeviceVendor(const cl::Device &dev);

// uses OpenCL 1.0 functions to create the command queue so that 1.1
// implementations can still functions
cl_int            makeCommandQueue(
  bool profiling_queue,
  cl_device_id dev_id,
  cl_context &context,
  cl_command_queue &queue);
