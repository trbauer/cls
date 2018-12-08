#include "cls_opts.hpp"
#include "cl_headers.hpp"

#include <string>

bool            getDeviceByName(
  const cls::opts &opts,
  std::string substr,
  cl_device_id &out_dev_id,
  std::string &err_msg);

cl_device_id    getDeviceByName(
  const cls::opts &opts,
  std::string substr);

bool            getDeviceByIndex(
  const cls::opts &opts,
  int dev_ix,
  cl_device_id &out_dev_id);

cl_device_id    getDeviceByIndex(
  const cls::opts &opts,
  int dev_ix);

cl_device_id    getDeviceDefault(
  const cls::opts &opts);

void            listDeviceInfo(
  const cls::opts &opts);

bool            hasExtension(
  cl_device_id dev_id,
  const char *ext);

enum class cl_spec {
  CL_1_0 = 100,
  CL_1_1 = 110,
  CL_1_2 = 120,
  CL_2_0 = 200,
  CL_2_1 = 210,
  CL_2_2 = 220,
};
cl_spec           getDeviceSpec(cl_device_id dev_id);

enum class vendor {
  AMD    = 0x2222,
  INTEL  = 0x8086,
  NVIDIA = 0x10DE,
  OTHER  = 0x7FFF,
};
vendor        getDeviceVendor(cl_device_id dev_id);

enum class microarch {
  // AMD_VEGA  = ((static_cast<int>(cl_vendor::CL_AMD) << 16) | 0xXXXX),
  //
  // INTEL_CPU_6600    = ((static_cast<int>(vendor::INTEL) << 16) | 0x6600),
  // INTEL_CPU_6600K   = ((static_cast<int>(vendor::INTEL) << 16) | 0x6601),
  // TODO: expand these ... ick (not tied to the naming or encoding, but
  // we do need them to be monotically disjoint from INTEL_GEN*
  //
  INTEL_GEN7P5 = ((static_cast<int>(vendor::INTEL) << 16) | 0x0075),
  INTEL_GEN8   = ((static_cast<int>(vendor::INTEL) << 16) | 0x0080),
  INTEL_GEN9   = ((static_cast<int>(vendor::INTEL) << 16) | 0x0090),
  INTEL_GEN9P5 = ((static_cast<int>(vendor::INTEL) << 16) | 0x0095), // KBL
  INTEL_GEN9P7 = ((static_cast<int>(vendor::INTEL) << 16) | 0x0097), // KBL+
  INTEL_GEN10  = ((static_cast<int>(vendor::INTEL) << 16) | 0x00A0),
  INTEL_GEN11  = ((static_cast<int>(vendor::INTEL) << 16) | 0x00B0),
  //
  // SPECIFY: we could split this up even more GV100, GV102, ... etc...
  // SPECIFY: or even maybe re-arrange by CUDA capability?
  NVIDIA_MAX  = ((static_cast<int>(vendor::NVIDIA) << 16) | 0x0005),
  NVIDIA_PAS  = ((static_cast<int>(vendor::NVIDIA) << 16) | 0x0006),
  NVIDIA_VOL  = ((static_cast<int>(vendor::NVIDIA) << 16) | 0x0007),
  NVIDIA_TUR  = ((static_cast<int>(vendor::NVIDIA) << 16) | 0x0008),
  //
  OTHER       = 0x7FFF0000,
};
static inline bool isIntelGEN(microarch p) {
  return
    static_cast<int>(p) >= static_cast<int>(microarch::INTEL_GEN7P5) &&
    static_cast<int>(p) <= static_cast<int>(microarch::INTEL_GEN11);
}
microarch   getDeviceMicroArchitecture(cl_device_id d);


// uses OpenCL 1.0 functions to create the command queue so that 1.1
// implementations can still functions
cl_int            makeCommandQueue(
  bool profiling_queue,
  cl_device_id dev_id,
  cl_context &context,
  cl_command_queue &queue);
