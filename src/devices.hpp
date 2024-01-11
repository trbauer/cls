#ifndef DEVICES_HPP
#define DEVICES_HPP
#include "cls_opts.hpp"
#include "cl_lib.hpp"
#include "../deps/mdapi/mdapi_wrapper.hpp"

#include <string>

//
// std::string getDeviceName(cl_device_id d);
//
cl_int getDeviceInfo(cl_device_id d, cl_device_info info, std::string &value);
cl_int getDeviceInfo(cl_device_id d, cl_device_info info, cl_uint &value);
//


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

cl_device_id    getDeviceDefault();

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
  CL_3_0 = 300,
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
  // AMD_NAVI  = ((static_cast<int>(cl_vendor::CL_AMD) << 16) | 0xXXXX),
  //
  // INTEL_CPU_6600    = ((static_cast<int>(vendor::INTEL) << 16) | 0x6600),
  // INTEL_CPU_6600K   = ((static_cast<int>(vendor::INTEL) << 16) | 0x6601),
  // TODO: expand these ... ick (not tied to the naming or encoding, but
  // we do need them to be monotically disjoint from INTEL_GEN*)
  //
  INTEL_GEN7P5 = ((static_cast<int>(vendor::INTEL) << 16) | 0x0075),
  INTEL_GEN8   = ((static_cast<int>(vendor::INTEL) << 16) | 0x0080),
  INTEL_GEN9   = ((static_cast<int>(vendor::INTEL) << 16) | 0x0090),
  INTEL_GEN9P5 = ((static_cast<int>(vendor::INTEL) << 16) | 0x0095), // KBL
  INTEL_GEN9P7 = ((static_cast<int>(vendor::INTEL) << 16) | 0x0097), // KBL+
  INTEL_GEN10  = ((static_cast<int>(vendor::INTEL) << 16) | 0x00A0),
  INTEL_GEN11  = ((static_cast<int>(vendor::INTEL) << 16) | 0x00B0),
  INTEL_XELPG  = ((static_cast<int>(vendor::INTEL) << 16) | 0x00C0),
  INTEL_XEHPG  = ((static_cast<int>(vendor::INTEL) << 16) | 0x00D0), // ARC
  INTEL_XEHPC  = ((static_cast<int>(vendor::INTEL) << 16) | 0x00E0), // MAX
  INTEL_XE2LPG = ((static_cast<int>(vendor::INTEL) << 16) | 0x0100),
  INTEL_XE2HPG = ((static_cast<int>(vendor::INTEL) << 16) | 0x0200),
  //
  // SPECIFY: we could split this up even more GV100, GV102, ... etc...
  // SPECIFY: or even maybe re-arrange by CUDA capability?
  NVIDIA_MAX  = ((static_cast<int>(vendor::NVIDIA) << 16) | 0x0005),
  NVIDIA_PAS  = ((static_cast<int>(vendor::NVIDIA) << 16) | 0x0006),
  NVIDIA_VOL  = ((static_cast<int>(vendor::NVIDIA) << 16) | 0x0007),
  NVIDIA_TUR  = ((static_cast<int>(vendor::NVIDIA) << 16) | 0x0008),
  NVIDIA_AMP  = ((static_cast<int>(vendor::NVIDIA) << 16) | 0x0009),
  NVIDIA_HOP  = ((static_cast<int>(vendor::NVIDIA) << 16) | 0x000A),
  //
  OTHER       = 0x7FFF0000,
};

static inline bool isIntelGEN(microarch p) {
  return
    static_cast<int>(p) >= static_cast<int>(microarch::INTEL_GEN7P5) &&
    static_cast<int>(p) <= static_cast<int>(microarch::INTEL_XE2HPG);
}

microarch   getDeviceMicroArchitecture(cl_device_id d);

static const char *format(microarch ma)
{
  switch (ma) {
  case microarch::INTEL_GEN7P5: return "intc_gen7p5";
  case microarch::INTEL_GEN8:   return "intc_gen8";
  case microarch::INTEL_GEN9:   return "intc_gen9";
  case microarch::INTEL_GEN9P5: return "intc_gen9p5";
  case microarch::INTEL_GEN9P7: return "intc_gen9p7";
  case microarch::INTEL_GEN10:  return "intc_gen10";
  case microarch::INTEL_GEN11:  return "intc_gen11";
  case microarch::INTEL_XELPG:  return "intc_xelpg";
  case microarch::INTEL_XEHPG:  return "intc_xehpg";
  case microarch::INTEL_XEHPC:  return "intc_xehpc";
  case microarch::INTEL_XE2LPG: return "intc_xe2lpg";
  case microarch::INTEL_XE2HPG: return "intc_xe2hpg";
  //
  case microarch::NVIDIA_MAX:   return "nvda_max";
  case microarch::NVIDIA_PAS:   return "nvda_pas";
  case microarch::NVIDIA_VOL:   return "nvda_vol";
  case microarch::NVIDIA_TUR:   return "nvda_tur";
  case microarch::NVIDIA_AMP:   return "nvda_amp";
  case microarch::NVIDIA_HOP:   return "nvda_hop";
  //
  default:
    return "?";
  }
}

// const char *getDriverPath(cl_device_id d);

#endif // DEVICES_HPP