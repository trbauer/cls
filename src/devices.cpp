#include "devices.hpp"
#include "text.hpp"
#include "system.hpp"

#include <algorithm>
#include <cstdlib>
#include <iostream>
#include <functional>
#include <vector>


#define CL_SYM_STR(X) #X
#define CL_COMMAND(CL_FUNCTION,...) \
  do { \
    cl_int _err = CL_FUNCTION(__VA_ARGS__); \
    if (_err != CL_SUCCESS) { \
      std::cerr << CL_SYM_STR(CL_FUNCTION) << ": " \
        << cls::status_to_symbol(_err) << "\n"; \
      exit(EXIT_FAILURE); \
    } \
  } while(0)

static std::vector<cl_device_id> get_device_ids()
{
  cl_platform_id *ps;
  cl_uint nps = 0;
  CL_COMMAND(clGetPlatformIDs, 0, nullptr, &nps);
  ps = (cl_platform_id *)alloca(sizeof(*ps)*(nps+1));
  CL_COMMAND(clGetPlatformIDs, nps, ps, nullptr);
  //
  std::vector<cl_device_id> ds;
  for (cl_uint i = 0; i < nps; i++) {
    if (ps[i] == nullptr)
      continue;
    cl_uint nds;
    CL_COMMAND(clGetDeviceIDs, ps[i], CL_DEVICE_TYPE_ALL, 0, nullptr, &nds);
    ds.resize(ds.size() + nds);
    CL_COMMAND(clGetDeviceIDs,
      ps[i],
      CL_DEVICE_TYPE_ALL,
      nds,
      ds.data() + ds.size() - nds,
      nullptr);
  }
  //
  return ds;
}

cl_int get_device_info(
  cl_device_id d, cl_device_info info, std::string &value)
{
  size_t n;
  //
  auto err = clGetDeviceInfo(d, info, 0, nullptr, &n);
  if (err != CL_SUCCESS)
    return err;
  //
  char *str = (char *)alloca(n + 1);
  memset(str, 0, n + 1);
  //
  err = clGetDeviceInfo(d, info, n + 1, str, nullptr);
  if (err == CL_SUCCESS)
    value = str;
  return err;
}

std::string get_device_info(
  cl_device_id d, cl_device_info info)
{
  size_t n;
  //
  CL_COMMAND(clGetDeviceInfo, d, info, 0, nullptr, &n);
  //
  char *str = (char *)alloca(n + 1);
  memset(str, 0, n + 1);
  //
  CL_COMMAND(clGetDeviceInfo, d, info, n + 1, str, nullptr);
  //
  return std::string(str);
}

std::string get_device_name(cl_device_id d)
{
  return get_device_info(d, CL_DEVICE_NAME);
}
cl_int get_device_info(cl_device_id d, cl_device_info info, cl_uint &value)
{
  auto err = clGetDeviceInfo(d, info, sizeof(value), &value, nullptr);
  return err;
}

bool get_device_by_name(
  const cls::opts &opts,
  std::string substr,
  cl_device_id &dev_id,
  std::string &err_msg)
{
  auto ds = get_device_ids();

  std::vector<cl_device_id> matching;
  std::stringstream ss;

  for (size_t i = 0; i < ds.size(); i++) {
    auto dev_nm = get_device_name(ds[i]);
    if (dev_nm.find(substr) != std::string::npos) {
      matching.push_back(ds[i]);
    }
    ss << "=> " << dev_nm << "\n";
  }
  if (matching.size() == 0) {
    err_msg = text::format("no matching device found from set:\n", ss.str());
    return false;
  } else if (matching.size() > 1) {
    err_msg = text::format("device name string is ambiguous in set:\n", ss.str());
    return false;
  } else {
    dev_id = matching.front();
    return true;
  }
}

cl_device_id get_device_by_name(
  const cls::opts &os,
  std::string substr)
{
  cl_device_id dev_id;
  std::string err_msg;
  if (!get_device_by_name(os,substr,dev_id,err_msg)) {
    FATAL("%s", err_msg.c_str());
  }
  return dev_id;
}

bool get_device_by_index(const cls::opts &os, int dev_ix, cl_device_id &dev_id)
{
  auto ds = get_device_ids();
  if (dev_ix >= (int)ds.size())
    return false;
  dev_id = ds[dev_ix];
  return true;
}

cl_device_id get_device_by_index(const cls::opts &os, int dev_ix)
{
  cl_device_id dev_id;
  if (!get_device_by_index(os, dev_ix, dev_id)) {
    FATAL("device index out of bounds");
  }
  return dev_id;
}

cl_device_id get_device_default()
{
  auto ds = get_device_ids();
  if (ds.empty()) {
    std::cerr << "get_device_default: no devices found\n";
  }
  return ds[0];
}

// list_device.cpp
void list_device_info_for_device(const cls::opts &os, cl_device_id d, int devIx);

void list_device_info(const cls::opts &os)
{
  if (!os.list_devices_specific.empty()) {
    for (auto &d : os.list_devices_specific) {
      list_device_info_for_device(os, d, -1);
    }
  } else {
    auto ds = get_device_ids();
    for (int ix = 0; ix < (int)ds.size(); ix++) {
      list_device_info_for_device(os, ds[ix], ix);
    }
  }
  std::cout.flush();
}

cl_spec get_device_spec(cl_device_id d) {
  std::string cl_version = get_device_info(d, CL_DEVICE_OPENCL_C_VERSION);
  if (cl_version.find("2.2") != std::string::npos) {
    return cl_spec::CL_2_2;
  } else if (cl_version.find("2.1") != std::string::npos) {
    return cl_spec::CL_2_1;
  } else if (cl_version.find("2.0") != std::string::npos) {
    return cl_spec::CL_2_0;
  } else if (cl_version.find("1.2") != std::string::npos) {
    return cl_spec::CL_1_2;
  } else if (cl_version.find("1.1") != std::string::npos) {
    return cl_spec::CL_1_1;
  } else {
    return cl_spec::CL_1_0;
  }
}

vendor get_device_vendor(cl_device_id d)
{
  cl_uint vendor = 0;
  CL_COMMAND(clGetDeviceInfo,
    d, CL_DEVICE_VENDOR_ID, sizeof(vendor), &vendor, nullptr);
  if (vendor == 0x8086) {
    return vendor::INTEL;
  } else if (vendor == 0x10DE) {
    return vendor::NVIDIA;
  } else {
    std::string nm = get_device_name(d);
    if (nm.find("AMD") != std::string::npos ||
      nm.find("Raedeon") != std::string::npos)
    {
      return vendor::AMD;
    } else if (nm.find("Intel") != std::string::npos) {
      return vendor::INTEL;
    } else if (nm.find("GTX") != std::string::npos ||
      nm.find("RTX") != std::string::npos)
    {
      return vendor::NVIDIA;
    }
    return vendor::OTHER;
  }
}

microarch get_device_microarch(cl_device_id d)
{
  const auto vend = get_device_vendor(d);
  std::string nm = get_device_name(d);
  auto nameHas = [&] (const char *substr) {
    return nm.find(substr) != std::string::npos;
  };
  auto nameHasAny = [&] (std::initializer_list<const char *> substrs) {
    for (const char *substr : substrs) {
      if (nameHas(substr)) {
        return true;
      }
    }
    return false;
  };
  microarch arch = microarch::OTHER;
  if (vend == vendor::INTEL) {
    // https://en.wikipedia.org/wiki/Intel_Graphics_Technology
    if (nameHas("HD Graphics")) {
      if (nameHasAny({" 4200"," 4400"," 4600"," 4700"," 5000"," 5100"," 5200"})) {
        arch = microarch::INTEL_GEN7P5;
      } else if (
        nameHasAny({" 5300"," 5500"," 5600", " 5700",
          " 6000", " 6100", " 6200", " 6300", " Gen8"}))
      {
        arch = microarch::INTEL_GEN8;
      } else if (
        nameHasAny({" 510"," 515"," 520"," 530"," 540"," 550"," 580", "Gen9"}))
      {
        arch = microarch::INTEL_GEN9;
      } else if (nameHasAny({" 610"," 615"," 620"," 630"," 640"," 650"})) {
        if (nameHas("UHD Graphics")) {
          arch = microarch::INTEL_GEN9P7;
        } else {
          arch = microarch::INTEL_GEN9P5;
        }
      } else if (nameHasAny({" 910"," 915"," 920"," 930"," 940"," 950", " Gen11"})) {
          arch = microarch::INTEL_GEN11;
      } else if (nameHasAny({" 710", " 730", " 740", " 750", " 770"})) {
          arch = microarch::INTEL_XELPG;
      } else {
          // TODO: later GEN architectures
      }
    } else if (nameHas("Data Center GPU")) {
      arch = microarch::INTEL_XEHPC;
    } else if (nameHasAny(
                   {"A310",
                    "A380",
                    "A580",
                    "A750",
                    "A770",
                    "A30M",
                    "A40",
                    "A50",
                    "A60"})) {
      arch = microarch::INTEL_XEHPG;
    } else if(nameHas("Intel(R) Core(TM)")) {
      // TODO: no thanks; this is a huge task (need to define all the CPUs we care about...)
    }
  } else if (vend == vendor::NVIDIA) {
    if (nameHasAny({"GTX 950","GTX 960","GTX 970","GTX 980"})) {
      arch = microarch::NVIDIA_MAX;
    } else if (nameHasAny({"GTX 1050", "GTX 1060", "GTX 1070", "GTX 1080"})) {
      arch = microarch::NVIDIA_PAS;
    } else if (nameHasAny({})) {
      // not sure what Volta used (no GTX parts?)
      arch = microarch::NVIDIA_VOL;
    } else if (nameHasAny({"GTX 1650", "GTX 1660", "RTX 20"})) {
      arch = microarch::NVIDIA_TUR;
    } else if (nameHasAny({"RTX 30"})) {
      arch = microarch::NVIDIA_AMP;
    } else if (nameHasAny({"RTX 40"})) {
      arch = microarch::NVIDIA_HOP;
    } else {
      // TODO: non GTX parts
      arch = microarch::OTHER;
    }
  } else if (vend == vendor::AMD) {
  // TODO:
  }
  return arch;
}

bool device_has_extension(cl_device_id dev, const char *ext)
{
  std::string exts = get_device_info(dev, CL_DEVICE_EXTENSIONS);
  return exts.find(ext) != std::string::npos;
}

#if 0
// need some sort of approach here
std::string device_driver_path(cl_device_id dev);
#endif
