#include "devices.hpp"
#include "text.hpp"
#include "system.hpp"

#include <algorithm>
#include <iostream>
#include <functional>


bool getDeviceByName(
  const cls::opts &opts,
  std::string substr,
  cl_device_id &dev_id,
  std::string &err_msg)
{
  // TODO: remove cl.hpp usage
  std::vector<cl::Platform> ps;
  cl::Platform::get(&ps);
  std::vector<cl::Device> matching;
  std::stringstream ss;
  for (auto &p : ps) {
    auto pnm = p.getInfo<CL_PLATFORM_NAME>();
    std::vector<cl::Device> ds;
    p.getDevices(CL_DEVICE_TYPE_ALL,&ds);
    for (auto &d : ds) {
      auto dev_nm = d.getInfo<CL_DEVICE_NAME>();
      if (dev_nm.find(substr) != std::string::npos) {
        matching.push_back(d);
      }
      ss << "=> " << dev_nm << "\n";
    }
  }
  if (matching.size() == 0) {
    err_msg = text::format("no matching device found from set: ", ss.str());
    return false;
  } else if (matching.size() > 1) {
    err_msg = text::format("device name string is ambiguous in set:: ", ss.str());
    return false;
  } else {
    dev_id = matching.front()();
    return true;
  }
}

cl_device_id getDeviceByName(
  const cls::opts &os,
  std::string substr)
{
  cl_device_id dev_id;
  std::string err_msg;
  if (!getDeviceByName(os,substr,dev_id,err_msg)) {
    FATAL("%s",err_msg.c_str());
  }
  return dev_id;
}

bool getDeviceByIndex(const cls::opts &os, int dev_ix, cl_device_id &dev_id)
{
  int curr_ix = 0;
  // TODO: remove cl.hpp usage
  std::vector<cl::Platform> ps;
  cl::Platform::get(&ps);
  for (auto &p : ps) {
    auto pnm = p.getInfo<CL_PLATFORM_NAME>();
    std::vector<cl::Device> ds;
    p.getDevices(CL_DEVICE_TYPE_ALL,&ds);
    for (auto &d : ds) {
      if (curr_ix++ == dev_ix) {
        dev_id = d();
        return true;
      }
    }
  }
  return false;
}
cl_device_id getDeviceByIndex(const cls::opts &os, int dev_ix)
{
  cl_device_id dev_id;
  if (!getDeviceByIndex(os, dev_ix, dev_id)) {
    FATAL("device index out of bounds");
  }
  return dev_id;
}

cl_device_id getDeviceDefault(const cls::opts &)
{
    std::vector<cl::Device> ds;
    cl::Platform::getDefault().getDevices(CL_DEVICE_TYPE_DEFAULT, &ds);
    if (ds.empty()) {
      FATAL("no devices on default platform");
    }
    return ds.back()();
}


// list_device.cpp
void listDeviceInfoForDevice(const cls::opts &os, cl_device_id d, int devIx);

void listDeviceInfo(const cls::opts &os)
{
  if (!os.list_devices_specific.empty()) {
    for (auto &d : os.list_devices_specific) {
      listDeviceInfoForDevice(os, d, -1);
    }
  } else {
    int curr_ix = 0;
    std::vector<cl::Platform> ps;
    cl::Platform::get(&ps);
    for (auto &p : ps) {
      std::vector<cl::Device> ds;
      p.getDevices(CL_DEVICE_TYPE_ALL, &ds);
      for (auto &d : ds) {
        listDeviceInfoForDevice(os, d(), curr_ix++);
      }
    }
  }
}

cl_spec getDeviceSpec(cl_device_id d) {
  std::string cl_version =
    cl::Device(d).getInfo<CL_DEVICE_OPENCL_C_VERSION>().c_str();
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

vendor           getDeviceVendor(cl_device_id d)
{
  const cl::Device dev(d);
  auto vendor = dev.getInfo<CL_DEVICE_VENDOR_ID>();
  if (vendor == 0x8086) {
    return vendor::INTEL;
  } else if (vendor == 0x10DE) {
    return vendor::NVIDIA;
  } else {
    std::string nm = dev.getInfo<CL_DEVICE_NAME>().c_str();
    if (nm.find("AMD") != std::string::npos || nm.find("Raedeon") != std::string::npos) {
      return vendor::AMD;
    } else if (nm.find("Intel") != std::string::npos) {
      return vendor::INTEL;
    } else if (nm.find("GTX") != std::string::npos || nm.find("RTX") != std::string::npos) {
      return vendor::NVIDIA;
    }
    return vendor::OTHER;
  }
}

microarch getDeviceMicroArchitecture(cl_device_id d)
{
  auto vend = getDeviceVendor(d);
  std::string nm =  cl::Device(d).getInfo<CL_DEVICE_NAME>().c_str();
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
          " 6000", " 6100", " 6200", " 6300"}))
      {
        arch = microarch::INTEL_GEN8;
      } else if (
        nameHasAny({" 510"," 515"," 520"," 530"," 540"," 550"," 580"}))
      {
        arch = microarch::INTEL_GEN9;
      } else if (nameHasAny({" 610"," 615"," 620"," 630"," 640"," 650"})) {
        if (nameHas("UHD Graphics")) {
          arch = microarch::INTEL_GEN9P7;
        } else {
          arch = microarch::INTEL_GEN9P5;
        }
      } else if (nameHasAny({" 910"," 915"," 920"," 930"," 940"," 950"})) {
          arch = microarch::INTEL_GEN11;
      } else {
        // TODO: later architectures
      }
    } else if(nameHas("Intel(R) Core(TM)")) {
      // TODO: no thanks; this is a huge task (need to define all the CPUs we care about...
    }
  } else if (vend == vendor::NVIDIA) {
    if (nameHasAny({"GTX 950","GTX 960","GTX 970","GTX 980"})) {
      arch = microarch::NVIDIA_MAX;
    } else if (nameHasAny({"GTX 1050","GTX 1060","GTX 1070","GTX 1080"})) {
      arch = microarch::NVIDIA_PAS;
    } else if (nameHasAny({})) {
      // not sure what Volta used (no GTX parts?)
      arch = microarch::NVIDIA_VOL;
    } else if (nameHasAny({"GTX 1660","RTX 2050","RTX 2060","RTX 2070","RTX 2080"})) {
      arch = microarch::NVIDIA_TUR;
    }
    // TODO: non GTX parts
    arch = microarch::OTHER;
  // } else if (vend == vendor::AMD) {
  // TODO:
  }
  return arch;
}


bool              hasExtension(cl_device_id dev, const char *ext)
{
  cl::Device devobj(dev);
  std::string s = devobj.getInfo<CL_DEVICE_EXTENSIONS>().c_str();
  return s.find(ext) != std::string::npos;
}


// We use the old 1.0 style command queue creation since the host running
// this might not be 1.2+.
#ifdef _MSC_VER
// disable the deprecation warning on clCreateCommandQueue
#pragma warning(disable : 4996)
#endif
cl_int makeCommandQueue(
  bool profiling_queue,
  cl_device_id dev_id,
  cl_context &ctx,
  cl_command_queue &queue)
{
  cl_command_queue_properties props = 0;
  if (profiling_queue) {
    props |= CL_QUEUE_PROFILING_ENABLE;
  }
  cl_int err;
  queue = clCreateCommandQueue(ctx, dev_id, props, &err);
  return err;
}
