#include "devices.hpp"
#include "text.hpp"
#include "system.hpp"

#include <algorithm>
#include <iostream>
#include <functional>


bool getDeviceByName(
  const cls::opts &opts,
  std::string substr,
  cl::Device &dev,
  std::string &err_msg)
{
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
    dev = matching.front();
    return true;
  }
}

cl::Device getDeviceByName(
  const cls::opts &os,
  std::string substr)
{
  cl::Device dev;
  std::string err_msg;
  if (!getDeviceByName(os,substr,dev,err_msg)) {
    FATAL("%s",err_msg.c_str());
  }
  return dev;
}

bool getDeviceByIndex(const cls::opts &os, int dev_ix, cl::Device &dev)
{
  int curr_ix = 0;
  std::vector<cl::Platform> ps;
  cl::Platform::get(&ps);
  for (auto &p : ps) {
    auto pnm = p.getInfo<CL_PLATFORM_NAME>();
    std::vector<cl::Device> ds;
    p.getDevices(CL_DEVICE_TYPE_ALL,&ds);
    for (auto &d : ds) {
      if (curr_ix++ == dev_ix) {
        dev = d;
        return true;
      }
    }
  }
  return false;
}
cl::Device getDeviceByIndex(const cls::opts &os, int dev_ix)
{
  cl::Device dev;
  if (!getDeviceByIndex(os, dev_ix, dev)) {
    FATAL("device index out of bounds");
  }
  return dev;
}

cl::Device getDeviceDefault(const cls::opts &)
{
    std::vector<cl::Device> ds;
    cl::Platform::getDefault().getDevices(CL_DEVICE_TYPE_DEFAULT, &ds);
    if (ds.empty()) {
      FATAL("no devices on default platform");
    }
    return ds.back();
}


// list_device.cpp
void listDeviceInfoForDevice(const cls::opts &os, const cl::Device &d, int devIx);

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
        listDeviceInfoForDevice(os, d, curr_ix++);
      }
    }
  }
}

cl_spec getDeviceSpec(const cl::Device &dev) {
  std::string cl_version = dev.getInfo<CL_DEVICE_OPENCL_C_VERSION>();
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

cl_vendor           getDeviceVendor(const cl::Device &dev)
{
  auto vendor = dev.getInfo<CL_DEVICE_VENDOR_ID>();
  if (vendor == 0x8086) {
    return cl_vendor::CL_INTEL;
  } else if (vendor == 0x10DE) {
    return cl_vendor::CL_NVIDIA;
  } else {
    std::string nm = dev.getInfo<CL_DEVICE_NAME>().c_str();
    if (nm.find("AMD") != std::string::npos || nm.find("Raedeon") != std::string::npos) {
      return cl_vendor::CL_NVIDIA;
    } else if (nm.find("Intel") != std::string::npos) {
      return cl_vendor::CL_INTEL;
    } else if (nm.find("GTX") != std::string::npos || nm.find("RTX") != std::string::npos) {
    }
    return cl_vendor::CL_OTHER;
  }
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
