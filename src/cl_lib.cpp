#include "cl_lib.hpp"

#include "system.hpp"
#include "fatal.hpp"
#include "devices.hpp"

#include <string>
#include <sstream>
#include <iostream>

using namespace cls;

#ifdef WIN32
  const char *LIB_FILE = "OpenCL.dll";
#else
  const char *LIB_FILE = "OpenCL.so";
#endif

const cl_lib cl_lib::DEFAULT {0, nullptr};

cl_lib::cl_lib(int _verbosity, cl_device_id dev_id) : verbosity(_verbosity) {
  lib = sys::load_library(LIB_FILE);
  if (!lib) {
    std::cerr << LIB_FILE << ": failed to load\n";
    return;
  }
  if (dev_id == nullptr) {
    dev_id = get_device_default();
  }

  auto get = [&](const char *func) {
    void *f = sys::get_symbol_address(lib, func);
    if (!f) {
      std::cerr << func << ": failed to find required OpenCL API\n";
      exit(EXIT_INTERNAL_ERROR);
    }
    return f;
  };
  auto find = [&](const char *func) {
    void *f = sys::get_symbol_address(lib, func);
    if (!f && verbosity >= 1) {
      std::cerr << func << ": failed to find optional OpenCL API\n";
    }
    return f;
  };

  //////////////////////////
  clGetDeviceInfo = (clGetDeviceInfo_Fn)get("clGetDeviceInfo");
  clCreateCommandQueue = (clCreateCommandQueue_Fn)get("clCreateCommandQueue");
  clGetEventProfilingInfo =
      (clGetEventProfilingInfo_Fn)get("clGetEventProfilingInfo");
  clGetExtensionFunctionAddressForPlatform =
      (clGetExtensionFunctionAddressForPlatform_Fn)find(
          "clGetExtensionFunctionAddressForPlatform");

  //////////////////////////
  // load extensions
  if (dev_id) {
    cl_platform_id plt_id = nullptr;
    auto           err = this->clGetDeviceInfo(
        dev_id, CL_DEVICE_PLATFORM, sizeof(cl_platform_id), &plt_id, nullptr);
    if (err != CL_SUCCESS) {
      std::cerr << "cl_lib: clDeviceInfo(CL_DEVICE_PLATFORM...) failed: " <<
        cls::status_to_symbol(err) << "\n";
      exit(EXIT_INTERNAL_ERROR);
    }


    if (!clGetExtensionFunctionAddressForPlatform) {
      std::cerr << LIB_FILE
                << ": failed to find clGetExtensionFunctionAddressForPlatform "
                   "(OpenCL 1.2 driver needed?)\n";
    }

    auto find_ext =
      [&]<typename T>(const char *func_name) -> T {
        if (!clGetExtensionFunctionAddressForPlatform)
          return nullptr;
        T func = (T)clGetExtensionFunctionAddressForPlatform(plt_id, func_name);
        if (func == nullptr) {
          if (verbosity >= 1) {
            std::cerr << func_name << ": unable to find extension function\n";
          }
        }
        return func;
      };
    clCreatePerfCountersCommandQueueINTEL =
        find_ext.template operator()<clCreatePerfCountersCommandQueueINTEL_Fn>(
            "clCreatePerfCountersCommandQueueINTEL");
  } // if loading extensions
} // cl_lib::cl_lib()


cl_lib::~cl_lib()
{
  if (lib)
    sys::close_library(lib);
  lib = nullptr;
}


/*
struct ocl_api {
  cl_command_queue    (CL_API_CALL *clCreatePerfCountersCommandQueueINTEL) (
      cl_context context,
      cl_device_id device,
      cl_command_queue_properties properties,
      cl_uint configuration,
      cl_int* errcode_ret) = nullptr;

#ifdef WIN32
  const char *LIB_FILE = "OpenCL.dll";
#else
  const char *LIB_FILE = "OpenCL.so";
#endif
  void *lib = nullptr;

  ocl_api() {
    lib = sys::load_library(LIB_FILE);

    clCreatePerfCountersCommandQueueINTEL =
      (cl_command_queue    (CL_API_CALL *) (
      cl_context,
      cl_device_id,
      cl_command_queue_properties,
      cl_uint,
      cl_int*))
        sys::get_symbol_address(lib, "clCreatePerfCountersCommandQueueINTEL");
  }
  ~ocl_api() {
    sys::close_library(lib);
  }
};

ocl_api ocl;
*/
