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
  const char *LIB_FILE = "libOpenCL.so";
#endif

const cl_lib cl_lib::DEFAULT {0, nullptr};

cl_lib::cl_lib(int _verbosity, cl_device_id dev_id, bool auto_ld_exts)
    : verbosity(_verbosity)
{
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
#define BIND_SYM(FSYM) \
  FSYM = (FSYM##_Fn)get(#FSYM)

  BIND_SYM(clGetPlatformIDs);
  BIND_SYM(clGetPlatformInfo);
  //
  BIND_SYM(clGetDeviceIDs);
  BIND_SYM(clGetDeviceInfo);
  //
  BIND_SYM(clCreateContext);
  BIND_SYM(clGetContextInfo);
  BIND_SYM(clReleaseContext);
  //
  BIND_SYM(clCreateCommandQueue);
  BIND_SYM(clCreateCommandQueueWithProperties);
  BIND_SYM(clReleaseCommandQueue);
  //
  BIND_SYM(clCreateBuffer);
  BIND_SYM(clReleaseMemObject);
  BIND_SYM(clGetMemObjectInfo);
  BIND_SYM(clGetImageInfo);
  //
  BIND_SYM(clSVMAlloc);
  BIND_SYM(clSVMFree);
  //
  BIND_SYM(clCreateImage);
  BIND_SYM(clCreateSamplerWithProperties);
  BIND_SYM(clReleaseSampler);
  //
  BIND_SYM(clCreateProgramWithSource);
  BIND_SYM(clCreateProgramWithBinary);
  BIND_SYM(clCreateProgramWithBuiltInKernels);
  BIND_SYM(clCreateProgramWithIL);
  BIND_SYM(clBuildProgram);
  BIND_SYM(clReleaseProgram);
  //
  BIND_SYM(clBuildProgram);
  BIND_SYM(clCreateKernel);
  BIND_SYM(clCreateKernelsInProgram);
  BIND_SYM(clSetKernelArg);
  BIND_SYM(clSetKernelArgSVMPointer);
  BIND_SYM(clGetKernelInfo);
  BIND_SYM(clGetKernelArgInfo);
  BIND_SYM(clGetKernelWorkGroupInfo);
  BIND_SYM(clGetKernelWorkGroupInfo);
  BIND_SYM(clGetKernelSubGroupInfo);
  BIND_SYM(clReleaseKernel);
  //
  BIND_SYM(clWaitForEvents);
  BIND_SYM(clGetEventInfo);
  BIND_SYM(clReleaseEvent);
  BIND_SYM(clGetEventProfilingInfo);
  //
  BIND_SYM(clFlush);
  BIND_SYM(clFinish);
  BIND_SYM(clEnqueueReadBuffer);
  BIND_SYM(clEnqueueWriteBuffer);
  BIND_SYM(clEnqueueReadImage);
  BIND_SYM(clEnqueueWriteImage);
  BIND_SYM(clEnqueueMapBuffer);
  BIND_SYM(clEnqueueMapImage);
  BIND_SYM(clEnqueueUnmapMemObject);
  BIND_SYM(clEnqueueNDRangeKernel);
  //
  clGetExtensionFunctionAddressForPlatform =
      (clGetExtensionFunctionAddressForPlatform_Fn)find(
          "clGetExtensionFunctionAddressForPlatform");

  //////////////////////////
  // load extensions
  if (auto_ld_exts) {
    load_extensions(dev_id);
  } // if loading extensions
} // cl_lib::cl_lib()

void cl_lib::load_extensions(cl_device_id dev_id) {
  if (!dev_id) {
    std::cerr << "cl_lib: invalid dev_id\n";
    return;
  }
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
}


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
