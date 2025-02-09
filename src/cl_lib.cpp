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
  // Hints on getting this working in WSL.
  // 1. Build POCL (see https://github.com/microsoft/WSL/issues/6951#issuecomment-1955857286 )
  // 2. (sudo make install)
  // 3. On my system pocl added
  //          /usr/local/etc/OpenCL/vendors/pocl.icd
  //  but my KHR loader required /etc/OpenCL/vendors
  // Other useful paths to know:
  //   /usr/lib/x86_64-linux-gnu/libOpenCL.so.1.0.0 (ICD loader that lib load takes)
  //   /usr/local/share/pocl/include/opencl-c.h
  //   /usr/local/lib/libpocl.so
  //   /usr/local/bin/poclcc
const char *LIB_FILE = "libOpenCL.so";
#endif

const cl_lib cl_lib::DEFAULT {0, nullptr};

cl_lib::cl_lib(int _verbosity, cl_device_id dev_id, bool auto_ld_exts)
    : verbosity(_verbosity)
{
  std::string lib_file = LIB_FILE;
  if (auto override = sys::find_env("CLS_OPENCL_LIB_PATH")) {
    lib_file = *override;
  }
  lib = sys::load_library(lib_file.c_str());
  if (!lib) {
    std::cerr << lib_file << ": failed to load\n";
    exit(EXIT_INTERNAL_ERROR);
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
  BIND_SYM(clGetProgramInfo);
  BIND_SYM(clGetProgramBuildInfo);
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
  BIND_SYM(clCreateSampler);
  //
  clGetExtensionFunctionAddressForPlatform =
      (clGetExtensionFunctionAddressForPlatform_Fn)find(
          "clGetExtensionFunctionAddressForPlatform");
  if (dev_id == nullptr) {
    // this is really get_device_default()
    cl_int err;
    cl_uint nps = 0;
    err = clGetPlatformIDs(0, nullptr, &nps);
    if (err != CL_SUCCESS) {
      std::cerr << "cl_lib: clGetPlatformIDs(0, nullptr, &nps) failed: "
                << cl_lib::status_to_symbol(err) << "\n";
      exit(EXIT_INTERNAL_ERROR);
    }
    std::vector<cl_platform_id> ps {nps};
    err = clGetPlatformIDs(nps, ps.data(), nullptr);
    if (err != CL_SUCCESS) {
      std::cerr << "cl_lib: clGetPlatformIDs(...) failed: "
                << cl_lib::status_to_symbol(err) << "\n";
      exit(EXIT_INTERNAL_ERROR);
    }

    for (cl_uint i = 0; i < nps; i++) {
      if (ps[i] == nullptr)
        continue;
      cl_uint nds = 0;
      err = clGetDeviceIDs(ps[i], CL_DEVICE_TYPE_ALL, 1, &dev_id, &nds);
      if (err != CL_SUCCESS) {
        std::cerr << "cl_lib: clGetDeviceIDs(...) failed: "
                  << cl_lib::status_to_symbol(err) << "\n";
        exit(EXIT_INTERNAL_ERROR);
      }
      if (nds > 0)
        break; // dev_id is set
    }
    if (dev_id == nullptr) {
      std::cerr << "cl_lib: unable to find device\n";
      // which would be strange since we've bound all these APIs here!
      exit(EXIT_INTERNAL_ERROR);
    }
  }

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
      cl_lib::status_to_symbol(err) << "\n";
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

std::string cl_lib::status_to_symbol(cl_int error)
{
#define CASE(X) case X: return #X
  switch (error) {
  CASE(CL_SUCCESS);
  CASE(CL_DEVICE_NOT_FOUND);
  CASE(CL_DEVICE_NOT_AVAILABLE);
  CASE(CL_COMPILER_NOT_AVAILABLE);
  CASE(CL_MEM_OBJECT_ALLOCATION_FAILURE);
  CASE(CL_OUT_OF_RESOURCES);
  CASE(CL_OUT_OF_HOST_MEMORY);
  CASE(CL_PROFILING_INFO_NOT_AVAILABLE);
  CASE(CL_MEM_COPY_OVERLAP);
  CASE(CL_IMAGE_FORMAT_MISMATCH);
  CASE(CL_IMAGE_FORMAT_NOT_SUPPORTED);
  CASE(CL_BUILD_PROGRAM_FAILURE);
  CASE(CL_MAP_FAILURE);
  CASE(CL_MISALIGNED_SUB_BUFFER_OFFSET);
  CASE(CL_EXEC_STATUS_ERROR_FOR_EVENTS_IN_WAIT_LIST);
  CASE(CL_COMPILE_PROGRAM_FAILURE);
  CASE(CL_LINKER_NOT_AVAILABLE);
  CASE(CL_LINK_PROGRAM_FAILURE);
  CASE(CL_DEVICE_PARTITION_FAILED);
  CASE(CL_KERNEL_ARG_INFO_NOT_AVAILABLE);
  CASE(CL_INVALID_VALUE);
  CASE(CL_INVALID_DEVICE_TYPE);
  CASE(CL_INVALID_PLATFORM);
  CASE(CL_INVALID_DEVICE);
  CASE(CL_INVALID_CONTEXT);
  CASE(CL_INVALID_QUEUE_PROPERTIES);
  CASE(CL_INVALID_COMMAND_QUEUE);
  CASE(CL_INVALID_HOST_PTR);
  CASE(CL_INVALID_MEM_OBJECT);
  CASE(CL_INVALID_IMAGE_FORMAT_DESCRIPTOR);
  CASE(CL_INVALID_IMAGE_SIZE);
  CASE(CL_INVALID_SAMPLER);
  CASE(CL_INVALID_BINARY);
  CASE(CL_INVALID_BUILD_OPTIONS);
  CASE(CL_INVALID_PROGRAM);
  CASE(CL_INVALID_PROGRAM_EXECUTABLE);
  CASE(CL_INVALID_KERNEL_NAME);
  CASE(CL_INVALID_KERNEL_DEFINITION);
  CASE(CL_INVALID_KERNEL);
  CASE(CL_INVALID_ARG_INDEX);
  CASE(CL_INVALID_ARG_VALUE);
  CASE(CL_INVALID_ARG_SIZE);
  CASE(CL_INVALID_KERNEL_ARGS);
  CASE(CL_INVALID_WORK_DIMENSION);
  CASE(CL_INVALID_WORK_GROUP_SIZE);
  CASE(CL_INVALID_WORK_ITEM_SIZE);
  CASE(CL_INVALID_GLOBAL_OFFSET);
  CASE(CL_INVALID_EVENT_WAIT_LIST);
  CASE(CL_INVALID_EVENT);
  CASE(CL_INVALID_OPERATION);
  CASE(CL_INVALID_GL_OBJECT);
  CASE(CL_INVALID_BUFFER_SIZE);
  CASE(CL_INVALID_MIP_LEVEL);
  CASE(CL_INVALID_GLOBAL_WORK_SIZE);
  CASE(CL_INVALID_PROPERTY);
  CASE(CL_INVALID_IMAGE_DESCRIPTOR);
  CASE(CL_INVALID_COMPILER_OPTIONS);
  CASE(CL_INVALID_LINKER_OPTIONS);
  CASE(CL_INVALID_DEVICE_PARTITION_COUNT);
  CASE(CL_INVALID_PIPE_SIZE);
  CASE(CL_INVALID_DEVICE_QUEUE);
  CASE(CL_INVALID_SPEC_ID);
  CASE(CL_MAX_SIZE_RESTRICTION_EXCEEDED);
  // from extensions
  CASE(CL_INVALID_ACCELERATOR_INTEL);
  CASE(CL_INVALID_ACCELERATOR_TYPE_INTEL);
  CASE(CL_INVALID_ACCELERATOR_DESCRIPTOR_INTEL);
  CASE(CL_ACCELERATOR_TYPE_NOT_SUPPORTED_INTEL);
  // others
  // CASE(CL_INVALID_GL_SHAREGROUP_REFERENCE_KHR); // cl_khr_gl_sharing
  CASE(CL_PLATFORM_NOT_FOUND_KHR); // cl_khr_icd
  // CASE(CL_INVALID_D3D10_DEVICE_KHR);
  // CASE(CL_INVALID_D3D10_RESOURCE_KHR);
  // CASE(CL_D3D10_RESOURCE_ALREADY_ACQUIRED_KHR);
  // CASE(CL_INVALID_D3D9_DEVICE_NV);
  // CASE(CL_INVALID_DX9_DEVICE_INTEL);
  // CASE(CL_INVALID_D3D9_RESOURCE_NV);
  // CASE(CL_INVALID_DX9_RESOURCE_INTEL);
  // CASE(CL_D3D9_RESOURCE_ALREADY_ACQUIRED_NV);
  // CASE(CL_DX9_RESOURCE_ALREADY_ACQUIRED_INTEL);
  // CASE(CL_D3D9_RESOURCE_NOT_ACQUIRED_NV);
  // CASE(CL_DX9_RESOURCE_NOT_ACQUIRED_INTEL);
  // CASE(CL_EGL_RESOURCE_NOT_ACQUIRED_KHR);
  // CASE(CL_INVALID_VA_API_MEDIA_ADAPTER_INTEL);
  // CASE(CL_VA_API_MEDIA_SURFACE_ALREADY_ACQUIRED_INTEL);
  // CASE(CL_VA_API_MEDIA_SURFACE_NOT_ACQUIRED_INTEL);
  // case -9999: nvidia buffer access violation
  default:
    std::stringstream ss;
    ss << error << "?";
    return ss.str();
  }
} // status_to_symbol
#undef CASE
