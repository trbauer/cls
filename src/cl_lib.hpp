#ifndef CL_LIB_HPP
#define CL_LIB_HPP

#include "cl_headers.hpp"

#include <ostream>


// Dynamic access to OpenCL APIs simplifies build dependencies
struct cl_lib {
  cl_lib(int _verbosity,
         cl_device_id dev_id = nullptr,
         bool auto_ld_exts = true);
  ~cl_lib();
  cl_lib(const cl_lib &) = delete;
  cl_lib & operator=(const cl_lib &) = delete;

private:
  int verbosity = 0;
  void *lib = nullptr;

  void load_extensions(cl_device_id id);
public:
  static const cl_lib DEFAULT;

  bool is_valid() const {return lib;}

  using clCreateCommandQueue_Fn = CL_API_ENTRY
  cl_command_queue(CL_API_CALL *)(
      cl_context /* context */,
      cl_device_id /* device */,
      cl_command_queue_properties /* properties */,
      cl_int * /* errcode_ret */);

  using clCreatePerfCountersCommandQueueINTEL_Fn = CL_API_ENTRY
  cl_command_queue(CL_API_CALL *)(
      cl_context /* context */,
      cl_device_id /* device */,
      cl_command_queue_properties /* properties */,
      cl_uint /* configuration */,
      cl_int * /* errcode_ret */);
  using clGetDeviceInfo_Fn =
    cl_int (CL_API_CALL *)(cl_device_id    /* device */,
                cl_device_info  /* param_name */,
                size_t          /* param_value_size */,
                void *          /* param_value */,
                size_t *        /* param_value_size_ret */);
  using clGetExtensionFunctionAddressForPlatform_Fn =
      CL_API_ENTRY void *(CL_API_CALL *)(
          cl_platform_id /* platform */, const char * /* func_name */);
  using clCreateContext_Fn = CL_API_ENTRY cl_context(CL_API_CALL *)(
      const cl_context_properties * /* properties */,
      cl_uint /* num_devices */,
      const cl_device_id * /* devices */,
      void(CL_CALLBACK * /* pfn_notify */)(
          const char *, const void *, size_t, void *),
      void * /* user_data */,
      cl_int * /* errcode_ret */);
  using clGetEventProfilingInfo_Fn = CL_API_ENTRY cl_int(CL_API_CALL *)(
      cl_event /* event */,
      cl_profiling_info /* param_name */,
      size_t /* param_value_size */,
      void * /* param_value */,
      size_t * /* param_value_size_ret */);


  // TODO: add new
  clGetDeviceInfo_Fn clGetDeviceInfo = nullptr;
  clCreateContext_Fn clCreateContext = nullptr;
  clCreateCommandQueue_Fn clCreateCommandQueue = nullptr;
  clGetEventProfilingInfo_Fn clGetEventProfilingInfo = nullptr;
  clGetExtensionFunctionAddressForPlatform_Fn clGetExtensionFunctionAddressForPlatform = nullptr;
  //
  clCreatePerfCountersCommandQueueINTEL_Fn clCreatePerfCountersCommandQueueINTEL = nullptr;

}; // cl_lib

#endif // CL_LIB_HPP