#ifndef CL_LIB_HPP
#define CL_LIB_HPP

#include "cl_headers.hpp"

#include <ostream>
#include <string>

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

  /////////////////////////////////////////////////////////////////////////////
  // clGetPlatformIDs
  using clGetPlatformIDs_Fn = CL_API_ENTRY cl_int(CL_API_CALL *)(
      cl_uint /* num_entries */,
      cl_platform_id * /* platforms */,
      cl_uint * /* num_platforms */);
  clGetPlatformIDs_Fn clGetPlatformIDs = nullptr;

  /////////////////////////////////////////////////////////////////////////////
  // clGetPlatformInfo
  using clGetPlatformInfo_Fn = CL_API_ENTRY cl_int(CL_API_CALL *)(
      cl_platform_id /* platform */,
      cl_platform_info /* param_name */,
      size_t /* param_value_size */,
      void * /* param_value */,
      size_t * /* param_value_size_ret */);
  clGetPlatformInfo_Fn clGetPlatformInfo = nullptr;

  /////////////////////////////////////////////////////////////////////////////
  // clGetDeviceIDs
  using clGetDeviceIDs_Fn = CL_API_ENTRY cl_int(CL_API_CALL *)(
      cl_platform_id /* platform */,
      cl_device_type /* device_type */,
      cl_uint /* num_entries */,
      cl_device_id * /* devices */,
      cl_uint * /* num_devices */);
  clGetDeviceIDs_Fn clGetDeviceIDs = nullptr;

  /////////////////////////////////////////////////////////////////////////////
  // clGetDeviceInfo
  using clGetDeviceInfo_Fn =
    cl_int (CL_API_CALL *)(cl_device_id    /* device */,
                cl_device_info  /* param_name */,
                size_t          /* param_value_size */,
                void *          /* param_value */,
                size_t *        /* param_value_size_ret */);
  clGetDeviceInfo_Fn clGetDeviceInfo = nullptr;

  /////////////////////////////////////////////////////////////////////////////
  // clCreateContext
  using clCreateContext_Fn = CL_API_ENTRY cl_context(CL_API_CALL *)(
      const cl_context_properties * /* properties */,
      cl_uint /* num_devices */,
      const cl_device_id * /* devices */,
      void(CL_CALLBACK * /* pfn_notify */)(
          const char *, const void *, size_t, void *),
      void * /* user_data */,
      cl_int * /* errcode_ret */);
  clCreateContext_Fn clCreateContext = nullptr;

  /////////////////////////////////////////////////////////////////////////////
  // clGetContextInfo
  using clGetContextInfo_Fn = CL_API_ENTRY cl_int(CL_API_CALL *)(
      cl_context /* context */,
      cl_context_info /* param_name */,
      size_t /* param_value_size */,
      void * /* param_value */,
      size_t * /* param_value_size_ret */);
  clGetContextInfo_Fn clGetContextInfo = nullptr;

  /////////////////////////////////////////////////////////////////////////////
  // clReleaseContext
  using clReleaseContext_Fn = CL_API_ENTRY
                      cl_int(CL_API_CALL *)(cl_context /* context */);
  clReleaseContext_Fn clReleaseContext = nullptr;

  /////////////////////////////////////////////////////////////////////////////
  // clCreateCommandQueueWithProperties
  using clCreateCommandQueueWithProperties_Fn = CL_API_ENTRY
  cl_command_queue(CL_API_CALL *)(
      cl_context /* context */,
      cl_device_id /* device */,
      const cl_queue_properties * /* properties */,
      cl_int * /* errcode_ret */);
  clCreateCommandQueueWithProperties_Fn clCreateCommandQueueWithProperties =
      nullptr;

  /////////////////////////////////////////////////////////////////////////////
  // clCreateCommandQueue
  using clCreateCommandQueue_Fn = CL_API_ENTRY
  cl_command_queue(CL_API_CALL *)(
      cl_context /* context */,
      cl_device_id /* device */,
      cl_command_queue_properties /* properties */,
      cl_int * /* errcode_ret */);
  clCreateCommandQueue_Fn clCreateCommandQueue = nullptr;

  /////////////////////////////////////////////////////////////////////////////
  // clReleaseCommandQueue
  using clReleaseCommandQueue_Fn = CL_API_ENTRY
  cl_int(CL_API_CALL *)(cl_command_queue /* command_queue */);
  clReleaseCommandQueue_Fn clReleaseCommandQueue = nullptr;

  /////////////////////////////////////////////////////////////////////////////
  // clCreateBuffer
  using clCreateBuffer_Fn = CL_API_ENTRY cl_mem(CL_API_CALL *)(
      cl_context /* context */,
      cl_mem_flags /* flags */,
      size_t /* size */,
      void * /* host_ptr */,
      cl_int * /* errcode_ret */);
  clCreateBuffer_Fn clCreateBuffer = nullptr;

  /////////////////////////////////////////////////////////////////////////////
  // clReleaseMemObject
  using clReleaseMemObject_Fn = CL_API_ENTRY
                        cl_int(CL_API_CALL *)(cl_mem /* memobj */);
  clReleaseMemObject_Fn clReleaseMemObject = nullptr;

  /////////////////////////////////////////////////////////////////////////////
  // clGetMemObjectInfo
  using clGetMemObjectInfo_Fn = CL_API_ENTRY cl_int(CL_API_CALL *)(
      cl_mem /* memobj */,
      cl_mem_info /* param_name */,
      size_t /* param_value_size */,
      void * /* param_value */,
      size_t * /* param_value_size_ret */);
  clGetMemObjectInfo_Fn clGetMemObjectInfo = nullptr;

  /////////////////////////////////////////////////////////////////////////////
  // clGetImageInfo
  using clGetImageInfo_Fn = CL_API_ENTRY cl_int(CL_API_CALL *)(
      cl_mem /* image */,
      cl_image_info /* param_name */,
      size_t /* param_value_size */,
      void * /* param_value */,
      size_t * /* param_value_size_ret */);
  clGetImageInfo_Fn clGetImageInfo = nullptr;

  /////////////////////////////////////////////////////////////////////////////
  // clSVMAlloc
  using clSVMAlloc_Fn =
      CL_API_ENTRY void *(CL_API_CALL *)(cl_context /* context */,
                                         cl_svm_mem_flags /* flags */,
                                         size_t /* size */,
                                         cl_uint /* alignment */);
  clSVMAlloc_Fn clSVMAlloc = nullptr;

  /////////////////////////////////////////////////////////////////////////////
  // clSVMFree
  using clSVMFree_Fn = CL_API_ENTRY void(CL_API_CALL *)(
      cl_context /* context */, void * /* svm_pointer */);
  clSVMFree_Fn clSVMFree = nullptr;

  /////////////////////////////////////////////////////////////////////////////
  // clCreateImage
  using clCreateImage_Fn = CL_API_ENTRY cl_mem(CL_API_CALL *)(
      cl_context /* context */,
      cl_mem_flags /* flags */,
      const cl_image_format * /* image_format */,
      const cl_image_desc * /* image_desc */,
      void * /* host_ptr */,
      cl_int * /* errcode_ret */);
  clCreateImage_Fn clCreateImage = nullptr;

  /////////////////////////////////////////////////////////////////////////////
  // clCreateSamplerWithProperties
  using clCreateSamplerWithProperties_Fn = CL_API_ENTRY
  cl_sampler(CL_API_CALL *)(
      cl_context /* context */,
      const cl_sampler_properties * /* normalized_coords */,
      cl_int * /* errcode_ret */);
  clCreateSamplerWithProperties_Fn clCreateSamplerWithProperties = nullptr;

  /////////////////////////////////////////////////////////////////////////////
  // clReleaseSampler
  using clReleaseSampler_Fn      = CL_API_ENTRY
                      cl_int(CL_API_CALL *)(cl_sampler /* sampler */);
  clReleaseSampler_Fn clReleaseSampler = nullptr;

  /////////////////////////////////////////////////////////////////////////////
  // clCreateProgramWithSource
  using clCreateProgramWithSource_Fn = CL_API_ENTRY cl_program(CL_API_CALL *)(
      cl_context /* context */,
      cl_uint /* count */,
      const char ** /* strings */,
      const size_t * /* lengths */,
      cl_int * /* errcode_ret */);
  clCreateProgramWithSource_Fn clCreateProgramWithSource = nullptr;

  /////////////////////////////////////////////////////////////////////////////
  // clCreateProgramWithBinary
  using clCreateProgramWithBinary_Fn = CL_API_ENTRY cl_program(CL_API_CALL *)(
      cl_context /* context */,
      cl_uint /* num_devices */,
      const cl_device_id * /* device_list */,
      const size_t * /* lengths */,
      const unsigned char ** /* binaries */,
      cl_int * /* binary_status */,
      cl_int * /* errcode_ret */);
  clCreateProgramWithBinary_Fn clCreateProgramWithBinary = nullptr;

  /////////////////////////////////////////////////////////////////////////////
  // clCreateProgramWithBuiltInKernels
  using clCreateProgramWithBuiltInKernels_Fn = CL_API_ENTRY
  cl_program(CL_API_CALL *)(
      cl_context /* context */,
      cl_uint /* num_devices */,
      const cl_device_id * /* device_list */,
      const char * /* kernel_names */,
      cl_int * /* errcode_ret */);
  clCreateProgramWithBuiltInKernels_Fn clCreateProgramWithBuiltInKernels =
      nullptr;

  /////////////////////////////////////////////////////////////////////////////
  // clReleaseProgram
  using clCreateProgramWithIL_Fn = CL_API_ENTRY cl_program(CL_API_CALL *)(
      cl_context /* context */,
      const void * /* il */,
      size_t /* length */,
      cl_int * /* errcode_ret */);
  clCreateProgramWithIL_Fn clCreateProgramWithIL = nullptr;

  /////////////////////////////////////////////////////////////////////////////
  // clReleaseProgram
  using clReleaseProgram_Fn = CL_API_ENTRY
                      cl_int(CL_API_CALL *)(cl_program /* program */);
  clReleaseProgram_Fn clReleaseProgram = nullptr;

  /////////////////////////////////////////////////////////////////////////////
  // clGetEventProfilingInfo
  using clBuildProgram_Fn = CL_API_ENTRY cl_int(CL_API_CALL *)(
      cl_program /* program */,
      cl_uint /* num_devices */,
      const cl_device_id * /* device_list */,
      const char * /* options */,
      void(CL_CALLBACK * /* pfn_notify */)(
          cl_program /* program */, void * /* user_data */),
      void * /* user_data */);
  clBuildProgram_Fn clBuildProgram = nullptr;

  /////////////////////////////////////////////////////////////////////////////
  // clCreateKernel
  using clCreateKernel_Fn = CL_API_ENTRY cl_kernel(CL_API_CALL *)(
        cl_program /* program */,
        const char * /* kernel_name */,
        cl_int * /* errcode_ret */);
  clCreateKernel_Fn clCreateKernel = nullptr;

  /////////////////////////////////////////////////////////////////////////////
  // clCreateKernelsInProgram
  using clCreateKernelsInProgram_Fn = CL_API_ENTRY cl_int(CL_API_CALL *)(
      cl_program /* program */,
      cl_uint /* num_kernels */,
      cl_kernel * /* kernels */,
      cl_uint * /* num_kernels_ret */);
  clCreateKernelsInProgram_Fn clCreateKernelsInProgram = nullptr;

  /////////////////////////////////////////////////////////////////////////////
  // clReleaseKernel
  using clReleaseKernel_Fn = CL_API_ENTRY
  cl_int(CL_API_CALL *)(cl_kernel /* kernel */);
  clReleaseKernel_Fn clReleaseKernel = nullptr;

  /////////////////////////////////////////////////////////////////////////////
  // clSetKernelArg
  using clSetKernelArg_Fn = CL_API_ENTRY cl_int(CL_API_CALL *)(
      cl_kernel /* kernel */,
      cl_uint /* arg_index */,
      size_t /* arg_size */,
      const void * /* arg_value */);
  clSetKernelArg_Fn clSetKernelArg = nullptr;

  /////////////////////////////////////////////////////////////////////////////
  // clSetKernelArgSVMPointer
  using clSetKernelArgSVMPointer_Fn = CL_API_ENTRY cl_int(CL_API_CALL *)(
      cl_kernel /* kernel */,
      cl_uint /* arg_index */,
      const void * /* arg_value */);
  clSetKernelArgSVMPointer_Fn clSetKernelArgSVMPointer = nullptr;

  /////////////////////////////////////////////////////////////////////////////
  // clGetKernelInfo
  using clGetKernelInfo_Fn = CL_API_ENTRY cl_int(CL_API_CALL *)(
      cl_kernel /* kernel */,
      cl_kernel_info /* param_name */,
      size_t /* param_value_size */,
      void * /* param_value */,
      size_t * /* param_value_size_ret */);
  clGetKernelInfo_Fn clGetKernelInfo = nullptr;

  /////////////////////////////////////////////////////////////////////////////
  // clGetKernelArgInfo
  using clGetKernelArgInfo_Fn = CL_API_ENTRY cl_int(CL_API_CALL *)(
      cl_kernel /* kernel */,
      cl_uint /* arg_indx */,
      cl_kernel_arg_info /* param_name */,
      size_t /* param_value_size */,
      void * /* param_value */,
      size_t * /* param_value_size_ret */);
  clGetKernelArgInfo_Fn clGetKernelArgInfo = nullptr;

  /////////////////////////////////////////////////////////////////////////////
  // clGetKernelWorkGroupInfo
  using clGetKernelWorkGroupInfo_Fn = CL_API_ENTRY cl_int(CL_API_CALL *)(
      cl_kernel /* kernel */,
      cl_device_id /* device */,
      cl_kernel_work_group_info /* param_name */,
      size_t /* param_value_size */,
      void * /* param_value */,
      size_t * /* param_value_size_ret */);
  clGetKernelWorkGroupInfo_Fn clGetKernelWorkGroupInfo = nullptr;
  /////////////////////////////////////////////////////////////////////////////
  // clGetKernelSubGroupInfo
  using clGetKernelSubGroupInfo_Fn = CL_API_ENTRY cl_int(CL_API_CALL *)(
      cl_kernel /* kernel */,
      cl_device_id /* device */,
      cl_kernel_sub_group_info /* param_name */,
      size_t /* input_value_size */,
      const void * /*input_value */,
      size_t /* param_value_size */,
      void * /* param_value */,
      size_t * /* param_value_size_ret */);
  clGetKernelSubGroupInfo_Fn clGetKernelSubGroupInfo = nullptr;

  /////////////////////////////////////////////////////////////////////////////
  // clWaitForEvents
  using clWaitForEvents_Fn = CL_API_ENTRY cl_int(CL_API_CALL *)(
      cl_uint /* num_events */, const cl_event * /* event_list */);
  clWaitForEvents_Fn clWaitForEvents = nullptr;

  /////////////////////////////////////////////////////////////////////////////
  // clGetEventInfo
  using clGetEventInfo_Fn = CL_API_ENTRY cl_int(CL_API_CALL *)(
      cl_event /* event */,
      cl_event_info /* param_name */,
      size_t /* param_value_size */,
      void * /* param_value */,
      size_t * /* param_value_size_ret */);
  clGetEventInfo_Fn clGetEventInfo = nullptr;
  /////////////////////////////////////////////////////////////////////////////
  // clReleaseEvent
  using clReleaseEvent_Fn = CL_API_ENTRY
                    cl_int(CL_API_CALL *)(cl_event /* event */);
  clReleaseEvent_Fn clReleaseEvent = nullptr;
  /////////////////////////////////////////////////////////////////////////////
  // clGetEventProfilingInfo
  using clGetEventProfilingInfo_Fn = CL_API_ENTRY cl_int(CL_API_CALL *)(
      cl_event /* event */,
      cl_profiling_info /* param_name */,
      size_t /* param_value_size */,
      void * /* param_value */,
      size_t * /* param_value_size_ret */);
  clGetEventProfilingInfo_Fn clGetEventProfilingInfo = nullptr;

  /////////////////////////////////////////////////////////////////////////////
  // clFlush
  using clFlush_Fn = CL_API_ENTRY
              cl_int(CL_API_CALL *)(cl_command_queue /* command_queue */);
  clFlush_Fn clFlush = nullptr;
  /////////////////////////////////////////////////////////////////////////////
  // clFinish
  using clFinish_Fn = CL_API_ENTRY
              cl_int(CL_API_CALL *)(cl_command_queue /* command_queue */);
  clFinish_Fn clFinish = nullptr;

  /////////////////////////////////////////////////////////////////////////////
  // clEnqueueReadBuffer
  using clEnqueueReadBuffer_Fn = CL_API_ENTRY cl_int(CL_API_CALL *)(
      cl_command_queue /* command_queue */,
      cl_mem /* buffer */,
      cl_bool /* blocking_read */,
      size_t /* offset */,
      size_t /* size */,
      void * /* ptr */,
      cl_uint /* num_events_in_wait_list */,
      const cl_event * /* event_wait_list */,
      cl_event * /* event */);
  clEnqueueReadBuffer_Fn clEnqueueReadBuffer = nullptr;

  /////////////////////////////////////////////////////////////////////////////
  // clEnqueueWriteBuffer
  using clEnqueueWriteBuffer_Fn = CL_API_ENTRY cl_int(CL_API_CALL *)(
      cl_command_queue /* command_queue */,
      cl_mem /* buffer */,
      cl_bool /* blocking_write */,
      size_t /* offset */,
      size_t /* size */,
      const void * /* ptr */,
      cl_uint /* num_events_in_wait_list */,
      const cl_event * /* event_wait_list */,
      cl_event * /* event */);
  clEnqueueWriteBuffer_Fn clEnqueueWriteBuffer = nullptr;

  /////////////////////////////////////////////////////////////////////////////
  // clEnqueueWriteBuffer
  using clEnqueueReadImage_Fn = CL_API_ENTRY cl_int (CL_API_CALL *)
  (cl_command_queue     /* command_queue */,
                   cl_mem               /* image */,
                   cl_bool              /* blocking_read */,
                   const size_t *       /* origin[3] */,
                   const size_t *       /* region[3] */,
                   size_t               /* row_pitch */,
                   size_t               /* slice_pitch */,
                   void *               /* ptr */,
                   cl_uint              /* num_events_in_wait_list */,
                   const cl_event *     /* event_wait_list */,
                   cl_event *           /* event */);
  clEnqueueReadImage_Fn clEnqueueReadImage = nullptr;

  /////////////////////////////////////////////////////////////////////////////
  // clEnqueueWriteImage
  using clEnqueueWriteImage_Fn = CL_API_ENTRY cl_int(CL_API_CALL *)(
      cl_command_queue /* command_queue */,
      cl_mem /* image */,
      cl_bool /* blocking_write */,
      const size_t * /* origin[3] */,
      const size_t * /* region[3] */,
      size_t /* input_row_pitch */,
      size_t /* input_slice_pitch */,
      const void * /* ptr */,
      cl_uint /* num_events_in_wait_list */,
      const cl_event * /* event_wait_list */,
      cl_event * /* event */);
  clEnqueueWriteImage_Fn clEnqueueWriteImage = nullptr;

  /////////////////////////////////////////////////////////////////////////////
  // clEnqueueMapBuffer
  using clEnqueueMapBuffer_Fn =
      CL_API_ENTRY void *(CL_API_CALL *)(cl_command_queue /* command_queue */,
                                         cl_mem /* buffer */,
                                         cl_bool /* blocking_map */,
                                         cl_map_flags /* map_flags */,
                                         size_t /* offset */,
                                         size_t /* size */,
                                         cl_uint /* num_events_in_wait_list */,
                                         const cl_event * /* event_wait_list */,
                                         cl_event * /* event */,
                                         cl_int * /* errcode_ret */);
  clEnqueueMapBuffer_Fn clEnqueueMapBuffer = nullptr;

  /////////////////////////////////////////////////////////////////////////////
  // clEnqueueMapImage
  using clEnqueueMapImage_Fn =
      CL_API_ENTRY void *(CL_API_CALL *)(cl_command_queue /* command_queue */,
                                         cl_mem /* image */,
                                         cl_bool /* blocking_map */,
                                         cl_map_flags /* map_flags */,
                                         const size_t * /* origin[3] */,
                                         const size_t * /* region[3] */,
                                         size_t * /* image_row_pitch */,
                                         size_t * /* image_slice_pitch */,
                                         cl_uint /* num_events_in_wait_list */,
                                         const cl_event * /* event_wait_list */,
                                         cl_event * /* event */,
                                         cl_int * /* errcode_ret */);
  clEnqueueMapImage_Fn clEnqueueMapImage = nullptr;

  /////////////////////////////////////////////////////////////////////////////
  // clEnqueueUnmapMemObject
    using clEnqueueUnmapMemObject_Fn = CL_API_ENTRY cl_int(CL_API_CALL *)(
        cl_command_queue /* command_queue */,
        cl_mem /* memobj */,
        void * /* mapped_ptr */,
        cl_uint /* num_events_in_wait_list */,
        const cl_event * /* event_wait_list */,
        cl_event * /* event */);
  clEnqueueUnmapMemObject_Fn clEnqueueUnmapMemObject = nullptr;

  /////////////////////////////////////////////////////////////////////////////
  // clEnqueueNDRangeKernel
  using clEnqueueNDRangeKernel_Fn = CL_API_ENTRY cl_int(CL_API_CALL *)(
      cl_command_queue /* command_queue */,
      cl_kernel /* kernel */,
      cl_uint /* work_dim */,
      const size_t * /* global_work_offset */,
      const size_t * /* global_work_size */,
      const size_t * /* local_work_size */,
      cl_uint /* num_events_in_wait_list */,
      const cl_event * /* event_wait_list */,
      cl_event * /* event */);
  clEnqueueNDRangeKernel_Fn clEnqueueNDRangeKernel = nullptr;

  /////////////////////////////////////////////////////////////////////////////
  /////////////////////////////////////////////////////////////////////////////
  /////////////////////////////////////////////////////////////////////////////
  // EXTENSION cl_khr_icd
  /////////////////////////////////////////////////////////////////////////////
  // clGetExtensionFunctionAddressForPlatform
  using clGetExtensionFunctionAddressForPlatform_Fn =
      CL_API_ENTRY void *(CL_API_CALL *)(cl_platform_id /* platform */,
                                         const char * /* func_name */);
  clGetExtensionFunctionAddressForPlatform_Fn
      clGetExtensionFunctionAddressForPlatform = nullptr;

  /////////////////////////////////////////////////////////////////////////////
  /////////////////////////////////////////////////////////////////////////////
  /////////////////////////////////////////////////////////////////////////////
  // EXTENSION Intel Graphics
  /////////////////////////////////////////////////////////////////////////////
  // clCreatePerfCountersCommandQueueINTEL
  using clCreatePerfCountersCommandQueueINTEL_Fn = CL_API_ENTRY
  cl_command_queue(CL_API_CALL *)(
      cl_context /* context */,
      cl_device_id /* device */,
      cl_command_queue_properties /* properties */,
      cl_uint /* configuration */,
      cl_int * /* errcode_ret */);
  clCreatePerfCountersCommandQueueINTEL_Fn
      clCreatePerfCountersCommandQueueINTEL = nullptr;

  /////////////////////////////////////////////////////////////////////////////
  static std::string status_to_symbol(cl_int error);
}; // cl_lib

#endif // CL_LIB_HPP