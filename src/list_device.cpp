///////////////////////////////////////////////////////////////////////////////
//
#include "devices.hpp"
#include "text.hpp"
#include "system.hpp"

#include <algorithm>
#include <iostream>
#include <functional>




#define DEVICE_INFO_KEY           ANSI_WHITE
#define DEVICE_INFO_VALUE_COLOR   ANSI_YELLOW
#define DEVICE_INFO_COLS          24



///////////////////////////////////////////////////////////////////////////////
// specific value type formatters
// NOTE: we cannot use any form of polymorphism because many types are aliased
// E.g. cl_bool = cl_uint
static void formatDeviceType(std::ostream &os, cl_device_type value)
{
  switch (value) {
  case CL_DEVICE_TYPE_CPU: os << "CL_DEVICE_TYPE_CPU"; break;
  case CL_DEVICE_TYPE_GPU: os << "CL_DEVICE_TYPE_GPU"; break;
  case CL_DEVICE_TYPE_ACCELERATOR: os << "CL_DEVICE_TYPE_ACCELERATOR"; break;
  case CL_DEVICE_TYPE_DEFAULT: os << "CL_DEVICE_TYPE_DEFAULT"; break;
  default: os << "0x" << std::hex << value << "?\n"; break;
  }
}

static void formatDeviceLocalMemType(std::ostream &os, cl_device_local_mem_type value)
{
  switch (value) {
  case CL_LOCAL: os << "CL_LOCAL"; break;
  case CL_GLOBAL: os << "CL_GLOBAL"; break;
  default: os << "0x" << std::hex << value << "?\n"; break;
  }
}

static void formatCacheType(std::ostream &os, cl_device_mem_cache_type value)
{
  switch (value) {
  case CL_NONE: os << "CL_NONE"; break;
  case CL_READ_ONLY_CACHE: os << "CL_READ_ONLY_CACHE"; break;
  case CL_READ_WRITE_CACHE: os << "CL_READ_WRITE_CACHE"; break;
  default: os << "0x" << std::hex << value << "?\n"; break;
  }
}
static void formatBool(std::ostream &os, cl_bool value)
{
  if (value == CL_FALSE)
    os << "CL_FALSE";
  else if (value == CL_TRUE)
    os << "CL_FALSE";
  else
    os << (int)value << "?";
}
// assumes     value
// assumes     const char *sep ...;
#define BITSET_CASE(X) \
  { \
    if (value & X) { \
      os << sep << #X; \
      sep = "|"; \
      value &= ~(X); \
    } \
  }
#define BITSET_FINISH() \
    if (value != 0) { \
      os << sep << "0x" << std::hex << value; \
    }
static void formatCommandQueueProperties(
    std::ostream &os, cl_command_queue_properties value)
{
    const char *sep = ""; // "|";
    BITSET_CASE(CL_QUEUE_OUT_OF_ORDER_EXEC_MODE_ENABLE);
    BITSET_CASE(CL_QUEUE_PROFILING_ENABLE);
    BITSET_CASE(CL_QUEUE_ON_DEVICE);
    BITSET_CASE(CL_QUEUE_ON_DEVICE_DEFAULT);
    BITSET_FINISH();
}
static void formatDeviceFpConfig(
  std::ostream &os, cl_device_fp_config value)
{
    const char *sep = ""; // "|";
    BITSET_CASE(CL_FP_DENORM);
    BITSET_CASE(CL_FP_INF_NAN);
    BITSET_CASE(CL_FP_ROUND_TO_NEAREST);
    BITSET_CASE(CL_FP_ROUND_TO_ZERO);
    BITSET_CASE(CL_FP_ROUND_TO_INF);
    BITSET_CASE(CL_FP_FMA);
    BITSET_CASE(CL_FP_SOFT_FLOAT);
    BITSET_CASE(CL_FP_CORRECTLY_ROUNDED_DIVIDE_SQRT);
    BITSET_FINISH();
}
static void formatDeviceExecCapabilities(
  std::ostream &os, cl_device_exec_capabilities value)
{
    const char *sep = ""; // "|";
    BITSET_CASE(CL_EXEC_KERNEL);
    BITSET_CASE(CL_EXEC_NATIVE_KERNEL);
    BITSET_FINISH();
}
static void formatDeviceSvmCapabilities(
  std::ostream &os, cl_device_svm_capabilities value)
{
    const char *sep = ""; // "|";
    BITSET_CASE(CL_DEVICE_SVM_COARSE_GRAIN_BUFFER);
    BITSET_CASE(CL_DEVICE_SVM_FINE_GRAIN_BUFFER);
    BITSET_CASE(CL_DEVICE_SVM_FINE_GRAIN_SYSTEM);
    BITSET_CASE(CL_DEVICE_SVM_ATOMICS);
    BITSET_FINISH();
}

template <typename T,int W>
static void fmtHex(
  std::ostream &os,
  T value)
{
  std::stringstream ss;
  ss << "0x" << std::setfill('0') << std::hex << std::setw(W) << value;
  os << ss.str();
}
static void fmtDeviceId(
  std::ostream &os,
  cl_uint x)
{
  fmtHex<cl_uint,4>(os,x);
}
///////////////////////////////////////////////////////////////////////////////
// new approach


template <typename T>
using Formatter = std::function<void(std::ostream&,T)>;

template <typename T>
static Formatter<T> default_formatter =
  [] (std::ostream& os, T value) {
      os << value;
  };

template <typename T>
static Formatter<T> mem_size_formatter =
  [] (std::ostream &os,T value) {
    const char *units = "B";
    if (value % (1024 * 1024) == 0) {
      value = (value >> 20);
      units = "MB";
    } else if (value % 1024 == 0) {
      value = (value >> 10);
      units = "KB";
    }
    default_formatter<T>(os, value);
    os << ANSI_RESET;
    os << " " << units;
  };

template <typename T>
static void emitDeviceInfo(
  std::ostream &os,
  cl_device_id dev_id,
  cl_device_info param,
  Formatter<T> format_value,
  const char *units = nullptr)
{
  T value;
  auto err = clGetDeviceInfo(dev_id, param, sizeof(T), &value, nullptr);
  if (err != CL_SUCCESS) {
    os <<
      ANSI_RED <<
      "[ERROR: " << cls::status_to_symbol(err) << "]" <<
      ANSI_RESET;
    return;
  }

  std::stringstream ss;
  format_value(ss, value);
  os << DEVICE_INFO_VALUE_COLOR <<
    std::setw(DEVICE_INFO_COLS) <<
    std::right <<
    ss.str();
  os << ANSI_RESET;

  if (units)
    std::cout << " " << units;
}

template <>
static void emitDeviceInfo(
  std::ostream &os,
  cl_device_id dev_id,
  cl_device_info param,
  Formatter<char*> format_value,
  const char *units)
{
  size_t size;
  auto err1 = clGetDeviceInfo(dev_id, param, 0, nullptr, &size);
  if (err1 != CL_SUCCESS) {
    os <<
      ANSI_RED <<
      "[ERROR: " << cls::status_to_symbol(err1) << "]" <<
      ANSI_RESET;
    return;
  }

  char *value = (char *)alloca(size + 1);
  auto err2 = clGetDeviceInfo(dev_id, param, size, value, nullptr);
  if (err2 != CL_SUCCESS) {
    os <<
      ANSI_RED <<
      "[ERROR: " << cls::status_to_symbol(err2) << "]" <<
      ANSI_RESET;
    return;
  }
  value[size] = 0;

  os << DEVICE_INFO_VALUE_COLOR << std::setw(DEVICE_INFO_COLS) << std::right;
  format_value(os, value);
  os << ANSI_RESET;

  if (units)
    std::cout << " " << units;
}


// array version
template <typename T>
static void emitDeviceInfo(
  std::ostream &os,
  cl_device_id dev_id,
  cl_device_info param,
  cl_device_info param_len,
  Formatter<T> format_value,
  const char *units = nullptr)
{
  cl_uint array_length;
  auto err1 = clGetDeviceInfo(
    dev_id, param_len, sizeof(array_length), &array_length, nullptr);
  if (err1 != CL_SUCCESS) {
    os <<
      ANSI_RED <<
      "[ERROR: " << cls::status_to_symbol(err1) << "]" <<
      ANSI_RESET;
    return;
  }

  T *value = (T *)alloca(array_length * sizeof(T));
  auto err2 = clGetDeviceInfo(
    dev_id, param, array_length * sizeof(T), value, nullptr);
  if (err2 != CL_SUCCESS) {
    os <<
      ANSI_RED <<
      "[ERROR: " << cls::status_to_symbol(err2) << "]" <<
      ANSI_RESET;
    return;
  }

  std::stringstream ss;
  ss << "{";
  for (cl_uint i = 0; i < array_length; i++) {
    if (i > 0)
      ss << ", ";
    format_value(ss, value[i]);
  }
  ss << "}";
  os << DEVICE_INFO_VALUE_COLOR << std::setw(DEVICE_INFO_COLS) << std::right <<
    ss.str() << ANSI_RESET;

  if (units)
    std::cout << " " << units;
}




static void emitParamName(const char *prop)
{
  std::cout << "    " << DEVICE_INFO_KEY << prop << ANSI_RESET << ": ";
  for (int i = 0, len = 48 - (int)strlen(prop); i < len; i++)
    std::cout << ' ';
}


void listDeviceInfoForDevice(const cls::Opts &os, const cl::Device &d, int devIx)
{
#define DEVICE_INFO_WITH0(PARAM_STR,PARAM,TYPE,FORMATTER,...) \
  { \
    emitParamName(PARAM_STR); \
    emitDeviceInfo<TYPE>(std::cout,d(),PARAM,FORMATTER,__VA_ARGS__); \
    std::cout << "\n"; \
  }
#define DEVICE_INFO(PARAM,TYPE,...) \
  DEVICE_INFO_WITH0(#PARAM,PARAM,TYPE,default_formatter<TYPE>,__VA_ARGS__)
#define DEVICE_INFO_WITH(PARAM,TYPE,FORMATTER,...) \
  DEVICE_INFO_WITH0(#PARAM,PARAM,TYPE,FORMATTER,__VA_ARGS__)
#define DEVICE_INFO_MEMSIZE(PARAM,TYPE) \
  DEVICE_INFO_WITH0(#PARAM,PARAM,TYPE,mem_size_formatter<TYPE>)
#define DEVICE_INFO_BOOL(PARAM) \
  DEVICE_INFO_WITH0(#PARAM,PARAM,cl_bool,formatBool)
#define DEVICE_INFO_STRING(PARAM) \
  DEVICE_INFO_WITH0(#PARAM,PARAM,char*,default_formatter<char*>)

#define DEVICE_INFO_ARRAY_WITH0(PARAM_STR,PARAM,TYPE,PARAM_LEN,FORMATTER) \
  { \
    emitParamName(PARAM_STR); \
    emitDeviceInfo<TYPE>(std::cout,d(),PARAM,PARAM_LEN,FORMATTER); \
    std::cout << "\n"; \
  }
#define DEVICE_INFO_ARRAY_WITH(PARAM,TYPE,PARAM_LEN,FORMATTER) \
  DEVICE_INFO_ARRAY_WITH0(#PARAM,PARAM,TYPE,PARAM_LEN,FORMATTER)
#define DEVICE_INFO_ARRAY(PARAM,TYPE,PARAM_LEN) \
  DEVICE_INFO_ARRAY_WITH0(#PARAM,PARAM,TYPE,PARAM_LEN,default_formatter<TYPE>)

  //
  //
  if (devIx >= 0) {
    std::cout << "DEVICE[" << devIx << "]: ";
  }
  auto vend = getDeviceVendor(d);
  bool is_intc = vend == cl_vendor::CL_INTEL;
  bool is_nvda = vend == cl_vendor::CL_NVIDIA;
  bool is_amd =  vend == cl_vendor::CL_AMD;

  if (is_intc) {
    std::cout << ANSI_COLOR_INTEL_BLUE;
  } else if (is_nvda) {
    std::cout << ANSI_COLOR_NVIDIA_GREEN;
  } else if (is_amd) {
    std::cout << ANSI_COLOR_AMD_ORANGE;
  }
  std::string device_name = d.getInfo<CL_DEVICE_NAME>().c_str();
  std::cout << std::setw(48) << std::left << device_name;
  std::cout << ANSI_RESET;
  if (os.verbosity <= 0) {
    auto dev_type = d.getInfo<CL_DEVICE_TYPE>();
    std::cout << " " << std::setw(4);
    switch (dev_type) {
    case CL_DEVICE_TYPE_CPU: std::cout << "CPU"; break;
    case CL_DEVICE_TYPE_GPU: std::cout << "GPU"; break;
    case CL_DEVICE_TYPE_ACCELERATOR: std::cout << "ACCELERATOR"; break;
    case CL_DEVICE_TYPE_DEFAULT: std::cout << "DEFAULT"; break;
    default: std::cout << "0x" << std::hex << dev_type << "?\n"; break;
    }
    std::cout <<
      "    " <<
      d.getInfo<CL_DEVICE_OPENCL_C_VERSION>().c_str() << "\n";
    return;
  }
  std::cout << "\n";

  auto spec = getDeviceSpec(d);

  bool is_2_2_plus = spec >= cl_spec::CL_2_2;
  bool is_2_1_plus = spec >= cl_spec::CL_2_1;
  bool is_2_0_plus = spec >= cl_spec::CL_2_0;
  bool is_1_2_plus = spec >= cl_spec::CL_1_2;
  bool is_1_1_plus = spec >= cl_spec::CL_1_1;

  std::string extensions_string = d.getInfo<CL_DEVICE_EXTENSIONS>().c_str();
  auto hasExtension = [&](const char *ext) {
    return extensions_string.find(ext) != std::string::npos;
  };

  auto START_GROUP = [](const char *name) {
    std::cout << "  === " <<  name << ":\n";
  };

   DEVICE_INFO_WITH(
     CL_DEVICE_QUEUE_ON_HOST_PROPERTIES, // CL_DEVICE_QUEUE_PROPERTIES
     cl_command_queue_properties,
     formatCommandQueueProperties);


  /////////////////////////////////////////////////////////////////////////////
  START_GROUP("DEVICE");
  DEVICE_INFO_WITH(CL_DEVICE_TYPE,cl_device_type,formatDeviceType);
  DEVICE_INFO_STRING(CL_DEVICE_VERSION);
  DEVICE_INFO_STRING(CL_DEVICE_VENDOR);
  DEVICE_INFO_WITH(CL_DEVICE_VENDOR_ID,cl_uint,fmtHex<cl_uint,4>);
  DEVICE_INFO_STRING(CL_DEVICE_OPENCL_C_VERSION);
  DEVICE_INFO_BOOL(CL_DEVICE_AVAILABLE);
  DEVICE_INFO_STRING(CL_DRIVER_VERSION);
  DEVICE_INFO_BOOL(CL_DEVICE_PREFERRED_INTEROP_USER_SYNC);
  DEVICE_INFO_STRING(CL_DEVICE_PROFILE);
  if (is_1_2_plus) {
    DEVICE_INFO(CL_DEVICE_PARTITION_MAX_SUB_DEVICES,cl_uint);
  }
  if (is_2_0_plus) {
   DEVICE_INFO_WITH(
     CL_DEVICE_QUEUE_ON_HOST_PROPERTIES, // CL_DEVICE_QUEUE_PROPERTIES
     cl_command_queue_properties,
     formatCommandQueueProperties);
  }
  if (is_2_1_plus || hasExtension("cl_khr_il_program")) {
    DEVICE_INFO_STRING(CL_DEVICE_IL_VERSION);
  }

  /////////////////////////////////////////////////////////////////////////////
  START_GROUP("COMPUTE");
  DEVICE_INFO(CL_DEVICE_MAX_CLOCK_FREQUENCY,cl_uint,"MHz");
  DEVICE_INFO(CL_DEVICE_MAX_COMPUTE_UNITS,cl_uint);
  DEVICE_INFO(CL_DEVICE_PROFILING_TIMER_RESOLUTION,size_t,"ns");
  DEVICE_INFO_BOOL(CL_DEVICE_ENDIAN_LITTLE);
  DEVICE_INFO_STRING(CL_DEVICE_BUILT_IN_KERNELS);
  DEVICE_INFO_WITH(
    CL_DEVICE_SINGLE_FP_CONFIG, cl_device_fp_config, formatDeviceFpConfig);
  if (hasExtension("cl_khr_fp16")) {
    DEVICE_INFO_WITH(
      CL_DEVICE_HALF_FP_CONFIG, cl_device_fp_config, formatDeviceFpConfig);
  }
  if (hasExtension("cl_khr_fp64")) {
    DEVICE_INFO_WITH(
      CL_DEVICE_DOUBLE_FP_CONFIG, cl_device_fp_config, formatDeviceFpConfig);
  }
  if (hasExtension("cl_khr_fp16")) {
    DEVICE_INFO(CL_DEVICE_PREFERRED_VECTOR_WIDTH_HALF,cl_uint);
    DEVICE_INFO(CL_DEVICE_NATIVE_VECTOR_WIDTH_HALF,cl_uint);
  }
  DEVICE_INFO(CL_DEVICE_PREFERRED_VECTOR_WIDTH_FLOAT,cl_uint);
  DEVICE_INFO(CL_DEVICE_NATIVE_VECTOR_WIDTH_FLOAT,cl_uint);
  if (hasExtension("cl_khr_fp64")) {
    DEVICE_INFO(CL_DEVICE_PREFERRED_VECTOR_WIDTH_DOUBLE,cl_uint);
    DEVICE_INFO(CL_DEVICE_NATIVE_VECTOR_WIDTH_DOUBLE,cl_uint);
  }

  /////////////////////////////////////////////////////////////////////////////
  START_GROUP("WORKGROUPS");
  DEVICE_INFO(CL_DEVICE_MAX_WORK_GROUP_SIZE,size_t,"items");
  // DEVICE_INFO(CL_DEVICE_MAX_WORK_ITEM_DIMENSIONS,cl_uint,"dimensions");
  DEVICE_INFO_ARRAY(
    CL_DEVICE_MAX_WORK_ITEM_SIZES, size_t, CL_DEVICE_MAX_WORK_ITEM_DIMENSIONS);
  if (is_2_1_plus || hasExtension("cl_khr_subgroups")) {
    DEVICE_INFO(CL_DEVICE_MAX_NUM_SUB_GROUPS,cl_uint);
    DEVICE_INFO_BOOL(CL_DEVICE_SUB_GROUP_INDEPENDENT_FORWARD_PROGRESS);
  }

  /////////////////////////////////////////////////////////////////////////////
  START_GROUP("MEMORY");
  if (is_1_2_plus)
    DEVICE_INFO(CL_DEVICE_PRINTF_BUFFER_SIZE,size_t);
  DEVICE_INFO(CL_DEVICE_ADDRESS_BITS,cl_uint,"b");
  DEVICE_INFO_MEMSIZE(CL_DEVICE_MEM_BASE_ADDR_ALIGN,cl_uint);
  DEVICE_INFO_MEMSIZE(CL_DEVICE_MAX_CONSTANT_BUFFER_SIZE,cl_ulong);
  DEVICE_INFO_MEMSIZE(CL_DEVICE_LOCAL_MEM_SIZE,cl_ulong);
  DEVICE_INFO_WITH(
    CL_DEVICE_LOCAL_MEM_TYPE,
    cl_device_local_mem_type,
    formatDeviceLocalMemType);
  DEVICE_INFO_MEMSIZE(CL_DEVICE_GLOBAL_MEM_SIZE,cl_ulong);
  DEVICE_INFO_MEMSIZE(CL_DEVICE_MAX_MEM_ALLOC_SIZE,cl_ulong);
  DEVICE_INFO_MEMSIZE(CL_DEVICE_GLOBAL_MEM_CACHE_SIZE,cl_ulong);
  DEVICE_INFO_WITH(
    CL_DEVICE_GLOBAL_MEM_CACHE_TYPE,
    cl_device_mem_cache_type,
    formatCacheType);
  DEVICE_INFO_MEMSIZE(CL_DEVICE_GLOBAL_MEM_CACHELINE_SIZE,cl_uint);
  DEVICE_INFO_BOOL(CL_DEVICE_ERROR_CORRECTION_SUPPORT);
  if (is_2_0_plus) {
    DEVICE_INFO_WITH(
      CL_DEVICE_SVM_CAPABILITIES,
      cl_device_svm_capabilities,
      formatDeviceSvmCapabilities);
    DEVICE_INFO(CL_DEVICE_PREFERRED_LOCAL_ATOMIC_ALIGNMENT,cl_uint,"B");
    DEVICE_INFO(CL_DEVICE_PREFERRED_PLATFORM_ATOMIC_ALIGNMENT,cl_uint,"B");

    DEVICE_INFO(CL_DEVICE_PIPE_MAX_PACKET_SIZE,cl_uint);
  }

  /////////////////////////////////////////////////////////////////////////////
  START_GROUP("IMAGES");
  DEVICE_INFO_BOOL(CL_DEVICE_IMAGE_SUPPORT);
  DEVICE_INFO(CL_DEVICE_IMAGE2D_MAX_HEIGHT,size_t,"px");
  DEVICE_INFO(CL_DEVICE_IMAGE2D_MAX_WIDTH,size_t,"px");
  DEVICE_INFO(CL_DEVICE_IMAGE3D_MAX_HEIGHT,size_t,"px");
  DEVICE_INFO(CL_DEVICE_IMAGE3D_MAX_WIDTH,size_t,"px");
  DEVICE_INFO(CL_DEVICE_IMAGE3D_MAX_DEPTH,size_t,"px");
  DEVICE_INFO(CL_DEVICE_MAX_SAMPLERS,cl_uint);
  if (is_1_2_plus) {
    DEVICE_INFO(CL_DEVICE_IMAGE_PITCH_ALIGNMENT,cl_uint);
    DEVICE_INFO(CL_DEVICE_IMAGE_BASE_ADDRESS_ALIGNMENT,cl_uint);
  }

  /////////////////////////////////////////////////////////////////////////////
  if (hasExtension("cl_intel_simultaneous_sharing")) {
    START_GROUP("cl_intel_simultaneous_sharing");
#ifndef CL_DEVICE_SIMULTANEOUS_INTEROPS_INTEL
#define CL_DEVICE_SIMULTANEOUS_INTEROPS_INTEL           0x4104
#endif
#ifndef CL_DEVICE_NUM_SIMULTANEOUS_INTEROPS_INTEL
#define CL_DEVICE_NUM_SIMULTANEOUS_INTEROPS_INTEL       0x4105
#endif
    DEVICE_INFO(CL_DEVICE_NUM_SIMULTANEOUS_INTEROPS_INTEL, cl_uint);
    DEVICE_INFO_ARRAY(
      CL_DEVICE_SIMULTANEOUS_INTEROPS_INTEL,
      cl_uint,
      CL_DEVICE_NUM_SIMULTANEOUS_INTEROPS_INTEL);
  }

  /////////////////////////////////////////////////////////////////////////////
  if (hasExtension("cl_intel_advanced_motion_estimation")) {
    START_GROUP("cl_intel_advanced_motion_estimation");
#ifndef CL_DEVICE_ME_VERSION_INTEL
#define CL_DEVICE_ME_VERSION_INTEL                      0x407E
#endif
    DEVICE_INFO(CL_DEVICE_ME_VERSION_INTEL,cl_uint);
  }

  /////////////////////////////////////////////////////////////////////////////
  if (hasExtension("cl_intel_XXXXX")) {
    START_GROUP("cl_intel_XXXXX");
#ifndef CL_DEVICE_TRANSFORM_MASK_MAX_WIDTH_INTEL
#define CL_DEVICE_TRANSFORM_MASK_MAX_WIDTH_INTEL        0x409C
#endif
#ifndef CL_DEVICE_TRANSFORM_MASK_MAX_HEIGHT_INTEL
#define CL_DEVICE_TRANSFORM_MASK_MAX_HEIGHT_INTEL       0x409D
#endif
#ifndef CL_DEVICE_TRANSFORM_FILTER_MAX_WIDTH_INTEL
#define CL_DEVICE_TRANSFORM_FILTER_MAX_WIDTH_INTEL      0x409E
#endif
#ifndef CL_DEVICE_TRANSFORM_FILTER_MAX_HEIGHT_INTEL
#define CL_DEVICE_TRANSFORM_FILTER_MAX_HEIGHT_INTEL     0x409F
#endif
    // I don't know the return type for sure and can't find the extension
    // DEVICE_INFO(CL_DEVICE_TRANSFORM_MASK_MAX_WIDTH_INTEL,cl_uint);
    // DEVICE_INFO(CL_DEVICE_TRANSFORM_MASK_MAX_HEIGHT_INTEL,cl_uint);
    // DEVICE_INFO(CL_DEVICE_TRANSFORM_FILTER_MAX_WIDTH_INTEL,cl_uint);
    // DEVICE_INFO(CL_DEVICE_TRANSFORM_FILTER_MAX_HEIGHT_INTEL,cl_uint);
  }

  if (hasExtension("cl_nv_device_attribute_query")) {
    // NVidia device properties
    // https://www.khronos.org/registry/cl/extensions/nv/cl_nv_device_attribute_query.txt
    START_GROUP("cl_nv_device_attribute_query");
#ifndef CL_DEVICE_COMPUTE_CAPABILITY_MAJOR_NV
#define CL_DEVICE_COMPUTE_CAPABILITY_MAJOR_NV       0x4000
#endif
#ifndef CL_DEVICE_COMPUTE_CAPABILITY_MINOR_NV
#define CL_DEVICE_COMPUTE_CAPABILITY_MINOR_NV       0x4001
#endif
#ifndef CL_DEVICE_REGISTERS_PER_BLOCK_NV
#define CL_DEVICE_REGISTERS_PER_BLOCK_NV            0x4002
#endif
#ifndef CL_DEVICE_WARP_SIZE_NV
#define CL_DEVICE_WARP_SIZE_NV                      0x4003
#endif
#ifndef CL_DEVICE_GPU_OVERLAP_NV
#define CL_DEVICE_GPU_OVERLAP_NV                    0x4004
#endif
#ifndef CL_DEVICE_KERNEL_EXEC_TIMEOUT_NV
#define CL_DEVICE_KERNEL_EXEC_TIMEOUT_NV            0x4005
#endif
#ifndef CL_DEVICE_INTEGRATED_MEMORY_NV
#define CL_DEVICE_INTEGRATED_MEMORY_NV              0x4006
#endif
#ifndef CL_DEVICE_ATTRIBUTE_ASYNC_ENGINE_COUNT_NV
#define CL_DEVICE_ATTRIBUTE_ASYNC_ENGINE_COUNT_NV   0x4007
#endif
#ifndef CL_DEVICE_PCI_BUS_ID_NV
#define CL_DEVICE_PCI_BUS_ID_NV                     0x4008
#endif
#ifndef CL_DEVICE_PCI_SLOT_ID_NV
#define CL_DEVICE_PCI_SLOT_ID_NV                    0x4009
#endif
    DEVICE_INFO(CL_DEVICE_COMPUTE_CAPABILITY_MAJOR_NV,cl_uint);
    DEVICE_INFO(CL_DEVICE_COMPUTE_CAPABILITY_MINOR_NV,cl_uint);
    DEVICE_INFO(CL_DEVICE_REGISTERS_PER_BLOCK_NV,cl_uint);
    DEVICE_INFO(CL_DEVICE_WARP_SIZE_NV,cl_uint,"channels");
    DEVICE_INFO(CL_DEVICE_REGISTERS_PER_BLOCK_NV,cl_uint);
    DEVICE_INFO_BOOL(CL_DEVICE_GPU_OVERLAP_NV);
    DEVICE_INFO_BOOL(CL_DEVICE_KERNEL_EXEC_TIMEOUT_NV);
    DEVICE_INFO_BOOL(CL_DEVICE_INTEGRATED_MEMORY_NV);
    DEVICE_INFO(CL_DEVICE_PCI_BUS_ID_NV,cl_uint);
    DEVICE_INFO(CL_DEVICE_PCI_SLOT_ID_NV,cl_uint);
  }

  if (hasExtension("cl_amd_device_attribute_query")) {
    // https://www.khronos.org/registry/OpenCL/extensions/amd/cl_amd_device_attribute_query.txt
    //
    // cl_device_topology_amd topology
    // #define CL_DEVICE_TOPOLOGY_AMD 0x4037
    // topology.raw.type == CL_DEVICE_TOPOLOGY_TYPE_PCIE_AMD
    //
  START_GROUP("cl_amd_device_attribute_query");
#ifndef CL_DEVICE_SIMD_PER_COMPUTE_UNIT_AMD
#define CL_DEVICE_SIMD_PER_COMPUTE_UNIT_AMD 0x4040
#endif
#ifndef CL_DEVICE_SIMD_WIDTH_AMD
#define CL_DEVICE_SIMD_WIDTH_AMD 0x4041
#endif
#ifndef CL_DEVICE_SIMD_INSTRUCTION_WIDTH_AMD
#define CL_DEVICE_SIMD_INSTRUCTION_WIDTH_AMD 0x4042
#endif
#ifndef CL_DEVICE_WAVEFRONT_WIDTH_AMD
#define CL_DEVICE_WAVEFRONT_WIDTH_AMD 0x4043
#endif
#ifndef CL_DEVICE_PCIE_ID_AMD
#define CL_DEVICE_PCIE_ID_AMD 0x4034
#endif
#ifndef CL_DEVICE_GFXIP_MAJOR_AMD
#define CL_DEVICE_GFXIP_MAJOR_AMD 0x404A
#endif
#ifndef CL_DEVICE_GFXIP_MINOR_AMD
#define CL_DEVICE_GFXIP_MINOR_AMD 0x404B
#endif
    DEVICE_INFO(CL_DEVICE_SIMD_PER_COMPUTE_UNIT_AMD, cl_uint);
    DEVICE_INFO(CL_DEVICE_SIMD_WIDTH_AMD, cl_uint);
    DEVICE_INFO(CL_DEVICE_SIMD_INSTRUCTION_WIDTH_AMD, cl_uint);
    DEVICE_INFO(CL_DEVICE_WAVEFRONT_WIDTH_AMD, cl_uint);
    DEVICE_INFO(CL_DEVICE_GFXIP_MAJOR_AMD, cl_uint);
    DEVICE_INFO(CL_DEVICE_GFXIP_MINOR_AMD, cl_uint);
    DEVICE_INFO(CL_DEVICE_PCIE_ID_AMD, cl_uint);
    // TODO: others ...
  }

  /////////////////////////////////////////////////////////////////////////////
  std::cout << "  === EXTENSIONS:\n";
  std::istringstream iss(extensions_string);
  std::vector<std::string> extensions;
  std::copy(std::istream_iterator<std::string>(iss),
    std::istream_iterator<std::string>(),
    std::back_inserter(extensions));
  std::sort(extensions.begin(), extensions.end());

  for (auto ext : extensions) {
    std::cout << "        ";
    if (ext.find("cl_intel_") != std::string::npos)
      std::cout << ANSI_COLOR_INTEL_BLUE;
    else if (ext.find("cl_nv_") != std::string::npos)
      std::cout << ANSI_COLOR_NVIDIA_GREEN;
    else if (ext.find("cl_amd_") != std::string::npos)
      std::cout << ANSI_COLOR_AMD_ORANGE;
    else
      std::cout << DEVICE_INFO_VALUE_COLOR;
    std::cout << ext << "\n";
  }
  std::cout << ANSI_RESET;
}

