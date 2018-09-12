#include "devices.hpp"
#include "text.hpp"
#include "system.hpp"

#include <iostream>

cl::Device getDeviceByName(
  const cls::Opts &opts,
  cl_device_type dt,
  std::string substr)
{
  std::vector<cl::Platform> ps;
  cl::Platform::get(&ps);
  std::vector<cl::Device> matching;
  std::stringstream ss;
  for (auto &p : ps) {
    auto pnm = p.getInfo<CL_PLATFORM_NAME>();
    std::vector<cl::Device> ds;
    p.getDevices(dt,&ds);
    for (auto &d : ds) {
      auto dev_nm = d.getInfo<CL_DEVICE_NAME>();
      if (dev_nm.find(substr) != std::string::npos) {
        matching.push_back(d);
      }
      ss << "=> " << dev_nm << "\n";
    }
  }
  if (matching.size() == 0) {
    FATAL("no matching device found from set:\n%s", ss.str().c_str());
  } else if (matching.size() > 1) {
    FATAL("device name string is ambiguous in set:\n%s", ss.str().c_str());
  } else {
    return matching.front();
  }
}
cl::Device getDeviceByIndex(const cls::Opts &os, cl_device_type dt, int dev_ix)
{
  int curr_ix = 0;
  std::vector<cl::Platform> ps;
  cl::Platform::get(&ps);
  for (auto &p : ps) {
    auto pnm = p.getInfo<CL_PLATFORM_NAME>();
    std::vector<cl::Device> ds;
    p.getDevices(dt,&ds);
    for (auto &d : ds) {
      if (curr_ix++ == dev_ix) {
        return d;
      }
    }
  }
  FATAL("device index out of bounds");
}

#define DEVICE_INFO_KEY     ANSI_WHITE
#define DEVICE_INFO_VALUE   ANSI_CYAN
#define DEVICE_INFO_COLS    16

static void emitPropertyKey(const char *prop)
{
  std::cout << "    " << DEVICE_INFO_KEY << prop << ANSI_RESET << ": ";
  for (int i = 0, len = 48 - (int)strlen(prop); i < len; i++)
    std::cout << ' ';

}
template <typename T>
static void emitPropertyWithUnits(
  const char *prop, const T &val, const char *units)
{
  emitPropertyKey(prop);

  std::cout << DEVICE_INFO_VALUE <<
    std::setw(DEVICE_INFO_COLS) << std::right << val;
  std::cout << ANSI_RESET;
  if (units && *units)
    std::cout << " " << units;
  std::cout << "\n";
}
template <typename T>
static void emitPropertyWithUnits2(
  const cl::Device &dev,
  cl_device_info param,
  const char *prop,
  const char *units)
{
  emitPropertyKey(prop);

  T val;
  auto err = clGetDeviceInfo(dev(), param, sizeof(T), &val, nullptr);
  if (err != CL_SUCCESS) {
    std::cout << "[ERROR: " << cls::ErrorToString(err) << "]\n";
    return;
  }

  std::cout << DEVICE_INFO_VALUE <<
    std::setw(DEVICE_INFO_COLS) << std::right << val;
  std::cout << ANSI_RESET;
  if (units && *units)
    std::cout << " " << units;
  std::cout << "\n";
}
template <typename T>
static void emitPropertyIntegralBytes(const char *prop, T val)
{
  const char *units = "B";
  if (val % (1024 * 1024) == 0) {
    val = (val >> 20);
    units = "MB";
  } else if (val % 1024 == 0) {
    val = (val >> 10);
    units = "KB";
  }
  emitPropertyWithUnits(prop, val, units);
}
static void emitBoolProperty(const char *prop, cl_bool val)
{
  emitPropertyKey(prop);
  std::cout << DEVICE_INFO_VALUE <<
    std::setw(DEVICE_INFO_COLS) << std::right << (val ? "CL_TRUE" : "CL_FALSE") <<
    ANSI_RESET << "\n";
}

#define EMIT_DEVICE_PROPERTY_UNITS(SYM,UNITS)  emitPropertyWithUnits(#SYM, d.getInfo<SYM>(), UNITS)
#define EMIT_DEVICE_PROPERTY(SYM)  emitPropertyWithUnits(#SYM, d.getInfo<SYM>(), nullptr)
#define EMIT_DEVICE_PROPERTY2(SYM,TYPE)  emitPropertyWithUnits2<TYPE>(d, SYM, #SYM, nullptr)
#define EMIT_DEVICE_PROPERTY_MEM(SYM)  emitPropertyIntegralBytes(#SYM, d.getInfo<SYM>())
#define EMIT_DEVICE_PROPERTY_BOOL(SYM)  emitBoolProperty(#SYM, d.getInfo<SYM>())


static void listDeviceInfo(cls::Opts os, cl::Device d, int devIx)
{
//  auto val = text::ansi_literal("\033[38;2;94;182;0m");
  if (devIx >= 0) {
    std::cout << "DEVICE[" << devIx << "]: ";
  }
  std::string name = d.getInfo<CL_DEVICE_NAME>().c_str();

  bool is_intc = name.find("Intel") != std::string::npos;
  bool is_nvda = name.find("GTX") != std::string::npos;
  bool is_amd = false;

  if (is_intc) {
    std::cout << ANSI_COLOR_INTEL_BLUE;
  } else if (name.find("GTX") != std::string::npos) {
    std::cout << ANSI_COLOR_NVIDIA_GREEN;
  // } else if (name.find("AMD") != std::string::npos) {
  //  std::cout << ANSI_COLOR_AMD_ORANGE;
  }
  std::cout << name;
  std::cout << ANSI_RESET;
  std::cout << "\n";
  if (os.verbosity > 0) {
    EMIT_DEVICE_PROPERTY(CL_DEVICE_VERSION);
    EMIT_DEVICE_PROPERTY(CL_DEVICE_VENDOR);
    EMIT_DEVICE_PROPERTY(CL_DRIVER_VERSION);
    EMIT_DEVICE_PROPERTY(CL_DEVICE_OPENCL_C_VERSION);
    emitPropertyKey("CL_DEVICE_TYPE");
    std::cout << DEVICE_INFO_VALUE;
    switch (d.getInfo<CL_DEVICE_TYPE>()) {
    case CL_DEVICE_TYPE_CPU: std::cout << "CL_DEVICE_TYPE_CPU"; break;
    case CL_DEVICE_TYPE_GPU: std::cout << "CL_DEVICE_TYPE_GPU"; break;
    case CL_DEVICE_TYPE_ACCELERATOR: std::cout << "CL_DEVICE_TYPE_ACCELERATOR"; break;
    case CL_DEVICE_TYPE_DEFAULT: std::cout << "CL_DEVICE_TYPE_DEFAULT"; break;
    default: std::cout << d.getInfo<CL_DEVICE_TYPE>() << "?"; break;
    }
    std::cout << ANSI_RESET<< "\n";

    std::cout << "  === COMPUTE:\n";
    EMIT_DEVICE_PROPERTY_UNITS(CL_DEVICE_MAX_CLOCK_FREQUENCY,"MHz");
    EMIT_DEVICE_PROPERTY(CL_DEVICE_MAX_COMPUTE_UNITS);
    EMIT_DEVICE_PROPERTY_UNITS(CL_DEVICE_PROFILING_TIMER_RESOLUTION, "ns");
    EMIT_DEVICE_PROPERTY_BOOL(CL_DEVICE_ENDIAN_LITTLE);
    EMIT_DEVICE_PROPERTY(CL_DEVICE_BUILT_IN_KERNELS);

    // EMIT_DEVICE_PROPERTY_MEM(CL_DEVICE_PRINTF_BUFFER_SIZE); (not supported

    emitPropertyKey("CL_DEVICE_SINGLE_FP_CONFIG"); {
      std::cout << DEVICE_INFO_VALUE;
      auto fpcfg = d.getInfo<CL_DEVICE_SINGLE_FP_CONFIG>();
      const char *sep = ""; // "|";
      if ((fpcfg & CL_FP_DENORM)) {
        std::cout << sep << "CL_FP_DENORM";
        sep = "|";
      }
      if ((fpcfg & CL_FP_INF_NAN)) {
        std::cout << sep << "CL_FP_INF_NAN";
        sep = "|";
      }
      if ((fpcfg & CL_FP_ROUND_TO_NEAREST)) {
        std::cout << sep << "CL_FP_ROUND_TO_NEAREST";
        sep = "|";
      }
      if ((fpcfg & CL_FP_ROUND_TO_ZERO)) {
        std::cout << sep << "CL_FP_ROUND_TO_ZERO";
        sep = "|";
      }
      if ((fpcfg & CL_FP_ROUND_TO_INF)) {
        std::cout << sep << "CL_FP_ROUND_TO_INF";
        sep = "|";
      }
      if ((fpcfg & CL_FP_FMA)) {
        std::cout << sep << "CL_FP_FMA";
        sep = "|";
      }
      if ((fpcfg & CL_FP_CORRECTLY_ROUNDED_DIVIDE_SQRT)) {
        std::cout << sep << "CL_FP_CORRECTLY_ROUNDED_DIVIDE_SQRT";
        sep = "|";
      }
      if ((fpcfg & CL_FP_SOFT_FLOAT)) {
        std::cout << sep << "CL_FP_SOFT_FLOAT";
        sep = "|";
      }
      std::cout << ANSI_RESET << "\n";
    }

    emitPropertyKey("CL_DEVICE_QUEUE_PROPERTIES"); {
      std::cout << DEVICE_INFO_VALUE;
      const char *sep = ""; // "|";
      auto devq = d.getInfo<CL_DEVICE_QUEUE_PROPERTIES>();
      if ((devq & CL_QUEUE_OUT_OF_ORDER_EXEC_MODE_ENABLE)) {
        std::cout << sep << "CL_QUEUE_OUT_OF_ORDER_EXEC_MODE_ENABLE";
        sep = "|";
      }
      if ((devq & CL_QUEUE_PROFILING_ENABLE)) {
        std::cout << sep << "CL_QUEUE_PROFILING_ENABLE";
        sep = "|";
      }
      std::cout << ANSI_RESET << "\n";
    }

    std::cout << "  === WORKGROUPS:\n";
    EMIT_DEVICE_PROPERTY_UNITS(CL_DEVICE_MAX_WORK_GROUP_SIZE,"items");
    auto dim = d.getInfo<CL_DEVICE_MAX_WORK_ITEM_SIZES>();
    emitPropertyKey("CL_DEVICE_MAX_WORK_ITEM_SIZES");
    std::cout << ANSI_CYAN << dim[0];
    for (size_t i = 1; i < dim.size(); i++) {
      std::cout << ANSI_RESET;
      std::cout << 'x' << ANSI_CYAN << dim[i];
    }
    std::cout << ANSI_RESET;
    std::cout << "\n";

    // just show 1 and 4 byte types
    EMIT_DEVICE_PROPERTY_UNITS(CL_DEVICE_PREFERRED_VECTOR_WIDTH_CHAR,nullptr);
    EMIT_DEVICE_PROPERTY_UNITS(CL_DEVICE_PREFERRED_VECTOR_WIDTH_FLOAT,nullptr);

    std::cout << "  === MEMORY:\n";
    EMIT_DEVICE_PROPERTY_UNITS(CL_DEVICE_ADDRESS_BITS,"b");
    EMIT_DEVICE_PROPERTY_MEM(CL_DEVICE_MEM_BASE_ADDR_ALIGN);
    EMIT_DEVICE_PROPERTY_MEM(CL_DEVICE_MAX_CONSTANT_BUFFER_SIZE);
    EMIT_DEVICE_PROPERTY_MEM(CL_DEVICE_LOCAL_MEM_SIZE);
    EMIT_DEVICE_PROPERTY_MEM(CL_DEVICE_GLOBAL_MEM_SIZE);
    EMIT_DEVICE_PROPERTY_MEM(CL_DEVICE_MAX_MEM_ALLOC_SIZE);
    emitPropertyKey("CL_DEVICE_GLOBAL_MEM_CACHE_TYPE"); {
      std::cout << DEVICE_INFO_VALUE;
      switch (d.getInfo<CL_DEVICE_GLOBAL_MEM_CACHE_TYPE>()) {
      case CL_NONE: std::cout << "CL_NONE"; break;
      case CL_READ_ONLY_CACHE: std::cout << "CL_READ_ONLY_CACHE"; break;
      case CL_READ_WRITE_CACHE: std::cout << "CL_READ_WRITE_CACHE"; break;
      default: std::cout << d.getInfo<CL_DEVICE_GLOBAL_MEM_CACHE_TYPE>() << "?\n"; break;
      }
      std::cout << ANSI_RESET << "\n";
    }
    EMIT_DEVICE_PROPERTY_MEM(CL_DEVICE_GLOBAL_MEM_CACHELINE_SIZE);
    EMIT_DEVICE_PROPERTY_MEM(CL_DEVICE_GLOBAL_MEM_CACHE_SIZE);
    EMIT_DEVICE_PROPERTY_BOOL(CL_DEVICE_ERROR_CORRECTION_SUPPORT);

    std::cout << "  === IMAGES:\n";
    EMIT_DEVICE_PROPERTY_BOOL(CL_DEVICE_IMAGE_SUPPORT);
    EMIT_DEVICE_PROPERTY_UNITS(CL_DEVICE_IMAGE2D_MAX_HEIGHT,"px");
    EMIT_DEVICE_PROPERTY_UNITS(CL_DEVICE_IMAGE2D_MAX_WIDTH,"px");


    if (is_intc) {
#ifndef CL_DEVICE_SIMULTANEOUS_INTEROPS_INTEL
#define CL_DEVICE_SIMULTANEOUS_INTEROPS_INTEL           0x4104
#endif
#ifndef CL_DEVICE_NUM_SIMULTANEOUS_INTEROPS_INTEL
#define CL_DEVICE_NUM_SIMULTANEOUS_INTEROPS_INTEL       0x4105
#endif
#ifndef CL_DEVICE_ME_VERSION_INTEL
#define CL_DEVICE_ME_VERSION_INTEL                      0x407E
#endif
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
      std::cout << "  === INTEL-SPECIFIC:\n";
      // EMIT_DEVICE_PROPERTY2(CL_DEVICE_NUM_SIMULTANEOUS_INTEROPS_INTEL,cl_uint);
      // EMIT_DEVICE_PROPERTY2(CL_DEVICE_NUM_SIMULTANEOUS_INTEROPS_INTEL,cl_uint[CL_DEVICE_NUM_SIMULTANEOUS_INTEROPS_INTEL]);
      // EMIT_DEVICE_PROPERTY3(CL_DEVICE_ME_VERSION_INTEL,???);
      // EMIT_DEVICE_PROPERTY2(CL_DEVICE_TRANSFORM_MASK_MAX_WIDTH_INTEL,cl_uint);
      // EMIT_DEVICE_PROPERTY2(CL_DEVICE_TRANSFORM_MASK_MAX_HEIGHT_INTEL,cl_uint);
      // EMIT_DEVICE_PROPERTY2(CL_DEVICE_TRANSFORM_FILTER_MAX_WIDTH_INTEL,cl_uint);
      // EMIT_DEVICE_PROPERTY2(CL_DEVICE_TRANSFORM_FILTER_MAX_HEIGHT_INTEL,cl_uint);
    }


    if (is_nvda) {
      // NVidia device properties
      // https://www.khronos.org/registry/cl/extensions/nv/cl_nv_device_attribute_query.txt
      std::cout << "  === NVIDIA-SPECIFIC:\n";
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
      cl_uint cc_maj = d.getInfo<CL_DEVICE_COMPUTE_CAPABILITY_MAJOR_NV>();
      cl_uint cc_min = d.getInfo<CL_DEVICE_COMPUTE_CAPABILITY_MINOR_NV>();
      std::stringstream ss;
      ss << cc_maj << "." << cc_min;
      emitPropertyWithUnits("CL_DEVICE_COMPUTE_CAPABILITY_{MAJOR,MINOR}_NV",ss.str(),nullptr);
      EMIT_DEVICE_PROPERTY_UNITS(CL_DEVICE_WARP_SIZE_NV,"channels");
      EMIT_DEVICE_PROPERTY(CL_DEVICE_REGISTERS_PER_BLOCK_NV);
      EMIT_DEVICE_PROPERTY_BOOL(CL_DEVICE_GPU_OVERLAP_NV);
      EMIT_DEVICE_PROPERTY_BOOL(CL_DEVICE_KERNEL_EXEC_TIMEOUT_NV);
      EMIT_DEVICE_PROPERTY_BOOL(CL_DEVICE_INTEGRATED_MEMORY_NV);
      EMIT_DEVICE_PROPERTY2(CL_DEVICE_PCI_BUS_ID_NV,cl_uint);
      EMIT_DEVICE_PROPERTY2(CL_DEVICE_PCI_SLOT_ID_NV,cl_uint);
    }
    if (is_amd) {
      // https://www.khronos.org/registry/OpenCL/extensions/amd/cl_amd_device_attribute_query.txt
      //
      // cl_device_topology_amd topology
      // CL_DEVICE_TOPOLOGY_AMD
      // topology.raw.type == CL_DEVICE_TOPOLOGY_TYPE_PCIE_AMD
      std::cout << "  === AMD-SPECIFIC:\n";
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

      EMIT_DEVICE_PROPERTY2(CL_DEVICE_SIMD_PER_COMPUTE_UNIT_AMD, cl_uint);
      EMIT_DEVICE_PROPERTY2(CL_DEVICE_SIMD_WIDTH_AMD, cl_uint);
      EMIT_DEVICE_PROPERTY2(CL_DEVICE_SIMD_INSTRUCTION_WIDTH_AMD, cl_uint);
      EMIT_DEVICE_PROPERTY2(CL_DEVICE_WAVEFRONT_WIDTH_AMD, cl_uint);
      EMIT_DEVICE_PROPERTY2(CL_DEVICE_GFXIP_MAJOR_AMD, cl_uint);
      EMIT_DEVICE_PROPERTY2(CL_DEVICE_GFXIP_MINOR_AMD, cl_uint);
      EMIT_DEVICE_PROPERTY2(CL_DEVICE_PCIE_ID_AMD, cl_uint);
    }

    // other possibilities cl_arm_printf

    std::cout << "  === EXTENSIONS:\n";
    std::istringstream iss(d.getInfo<CL_DEVICE_EXTENSIONS>());
    std::vector<std::string> tokens;
    std::copy(std::istream_iterator<std::string>(iss),
      std::istream_iterator<std::string>(),
      std::back_inserter(tokens));
    std::cout << ANSI_CYAN;
    for (auto tk : tokens) {
      std::cout << "        " << tk << "\n";
    }
    std::cout << ANSI_RESET;

    // EMIT_DEVICE_PROPERTY_UNITS(CL_DEVICE_IMAGE_PITCH_ALIGNMENT,nullptr); // OCL 2.0?


  }
}

void listDeviceInfo(const cls::Opts &opts)
{
  if (opts.devices.size() > 0) {
    for (auto &d : opts.devices) {
      listDeviceInfo(opts, d, -1);
    }
  } else {
    int curr_ix = 0;
    std::vector<cl::Platform> ps;
    cl::Platform::get(&ps);
    for (auto &p : ps) {
      std::vector<cl::Device> ds;
      p.getDevices(CL_DEVICE_TYPE_ALL, &ds);

      for (auto &d : ds) {
        listDeviceInfo(opts, d, curr_ix++);
      }
    }
  }
}
