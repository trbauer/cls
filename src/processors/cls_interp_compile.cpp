#include "../devices.hpp"
#include "../parser/intel_gen_binary_decoder.hpp"
#include "../parser/spirv_decoder.hpp"
#include "../system.hpp"
#include "../text.hpp"
#include "cls_interp_internal.hpp"
#include "../cl_lib.hpp"
#include "../../deps/mdapi/mdapi_wrapper.hpp"

#include <cmath>
#include <iostream>
#include <limits>
#include <numeric>
#include <set>
#include <sstream>
#include <type_traits>


struct script_compiler : cl_interface {
  const opts &os;
  const script &s;
  compiled_script_impl *csi = nullptr;

  script_compiler(
    diagnostics &ds,
    const opts &_os,
    const script &_s,
    compiled_script_impl *_csi)
      : cl_interface(ds, _s.source),
        os(_os),
        s(_s),
        csi(_csi)
  {
  }
  void compile();
private:
  void compile_dispatch(const dispatch_spec *ds);
  dispatch_command *compile_dispatch_args(const dispatch_spec *ds);
  diffu_command *compile_diff_u(const diff_spec *ds);
  diffs_command *compile_diff_s(const diff_spec *ds);
  print_command *compile_print(const print_spec *ps);
  const type *infer_surface_element_type(const loc &at, surface_object *so);

  kernel_object &compile_kernel(const kernel_spec *ks);
  program_object &compile_program(const program_spec *ps);
  device_object &create_device_object(const device_spec *ds);

  std::string get_labeled_build_log(
    const loc &at, cl_program p, cl_device_id dev_id);
};

// a dummy type for size_t[3]
// cannot use cls::ndr since that uses the constructor to infer rank and
// has the extra field
struct ndr_temp {
  size_t dims[3];
  ndr_temp() {dims[0] = dims[1] = dims[2] = 0;}

  // HACK: unreachable for ndr_temp,
  // but suffices the template for emit_compiled_kernel_property
  ndr_temp operator%(size_t) {return *this;}
  ndr_temp operator/(size_t) {return *this;}
  bool operator==(size_t) {return false;}
};

std::ostream &operator <<(std::ostream &os, ndr_temp t) {
  os << t.dims[0] << " x " << t.dims[1] << " x " << t.dims[2];
  return os;
}

#if 0
// dummy type to represent size_t[3] and emit to an ostream
static void emit_compiled_kernel_property_dims(
  cl_kernel kernel,
  cl_device_id device,
  cl_int param,
  const char *param_name)
{
  std::cout << std::setw(48) << param_name << ": ";
  size_t vals[3];
  auto err =
    clGetKernelWorkGroupInfo(
      kernel, device, param,
      sizeof(vals),
      &vals[0],
      nullptr);
  if (err != CL_SUCCESS) {
    std::cout << text::spans::RED(status_to_symbol(err));
  } else {
    std::cout <<
      text::spans::YELLOW(vals[0]) << " x " <<
      text::spans::YELLOW(vals[1]) << " x " <<
      text::spans::YELLOW(vals[0]);
  }
  std::cout << "\n";
}
#endif

static void emit_param_name(const char *param_name)
{
  std::cout << std::left <<
    std::setw(48) << (std::string(param_name) + ':') << " ";
}

template <typename T>
static void emit_compiled_kernel_workgroup_property(
  cl_kernel kernel,
  cl_device_id device,
  cl_int param,
  const char *param_name,
  bool is_mem,
  const char *units = nullptr)
{
  std::cout << text::ANSI_FADED;
  emit_param_name(param_name);
  T val;
  auto err =
    clGetKernelWorkGroupInfo(kernel, device, param, sizeof(T), &val, nullptr);
  if (err != CL_SUCCESS) {
    std::cout << text::ANSI_RED << cl_lib::status_to_symbol(err);
  } else {
    std::cout << text::ANSI_FADED_YELLOW;
    if (is_mem && !units) {
      if (val % 1024 == 0)
        std::cout << (val / 1024) << " K";
      else
        std::cout << val << " B";
    // } else if (std::is_unsigned<T>()) {
    //  std::cout << "0x" << std::hex << std::uppercase << val;
    } else {
      std::cout << val;
      if (units)
        std::cout << units;
    }
  }
  std::cout << text::ANSI_RESET;
  std::cout << "\n";
}

template <typename T>
static void emit_compiled_kernel_property(
  cl_kernel kernel,
  cl_device_id device,
  cl_int param,
  const char *param_name,
  bool is_mem,
  const char *units = nullptr)
{
  std::cout << text::ANSI_FADED;
  emit_param_name(param_name);
  T val;
  auto err =
    clGetKernelInfo(kernel, param, sizeof(T), &val, nullptr);
  if (err != CL_SUCCESS) {
    std::cout << text::ANSI_RED << cl_lib::status_to_symbol(err);
  } else {
    std::cout << text::ANSI_FADED_YELLOW;
    if (is_mem && !units) {
      if (val % 1024 == 0)
        std::cout << (val / 1024) << " K";
      else
        std::cout << val << " B";
    } else if (std::is_unsigned<T>()) {
      std::cout << "0x" << std::hex << std::uppercase << val;
    } else {
      std::cout << val;
      if (units)
        std::cout << units;
    }
  }
  std::cout << text::ANSI_RESET;
  std::cout << "\n";
}

static void emit_compiled_kernel_property_str(
  cl_kernel kernel,
  cl_device_id device,
  cl_int param,
  const char *param_name)
{
  std::cout << text::ANSI_FADED;
  emit_param_name(param_name);
  size_t n;
  auto err = clGetKernelInfo(kernel, param, 0, nullptr, &n);
  if (err != CL_SUCCESS) {
    std::cout << text::ANSI_RED << cl_lib::status_to_symbol(err);
  } else {
    std::vector<char> cs;
    cs.resize(n + 1, 0);
    err =
        clGetKernelInfo(kernel, param, n, cs.data(), nullptr);
    if (err != CL_SUCCESS) {
      std::cout << text::ANSI_RED << cl_lib::status_to_symbol(err);
    } else {
      const char *val = cs.data();
      std::cout << text::ANSI_FADED_YELLOW;
      std::cout << val;
    }
  }
  std::cout << text::ANSI_RESET;
  std::cout << "\n";
}

using clGetSubgroupInfo_Fn = cl_int (*)(
    cl_kernel                /* kernel */,
    cl_device_id             /* device */,
    cl_kernel_sub_group_info /* param_name */,
    size_t                   /* input_value_size */,
    const void              */* input_value */,
    size_t                   /* param_value_size */,
    void                    */* param_value */,
    size_t                  */* param_value_size_ret*/);

clGetSubgroupInfo_Fn find_subgroup_function()
{
  static clGetSubgroupInfo_Fn function;
  if (function == nullptr) {
    void *lib = sys::load_library("OpenCL");
    function = (clGetSubgroupInfo_Fn)
      sys::get_symbol_address(lib, "clGetKernelSubGroupInfo");
    if (function == nullptr)
      function = (clGetSubgroupInfo_Fn)
        sys::get_symbol_address(lib, "clGetKernelSubGroupInfoKHR");
    if (function == nullptr)
      function = (clGetSubgroupInfo_Fn)
        sys::get_symbol_address(lib, "clGetKernelSubGroupInfoIntel");
  }
  return function;
}

template <typename T>
static void emit_compiled_kernel_subgroup_property(
  clGetSubgroupInfo_Fn fn,
  cl_kernel kernel,
  cl_device_id device,
  cl_kernel_sub_group_info param,
  const char *param_name,
  size_t inp_size,
  const void *inp,
  bool is_mem,
  const char *units = nullptr)
{
  std::cout << text::ANSI_FADED;
  emit_param_name(param_name);
  T val;
  auto err =
    fn(kernel, device, param, inp_size, inp, sizeof(T), &val, nullptr);
  if (err != CL_SUCCESS) {
    std::cout << text::ANSI_RED << cl_lib::status_to_symbol(err);
  } else {
    std::cout << text::ANSI_FADED_YELLOW;
    if (is_mem && !units) {
      if (val % 1024 == 0)
        std::cout << (val / 1024) << " K";
      else
        std::cout << val << " B";
    } else {
      std::cout << val;
      if (units)
        std::cout << " " << units;
    }
  }
  std::cout << text::ANSI_RESET;
  std::cout << "\n";
}

static void emit_compiled_kernel_properties(
  diagnostics &ds, kernel_object &ko)
{
  cl_kernel kernel = ko.kernel;
  cl_device_id device = ko.program->device->device;
  cl_spec spec = get_device_spec(device);


#define KERNEL_WORKGROUP_PROPERTY(MINSPEC,PARAM,TYPE) \
  if (spec >= (MINSPEC)) \
    emit_compiled_kernel_workgroup_property<TYPE>(\
      kernel, device, PARAM, #PARAM, false, nullptr)
#define KERNEL_WORKGROUP_PROPERTY_DIM(MINSPEC,PARAM) \
  if (spec >= (MINSPEC)) \
    emit_compiled_kernel_workgroup_property(\
      kernel, device, PARAM, #PARAM)
#define KERNEL_WORKGROUP_PROPERTY_MEM(MINSPEC,PARAM) \
  if (spec >= (MINSPEC)) \
    emit_compiled_kernel_workgroup_property<cl_ulong>(\
      kernel, device, PARAM, #PARAM, true, nullptr)

  std::cout << text::ANSI_FADED << "=== clGetKernelWorkGroupInfo\n"
            << text::ANSI_RESET;

  // only valid if it's a builtin kernel
  // KERNEL_WORKGROUP_PROPERTY(cl_spec::CL_1_2, CL_KERNEL_GLOBAL_WORK_SIZE, ndr_temp);
  KERNEL_WORKGROUP_PROPERTY(cl_spec::CL_1_0, CL_KERNEL_WORK_GROUP_SIZE, size_t);
  KERNEL_WORKGROUP_PROPERTY(
      cl_spec::CL_1_0, CL_KERNEL_COMPILE_WORK_GROUP_SIZE, ndr_temp);
  KERNEL_WORKGROUP_PROPERTY(
    cl_spec::CL_1_1, CL_KERNEL_PREFERRED_WORK_GROUP_SIZE_MULTIPLE, size_t);
  KERNEL_WORKGROUP_PROPERTY_MEM(cl_spec::CL_1_0, CL_KERNEL_LOCAL_MEM_SIZE);
  KERNEL_WORKGROUP_PROPERTY_MEM(cl_spec::CL_1_1, CL_KERNEL_PRIVATE_MEM_SIZE);

  if (device_has_extension(device, "cl_khr_subgroups")) {
    std::cout << text::ANSI_FADED << "=== clGetKernelSubGroupInfo\n"
              << text::ANSI_RESET;
    auto cl_subgroup_fn = find_subgroup_function();
#define SUBGROUP_PROPERTY(PARAM, TYPE, INPSZ, INP, UNITS) \
  emit_compiled_kernel_subgroup_property<TYPE>(\
    cl_subgroup_fn, kernel, device, PARAM, #PARAM, INPSZ, INP, false, UNITS)

    // requires input of dispatch size or some other input we won't have here
    // CL_KERNEL_MAX_SUB_GROUP_SIZE_FOR_NDRANGE
    // CL_KERNEL_SUB_GROUP_COUNT_FOR_NDRANGE
    // CL_KERNEL_LOCAL_SIZE_FOR_SUB_GROUP_COUNT - local size to create num sgs
    SUBGROUP_PROPERTY(
        CL_KERNEL_MAX_NUM_SUB_GROUPS, size_t, 0, nullptr, "sub groups");
    SUBGROUP_PROPERTY(
        CL_KERNEL_COMPILE_NUM_SUB_GROUPS, size_t, 0, nullptr, "sub groups");
    if (device_has_extension(device, "cl_intel_required_subgroup_size")) {
      SUBGROUP_PROPERTY(
          CL_KERNEL_COMPILE_SUB_GROUP_SIZE_INTEL,
          size_t,
          0,
          nullptr,
          "work items");
    }
  }

  if (device_has_extension(device, "cl_intel_required_subgroup_size")) {
    std::cout << text::ANSI_FADED << "=== clGetKernelSubGroupInfoINTEL\n"
              << text::ANSI_RESET;
    KERNEL_WORKGROUP_PROPERTY_MEM(cl_spec::CL_1_0, CL_KERNEL_SPILL_MEM_SIZE_INTEL);
    //
  }

  // units are an optional last parameter
#define KERNEL_PROPERTY(MINSPEC, PARAM, TYPE, UNITS) \
  if (spec >= (MINSPEC)) \
    emit_compiled_kernel_property<TYPE>(\
      kernel, device, PARAM, #PARAM, false, UNITS)
#define KERNEL_PROPERTY_STR(MINSPEC, PARAM) \
  if (spec >= (MINSPEC)) \
    emit_compiled_kernel_property_str(\
      kernel, device, PARAM, #PARAM)
  std::cout << text::ANSI_FADED << "=== clGetKernelInfo\n"
            << text::ANSI_RESET;

  // currently always returns INVALID_VALUE... (also tested priming call with big string)
  KERNEL_PROPERTY_STR(cl_spec::CL_1_0, CL_KERNEL_ATTRIBUTES);
  if (is_intel_gen(device)) {
    // always returns INVALID_VALUE
    KERNEL_PROPERTY(
        cl_spec::CL_1_0, CL_KERNEL_BINARY_GPU_ADDRESS_INTEL, cl_ulong, nullptr);
  }
  // CL_KERNEL_BINARY_PROGRAM_INTEL should return bits

#undef KERNEL_WORKGROUP_PROPERTY
#undef SUBGROUP_PROPERTY
#undef KERNEL_PROPERTY
}

kernel_object &script_compiler::compile_kernel(const kernel_spec *ks)
{
  auto itr = csi->kernels.find(ks);
  if (itr != csi->kernels.find_end()) {
    return *itr->second;
  }
  program_object *po = &compile_program(ks->program);
  kernel_object &ko = csi->kernels.emplace_back(ks,ks,po);

  debug_at(ks->defined_at, "compiling kernel");

  //////////////////////////////////////////////
  // call createKernel on parent object
  //
  // NOTE: avoid CL_COMMAND_CREATE macro because we want error code
  cl_int err_ck = 0;
  auto k = clCreateKernel(ko.program->program, ks->name.c_str(), &err_ck);
  if (err_ck == CL_SUCCESS) {
    ko.kernel = k;
  } else if (err_ck == CL_INVALID_KERNEL_NAME) {
    std::stringstream ss;
    std::vector<cl_kernel> all_ks;
    cl_uint nks;
    cl_int err =
      clCreateKernelsInProgram(po->program, 0, nullptr, &nks);
    if (err != CL_SUCCESS) {
      fatal_at(ks->defined_at, "clCreateKernelsInProgram: ",
        cl_lib::status_to_symbol(err));
    }
    all_ks.resize(nks);
    err =
      clCreateKernelsInProgram(po->program, nks, all_ks.data(), nullptr);
    if (err != CL_SUCCESS) {
      fatal_at(ks->defined_at, "clCreateKernelsInProgram: ",
        cl_lib::status_to_symbol(err));
    }
    ss << "valid kernels in the program are:\n";
    for (cl_kernel k : all_ks) {
      size_t n;
      auto err = clGetKernelInfo(k, CL_KERNEL_FUNCTION_NAME, 0, nullptr, &n);
      if (err != CL_SUCCESS)
        fatal_at(ks->defined_at, "clGetKernelInfo(CL_KERNEL_FUNCTION_NAME): ",
          cl_lib::status_to_symbol(err));

      char *name = (char *)alloca(n + 1);
      memset(name, 0, n + 1);
      err = clGetKernelInfo(k, CL_KERNEL_FUNCTION_NAME, n + 1, name, nullptr);
      if (err != CL_SUCCESS)
        fatal_at(ks->defined_at, "clGetKernelInfo(CL_KERNEL_FUNCTION_NAME): ",
          cl_lib::status_to_symbol(err));
      ss << " * " << name << "\n";
    }
    fatal_at(ks->defined_at,"unable to find kernel in program\n",ss.str());
  } else {
    fatal_at(
        ks->defined_at,
        "error creating kernel",
        cl_lib::status_to_symbol(err_ck));
  }

  // find the kernel info from the program info
  for (kernel_info &ki : po->program_info->kernels) {
    if (ki.name == ko.spec->name) {
      ko.kernel_info = &ki;
      break;
    }
  }
  if (ko.kernel_info == nullptr) {
    fatal_at(ks->defined_at,"unable to find kernel info in source file");
  }

  // Fetch the required workgroup size
  // __attribute__((reqd_work_group_size(X, Y, Z)))
  CL_COMMAND(ks->defined_at,
    clGetKernelWorkGroupInfo,
      ko.kernel,
      po->device->device,
      CL_KERNEL_COMPILE_WORK_GROUP_SIZE,
      sizeof(ko.kernel_info->reqd_word_group_size),
      ko.kernel_info->reqd_word_group_size,
      nullptr);

  if (is_debug()) {
    debug_at(ks->defined_at,
      "========== clGetKernelWorkGroupInfo ==========\n");
    emit_compiled_kernel_properties(get_diagnostics(), ko);
  }

  return ko;
}

std::string script_compiler::get_labeled_build_log(
  const loc &at, cl_program p, cl_device_id dev_id)
{
  size_t len = 0;
  CL_COMMAND(at,
    clGetProgramBuildInfo,
      p, dev_id, CL_PROGRAM_BUILD_LOG, 0, nullptr, &len);
  char *log_buf = (char *)alloca(len + 1);

  CL_COMMAND(at,
    clGetProgramBuildInfo,
      p, dev_id, CL_PROGRAM_BUILD_LOG, len, log_buf, nullptr);
  log_buf[len] = 0;

  std::stringstream ss;
  ss << "[" << at.str() << "]: ";
  return text::prefix_lines(
    ss.str(),
    log_buf);
}

program_object &script_compiler::compile_program(const program_spec *ps)
{
  auto itr = csi->programs.find(ps);
  if (itr != csi->programs.find_end()) {
    return *itr->second;
  }

  debug_at(ps->defined_at, "building program");

  device_object  *dobj = &create_device_object(&ps->device);
  program_object &po   = csi->programs.emplace_back(ps, ps, dobj);
  // program_object &po = program_object(ps,dobj);
  // program_object &po = csi->programs[ps] = program_object(ps,dobj);
  // auto eitr = csi->programs.emplace(ps,ps,dobj);
  // program_object &po = eitr.first->second;

  if (!sys::file_exists(ps->path)) {
    loc at = ps->defined_at;
    at.extent = (uint32_t)ps->path.size();
    fatal_at(at, "file not found");
  }
  bool is_bin =
    strsfx(".bin", ps->path) ||
    strsfx(".ptx", ps->path) ||
    strsfx(".obj", ps->path);
  bool is_spv =
    strsfx(".spv", ps->path);
  bool is_clc =
    strsfx(".cl", ps->path) ||
    strsfx(".clc", ps->path);
  if (!is_bin && !is_spv && !is_clc) {
    // look at the file contents
    auto bs = sys::read_file_binary(ps->path);
    bool has_non_ascii = false;
    for (uint8_t c : bs) {
      if (!::isprint(c)) {
        has_non_ascii = true;
        break;
      }
    }
    if (has_non_ascii) {
      is_spv =
        bs.size() > 4 &&
        *((const uint32_t *)bs.data()) == SPIRV_MAGIC;
      is_bin =
        bs.size() > 4 &&
        *((const uint32_t *)bs.data()) == ELF_MAGIC;
    }
    if (!is_spv && !is_bin) {
      is_bin = has_non_ascii;
      warning_at(ps->defined_at,
        "unable to infer program type from extension; "
        "based on contents we're assuming ", (is_bin ? "binary" : "text"), "\n",
        "NOTE: .cl, .clc imply OpenCL C, .spv implies SPIR-V, "
        "and .bin, .ptx, and .obj imply binary\n");
    }
  }

  std::string build_opts = ps->build_options;
  std::string build_opts_with_arg_info = build_opts;
  auto vend = get_device_vendor(po.device->device);
  if (os.use_kernel_arg_info) {
    if (is_bin && vend != vendor::INTEL) {
      // according to the OpenCL rules argument info doesn't have to be there
      // for binaries; Intel does include this though
      warning_at(ps->defined_at,
        "trying to use kernel argument info on non-Intel binary "
        "(this may not work)");
    }
    if (get_device_spec(po.device->device) <  cl_spec::CL_1_2)
      fatal_at(ps->defined_at,"kernel argument info needs OpenCL 1.2+");
    if (build_opts.empty()) {
      build_opts_with_arg_info = "-cl-kernel-arg-info";
    } else if (build_opts.find("-cl-kernel-arg-info") == std::string::npos) {
      build_opts_with_arg_info += " -cl-kernel-arg-info";
    }
  }

  program_source src;
  src.path = ps->path;
  src.build_opts = build_opts;
  src.kind =
    is_bin ? program_source::BINARY :
      is_spv ? program_source::SPIRV :
        program_source::SOURCE;

  cl_context ctx = po.device->context;

  if (is_clc) {
    std::string inp = sys::read_file_text(ps->path);
    const char *src = inp.c_str();
    size_t len = inp.size();
    CL_COMMAND_CREATE(po.program, ps->defined_at,
      clCreateProgramWithSource,
      ctx, 1, &src, &len);

    cl_int bp_err =
      clBuildProgram(po.program, 1, &po.device->device,
        build_opts.c_str(), nullptr, nullptr);
    if (bp_err == CL_BUILD_PROGRAM_FAILURE) {
      std::stringstream ss;
      ss << "failed to build source:\n";
      ss << get_labeled_build_log(
        ps->defined_at,
        po.program,
        po.device->device) << "\n";
      fatal_at(ps->defined_at, ss.str());
    } else if (bp_err == CL_SUCCESS && os.verbosity >= 2) {
      // CL_PROGRAM_BUILD_GLOBAL_VARIABLE_TOTAL_SIZE
      debug_at(ps->defined_at, get_labeled_build_log(
        ps->defined_at,
        po.program,
        po.device->device));

      cl_program_binary_type bt = 0;
      CL_COMMAND(
          ps->defined_at,
          clGetProgramBuildInfo,
          po.program,
          po.device->device,
          CL_PROGRAM_BINARY_TYPE,
          sizeof(bt),
          &bt,
          nullptr);
      auto btstr = text::expand_bitset(
          bt,
          {
              {CL_PROGRAM_BINARY_TYPE_NONE, "CL_PROGRAM_BINARY_TYPE_NONE"},
              {CL_PROGRAM_BINARY_TYPE_COMPILED_OBJECT,
               "CL_PROGRAM_BINARY_TYPE_COMPILED_OBJECT"},
              {CL_PROGRAM_BINARY_TYPE_LIBRARY,
               "CL_PROGRAM_BINARY_TYPE_LIBRARY"},
              {CL_PROGRAM_BINARY_TYPE_EXECUTABLE,
               "CL_PROGRAM_BINARY_TYPE_EXECUTABLE"},
          });
      debug_at(ps->defined_at, "program binary is: ", btstr);
    } else if (bp_err != CL_SUCCESS) {
      fatal_at(
          ps->defined_at,
          "failed to build program ",
          cl_lib::status_to_symbol(bp_err));
    }

    if (os.save_binaries) {
      std::string bin_ext;
      if (vend == vendor::NVIDIA)
        bin_ext = ".ptx";
      else // Intel is ELF; I don't know what others do.
        bin_ext = ".bin";
      // foo/bar.cl -> bar.bin
      std::stringstream ss;
      ss << sys::drop_extension(sys::take_file(ps->path));
      static int kernel_index = 0;
      int my_index = kernel_index++;
      if (my_index > 0) {
        ss << my_index;
      }
      ss << bin_ext;
      auto bin_path = ss.str();

      size_t bits_len = 0;
      CL_COMMAND(ps->defined_at,
        clGetProgramInfo,
          po.program,
          CL_PROGRAM_BINARY_SIZES,
          sizeof(bits_len),
          &bits_len,
          nullptr);
      unsigned char *bits = (unsigned char *)alloca(bits_len);
      CL_COMMAND(ps->defined_at,
        clGetProgramInfo,
          po.program, CL_PROGRAM_BINARIES, bits_len, &bits, nullptr);

      // int err_x = 0;
      // CL_COMMAND_CREATE(po.program, ps->defined_at,
      //   clCreateProgramWithBinary,
      //     ctx,
      //     1,
      //     &dev,
      //     &bits_len,
      //     (const unsigned char **)&bits,
      //     &err_x);
      verbose(bin_path, ": dumping binary");
      sys::write_bin_file(bin_path, bits, bits_len);
    }
  } else { // is_bin || is_spv
    auto bits = sys::read_file_binary(ps->path);
    if (is_spv) {
      CL_COMMAND_CREATE(po.program, ps->defined_at,
        clCreateProgramWithIL,
        ctx,
        bits.data(),
        bits.size());
    } else {
      const uint8_t *bin = bits.data();
      size_t bin_len = bits.size();
      CL_COMMAND_CREATE(po.program, ps->defined_at,
        clCreateProgramWithBinary,
          ctx,
          1,
          &po.device->device,
          &bin_len,
          &bin,
          nullptr);
    }
    CL_COMMAND(ps->defined_at,
      clBuildProgram,
        po.program,
        1,
        &po.device->device,
        build_opts.empty() ? nullptr : build_opts.c_str(),
        nullptr,
        nullptr);
  }

  ////////////////////////////////////////////////
  // kernel info
  if (os.use_kernel_arg_info) {
    // use clGetKernelArgInfo* for types etc...
    po.program_info = parse_program_info_from_api(
      os,
      get_diagnostics(),
      ps->defined_at,
      po.program,
      po.device->device);
  } else {
    // use kargs library
    po.program_info = parse_program_info(
      os,
      get_diagnostics(),
      ps->defined_at,
      src,
      po.device->device);
  }

  cl_uint bytes_per_addr;
  if (get_device_info(
    po.device->device, CL_DEVICE_ADDRESS_BITS, bytes_per_addr) != CL_SUCCESS)
  {
    fatal_at(ps->defined_at, "clGetDeviceInfo(CL_DEVICE_ADDRESS_BITS)");
  }
  bytes_per_addr /= 8;
  if (bytes_per_addr != po.program_info->pointer_size) {
    fatal_at(ps->defined_at,
      "mismatch between clGetDeviceInfo(CL_DEVICE_ADDRESS_BITS) "
      "and program's reported byte size");
  }

  if (os.verbose_enabled()) {
    std::stringstream ss;
    ss << "program info=====================================\n";
    ss << "  types:\n";
    for (const type *t : po.program_info->types) {
      ss << " - " << t->syntax() << " (" << t->size() << "B)\n";
      if (t->is<type_struct>()) {
        ss << "  {\n";
        auto &ts = t->as<type_struct>();
        for (size_t i = 0; i < ts.elements_length; i++) {
          ss << "    " << ts.elements[i]->syntax() << "\n";
        }
        ss << "  }\n";
      }
    }
    ss << "  pointer size: " << po.program_info->pointer_size << "B\n";

    for (const kernel_info &k : po.program_info->kernels) {
      ss <<
        "  - kernel: " << k.name << "\n";
      ss <<
        "  - reqd_word_group_size: " <<
        k.reqd_word_group_size[0] << ", " <<
        k.reqd_word_group_size[1] << ", " <<
        k.reqd_word_group_size[2] << "\n";
      ss <<
        "  - args:\n";
      for (const arg_info &ai : k.args) {
        ss << "    - " << ai.type_syntax() << " " << ai.name << "\n";
      }
    }
    debug_at(ps->defined_at, ss.str());
  }

  return po;
}

static void CL_CALLBACK dispatch_context_notify(
  const char *errinfo,
  const void *private_info, size_t cb,
  void *user_data)
{
  device_object *dobj = (device_object *)user_data;
  if (!dobj) {
    std::cerr << "DRIVER ERROR: driver failed to pass user environment to "
      "context callback\n";
    if (errinfo)
      std::cerr << errinfo;
    return;
  }
  dobj->context_notify(errinfo, private_info, cb, user_data);
}

// We use the old 1.0 style command queue creation since the host running
// this might not be 1.2+.
#ifdef _MSC_VER
// disable the deprecation warning on clCreateCommandQueue
#pragma warning(disable : 4996)
#endif
static cl_command_queue make_command_queue(
  script_compiler &ds,
  loc at,
  const cl_lib &cl,
  const mdapi_lib *md,
  bool profiling_queue,
  std::string metric_counter_set,
  bool no_device_check,
  cl_device_id dev_id,
  cl_context &ctx)
{
  cl_command_queue queue = nullptr;
  cl_command_queue_properties props = 0;
  if (profiling_queue) {
    props |= CL_QUEUE_PROFILING_ENABLE;
  }
  cl_int err = 0;
  const char *cl_function = "clCreateCommandQueue";
  if (!metric_counter_set.empty()) {
    props |= CL_QUEUE_PROFILING_ENABLE; // -tMD implies -tCL?
    if (!is_intel_gen(dev_id) && !no_device_check) {
      ds.fatal_at(at, "-tMD=.. set, but device does not support MDAPI");
    }
    if (md == nullptr) {
      ds.internal_at(at, "-tMD=.. set, but library not loaded");
    }
    if (cl.clCreatePerfCountersCommandQueueINTEL == nullptr) {
      ds.fatal_at(
          at,
          "-tMD=.. cannot find clCreatePerfCountersCommandQueueINTEL "
          "on this device");
    }

    cl_function = "clCreatePerfCountersCommandQueueINTEL";
    cl_uint mdapi_config = md->get_configuration();
    queue                = cl.clCreatePerfCountersCommandQueueINTEL(
        ctx, dev_id, props, mdapi_config, &err);
  } else {
    queue = clCreateCommandQueue(ctx, dev_id, props, &err);
  }
  if (err != CL_SUCCESS) {
    ds.fatal_at(
      at,
      cl_function, ": failed to create command queue for device "
      "(", cl_lib::status_to_symbol(err), ")");
  }
  return queue;
}

device_object &script_compiler::create_device_object(const device_spec *ds)
{
  cl_device_id dev_id;
  switch (ds->skind) {
  case device_spec::BY_DEFAULT:
    dev_id = get_device_default();
    break;
  case device_spec::BY_INDEX:
    if (!get_device_by_index(os, ds->by_index_value, dev_id)) {
      fatal_at(ds->defined_at,"invalid device index");
    }
    break;
  case device_spec::BY_NAME: {
    std::string err_msg;
    if (!get_device_by_name(os, ds->by_name_value, dev_id, err_msg))
      fatal_at(ds->defined_at, "invalid device specification ", err_msg);
    break;
  }
  default:
    fatal_at(ds->defined_at, "invalid device spec");
    break;
  }

  auto dev_key = device_key(dev_id, ds->instance);
  auto itr = csi->devices.find(dev_key);
  if (itr != csi->devices.find_end()) {
    return *itr->second;
  }

  device_object &dobj =
      csi->devices.emplace_back(dev_key, get_diagnostics(), ds, dev_id, os.verbosity);
  if (!os.metric_counter_set.empty()) {
    if (!is_intel_gen(dev_id) && !os.no_device_check) {
      fatal_at(ds->defined_at, "-tMD=.. set but device does not support MDAPI");
    }
    dobj.md = new mdapi_lib();
    if (!dobj.md->is_loaded())
      fatal_at(
          ds->defined_at,
          "-tMD=",
          os.metric_counter_set,
          ": failed to load MDAPI library (", dobj.md->get_error(), ")");
    if (!dobj.md->bind(os.metric_counter_set.c_str()))
      fatal_at(
          ds->defined_at,
          "-tMD=", os.metric_counter_set, ": failed to bind to counter set");
  }
  cl_lib *cl = new cl_lib(os.verbosity, dev_id);

  cl_context context;
  // const cl_context_properties props {...};
  CL_COMMAND_CREATE(context, ds->defined_at,
    clCreateContext,
      nullptr,
      1,
      &dev_id,
      dispatch_context_notify,
      (void *)&dobj);
  dobj.context = context;
  if (os.verbosity >= 2) {
    std::string dev_nm;
    get_device_info(dobj.device, CL_DEVICE_NAME, dev_nm);
    debug_at(ds->defined_at, "created context on ", dev_nm);
  }

  dobj.queue = make_command_queue(
      *this,
      ds->defined_at,
      *dobj.cl,
      dobj.md,
      os.prof_time,
      os.metric_counter_set,
      os.no_device_check,
      dev_id,
      dobj.context);

  return dobj;
}



void script_compiler::compile()
{
  // Contruct contexts, command queues, programs, kernels, and whatnot
  // Construct contexts and command queues (device_state) for all
  // device_spec's that appear in the script and compile programs.
  for (const statement_spec *st : s.statement_list.statements) {
    if (st->skind == statement_spec::DISPATCH) {
      compile_dispatch((const dispatch_spec *)st);
    }
  }

  // Loop through all the statements
  //  - compile dispatch arguments
  //  - compile diff command (needs args for surface info type)
  for (size_t st_ix = 0; st_ix < s.statement_list.statements.size(); st_ix++) {
    const statement_spec *st = s.statement_list.statements[st_ix];
    if (st->skind == statement_spec::DISPATCH) {
      dispatch_command *dc = compile_dispatch_args((const dispatch_spec *)st);
      csi->instructions.emplace_back(dc);
    } else if (st->skind == statement_spec::DIFF) {
      const diff_spec *ds = (const diff_spec *)st;
      if (ds->ref.value->is_atom()) {
        diffu_command *dc = compile_diff_u(ds);
        csi->instructions.emplace_back(dc);
      } else {
        diffs_command *dc = compile_diff_s(ds);
        csi->instructions.emplace_back(dc);
      }
    } else if (st->skind == statement_spec::PRINT) {
      print_command *pc = compile_print((const print_spec *)st);
      csi->instructions.emplace_back(pc);
    } else if (st->skind == statement_spec::SAVE_BUFFER) {
      const save_spec *ss = (const save_spec *)st;
      saveb_command *sc = new saveb_command(ss, nullptr);
      csi->instructions.emplace_back(sc);
    } else if (st->skind == statement_spec::SAVE_IMAGE) {
      const save_image_spec *ss = (const save_image_spec *)st;
      cl_channel_order ch_ord = CL_RGBA;
      cl_channel_type ch_type = CL_UNSIGNED_INT8;
      switch (ss->format) {
      case save_image_spec::FLOAT4_RGBA:
        ch_ord = CL_RGBA;
        ch_type = CL_FLOAT;
        break;
      case save_image_spec::UCHAR4_RGBA:
        ch_ord = CL_RGBA;
        ch_type = CL_UNSIGNED_INT8;
        break;
      default: fatal_at(ss->defined_at, "unsupported channel order");
      }
      savei_command *sic = new savei_command(ss, nullptr, ch_ord, ch_type);
      csi->instructions.emplace_back(sic);
    } else if (st->skind == statement_spec::LET) {
      ; // nop
    } else {
      internal_at(st->defined_at, "NOT IMPLEMENTED");
    }
  } // for

  // Binding surface_object's to their non-dispatch commands.
  // (e.g. print, diff, save) and bind surface_object*'s to their value.
  // Loop through all memory object statements and link them to the surface
  // objects they process.  We do this as a second pass because
  // surface_objects are only created once they are used in a dispatch.
  // E.g.
  //    let X = random:w
  //    print<int>(X)  <<<<<<<<<<<<< surface object used before dispatch
  //    #0`foo.cl`kernel<...>(X);
  for (script_instruction &si : csi->instructions) {
    auto lookup_surface_object = [&] (const refable<init_spec_mem> &val) {
      surface_object *so = &csi->surfaces.get(val);
      if (so == nullptr) {
        fatal_at(si.defined_at(), "non-existent surface object "
          "(was it used in an enqueue?)");
      }
      return so;
    };
    switch (si.skind)
    {
    case script_instruction::DISPATCH: break; // skip
    case script_instruction::DIFFS: {
      diffs_command *dfsc = si.dfsc;
      // sut surface
      dfsc->so_sut = lookup_surface_object(dfsc->spec->sut.value);
      debug_at(
        dfsc->spec->defined_at, "bound (SUT) surface to ",
        dfsc->so_sut->str());

      // bind the element type if it's not explicitly set
      if (!dfsc->element_type) {
        dfsc->element_type =
          infer_surface_element_type(dfsc->spec->defined_at, dfsc->so_sut);
        if (dfsc->element_type)
          debug_at(dfsc->spec->defined_at,
            "bound element type to ", dfsc->element_type->syntax());
      }

      // reference surface
      const init_spec_mem *ism_ref =
        (const init_spec_mem *)dfsc->spec->ref.value;
      if (csi->surfaces.find(ism_ref) != csi->surfaces.find_end()) {
        dfsc->so_ref = &csi->surfaces.get(ism_ref);
        debug_at(
          dfsc->spec->defined_at,
          "bound (REF) surface to ",
          dfsc->so_ref->str());
        if (dfsc->so_sut->size_in_bytes != dfsc->so_ref->size_in_bytes) {
          fatal_at(dfsc->spec->defined_at, "surface sizes mismatch");
        }
      } else {
        // it's an immediate object, we have to create a dummy surface
        // we use the SUT object properties for reference
        //
        // e.g. diff<int>(seq(4,2):r, SUT)
        //
        if (dfsc->so_sut->dispatch_uses.empty()) {
          fatal_at(dfsc->spec->defined_at,
            "cannot diff against unused memory object");
        }
        if (!dfsc->element_type) {
          fatal_at(dfsc->spec->defined_at,
            "explicit type required for reference surface initializer");
        }
        // use SUT to dtermine the buffer type etc...
        dispatch_command *dc =
          std::get<0>(dfsc->so_sut->dispatch_uses.front());
        const arg_info &ai = std::get<2>(dfsc->so_sut->dispatch_uses.front());
        cl_mem_flags cl_mfs = CL_MEM_READ_ONLY;
        cl_mem memobj = nullptr;
        cl_context context = dc->kernel->program->device->context;
        CL_COMMAND_CREATE(memobj, ism_ref->defined_at,
          clCreateBuffer,
            context,
            cl_mfs,
            dfsc->so_sut->size_in_bytes,
            nullptr);
        dfsc->so_ref = csi->define_surface(
          ism_ref,
          surface_object::SO_BUFFER,
          dfsc->so_sut->size_in_bytes,
          memobj,
          dfsc->so_sut->queue);
        dfsc->so_ref->dummy_object = true; // only used for comparison
        with_buffer_map_write(
          ism_ref->defined_at,
          dfsc->so_ref,
          [&](void *host_ptr) {
            evaluator::context ec(*dc);
            const type *elem_type = dfsc->element_type;
            if (!elem_type) {
              // typeless diff needs the element from the SUT
              // diff(22:r,SUT) needs to take the type from SUT
              if (!dfsc->so_sut->dispatch_uses.empty()) {
                fatal_at(dfsc->spec->defined_at,"cannot infer element type");
              }
              const type &t =
                *std::get<2>(dfsc->so_sut->dispatch_uses.back()).arg_type;
              if (!t.is<type_ptr>()) {
                fatal_at(dfsc->spec->defined_at,
                  "inferred element type (from SUT) is not a pointer");
              }
              elem_type = t.as<type_ptr>().element_type;
            }
            csi->init_surface(*dfsc->so_ref, ec, elem_type, host_ptr);
          });
      }
      break;
    }
    case script_instruction::DIFFU: {
      diffu_command *dfuc = si.dfuc;
      si.dfuc->so = lookup_surface_object(dfuc->spec->sut.value);
      debug_at(si.dfuc->spec->defined_at, "bound surface to ", dfuc->so->str());
      if (!dfuc->element_type) {
        dfuc->element_type =
          infer_surface_element_type(dfuc->spec->defined_at, dfuc->so);
        if (dfuc->element_type)
          debug_at(dfuc->spec->defined_at, "bound element type to ",
            dfuc->element_type->syntax());
      }
      break;
    }
    case script_instruction::PRINT:
      si.prc->so = lookup_surface_object(si.prc->spec->arg);

      debug_at(si.prc->spec->defined_at, "bound surface to ", si.prc->so->str());
      if (!si.prc->element_type) {
        si.prc->element_type =
          infer_surface_element_type(si.prc->spec->defined_at, si.prc->so);
        if (si.prc->element_type)
          debug_at(si.prc->spec->defined_at, "bound element type to ",
            si.prc->element_type->syntax());
      }
      break;
    case script_instruction::SAVEB:
      si.svbc->so = lookup_surface_object(si.svbc->spec->arg.value);
      debug_at(
        si.svbc->spec->defined_at, "bound surface to ", si.svbc->so->str());
      break;
    case script_instruction::SAVEI:
    {
      si.svic->so = lookup_surface_object(si.svic->spec->arg.value);
      debug_at(
        si.svic->spec->defined_at, "bound surface to ", si.svic->so->str());
      si.svic->width = si.svic->spec->width;
      si.svic->height = si.svic->spec->height;
      // deduce the image size if necessary (based on use)
      if (si.svic->width == 0 || si.svic->height == 0) {
        for (const surface_object::use &u : si.svic->so->dispatch_uses) {
          const dispatch_command &dc = *std::get<0>(u);
          if (dc.global_size.rank() != 2) {
            fatal_at(si.svic->spec->defined_at,
              "cannot infer image size from dispatch use "
              "(used in non-2d kernel)");
          }
          if (si.svic->width != 0 &&
            si.svic->width != dc.global_size.dims[0])
          {
            fatal_at(si.svic->spec->defined_at,
              "cannot infer image width from dispatch use "
              "(mismatch in size)");
          } else if (si.svic->width == 0) {
            si.svic->width = dc.global_size.dims[0];
          }
          if (si.svic->height != 0 &&
            si.svic->height != dc.global_size.dims[1])
          {
            fatal_at(si.svic->spec->defined_at,
              "cannot infer image height from dispatch use "
              "(mismatch in size)");
          } else if (si.svic->height == 0) {
            si.svic->height = dc.global_size.dims[1];
          }
        }
      }
      size_t accessed_size =
        si.svic->width * si.svic->height *
        channels_per_pixel(si.svic->channel_order) *
        bytes_per_channel(si.svic->channel_type);
      if (accessed_size > si.svic->so->size_in_bytes) {
        warning_at(si.svic->spec->defined_at,
          "image access overflows buffer size");
      }
      break;
    }
    } // switch
  } // for (binding surfaces to non-dispatch commands)



  /////////////////////////////////////////////////////////////////////////////
  // Ensure all surfaces are used within the same context
  for (surface_object *so : csi->surfaces) {
    device_object *dobj = nullptr;
    for (const surface_object::use &du : so->dispatch_uses) {
      const dispatch_command *dc = std::get<0>(du);
      if (dobj == nullptr) {
        dobj = dc->dobj;
      } else if (dobj != dc->dobj) {
        fatal_at(so->init->defined_at,
          "memory object used across cl_context's (c.f. lines ",
          dobj->spec->defined_at.line, " and ",
          dc->dobj->spec->defined_at.line, ")");
      }
    }
  }
}

void script_compiler::compile_dispatch(const dispatch_spec *ds)
{
  debug_at(ds->defined_at, "compiling dispatch");
  kernel_object &ko = compile_kernel(ds->kernel);
  dispatch_command &dc = csi->dispatches.emplace_back(ds,ds,&ko);

  evaluator e(csi);
  if (ds->global_size.size() > 3)
    fatal_at(ds->defined_at,"dimension must be <= 3");
  dc.global_size.num_dims = ds->global_size.size();

  auto evaluate_dimension = [&](const init_spec_atom *expr) {
    // later iterations will see the value of earlier iterations
    // => we can support something like <1024x(g.x/2)x(g.x*g.y/4)>
    //    pretty awesome!
    evaluator::context ec(dc);
    // ec holds const references, but that isn't seeing the updates
    // so we rebuild it ec within each loop
    val v = e.eval_to<size_t>(ec, expr);
    //
    if (v.s64 < 0) {
      fatal_at(expr->defined_at, "negative dimension");
    }
    //
    return (size_t)v.u64;
  };

  for (size_t i = 0; i < ds->global_size.size(); i++) {
    dc.global_size.dims[i] = evaluate_dimension(ds->global_size[i]);
  }
  if (ds->local_size.empty()) {
    // use the required workgroup size
    const size_t *rqsz = ko.kernel_info->reqd_word_group_size;
    if (rqsz[0] != 0 && rqsz[1] != 0 && rqsz[2] != 0) {
      switch (dc.global_size.rank()) {
      case 1: dc.local_size = ndr(rqsz[0]);                   break;
      case 2: dc.local_size = ndr(rqsz[0], rqsz[1]);          break;
      case 3: dc.local_size = ndr(rqsz[0], rqsz[1], rqsz[2]); break;
      }
    }
  } else {
    // have to do the locals too
    dc.local_size.num_dims = ds->local_size.size();
    for (size_t i = 0; i < ds->local_size.size(); i++) {
      dc.local_size.dims[i] = evaluate_dimension(ds->local_size[i]);
    }
  }
}

static std::string kernel_args_error_message(
  const kernel_object &ko,
  const char *msg)
{
  const auto &ais = ko.kernel_info->args;

  std::stringstream ss;
  ss << msg << "; arguments should be:\n";
  for (cl_uint arg_index = 0;
    arg_index < (cl_uint)ais.size();
    arg_index++)
  {
    const arg_info &ai = ais[arg_index];
    ss << "  - " << std::setw(24) << std::left << ai.type_syntax();
    ss << "  " << ai.name << "\n";
  }
  return ss.str();
}

dispatch_command *script_compiler::compile_dispatch_args(
  const dispatch_spec *ds)
{
  dispatch_command &dc = csi->dispatches.get(ds);
  kernel_object &ko = *dc.kernel;
  const auto &ais = ko.kernel_info->args;
  if (ais.size() != ds->arguments.size()) {
    fatal_at(ds->defined_at,
      kernel_args_error_message(ko, "wrong number of arguments to kernel"));
  }
  size_t ptr_size = ko.program->device->pointer_size;
  for (cl_uint arg_index = 0;
    arg_index < (cl_uint)ds->arguments.size();
    arg_index++)
  {
    std::stringstream ss;
    const arg_info &ai = ais[arg_index];
    const refable<init_spec> &ris = ds->arguments[arg_index];
    const init_spec *is = ris;
    if (ai.addr_qual == CL_KERNEL_ARG_ADDRESS_LOCAL) {
      csi->e->set_kernel_arg_slm(arg_index,dc,ss,ris,ai);
    } else if (ai.arg_type->is<type_ptr>()) {
      csi->e->set_kernel_arg_buffer(
        arg_index, dc, ss, ris.defined_at, ris, ai);
    } else if (ai.arg_type->is<type_builtin>() &&
      ai.arg_type->as<type_builtin>().is_surface())
    {
      csi->e->set_kernel_arg_image(
        arg_index, dc, ss, ris.defined_at, ris, ai);
    } else if (ai.arg_type->is<type_builtin>() &&
      ai.arg_type->as<type_builtin>().skind == type_builtin::SAMPLER)
    {
      csi->e->set_kernel_arg_sampler(arg_index, dc, ss, ris, ai);
    } else {
      // A uniform argument
      csi->e->set_kernel_arg_immediate(arg_index, dc, ss, ris, ai);
    }
    dc.evaluated_args.push_back(ss.str());
  } // for kernel args
  return &dc;
}

diffu_command *script_compiler::compile_diff_u(const diff_spec *ds)
{
  // we patch surface and type information later
  return new diffu_command(ds,nullptr,nullptr);
}
diffs_command *script_compiler::compile_diff_s(const diff_spec *ds)
{
  // we patch surface and type information later
  return new diffs_command(ds,nullptr,nullptr,nullptr);
}

print_command *script_compiler::compile_print(const print_spec *ps)
{
  // we patch surface and type information later
  return new print_command(ps,nullptr,ps->element_type);
}

const type *script_compiler::infer_surface_element_type(
  const loc &at, surface_object *so)
{
  if (so->dispatch_uses.empty()) {
    fatal_at(so->init->defined_at,
      "memory object never used in a dispatch (so we can't infer it's type)");
  }
  dispatch_command *dc = std::get<0>(so->dispatch_uses.front());
  kernel_object &ko = *dc->kernel;
  const auto &ais = ko.kernel_info->args;
  for (cl_uint arg_index = 0;
    arg_index < (cl_uint)dc->spec->arguments.size();
    arg_index++)
  {
    const arg_info &ai = ais[arg_index];
    const refable<init_spec> &ris = dc->spec->arguments[arg_index];
    const init_spec *is = ris;
    if (is == so->init) {
      if (!ai.arg_type->is<type_ptr>()) {
        // TODO: I am not sure if this check is needed
        //       We could skip the error and try another dispatch command
        fatal_at(at, "INTERNAL ERROR: surface type used as non-pointer type");
      }
      return ai.arg_type->as<type_ptr>().element_type;
    }
  }
  fatal_at(at, "INTERNAL ERROR: surface object mis-linked to dispatch");
  return nullptr;
}

#if 0
const type *script_compiler::infer_surface_element_type(
  surface_object *so, size_t from_st_ix)
{
  statement_spec *from_st = s.statement_list.statements[from_st_ix];
  for (int ix = (int)from_st_ix - 1; ix >= 0; ix--) {
    const statement_spec *st1 = s.statement_list.statements[ix];
    if (st1->skind == statement_spec::DISPATCH) {
      const dispatch_spec *ds = (const dispatch_spec *)st1;
      dispatch_command &dc = csi->dispatches.get(ds);
      kernel_object &ko = *dc.kernel;
      const auto &ais = ko.kernel_info->args;
      for (cl_uint arg_index = 0;
        arg_index < (cl_uint)ds->arguments.size();
        arg_index++)
      {
        const arg_info &ai = ais[arg_index];
        const refable<init_spec> &ris = ds->arguments[arg_index];
        const init_spec *is = ris;
        if (is == so->spec) {
          if (!ai.type.is<type_ptr>()) {
            // TODO: I am not sure if this check is needed
            //       We could skip the error and continue on to the next
            fatal_at(from_st->defined_at,
              "INTERNAL ERROR: surface type used as non-pointer type");
          }
          return ai.type.as<type_ptr>().element_type;
        }
      }
    }
  } // for arg
  return nullptr;
}
#endif

compiled_script cls::compile(
  const opts &os,
  const script &s,
  diagnostics &ds)
{
  compiled_script cs;
  auto *csi = new compiled_script_impl(ds, os, s);
  cs.impl = csi;

  script_compiler sc {ds, os, s, csi};
  sc.compile();

  return cs;
}
