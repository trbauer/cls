#include "cls_interp_internal.hpp"
#include "../devices.hpp"
#include "../system.hpp"

#if __has_include(<filesystem>)
#include <filesystem>
namespace fs = std::filesystem;
#elif __has_include(<experimental/filesystem>)
#include <experimental/filesystem>
namespace fs = std::experimental::filesystem;
#else
#error "#include <filesystem> not found"
#endif
#include <limits>
#include <type_traits>

using namespace cls::k;

struct script_compiler : public fatal_handler {
  const script &s;
  const opts &os;
  compiled_script_impl *csi = nullptr;

  script_compiler(
    const opts &_os, const script &_s, compiled_script_impl *_csi)
    : fatal_handler(_s.source)
    , os(_os)
    , s(_s)
    , csi(_csi)
  {
  }
  void compile();
private:
  void compileDispatch(const dispatch_spec *ds);
  kernel_object &compileKernel(const kernel_spec *ks);
  program_object &compileProgram(const program_spec *ps);
  device_object &createDeviceObject(const device_spec *ds);

  void createKernelArgument(
    cl_uint arg_index,
    const init_spec *is,
    const type &t,
    uint8_t *&ptr);
  void createKernelArgumentNum(
    cl_uint arg_index,
    const init_spec *is,
    const type_num &t,
    uint8_t *&ptr);
  template<typename T>
  void createKernelArgumentIntegral(
    loc arg_loc,
    cl_uint arg_index,
    int64_t val,
    uint8_t *&ptr);
  void createKernelArgumentStruct(
    cl_uint arg_index,
    const init_spec *is,
    const type_struct &ts,
    uint8_t *&ptr);
  // other argument setters
};


kernel_object &script_compiler::compileKernel(const kernel_spec *ks)
{
  auto itr = csi->kernels.find(ks);
  if (itr != csi->kernels.find_end()) {
    return *itr->second;
  }
  if (os.verbosity >= 2) {
    formatMessageWithContext(
      std::cout, ks->defined_at, input(), "compiling kernel");
  }
  program_object *po = &compileProgram(ks->program);
  kernel_object &ko = csi->kernels.emplace_back(ks,ks,po);
  // kernel_object ko = kernel_object(ks,po);
  // kernel_object &ko = csi->kernels[ks] = kernel_object(ks,po);
  // auto eitr = csi->kernels.emplace(ks,ks,po);
  // kernel_object &ko = eitr.first->second;

  //////////////////////////////////////////////
  // call createKernel on parent object
  //
  cl_program p = (*ko.program->program)();
  cl_int err = 0;
  auto k = clCreateKernel(p,ks->name.c_str(),&err);
  if (err == CL_SUCCESS) {
    ko.kernel = new cl::Kernel(k);
  } else if (err == CL_INVALID_KERNEL_NAME) {
    std::stringstream ss;
    try {
      std::vector<cl::Kernel> all_kernels;
      po->program->createKernels(&all_kernels);
      ss << "\n" << "valid kernels in the program are:\n";
      for (const cl::Kernel &k : all_kernels) {
        ss << " * " << k.getInfo<CL_KERNEL_FUNCTION_NAME>().c_str() << "\n";
      }
    } catch (const cl::Error &) { /* ignore it (use concise ouput) */ }
    fatalAt(ks->defined_at,"unable to find kernel in program",ss.str());
  } else {
    fatalAt(ks->defined_at,"error creating kernel",status_to_symbol(err));
  }

  // TODO: parse the args (or use parent args)

  return ko;
}


static std::string getLabeledBuildLog(loc at, cl::Program &prog) {
  auto logs = prog.getBuildInfo<CL_PROGRAM_BUILD_LOG>();
  std::stringstream ss;
  ss << "[" << at.str() << "]: ";
  return text::prefix_lines(
    ss.str(),
    logs.front().second.c_str());
}

program_object &script_compiler::compileProgram(const program_spec *ps)
{
  auto itr = csi->programs.find(ps);
  if (itr == csi->programs.find_end()) {
    return *itr->second;
  }
  if (os.verbosity >= 2) {
    formatMessageWithContext(
      std::cout, ps->defined_at, input(), "building program");
  }
  device_object *dobj = &createDeviceObject(&ps->device);
  program_object &po = csi->programs.emplace_back(ps,ps,dobj);
  // program_object &po = program_object(ps,dobj);
  // program_object &po = csi->programs[ps] = program_object(ps,dobj);
  // auto eitr = csi->programs.emplace(ps,ps,dobj);
  // program_object &po = eitr.first->second;

  if (!sys::file_exists(ps->path)) {
    fatalAt(ps->defined_at, "file not found");
  }
  bool is_bin =
    strsfx(".bin",ps->path) ||
    strsfx(".ptx",ps->path) ||
    strsfx(".obj",ps->path);
  bool is_clc =
    strsfx(".cl",ps->path) ||
    strsfx(".clc",ps->path);
  if (!is_bin && !is_clc) {
    // look at the file contents
    auto bs = sys::read_file_binary(ps->path);
    for (uint8_t c : bs) {
      if (!::isprint(c)) {
        is_bin = true;
        break;
      }
    }
    os.verbose() << ps->path << ": unable to infer program type from extension\n"
      << "based on contents assuming " << (is_bin ? "binary" : "text");
  }
  auto fatalHere =
    [&](const char *do_what, const char *with_what, cl_int err) {
      auto dev_nm = po.device->device.getInfo<CL_DEVICE_NAME>().c_str();
      fatalAt(ps->defined_at,
        ps->path, ": failed to ", do_what, " with ", with_what,
        " (", status_to_symbol(err), ") on device [",dev_nm,"]");
    };

  std::string build_opts = ps->build_options;
  std::string build_opts_with_arg_info = build_opts;
  auto vend = getDeviceVendor(po.device->device);
  if (os.use_kernel_arg_info) {
    if (is_bin && vend != cl_vendor::CL_INTEL)
      os.warning() <<
        "trying to use kernel argument info on non-Intel binary\n";
    if (getDeviceSpec(po.device->device) <  cl_spec::CL_1_2)
      fatalAt(ps->defined_at,"kernel argument info needs OpenCL 1.2+");
    if (build_opts.empty()) {
      build_opts_with_arg_info = "-cl-kernel-arg-info";
    } else if (build_opts.find("-cl-kernel-arg-info") == std::string::npos) {
      build_opts_with_arg_info += " -cl-kernel-arg-info";
    }
  }

  program_source src;
  src.path = ps->path;
  src.build_opts = build_opts;
  src.is_binary = is_bin;

  if (is_bin) {
    std::vector<cl::Device> devs{po.device->device};
    cl::Program::Binaries bins{sys::read_file_binary(ps->path)};
    try {
      po.program = new cl::Program(
        *po.device->context,
        devs,
        bins); // clCreateProgramWithBinary
    } catch (const cl::Error &err) {
      fatalHere("create","binary",err.err());
    }
    try {
      if (!build_opts.empty()) {
        os.warning() << "non-empty options to clBuildProgram with binary program";
      }
      po.program->build( // clBuildProgram (yep, it's still required for binary)
        build_opts.empty() ?
        nullptr : build_opts.c_str());
    } catch (const cl::Error &err) {
      fatalHere("build","binary",err.err());
    }
  } else { // is_clc (text)
    std::string inp = sys::read_file_text(ps->path);
    try {
      po.program = new cl::Program(*po.device->context, inp); // clCreateProgramWithSource
    } catch (const cl::Error &err) {
      fatalHere("create","source",err.err());
    }
    try {
      po.program->build(build_opts_with_arg_info.c_str()); // clBuildProgram
      if (os.verbosity >= 2) {
        os.debug() <<
          getLabeledBuildLog(ps->defined_at, *po.program);
      }
      if (os.save_binaries) {
        std::string bin_ext;
        if (vend == cl_vendor::CL_NVIDIA)
          bin_ext = ".ptx";
        else
          bin_ext = ".bin";
        auto bin_path =
          fs::path(".") / fs::path(ps->path).filename().replace_extension(bin_ext);
        auto bin = po.program->getInfo<CL_PROGRAM_BINARIES>().front();
        os.verbose() << "dumping binary " << bin_path << "\n";
        sys::write_bin_file(bin_path.string(),bin.data(),bin.size());
      }
    } catch (const cl::Error &err) {
      if (err.err() == CL_BUILD_PROGRAM_FAILURE) {
        std::stringstream ss;
        ss << "failed to build source:\n";
        ss << getLabeledBuildLog(
          ps->defined_at, *po.program) << "\n";
        fatalAt(ps->defined_at,ss.str());
      } else {
        fatalHere("build","source",err.err());
      }
    } // catch
  } // endif text

  ////////////////////////////////////////////////
  // TODO: kernel args
  if (os.use_kernel_arg_info) {
    // create all the kernels
    // parse each kernel arg
    fatalAt(ps->defined_at, "--use-kernel-args not supported yet");
  } else {
    // use kargs library
    po.program_info = cls::k::parseProgramInfo(os, this, ps->defined_at, src);
  }

  return po;
}

static void CL_CALLBACK dispatchContextNotify(
  const char *errinfo,
  const void *private_info, size_t cb,
  void *user_data)
{
  device_object *dobj = (device_object *)user_data;
  dobj->contextNotify(errinfo, private_info, cb, user_data);
}

device_object &script_compiler::createDeviceObject(const device_spec *ds)
{
  auto itr = csi->devices.find(ds);
  if (itr == csi->devices.find_end()) {
    return *itr->second;
  }

  cl::Device dev;
  switch (ds->kind) {
  case device_spec::BY_DEFAULT:
    dev = getDeviceDefault(os);
    break;
  case device_spec::BY_INDEX:
    if (!getDeviceByIndex(os, ds->by_index_value,dev)) {
      fatalAt(ds->defined_at,"invalid device index");
    }
    break;
  case device_spec::BY_NAME: {
    std::string err_msg;
    if (!getDeviceByName(os, ds->by_name_value, dev, err_msg))
      fatalAt(ds->defined_at, "invalid device specification ", err_msg);
    break;
  }
  default:
    fatalAt(ds->defined_at, "invalid device spec");
    break;
  }

  // device_object &dobj = csi->devices[ds] = device_object(ds,dev);
  // auto eitr = csi->devices.emplace(ds,ds,dev);
  // auto eitr = csi->devices.emplace(std::make_pair(ds,device_object(ds,dev)));
  // auto eitr = csi->devices.emplace(ds,
  //  std::forward<device_object>(device_object(ds,dev)));
  // device_object &dobj = eitr.first->second;
  // csi->device_object_list.emplace_back(ds,ds,dev);
  // device_object &dobj = csi->device_object_list.back();
  //
  // device_object &dobj2 = csi->devices2.get(ds,ds,dev);
  device_object &dobj = csi->devices.emplace_back(ds, ds, dev);

  try {
    std::vector<cl::Device> devs{dev};
    dobj.context = new cl::Context(
      devs,
      nullptr,
      dispatchContextNotify,
      (void *)&dobj);
  } catch (const cl::Error &err) {
    fatalAt(ds->defined_at,"clCreateContext: ",status_to_symbol(err.err()));
  }

  cl_command_queue cq;
  auto cl_err = makeCommandQueue(os.prof_time, dev(), (*dobj.context)(), cq);
  if (cl_err != CL_SUCCESS) {
    fatalAt(
      ds->defined_at,
      "clCreateCommandQueue: failed to create command queue for device "
      "(", status_to_symbol(cl_err), ")");
  }
  dobj.queue = new cl::CommandQueue(cq);

  return dobj;
}

void script_compiler::compileDispatch(const dispatch_spec *ds)
{
  kernel_object &ko = compileKernel(ds->kernel);
  dispatch_command &dc = csi->dispatches.emplace_back(ds,ds,&ko);

  const auto &gsz = ds->global_size.dims;
  switch (gsz.size()) {
  case 1: dc.global_size = cl::NDRange(gsz[0]);               break;
  case 2: dc.global_size = cl::NDRange(gsz[0],gsz[1]);        break;
  case 3: dc.global_size = cl::NDRange(gsz[0],gsz[1],gsz[2]); break;
  default:
    fatalAt(
      ds->global_size.defined_at,
      "global rank (dimension) too large (must be <=3)");
  }

  auto fmtNDR =
    [] (const cl::NDRange &ndr) {
      if (ndr == cl::NullRange) {
        return std::string("NULL");
      }
      std::stringstream ss;
      for (size_t i = 0; i < ndr.dimensions(); i++) {
        if (i != 0) ss << 'x';
        ss << ndr.get()[i];
      }
      return ss.str();
    };

  const auto &lsz = ds->local_size.dims;
  if (lsz.size() > 0 && gsz.size() != lsz.size()) {
    fatalAt(ds->local_size.defined_at,
      "local rank doesn't match global rank (dimensions)");
  }
  switch (lsz.size()) {
  case 0: { // auto size
    // bug in Intel CPU driver means we have to use the C API
    // __attribute__((reqd_work_group_size(X, Y, Z)))
    size_t rqsz[3]{0,0,0};
    auto err = clGetKernelWorkGroupInfo(
      (*ko.kernel)(),
      dc.ko->program->device->device(),
      CL_KERNEL_COMPILE_WORK_GROUP_SIZE,
      sizeof(rqsz),
      &rqsz,
      nullptr);
    if (err != CL_SUCCESS) {
      fatalAt(ds->local_size.defined_at,
        "clGetKernelWorkGroupInfo<CL_KERNEL_COMPILE_WORK_GROUP_SIZE> "
        "returned ", status_to_symbol(err));
    }
    if (rqsz[0] == 0 && rqsz[1] == 0 && rqsz[2] == 0) {
      dc.local_size = cl::NullRange;
    } else {
      switch (gsz.size()) {
      case 1: dc.local_size = cl::NDRange(rqsz[0]);                 break;
      case 2: dc.local_size = cl::NDRange(rqsz[0],rqsz[1]);         break;
      case 3: dc.local_size = cl::NDRange(rqsz[0],rqsz[1],rqsz[2]); break;
      }
    }
    break;
  }
  case 1: dc.local_size = cl::NDRange(lsz[0]);               break;
  case 2: dc.local_size = cl::NDRange(lsz[0],lsz[1]);        break;
  case 3: dc.local_size = cl::NDRange(lsz[0],lsz[1],lsz[2]); break;
  default:
    fatalAt(ds->local_size.defined_at, "local rank too large (must be <=3)");
  } // local size

  os.verbose() <<
    "using global size: " << fmtNDR(dc.global_size) << "\n" <<
    "using local size:  " << fmtNDR(dc.local_size)  << "\n";
}

void script_compiler::compile()
{
  // Contruct contexts, command queuues, programs, kernels, and whatnot
  // Construct contexts and command queues (device_state) for all
  // device_spec's that appear in the script and compile programs.
  for (const statement_spec *st : s.statement_list.statements) {
    if (st->kind == statement_spec::DISPATCH) {
      compileDispatch((const dispatch_spec *)st);
    }
  }

  // Loop through all let's and dispatch arguments
  // Set scalar arguments
  for (const statement_spec *st : s.statement_list.statements) {
    if (st->kind == statement_spec::DISPATCH) {
      const dispatch_spec *ds = (const dispatch_spec *)st;
      dispatch_command &dc = csi->dispatches.get(ds);
      const auto &ais = dc.ko->kernel_info->args;
      if (ais.size() != ds->arguments.size()) {
        fatalAt(ds->defined_at,"wrong number of arguments to kernel");
      }
      size_t ptr_size =
        dc.ko->program->device->device.getInfo<CL_DEVICE_ADDRESS_BITS>()/8;
      for (cl_uint i = 0; i < (cl_uint)ds->arguments.size(); i++) {
        const init_spec *is = ds->arguments[i];
        const arg_info &ai = ais[i];
        std::vector<uint8_t> arg_buffer;
        arg_buffer.resize(ai.type->size(ptr_size));
        uint8_t *buffer = arg_buffer.data();
        createKernelArgument(i, is, *ai.type, buffer);
      }

    } else if (st->kind == statement_spec::LET) {
      const let_spec *ls = (const let_spec *)st;
      fatalAt(ls->defined_at,"NOT IMPLEMENTED");
    }
  }
}


// we match formal type over actual type
void script_compiler::createKernelArgument(
  cl_uint arg_index,
  const init_spec *is,
  const type &t,
  uint8_t *&buffer)
{
  if (std::holds_alternative<type_num>(t.var)) {
    createKernelArgumentNum(arg_index, is, std::get<type_num>(t.var), buffer);
  } else if (std::holds_alternative<type_struct>(t.var)) {
    createKernelArgumentStruct(
      arg_index, is, std::get<type_struct>(t.var), buffer);
  // else if (enum) {
  // else if (built-in) {
  } else {
    fatalAt(is->defined_at," unsupported kernel argument type");
  }
}

void script_compiler::createKernelArgumentNum(
  cl_uint arg_index,
  const init_spec *is,
  const type_num &tn,
  uint8_t *&buffer)
{
  switch (tn.kind) {
  case type_num::UNSIGNED:
  case type_num::SIGNED:
    if (is->kind != init_spec::IS_INT) {
      fatalAt(is->defined_at,"kernel argument requires integer parameter");
    } else {
      int64_t val = ((const init_spec_int *)is)->value;
      if (tn.kind == type_num::UNSIGNED) {
        switch (tn.size_in_bytes) {
        case 1:
          createKernelArgumentIntegral<uint8_t>(is->defined_at,arg_index,val,buffer);
          break;
        case 2:
          createKernelArgumentIntegral<uint16_t>(is->defined_at,arg_index,val,buffer);
          break;
        case 4:
          createKernelArgumentIntegral<uint32_t>(is->defined_at,arg_index,val,buffer);
          break;
        case 8:
          createKernelArgumentIntegral<uint64_t>(is->defined_at,arg_index,val,buffer);
          break;
        default:
          fatalAt(is->defined_at,"INTERNAL ERROR: unexpected primitive size");
        }
      } else { // signed
        switch (tn.size_in_bytes) {
        case 1:
          createKernelArgumentIntegral<int8_t>(is->defined_at,arg_index,val,buffer);
          break;
        case 2:
          createKernelArgumentIntegral<int16_t>(is->defined_at,arg_index,val,buffer);
          break;
        case 4:
          createKernelArgumentIntegral<int32_t>(is->defined_at,arg_index,val,buffer);
          break;
        case 8:
          createKernelArgumentIntegral<int64_t>(is->defined_at,arg_index,val,buffer);
          break;
        default:
          fatalAt(is->defined_at,"INTERNAL ERROR: unexpected primitive size");
        }
      }
    }
    break;
  case type_num::FLOATING: {
    double f64 = 0.0;
    if (is->kind != init_spec::IS_FLT) {
      f64 = ((const init_spec_float *)is)->value;
    } else if (is->kind != init_spec::IS_INT) {
      f64 = (double)((const init_spec_int *)is)->value;
    } else {
      fatalAt(is->defined_at,"kernel argument requires floating-point parameter");
    }
    switch (tn.size_in_bytes) {
    case 2: {
      if (f64 > 65504.0)
        fatalAt(is->defined_at,"kernel argument too large for half type");
      else if (f64 < -65504.0)
        fatalAt(is->defined_at,"kernel argument too low for half type");
      else if (std::abs(f64) < 5.96046e-8)
        fatalAt(is->defined_at,"kernel argument too small for half type");
      uint16_t f16 = float_to_half((float)f64);
      memcpy(buffer, &f16, sizeof(f16));
      buffer += 2;
    }
    case 4: {
      // if (f64 != (float)f64) { FAILS on repeated values
      //  fatalAt(is->defined_at,"precision loss in parameter");
      // }
      if (f64 > std::numeric_limits<float>::max())
        fatalAt(is->defined_at,"kernel argument too large for half type");
      else if (f64 < std::numeric_limits<float>::lowest())
        fatalAt(is->defined_at,"kernel argument too low for half type");
      else if (std::abs(f64) < std::numeric_limits<float>::min())
        fatalAt(is->defined_at,"kernel argument too small for half type");
      float f32 = (float)f64;
      memcpy(buffer, &f32, sizeof(f32));
      buffer += 4;
      break;
    }
    case 8:
      memcpy(buffer, &f64, sizeof(f64));
      buffer += 8;
      break;
    default:
      fatalAt(is->defined_at,"INTERNAL ERROR: unexpected primitive size");
    }
  } // case type_num::FLOATING
  default:
    fatalAt(is->defined_at,"INTERNAL ERROR: unexpected primitive kind");
  } // switch
}

void script_compiler::createKernelArgumentStruct(
  cl_uint arg_index,
  const init_spec *is,
  const type_struct &ts,
  uint8_t *&buffer)
{
  if (is->kind == init_spec::IS_REC) {
    const init_spec_record *isr = (const init_spec_record *)is;
    if (isr->children.size() != ts.elements_length) {
      fatalAt(is->defined_at,
        "structure initializer has wrong number of elements");
    }
    for (size_t i = 0; i < ts.elements_length; i++) {
      createKernelArgument(
        arg_index,
        isr->children[i],
        *ts.elements[i],
        buffer);
    }
  } else {
    fatalAt(is->defined_at,
      "structure argument requires structure initializer");
  }
}


template<typename T>
void script_compiler::createKernelArgumentIntegral(
  loc arg_loc,
  cl_uint arg_index,
  int64_t val64,
  uint8_t *&buffer)
{
  if (std::is_unsigned<T>()) {
    if ((uint64_t)val64 > (uint64_t)std::numeric_limits<T>::max()) {
      fatalAt(arg_loc,"kernel argument ", arg_index,
        " is too large (overflows) for formal type");
    }
  } else if (std::is_signed<T>()) {
    if (val64 > (int64_t)std::numeric_limits<T>::max()) {
      fatalAt(arg_loc,"kernel argument ", arg_index,
        " is too large (overflows) for formal type");
    }
    if (val64 < (int64_t)std::numeric_limits<T>::lowest()) {
      fatalAt(arg_loc,"kernel argument ", arg_index,
        " is too large (overflows) for formal type");
    }
  }
  T val = (T)val64;
  memcpy(buffer, &val, sizeof(T));
  buffer += sizeof(T);
}

compiled_script cls::compile(const opts &os, const script &s)
{
  compiled_script cs;
  auto *csi = new compiled_script_impl(s);
  cs.impl = csi;

  script_compiler sc(os,s,csi);
  sc.compile();

  return cs;
}
