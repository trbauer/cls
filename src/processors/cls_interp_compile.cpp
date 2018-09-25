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

struct script_compiler : public fatal_handler {
  const script &s;
  const Opts &os;
  compiled_script_impl *csi = nullptr;

  script_compiler(
    const Opts &_os, const script &_s, compiled_script_impl *_csi)
    : fatal_handler(*_s.source)
    , os(_os)
    , s(_s)
    , csi(_csi)
  {
  }
  void compile();
private:
  kernel_object &compileKernel(const kernel_spec *ks);
  program_object &compileProgram(const program_spec *ps);
  device_object &allocateDeviceObject(const device_spec *ds);
};


kernel_object &script_compiler::compileKernel(const kernel_spec *ks)
{
  auto itr = csi->kernels.find(ks);
  if (itr == csi->kernels.end()) {
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
  if (itr == csi->programs.end()) {
    return *itr->second;
  }
  if (os.verbosity >= 2) {
    formatMessageWithContext(
      std::cout, ps->defined_at, input(), "building program");
  }
  device_object *dobj = &allocateDeviceObject(&ps->device);
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

device_object &script_compiler::allocateDeviceObject(const device_spec *ds)
{
  auto itr = csi->devices.find(ds);
  if (itr == csi->devices.end()) {
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

void script_compiler::compile()
{
  // Contruct contexts, command queuues, programs, kernels, and whatnot
  // Construct contexts and command queues (device_state) for all
  // device_spec's that appear in the script and compile programs.
  for (const statement_spec *st : s.statements) {
    if (st->kind == statement_spec::DISPATCH) {
      const dispatch_spec *ds = (const dispatch_spec *)st;
      kernel_object &ko = compileKernel(ds->kernel);
      csi->dispatches.emplace_back(ds,&ko);

      // TODO: parse arguments for the dispatch
    }
  }
}

compiled_script cls::compile(const Opts &os, const script &s)
{
  compiled_script cs;
  auto *csi = new compiled_script_impl(s);
  cs.impl = csi;

  script_compiler sc(os,s,csi);
  sc.compile();

  return cs;
}
