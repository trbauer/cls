#include "cls_interp_internal.hpp"
#include "../devices.hpp"
#include "../system.hpp"



#include <cmath>
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
#include <numeric>
#include <type_traits>


struct script_compiler : public interp_fatal_handler {
  const script &s;
  compiled_script_impl *csi = nullptr;

  script_compiler(
    const opts &_os, const script &_s, compiled_script_impl *_csi)
    : interp_fatal_handler(_os,_s.source), s(_s), csi(_csi)
  {
  }
  void compile();
private:
  void compileDispatch(const dispatch_spec *ds);
  kernel_object &compileKernel(const kernel_spec *ks);
  program_object &compileProgram(const program_spec *ps);
  device_object &createDeviceObject(const device_spec *ds);
};

kernel_object &script_compiler::compileKernel(const kernel_spec *ks)
{
  auto itr = csi->kernels.find(ks);
  if (itr != csi->kernels.find_end()) {
    return *itr->second;
  }
  program_object *po = &compileProgram(ks->program);
  kernel_object &ko = csi->kernels.emplace_back(ks,ks,po);

  debug(ks->defined_at,"compiling kernel");

  //////////////////////////////////////////////
  // call createKernel on parent object
  //
  cl_program p = (*ko.program->program)();
  cl_int err_ck = 0;
  auto k = clCreateKernel(p,ks->name.c_str(),&err_ck);
  if (err_ck == CL_SUCCESS) {
    ko.kernel = new cl::Kernel(k);
  } else if (err_ck == CL_INVALID_KERNEL_NAME) {
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
    fatalAt(ks->defined_at,"error creating kernel",status_to_symbol(err_ck));
  }

  // parse the args (or use parent args)
  if (os.use_kernel_arg_info) {
    po->program_info.kernels.emplace_back();
    kernel_info &ki = po->program_info.kernels.back();
    ki.name = ks->name;
    memset(ki.reqd_word_group_size,0,sizeof(ki.reqd_word_group_size));
    ko.kernel_info = &ki;
    // TODO: walk arg_info.
    fatalAt(ks->defined_at,"--use-kernel-args not supported yet");
  } else {
    for (kernel_info &ki : po->program_info.kernels) {
      if (ki.name == ko.spec->name) {
        ko.kernel_info = &ki;
        break;
      }
    }
    if (ko.kernel_info == nullptr) {
      fatalAt(ks->defined_at,"unable to find kernel info in source file");
    }
  }

  // Fetch the required workgroup size
  // a bug in Intel CPU driver means we have to use the C API (cl.hpp blows up)
  // __attribute__((reqd_work_group_size(X, Y, Z)))
  CL_COMMAND(ks->defined_at,
    clGetKernelWorkGroupInfo,
      (*ko.kernel)(),
      po->device->device(),
      CL_KERNEL_COMPILE_WORK_GROUP_SIZE,
      sizeof(ko.kernel_info->reqd_word_group_size),
      ko.kernel_info->reqd_word_group_size,
      nullptr);

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
  if (itr != csi->programs.find_end()) {
    return *itr->second;
  }
  if (os.verbosity >= 2) {
    debug(ps->defined_at, "building program");
  }
  device_object *dobj = &createDeviceObject(&ps->device);
  program_object &po = csi->programs.emplace_back(ps,ps,dobj);
  // program_object &po = program_object(ps,dobj);
  // program_object &po = csi->programs[ps] = program_object(ps,dobj);
  // auto eitr = csi->programs.emplace(ps,ps,dobj);
  // program_object &po = eitr.first->second;

  if (!sys::file_exists(ps->path)) {
    loc at = ps->defined_at;
    at.extent = (uint32_t)ps->path.size();
    fatalAt(at, "file not found");
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
    os.warning() << ps->path << ": unable to infer program type from extension\n"
      << "based on contents we're assuming " << (is_bin ? "binary" : "text") << "\n"
      << "NOTE: we recognize .cl, .clc as text and .bin, .ptx, and .obj as binary\n";
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
        os.warning() <<
          "non-empty options to clBuildProgram with binary program";
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
    size_t bytes_per_addr = po.device->device.getInfo<CL_DEVICE_ADDRESS_BITS>() / 8;
    po.program_info = cls::k::parseProgramInfo(
      os,
      this,
      ps->defined_at,
      src,
      bytes_per_addr);
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
  if (itr != csi->devices.find_end()) {
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
  if (os.verbosity >= 2) {
    debug(ds->defined_at,"compiling dispatch");
  }
  kernel_object &ko = compileKernel(ds->kernel);
  dispatch_command &dc = csi->dispatches.emplace_back(ds,ds,&ko);

  if (ds->local_size.rank() > 0 && ds->global_size.rank() != ds->local_size.rank()) {
    fatalAt(ds->local_size_loc,
      "local rank doesn't match global rank (dimensions)");
  }
  if (ds->local_size.rank() != 0) {
    const size_t *rqsz = ko.kernel_info->reqd_word_group_size;
    if (rqsz[0] != 0 && rqsz[1] != 0 && rqsz[2] != 0) {
      switch (dc.global_size.rank()) {
      case 1: dc.local_size = ndr(rqsz[0]);                 break;
      case 2: dc.local_size = ndr(rqsz[0],rqsz[1]);         break;
      case 3: dc.local_size = ndr(rqsz[0],rqsz[1],rqsz[2]); break;
      }
    }
  }
}

void script_compiler::compile()
{
  // Contruct contexts, command queues, programs, kernels, and whatnot
  // Construct contexts and command queues (device_state) for all
  // device_spec's that appear in the script and compile programs.
  for (const statement_spec *st : s.statement_list.statements) {
    if (st->kind == statement_spec::DISPATCH) {
      compileDispatch((const dispatch_spec *)st);
    }
  }

  // Loop through all let's and dispatch arguments
  // Set arguments
  for (const statement_spec *st : s.statement_list.statements) {
    if (st->kind == statement_spec::DISPATCH) {
      const dispatch_spec *ds = (const dispatch_spec *)st;
      dispatch_command &dc = csi->dispatches.get(ds);
      kernel_object &ko = *dc.kernel;
      const auto &ais = ko.kernel_info->args;
      if (ais.size() != ds->arguments.size()) {
        fatalAt(ds->defined_at, "wrong number of arguments to kernel");
      }
      size_t ptr_size = ko.program->device->pointer_size;
      for (cl_uint arg_index = 0;
        arg_index < (cl_uint)ds->arguments.size();
        arg_index++)
      {
        std::stringstream ss;
        const arg_info &ai = ais[arg_index];
        const refable<init_spec*> &ris = ds->arguments[arg_index];
        const init_spec *is = ris;
        if (ai.addr_qual == CL_KERNEL_ARG_ADDRESS_LOCAL) {
          csi->e->setKernelArgSLM(arg_index,dc,ss,ris,ai);
        } else if (std::holds_alternative<type_ptr>(ai.type.var)) {

          csi->e->setKernelArgMemobj(
            arg_index, dc, ss, ris.defined_at, ris, ai);
        } else {
          // A uniform argument
          csi->e->setKernelArgImmediate(arg_index, dc, ss, ris, ai);
        }
        dc.evaluated_args.push_back(ss.str());
      } // for kernel args
      csi->instructions.emplace_back(&dc);
    } else if (st->kind == statement_spec::DIFF) {
      diff_spec *ds = (diff_spec *)st;
      surface_object *so = &csi->surfaces.get(ds->sut.value);
      diff_command *dc = new diff_command(ds,so);
      csi->instructions.emplace_back(dc);
//    } else if (st->kind == statement_spec::LET) {
//      const let_spec *ls = (const let_spec *)st;
//      // fatalAt(ls->defined_at, "NOT IMPLEMENTED");
    }
  }
}

compiled_script cls::compile(const opts &os, const script &s)
{
  compiled_script cs;
  auto *csi = new compiled_script_impl(os,s);
  cs.impl = csi;

  script_compiler sc(os,s,csi);
  sc.compile();

  return cs;
}
