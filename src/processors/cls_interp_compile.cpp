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
#include <set>
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
  dispatch_command *compileDispatchArgs(const dispatch_spec *ds);
  diffu_command *compileDiffU(const diff_spec *ds);
  diffs_command *compileDiffS(const diff_spec *ds);
  print_command *compilePrint(const print_spec *ps);
  const type *inferSurfaceElementType(const loc &at, surface_object *so);

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

  // Loop through all the statements
  //  - compile dispatch arguments
  //  - compile diff command (needs args for surface info type)
  for (size_t st_ix = 0; st_ix < s.statement_list.statements.size(); st_ix++) {
    const statement_spec *st = s.statement_list.statements[st_ix];
    if (st->kind == statement_spec::DISPATCH) {
      dispatch_command *dc = compileDispatchArgs((const dispatch_spec *)st);
      csi->instructions.emplace_back(dc);
    } else if (st->kind == statement_spec::DIFF) {
      const diff_spec *ds = (const diff_spec *)st;
      if (ds->ref.value->is_atom()) {
        diffu_command *dc = compileDiffU(ds);
        csi->instructions.emplace_back(dc);
      } else {
        diffs_command *dc = compileDiffS(ds);
        csi->instructions.emplace_back(dc);
      }
    } else if (st->kind == statement_spec::PRINT) {
      print_command *pc = compilePrint((const print_spec *)st);
      csi->instructions.emplace_back(pc);
    } else if (st->kind == statement_spec::SAVE) {
      const save_spec *ss = (const save_spec *)st;
      // surface_object *so =  &csi->surfaces.get(ss->arg.value);
      save_command *sc = new save_command(ss,nullptr);
      csi->instructions.emplace_back(sc);
    } else if (st->kind == statement_spec::LET) {
      ; // nop
    } else {
      internalAt(st->defined_at, "NOT IMPLEMENTED");
    }
  } // for

  // (e.g. print, diff, save) and bind surface_object*'s to their value.
  // Loop through all memory object statements and link them to the surface
  // objects they process.  We do this as a second pass because
  // surface_objects are only created once they are used in a dispatch.
  // E.g.
  //    let X = random:w
  //    print<int>(X)  <<<<<<<<<<<<< surface object used before dispatch
  //    #0`foo.cl`kernel<...>(X);
  for (script_instruction &si : csi->instructions) {
    switch (si.kind)
    {
    case script_instruction::DIFFS: {
      diffs_command *dfsc = si.dfsc;
      // sut surface
      dfsc->so_sut = &csi->surfaces.get(dfsc->spec->sut.value);
      debug(dfsc->spec,"bound (SUT) surface to ",dfsc->so_sut->str());

      // bind the element type if it's not explicitly set
      if (!dfsc->element_type) {
        dfsc->element_type =
          inferSurfaceElementType(dfsc->spec->defined_at, dfsc->so_sut);
        if (dfsc->element_type)
          debug(dfsc->spec,"bound element type to ",
            dfsc->element_type->syntax());
      }

      // reference surface
      const init_spec_mem *ism_ref = (const init_spec_mem *)dfsc->spec->ref.value;
      if (csi->surfaces.find(ism_ref) != csi->surfaces.find_end()) {
        dfsc->so_ref = &csi->surfaces.get(ism_ref);
        debug(dfsc->spec,"bound (REF) surface to ",dfsc->so_ref->str());
        if (dfsc->so_sut->size_in_bytes != dfsc->so_ref->size_in_bytes) {
          fatalAt(dfsc->spec->defined_at,"surface sizes mismatch");
        }
      } else {
        // it's an immediate object, we have to create a dummy surface
        // we use the SUT object properties for reference
        //
        // e.g. diff<int>(seq(4,2):r, SUT)
        //
        if (dfsc->so_sut->dispatch_uses.empty()) {
          fatalAt(dfsc->spec->defined_at,
            "cannot diff against unused memory object");
        }
        if (!dfsc->element_type) {
          fatalAt(dfsc->spec->defined_at,
            "explicit type required for reference surface initializer");
        }
        // use SUT to dtermine the buffer type etc...
        dispatch_command *dc = std::get<0>(dfsc->so_sut->dispatch_uses.front());
        const arg_info &ai = std::get<2>(dfsc->so_sut->dispatch_uses.front());
        cl_mem_flags cl_mfs = CL_MEM_READ_ONLY;
        cl_mem memobj = nullptr;
        cl_context context = (*dc->kernel->program->device->context)();
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
        withBufferMapWrite(
          ism_ref->defined_at,
          dfsc->so_ref,
          [&](void *host_ptr) {
            evaluator::context ec(dc->global_size,dc->local_size);
            const type *elem_type = dfsc->element_type;
            if (!elem_type) {
              // typeless diff needs the element from the SUT
              // diff(22:r,SUT) needs to take the type from SUT
              if (!dfsc->so_sut->dispatch_uses.empty()) {
                fatalAt(dfsc->spec->defined_at,"cannot infer element type");
              }
              const type &t = std::get<2>(dfsc->so_sut->dispatch_uses.back()).type;
              if (!t.is<type_ptr>()) {
                fatalAt(dfsc->spec->defined_at,
                  "inferred element type (from SUT) is not a pointer");
              }
              elem_type = t.as<type_ptr>().element_type;
            }
            csi->init_surface(*dfsc->so_ref,ec,elem_type,host_ptr);
          });
      }
      break;
    }
    case script_instruction::DIFFU: {
      diffu_command *dfuc = si.dfuc;
      si.dfuc->so = &csi->surfaces.get(dfuc->spec->sut.value);
      debug(si.dfuc->spec,"bound surface to ",dfuc->so->str());
      if (!dfuc->element_type) {
        dfuc->element_type =
          inferSurfaceElementType(dfuc->spec->defined_at, dfuc->so);
        if (dfuc->element_type)
          debug(dfuc->spec,"bound element type to ",
            dfuc->element_type->syntax());
      }
      break;
    }
    case script_instruction::PRINT:
      si.prc->so = &csi->surfaces.get(si.prc->spec->arg.value);
      debug(si.prc->spec,"bound surface to ",si.prc->so->str());
      if (!si.prc->element_type) {
        si.prc->element_type =
          inferSurfaceElementType(si.prc->spec->defined_at, si.prc->so);
        if (si.prc->element_type)
          debug(si.prc->spec,"bound element type to ",
            si.prc->element_type->syntax());
      }
      break;
    case script_instruction::SAVE:
      si.svc->so = &csi->surfaces.get(si.svc->spec->arg.value);
      debug(si.svc->spec,"bound surface to ",si.svc->so->str());
      break;
    } // switch
  } // for
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

dispatch_command *script_compiler::compileDispatchArgs(
  const dispatch_spec *ds)
{
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
    const refable<init_spec> &ris = ds->arguments[arg_index];
    const init_spec *is = ris;
    if (ai.addr_qual == CL_KERNEL_ARG_ADDRESS_LOCAL) {
      csi->e->setKernelArgSLM(arg_index,dc,ss,ris,ai);
    } else if (ai.type.is<type_ptr>()) {
      csi->e->setKernelArgMemobj(
        arg_index, dc, ss, ris.defined_at, ris, ai);
    } else {
      // A uniform argument
      csi->e->setKernelArgImmediate(arg_index, dc, ss, ris, ai);
    }
    dc.evaluated_args.push_back(ss.str());
  } // for kernel args
  return &dc;
}

diffu_command *script_compiler::compileDiffU(const diff_spec *ds)
{
#if 0
  surface_object *so = &csi->surfaces.get(ds->sut.value);
  const type *element_type = ds->element_type;

  if (!element_type) {
    // infer the template type by using the last type where the surface
    // is used
    // e.g. if the surface was last used as: global int2 *buffer,
    // then we use int2
    element_type = inferSurfaceElementType(so, st_ix);
  } // element_type != nullptr

  return new diffu_command(ds,so,element_type);
#endif
  return new diffu_command(ds,nullptr,nullptr);
}
diffs_command *script_compiler::compileDiffS(const diff_spec *ds)
{
  return new diffs_command(ds,nullptr,nullptr,nullptr);
}

print_command *script_compiler::compilePrint(const print_spec *ps)
{
#if 0
  surface_object *so = &csi->surfaces.get(ps->arg.value);
  const type *element_type = ps->element_type;

  if (!element_type) {
    // infer the template type by using the last type where the surface
    // is used
    // e.g. if the surface was last used as: global int2 *buffer,
    // then we use "int2"
    element_type = inferSurfaceElementType(so, st_ix);
  } // element_type != nullptr

  return new print_command(ps,so,element_type);
#endif
  return new print_command(ps,nullptr,ps->element_type);
}

const type *script_compiler::inferSurfaceElementType(
  const loc &at, surface_object *so)
{
  if (so->dispatch_uses.empty()) {
    fatalAt(so->spec->defined_at,
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
    if (is == so->spec) {
      if (!ai.type.is<type_ptr>()) {
        // TODO: I am not sure if this check is needed
        //       We could skip the error and try another dispatch command
        fatalAt(at, "INTERNAL ERROR: surface type used as non-pointer type");
      }
      return ai.type.as<type_ptr>().element_type;
    }
  }
  fatalAt(at, "INTERNAL ERROR: surface object mis-linked to dispatch");
  return nullptr;
}

#if 0
const type *script_compiler::inferSurfaceElementType(
  surface_object *so, size_t from_st_ix)
{
  statement_spec *from_st = s.statement_list.statements[from_st_ix];
  for (int ix = (int)from_st_ix - 1; ix >= 0; ix--) {
    const statement_spec *st1 = s.statement_list.statements[ix];
    if (st1->kind == statement_spec::DISPATCH) {
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
            fatalAt(from_st->defined_at,
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

compiled_script cls::compile(const opts &os, const script &s)
{
  compiled_script cs;
  auto *csi = new compiled_script_impl(os,s);
  cs.impl = csi;

  script_compiler sc(os,s,csi);
  sc.compile();

  return cs;
}
