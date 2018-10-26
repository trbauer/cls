#include "cls_interp_internal.hpp"
#include "../devices.hpp"
#include "../system.hpp"
#include "../text.hpp"

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

  std::string getLabeledBuildLog(
    const loc &at, cl_program p, cl_device_id dev_id);
};

// a dummy type for size_t[3]
// cannot use cls::ndr since that uses the constructor to infer rank and
// has the extra field
struct ndr_temp {
  size_t dims[3];
  ndr_temp() {dims[0] = dims[1] = dims[2] = 0;}

  // HACK: unreachable for ndr_temp,
  // but suffices the template for emitCompiledKernelProperty
  ndr_temp operator%(size_t) {return *this;}
  ndr_temp operator/(size_t) {return *this;}
  bool operator==(size_t) {return false;}
};

std::ostream &operator <<(std::ostream &os, ndr_temp t) {
  os << t.dims[0] << " x " << t.dims[1] << " x " << t.dims[2];
  return os;
}

// dummy type to represent size_t[3] and emit to an ostream
template <typename T>
static void emitCompiledKernelProperty(
  cl_kernel kernel,
  cl_device_id device,
  cl_int param,
  const char *param_name,
  bool is_mem,
  const char *units = nullptr)
{
  std::cout << std::setw(48) << param_name << ": ";
  T val;
  auto err =
    clGetKernelWorkGroupInfo(kernel, device, param, sizeof(T), &val, nullptr);
  if (err != CL_SUCCESS) {
    std::cout << text::spans::RED(status_to_symbol(err));
  } else {
    if (is_mem && !units) {
      if (val % 1024 == 0)
        std::cout << text::spans::YELLOW(val/1024) << " K";
      else
        std::cout << text::spans::YELLOW(val) << " B";
    } else {
      std::cout << text::spans::YELLOW(val);
      if (units)
        std::cout << units;
    }
  }
  std::cout << "\n";
}

typedef cl_int (*clGetKernelSubGroupInfo_TYPE) (
  cl_kernel,
 	cl_device_id,
 	cl_kernel_sub_group_info,
 	size_t, const void *,
 	size_t, void * , size_t *);

clGetKernelSubGroupInfo_TYPE findSubgroupFunction()
{
  static clGetKernelSubGroupInfo_TYPE function;
  if (function == nullptr) {
    void *lib = sys::load_library("OpenCL");
    function = (clGetKernelSubGroupInfo_TYPE)
      sys::get_symbol_address(lib,"clGetKernelSubGroupInfo");
    if (function == nullptr)
      function = (clGetKernelSubGroupInfo_TYPE)
        sys::get_symbol_address(lib,"clGetKernelSubGroupInfoKHR");
    if (function == nullptr)
      function = (clGetKernelSubGroupInfo_TYPE)
        sys::get_symbol_address(lib,"clGetKernelSubGroupInfoIntel");
  }
  return function;
}

static void emitCompiledKernelProperties(
  fatal_handler *fh, kernel_object &ko)
{
  cl_kernel kernel = (*ko.kernel)();
  cl_device_id device = ko.program->device->device();
  cl_spec spec = getDeviceSpec(ko.program->device->device);

  // units are an optional last parameter
#define KERNEL_PROPERTY(MINSPEC,PARAM,TYPE,...) \
  if (spec >= (MINSPEC)) \
    emitCompiledKernelProperty<TYPE>(kernel, device, PARAM, #PARAM, false, __VA_ARGS__)
#define KERNEL_PROPERTY_MEM(MINSPEC,PARAM) \
  if (spec >= (MINSPEC)) \
    emitCompiledKernelProperty<cl_ulong>(kernel, device, PARAM, #PARAM, true, nullptr)

  KERNEL_PROPERTY(cl_spec::CL_1_2, CL_KERNEL_GLOBAL_WORK_SIZE, ndr_temp);
  KERNEL_PROPERTY(cl_spec::CL_1_0, CL_KERNEL_WORK_GROUP_SIZE, size_t);
  KERNEL_PROPERTY(cl_spec::CL_1_0, CL_KERNEL_WORK_GROUP_SIZE, size_t);
  KERNEL_PROPERTY(cl_spec::CL_1_0, CL_KERNEL_COMPILE_WORK_GROUP_SIZE, ndr_temp);
  KERNEL_PROPERTY(cl_spec::CL_1_1, CL_KERNEL_PREFERRED_WORK_GROUP_SIZE_MULTIPLE, size_t);
  KERNEL_PROPERTY_MEM(cl_spec::CL_1_0, CL_KERNEL_LOCAL_MEM_SIZE);
  KERNEL_PROPERTY_MEM(cl_spec::CL_1_1, CL_KERNEL_PRIVATE_MEM_SIZE);
  if (hasExtension(device,"cl_intel_required_subgroup_size")) {
    KERNEL_PROPERTY_MEM(cl_spec::CL_1_0, CL_KERNEL_SPILL_MEM_SIZE_INTEL);
    //
    std::cout << std::setw(48) << "CL_KERNEL_COMPILE_SUB_GROUP_SIZE_INTEL: ";
    size_t sbsi = 0;
    auto cl_subgroup_function = findSubgroupFunction();
    if (cl_subgroup_function == nullptr) {
      std::cout << text::spans::RED("unable to load clGetKernelSubGroupInfo*");
    } else {
      auto sbsi_err =
        (*cl_subgroup_function)(kernel, device,
          CL_KERNEL_COMPILE_SUB_GROUP_SIZE_INTEL,
          0, nullptr,
          sizeof(size_t), &sbsi, nullptr);
      if (sbsi_err == CL_SUCCESS) {
        std::cout << text::spans::YELLOW(sbsi);
      } else {
        std::cout << text::spans::RED(status_to_symbol(sbsi_err));
      }
    }
    std::cout << "\n";
  }
}

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
  // NOTE: avoid CL_COMMAND_CREATE macro because we want error code
  cl_int err_ck = 0;
  auto k = clCreateKernel(ko.program->program,ks->name.c_str(),&err_ck);
  if (err_ck == CL_SUCCESS) {
    ko.kernel = new cl::Kernel(k);
  } else if (err_ck == CL_INVALID_KERNEL_NAME) {
    std::stringstream ss;
    try {
      std::vector<cl::Kernel> all_kernels;
      cl::Program p(po->program);
      p.createKernels(&all_kernels);
      ss << "\n" << "valid kernels in the program are:\n";
      for (const cl::Kernel &k : all_kernels) {
        ss << " * " << k.getInfo<CL_KERNEL_FUNCTION_NAME>().c_str() << "\n";
      }
    } catch (const cl::Error &) { /* ignore it (use concise ouput) */ }
    fatalAt(ks->defined_at,"unable to find kernel in program",ss.str());
  } else {
    fatalAt(ks->defined_at,"error creating kernel",status_to_symbol(err_ck));
  }

  // find the kernel info from the program info
  for (kernel_info &ki : po->program_info.kernels) {
    if (ki.name == ko.spec->name) {
      ko.kernel_info = &ki;
      break;
    }
  }
  if (ko.kernel_info == nullptr) {
    fatalAt(ks->defined_at,"unable to find kernel info in source file");
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

  if (os.verbosity > 1) {
    formatMessageWithContext(
      std::cout,
      ks->defined_at,
      &text::ANSI_YELLOW,
      input(),
      "========== clGetKernelWorkGroupInfo ==========");
    std::cout << "\n";
    emitCompiledKernelProperties(this, ko);
  }

  return ko;
}

std::string script_compiler::getLabeledBuildLog(
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

  cl_context ctx = (*po.device->context)();
  cl_device_id dev = po.device->device();

  if (is_bin) {
    auto bits = sys::read_file_binary(ps->path);
    size_t len = bits.size();
    const unsigned char *bins = bits.data();
    cl_int bs_errs = 0;
    CL_COMMAND_CREATE(po.program, ps->defined_at,
      clCreateProgramWithBinary,
        ctx,
        1,
        &dev,
        &len,
        &bins,
        &bs_errs);
    CL_COMMAND(ps->defined_at,
      clBuildProgram,
        po.program,
        1,
        &dev,
        build_opts.empty() ? nullptr : build_opts.c_str(),
        nullptr,
        nullptr);
  } else { // is_clc (text)
    std::string inp = sys::read_file_text(ps->path);
    const char *src = inp.c_str();
    size_t len = inp.size();
    CL_COMMAND_CREATE(po.program, ps->defined_at,
      clCreateProgramWithSource,
        ctx,1,&src,&len);

    cl_int bp_err =
      clBuildProgram(po.program, 1, &dev, build_opts.c_str(), nullptr, nullptr);
    if (bp_err == CL_BUILD_PROGRAM_FAILURE) {
      std::stringstream ss;
      ss << "failed to build source:\n";
      ss << getLabeledBuildLog(ps->defined_at, po.program, dev) << "\n";
      fatalAt(ps->defined_at,ss.str());
    } else if (bp_err == CL_SUCCESS && os.verbosity >= 2) {
      os.debug() << getLabeledBuildLog(ps->defined_at, po.program, dev);
    } else if (bp_err != CL_SUCCESS) {
      fatalAt(
        ps->defined_at, "failed to biuld program ", status_to_symbol(bp_err));
    }
    if (os.save_binaries) {
      std::string bin_ext;
      if (vend == cl_vendor::CL_NVIDIA)
        bin_ext = ".ptx";
      else // INTC AMD are usually ELF format
        bin_ext = ".bin";
      auto bin_path =
        fs::path(".") / fs::path(ps->path).filename().replace_extension(bin_ext);

      size_t bits_len = 0;
      CL_COMMAND(ps->defined_at,
        clGetProgramInfo,
          po.program,
          CL_PROGRAM_BINARY_SIZES,
          sizeof(bits_len),
          &bits_len,
          nullptr);
      unsigned char *bits = (unsigned char *)alloca(bits_len);
      // unsigned char **bits_array = &bits;
      CL_COMMAND(ps->defined_at,
        clGetProgramInfo,
          po.program, CL_PROGRAM_BINARIES, bits_len, &bits, nullptr);
      // auto bin = po.program->getInfo<CL_PROGRAM_BINARIES>().front();
      os.verbose() << "dumping binary " << bin_path << "\n";
      // sys::write_bin_file(bin_path.string(),bin.data(),bin.size());
      sys::write_bin_file(bin_path.string(),bits,bits_len);
    }
  } // endif text

  ////////////////////////////////////////////////
  // kernel info
  size_t bytes_per_addr =
    po.device->device.getInfo<CL_DEVICE_ADDRESS_BITS>() / 8;
  if (os.use_kernel_arg_info) {
    // use clGetKernelArgInfo* for types etc...
    po.program_info = parseProgramInfoFromAPI(
      os,
      this,
      ps->defined_at,
      po.program,
      po.device->device(),
      bytes_per_addr);
  } else {
    // use kargs library
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
  cl::Device dev;
  switch (ds->skind) {
  case device_spec::BY_DEFAULT:
    dev = getDeviceDefault(os);
    break;
  case device_spec::BY_INDEX:
    if (!getDeviceByIndex(os, ds->by_index_value, dev)) {
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

  auto dev_key = device_key(dev(),ds->instance);
  auto itr = csi->devices.find(dev_key);
  if (itr != csi->devices.find_end()) {
    return *itr->second;
  }

  device_object &dobj = csi->devices.emplace_back(dev_key, ds, dev);
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
    if (st->skind == statement_spec::DISPATCH) {
      compileDispatch((const dispatch_spec *)st);
    }
  }

  // Loop through all the statements
  //  - compile dispatch arguments
  //  - compile diff command (needs args for surface info type)
  for (size_t st_ix = 0; st_ix < s.statement_list.statements.size(); st_ix++) {
    const statement_spec *st = s.statement_list.statements[st_ix];
    if (st->skind == statement_spec::DISPATCH) {
      dispatch_command *dc = compileDispatchArgs((const dispatch_spec *)st);
      csi->instructions.emplace_back(dc);
    } else if (st->skind == statement_spec::DIFF) {
      const diff_spec *ds = (const diff_spec *)st;
      if (ds->ref.value->is_atom()) {
        diffu_command *dc = compileDiffU(ds);
        csi->instructions.emplace_back(dc);
      } else {
        diffs_command *dc = compileDiffS(ds);
        csi->instructions.emplace_back(dc);
      }
    } else if (st->skind == statement_spec::PRINT) {
      print_command *pc = compilePrint((const print_spec *)st);
      csi->instructions.emplace_back(pc);
    } else if (st->skind == statement_spec::SAVE) {
      const save_spec *ss = (const save_spec *)st;
      // surface_object *so =  &csi->surfaces.get(ss->arg.value);
      save_command *sc = new save_command(ss,nullptr);
      csi->instructions.emplace_back(sc);
    } else if (st->skind == statement_spec::LET) {
      ; // nop
    } else {
      internalAt(st->defined_at, "NOT IMPLEMENTED");
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
    switch (si.skind)
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
        std::stringstream ss;
        ss <<
          "memory object used across cl_context's (c.f. lines " <<
            dobj->spec->defined_at.line << " and " <<
            dc->dobj->spec->defined_at.line << ")";
        fatalAt(so->spec->defined_at, ss.str());
      }
    }
  }
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
  // we patch surface and type information later
  return new diffu_command(ds,nullptr,nullptr);
}
diffs_command *script_compiler::compileDiffS(const diff_spec *ds)
{
  // we patch surface and type information later
  return new diffs_command(ds,nullptr,nullptr,nullptr);
}

print_command *script_compiler::compilePrint(const print_spec *ps)
{
  // we patch surface and type information later
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
