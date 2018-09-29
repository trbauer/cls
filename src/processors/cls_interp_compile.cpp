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
#include <random>
#include <type_traits>

using namespace cls::k;


struct arg_setter {
  const init_spec *spec;
  size_t capacity;
  uint8_t *base, *curr;
  fatal_handler *fh;
  bool owns_memory;
  arg_setter (const init_spec *_spec, fatal_handler *_fh, size_t _len)
    : spec(_spec), fh(_fh), capacity(_len)
  {
    curr = base = new uint8_t[capacity];
    owns_memory = true;
    memset(curr, 0, capacity);
  };
  arg_setter (uint8_t *ptr) : base(ptr) {
    curr = base;
    owns_memory = false;
  }
  ~arg_setter() {
    if (owns_memory)
      delete base;
  }

  const uint8_t *ptr() const {return base;}
  size_t         num_left() const {return capacity - length();}
  size_t         length() const {return (curr - base);}
  uint8_t       *take_ownership() {uint8_t *t = base;base = nullptr;return t;}

  template <typename T>
  void copyIn(const T &t) {
    if (sizeof(t) > num_left) {
      fh->fatalAt(spec->defined_at,"INTERNAL ERROR: overflow setting argument");
    }
    memcpy(&curr, &t, sizeof(t));
    curr += sizeof(t);
  }
};

struct script_compiler : public fatal_handler {
  const script &s;
  const opts &os;
  compiled_script_impl *csi = nullptr;

  script_compiler(
    const opts &_os, const script &_s, compiled_script_impl *_csi)
    : fatal_handler(_s.source), os(_os), s(_s), csi(_csi)
  {
  }
  void compile();
private:
  void compileDispatch(const dispatch_spec *ds);
  kernel_object &compileKernel(const kernel_spec *ks);
  program_object &compileProgram(const program_spec *ps);
  device_object &createDeviceObject(const device_spec *ds);

  void createKernelArgument(
    dispatch_command &dc,
    cl_uint arg_index,
    const init_spec *is,
    const arg_info &ai);
  void createNonPointerKernelArgument(
    dispatch_command &dc,
    cl_uint arg_index,
    const init_spec *is,
    const type &t,
    arg_setter &as);
  void createKernelArgumentNum(
    cl_uint arg_index,
    const init_spec *is,
    const type_num &t,
    arg_setter &as);
  template<typename T>
  void createKernelArgumentIntegral(
    loc arg_loc,
    cl_uint arg_index,
    int64_t val,
    arg_setter &as);
  void createKernelArgumentStruct(
    cl_uint arg_index,
    const init_spec *is,
    const type_struct &ts,
    arg_setter &as);
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
  // a bug in Intel CPU driver means we have to use the C API
  // __attribute__((reqd_work_group_size(X, Y, Z)))
  cl_int err_wgsz = clGetKernelWorkGroupInfo(
    (*ko.kernel)(),
    po->device->device(),
    CL_KERNEL_COMPILE_WORK_GROUP_SIZE,
    sizeof(ko.kernel_info->reqd_word_group_size),
    ko.kernel_info->reqd_word_group_size,
    nullptr);
  if (err_wgsz != CL_SUCCESS) {
    fatalAt(ks->defined_at,
      "clGetKernelWorkGroupInfo<CL_KERNEL_COMPILE_WORK_GROUP_SIZE> "
      "returned ", status_to_symbol(err_wgsz));
  }

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
    const size_t *rqsz = ko.kernel_info->reqd_word_group_size;
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
      kernel_object &ko = *dc.kernel;
      const auto &ais = ko.kernel_info->args;
      if (ais.size() != ds->arguments.size()) {
        fatalAt(ds->defined_at,"wrong number of arguments to kernel");
      }
      size_t ptr_size = ko.program->device->pointer_size;
      for (cl_uint i = 0; i < (cl_uint)ds->arguments.size(); i++) {
        const init_spec *is = ds->arguments[i];
        const arg_info &ai = ais[i];
        createKernelArgument(dc, i, is, ai);
      }
    } else if (st->kind == statement_spec::LET) {
      const let_spec *ls = (const let_spec *)st;
      fatalAt(ls->defined_at,"NOT IMPLEMENTED");
    }
  }
}

/*
template <typename T, bool IS_INT = std::is_integral<T>()>
static T evalBitwiseOp(
  script_compiler &sc,
  dispatch_command &dc,
  const init_spec_bin_expr *be,
  T vl,
  T vr);
template <typename T,true>
static T evalBitwiseOp(
  script_compiler &sc,
  dispatch_command &dc,
  const init_spec_bin_expr *be,
  T vl,
  T vr)
{
  switch (be->kind) {
  case init_spec_bin_expr::E_OR: return vl | vr;
  case init_spec_bin_expr::E_XOR: return vl ^ vr;
  case init_spec_bin_expr::E_AND: return vl & vr;
  case init_spec_bin_expr::E_LSH:
    if (vr > 8*sizeof(T))
      sc.warningAt(dim->defined_at,"shift amount exceeds type size");
    return vl << vr;
  case init_spec_bin_expr::E_RSH:
    if (vr > 8*sizeof(T))
      sc.warningAt(dim->defined_at,"shift amount exceeds type size");
    return vl >> vr;
  default: return (T)0;
  }
}
template <typename T,false>
static T evalBitwiseOp(
  script_compiler &sc,
  dispatch_command &dc,
  const init_spec_bin_expr *be,
  T vl,
  T vr)
{
  sc.fatalAt(be->defined_at,"floating point not permitted on bitwise ops");
}

template <typename T,bool IS_INT = std::is_integral<T>()>
static T eval_mod(T vl, T vr);
template <typename T,bool IS_INT = true>
static T eval_mod(T vl, T vr) {return vl % vr;}
template <typename T,false>
static T eval_mod(T vl, T vr) {return (T)std::fmod(vl,vr);}

template <typename T,bool IS_INT = std::is_integral<T>()>
static T complement(script_compiler &sc, T v);
template <typename T,true>
static T complement(T v) {return ~v;}
template <typename T,false>
static T complement(T v) {return v;}

template <typename T,bool IS_FLT = std::is_floating_point<T>()>
static T transcendental(
  script_compiler &sc,
  const init_spec_unr_expr *ue,
  T v);
template <typename T,true>
static T transcendental(
  script_compiler &sc,
  const init_spec_unr_expr *ue,
  T v)
{
  switch (ue->kind) {
  case init_spec_unr_expr::E_SIN: return std::sin(v);
  case init_spec_unr_expr::E_COS: return std::cos(v);
  case init_spec_unr_expr::E_TAN: return std::tan(v);
  default: sc.fatalAt(loc,"unsupported unary expression for type"); return v;
  }
}
template <typename T,false>
static T transcendental(
  script_compiler &sc,
  const init_spec_unr_expr *ue,
  T v)
{
  sc.fatalAt(ue->defined_at,"cannot apply function to integral type");
  return v;
}

template <typename T>
static T eval(
  script_compiler &sc,
  dispatch_command &dc,
  const init_spec_atom *e)
{
  switch (e->kind) {
  case init_spec::IS_INT:
    return (T)((const init_spec_int *)e)->value;
  case init_spec::IS_FLT:
    if (!std::is_floating_point<T>()) {
      sc.fatalAt(e->defined_at,"cannot convert float to integer");
    }
    return (T)((const init_spec_float *)e)->value;
  case init_spec::IS_BEX: {
    const init_spec_bin_expr *be = ((const init_spec_bin_expr *)e);
    T vl = eval<T>(sc, dc, be->el), vr = eval<T>(sc, dc, be->er);
    switch (be->kind) {
    case init_spec_bin_expr::E_OR:
    case init_spec_bin_expr::E_XOR:
    case init_spec_bin_expr::E_AND:
    case init_spec_bin_expr::E_OR:
    case init_spec_bin_expr::E_LSH:
    case init_spec_bin_expr::E_RSH:
      return evalBitwiseOp<T>(sc, dc, be, vl, vr);
    case init_spec_bin_expr::E_ADD:
      if (std::is_integral<T>() &&
        vl > 0 && vr > 0 && ((vl + vr) < vl || (vl + vr) < vr))
      {
        sc.warningAt(e->defined_at,"integer overflow");
      }
      return vl + vr;
    case init_spec_bin_expr::E_SUB:
      if (std::is_unsigned<T>() && vl > vr)
        sc.warningAt(e->defined_at,"integer underflow");
      return vl - vr;
    case init_spec_bin_expr::E_MUL:
      if std::is_integral<T>() &&
        (vl > 0 && vr > 0 && ((vl*vr) < vl || (vl*vr) < vr))
      {
        sc.warningAt(e->defined_at,"integer overflow");
      }
      return vl * vr;
    case init_spec_bin_expr::E_DIV:
      if (std::is_integral<T>() && vr == (T)0)
        sc.fatalAt(e->defined_at,"division by 0");
      return vl / vr;
    case init_spec_bin_expr::E_MOD:
      if (std::is_integral<T>() && vr == (T)0)
        sc.fatalAt(e->defined_at,"division by 0");
      return eval_mod<T>(vl,vr);
    default:
      sc.fatalAt(e->defined_at,"unsupported binary expression for type");
    }
    break;
  }
  case init_spec::IS_UEX: {
    const init_spec_unr_expr *ue = ((const init_spec_unr_expr *)e);
    T v = eval<T>(sc,dc,ue->e);
    switch (ue->kind) {
    case init_spec_unr_expr::E_NEG:
      if (std::is_unsigned<T>())
        sc.warningAt(e->defined_at,"negation of unsigned value");
      return -v;
    case init_spec_unr_expr::E_COMPL:
      if (std::is_floating_point<T>())
        sc.fatalAt(e->defined_at,"complement of floating point value");
      return complement<T>(v);
    case init_spec_unr_expr::E_ABS:
      return std::abs(v);
    case init_spec_unr_expr::E_SQT:
      return std::sqrt(v);
    case init_spec_unr_expr::E_SIN:
    case init_spec_unr_expr::E_COS:
    case init_spec_unr_expr::E_TAN:
      return transcendental<T>(sc,ue,v);
    default:
      sc.fatalAt(e->defined_at,"unsupported unary expression for type");
    }
    break;
  }
  case init_spec::IS_BIV: {
    auto computeDim =
      [&](const cl::NDRange &ndr, int dim_ix, int dim_len) {
        if (dim_ix + dim_len > ndr.dimension())
          sc.fatalAt(
            e->defined_at,
            &ndr == &dc.global_size ? "global" : "local",
            " dimension size is out of bounds for this dispatch");
        size_t prod = 1;
        for (int ix = dim_ix; ix < dim_ix + dim_len; i++)
          prod *= ndr.get()[dim_ix];
      };
    switch (((const init_spec_builtin *)e)->kind) {
    case init_spec_builtin::BIV_GX:   computeDim(dc.global_size, 0, 1);
    case init_spec_builtin::BIV_GY:   computeDim(dc.global_size, 1, 1);
    case init_spec_builtin::BIV_GZ:   computeDim(dc.global_size, 2, 1);
    case init_spec_builtin::BIV_GXY:  computeDim(dc.local_size,  0, 2);
    case init_spec_builtin::BIV_GXYZ: computeDim(dc.local_size,  0, 3);
    case init_spec_builtin::BIV_LX:   computeDim(dc.local_size,  0, 1);
    case init_spec_builtin::BIV_LY:   computeDim(dc.local_size,  1, 1);
    case init_spec_builtin::BIV_LZ:   computeDim(dc.local_size,  2, 1);
    case init_spec_builtin::BIV_LXY:  computeDim(dc.local_size,  0, 2);
    case init_spec_builtin::BIV_LXYZ: computeDim(dc.local_size,  0, 3);
    default: sc.fatalAt(e->defined_at,"unsupported expression");
    }
    break;
  }
  default:
    sc.fatalAt(e->defined_at,"unsupported expression");
    break;
  }
  return (T)0;
}
*/
struct val {
  bool is_f;
  bool is_u;
  union {
    int64_t  s64;
    uint64_t u64;
    double   f64;
  };
  val() : val((int64_t)0) {}
  val(int64_t _s64) : s64(_s64), is_f(false), is_u(false) {}
  val(uint64_t _u64) : u64(_u64), is_f(false), is_u(true) {}
  val(double _f64) : f64(_f64), is_f(true), is_u(false) {}
  bool is_float() const {return is_f;}
  bool is_int() const {return !is_f;}
  bool is_unsigned() const {return is_u;}
};

static val eval(
  script_compiler &sc,
  dispatch_command &dc,
  const init_spec_atom *e);

static val evalI(
  script_compiler &sc,
  dispatch_command &dc,
  const init_spec_atom *e)
{
  val v = eval(sc, dc, e);
  if (!v.is_int())
    sc.fatalAt(e->defined_at,"argument must be integral");
  return v;
}
static val evalF(
  script_compiler &sc,
  dispatch_command &dc,
  const init_spec_atom *e)
{
  val v = eval(sc, dc, e);
  if (!v.is_float())
    sc.fatalAt(e->defined_at,"argument must be floating point");
  return v;
}

static val eval(
  script_compiler &sc,
  dispatch_command &dc,
  const init_spec_atom *e)
{
  switch (e->kind) {
  case init_spec::IS_INT:
    return val(((const init_spec_int *)e)->value);
  case init_spec::IS_FLT:
    return val(((const init_spec_float *)e)->value);
  case init_spec::IS_BEX: {
    const init_spec_bin_expr *be = ((const init_spec_bin_expr *)e);
    switch (be->kind) {
    case init_spec_bin_expr::E_OR:
    case init_spec_bin_expr::E_XOR:
    case init_spec_bin_expr::E_AND:
    case init_spec_bin_expr::E_LSH:
    case init_spec_bin_expr::E_RSH: {
      val
        vl = evalI(sc, dc, be->el),
        vr = evalI(sc, dc, be->er);
      switch (be->kind) {
      case init_spec_bin_expr::E_OR:   vl.s64 |= vr.s64; break;
      case init_spec_bin_expr::E_XOR:  vl.s64 ^= vr.s64; break;
      case init_spec_bin_expr::E_AND:  vl.s64 &= vr.s64; break;
      case init_spec_bin_expr::E_LSH:
        if (vr.s64 > 63)
          sc.warningAt(be->er->defined_at,"shift amount too large");
        vl.s64 <<= vr.s64;
        break;
      case init_spec_bin_expr::E_RSH:
        if (vr.s64 > 63)
          sc.warningAt(be->er->defined_at,"shift amount too large");
        vl.s64 >>= vr.s64; break;
      }
      return vl;
    } // end bitwise ops
    case init_spec_bin_expr::E_POW:
    case init_spec_bin_expr::E_MAX:
    case init_spec_bin_expr::E_MIN:
    case init_spec_bin_expr::E_GCD:
    case init_spec_bin_expr::E_LCM:
    case init_spec_bin_expr::E_ADD:
    case init_spec_bin_expr::E_SUB:
    case init_spec_bin_expr::E_MUL:
    case init_spec_bin_expr::E_DIV:
    case init_spec_bin_expr::E_MOD: {
      val vl = eval(sc, dc, be->el);
      val vr = eval(sc, dc, be->er);
      if (vl.is_float() || vr.is_float()) {
        if (!vl.is_float())
          vl.f64 = (double)vl.s64;
        if (!vr.is_float())
          vr.f64 = (double)vr.s64;
        switch (be->kind) {
        case init_spec_bin_expr::E_POW: vl.f64 = std::pow(vl.f64,vr.f64); break;
        case init_spec_bin_expr::E_MAX: vl.f64 = std::max(vl.f64,vr.f64); break;
        case init_spec_bin_expr::E_MIN: vl.f64 = std::min(vl.f64,vr.f64); break;
        case init_spec_bin_expr::E_LCM:
        case init_spec_bin_expr::E_GCD:
          sc.fatalAt(e->defined_at,"function requires integer arguments");
        case init_spec_bin_expr::E_ADD: vl.f64 += vr.f64; break;
        case init_spec_bin_expr::E_SUB: vl.f64 -= vr.f64; break;
        case init_spec_bin_expr::E_MUL: vl.f64 *= vr.f64; break;
        case init_spec_bin_expr::E_DIV: vl.f64 /= vr.f64; break;
        case init_spec_bin_expr::E_MOD: vl.f64 = std::fmod(vl.f64,vr.f64); break;
        default: sc.fatalAt(e->defined_at,"unsupported binary expression");
        }
      } else { // integer
        switch (be->kind) {
        case init_spec_bin_expr::E_POW:
          sc.fatalAt(e->defined_at,"function requires floating-point arguments"); break;
        case init_spec_bin_expr::E_MAX: vl.s64 = std::max(vl.s64,vr.s64); break;
        case init_spec_bin_expr::E_MIN: vl.s64 = std::min(vl.s64,vr.s64); break;
        case init_spec_bin_expr::E_GCD: vl.s64 = std::gcd(vl.s64,vr.s64); break;
        case init_spec_bin_expr::E_LCM: vl.s64 = std::lcm(vl.s64,vr.s64); break;

        case init_spec_bin_expr::E_ADD: vl.s64 += vr.s64; break;
        case init_spec_bin_expr::E_SUB: vl.s64 -= vr.s64; break;
        case init_spec_bin_expr::E_MUL: vl.s64 *= vr.s64; break;
        case init_spec_bin_expr::E_DIV:
        case init_spec_bin_expr::E_MOD:
          if (vl.s64 == 0)
            sc.fatalAt(e->defined_at,"division by zero");
          if (be->kind == init_spec_bin_expr::E_DIV)
            vl.s64 /= vr.s64;
          else
            vl.s64 %= vl.s64;
          break;
        default: sc.fatalAt(e->defined_at,"unsupported binary expression");
        }
      } // else integer
      return vl;
    } // end arithmetic/transcendental functions
    default: sc.fatalAt(e->defined_at,"unsupported binary expression");
    } // end binary expression switch
  } // binary expression
  case init_spec::IS_UEX: {
    const init_spec_unr_expr *ue = ((const init_spec_unr_expr *)e);
    val v = eval(sc,dc,ue->e);
    switch (ue->kind) {
    case init_spec_unr_expr::E_NEG:
      if (v.is_float())
        v.f64 = -v.f64;
      else
        v.s64 = -v.s64;
      break;
    case init_spec_unr_expr::E_COMPL:
      if (v.is_float())
        sc.fatalAt(e->defined_at,"complement of floating point value");
      else
        v.s64 = ~v.s64;
      break;
    case init_spec_unr_expr::E_ABS:
      if (v.is_float())
        v.f64 = std::abs(v.f64);
      else
        v.s64 = std::abs(v.s64);
      break;
    case init_spec_unr_expr::E_SQT:
    case init_spec_unr_expr::E_EXP:
    case init_spec_unr_expr::E_LOG:
    case init_spec_unr_expr::E_LOG2:
    case init_spec_unr_expr::E_LOG10:
    case init_spec_unr_expr::E_SIN:
    case init_spec_unr_expr::E_COS:
    case init_spec_unr_expr::E_TAN:
    case init_spec_unr_expr::E_ATAN:
      if (!v.is_float())
        sc.fatalAt(e->defined_at,"floating-point argument required");
      switch (ue->kind) {
      case init_spec_unr_expr::E_SQT:   v.f64 = std::sqrt(v.f64);  break;
      case init_spec_unr_expr::E_EXP:   v.f64 = std::exp(v.f64);   break;
      case init_spec_unr_expr::E_LOG:   v.f64 = std::log(v.f64);   break;
      case init_spec_unr_expr::E_LOG2:  v.f64 = std::log2(v.f64);  break;
      case init_spec_unr_expr::E_LOG10: v.f64 = std::log10(v.f64); break;
      case init_spec_unr_expr::E_SIN:   v.f64 = std::sin(v.f64);   break;
      case init_spec_unr_expr::E_COS:   v.f64 = std::cos(v.f64);   break;
      case init_spec_unr_expr::E_TAN:   v.f64 = std::tan(v.f64);   break;
      case init_spec_unr_expr::E_ATAN:  v.f64 = std::atan(v.f64);  break;
      }
      break;
    default:
      sc.fatalAt(e->defined_at,"unsupported unary expression for type");
    }
    break;
  }
  case init_spec::IS_BIV: {
    auto computeDim =
      [&](const cl::NDRange &ndr, int dim_ix, int dim_len) {
        if (dim_ix + dim_len > ndr.dimensions())
          sc.fatalAt(
            e->defined_at,
            &ndr == &dc.global_size ? "global" : "local",
            " dimension size is out of bounds for this dispatch");
        size_t prod = 1;
        for (int ix = dim_ix; ix < dim_ix + dim_len; ix++)
          prod *= ndr.get()[ix];
        return val(prod);
      };
    switch (((const init_spec_builtin *)e)->kind) {
    case init_spec_builtin::BIV_GX:   return computeDim(dc.global_size, 0, 1);
    case init_spec_builtin::BIV_GY:   return computeDim(dc.global_size, 1, 1);
    case init_spec_builtin::BIV_GZ:   return computeDim(dc.global_size, 2, 1);
    case init_spec_builtin::BIV_GXY:  return computeDim(dc.local_size,  0, 2);
    case init_spec_builtin::BIV_GXYZ: return computeDim(dc.local_size,  0, 3);
    case init_spec_builtin::BIV_LX:   return computeDim(dc.local_size,  0, 1);
    case init_spec_builtin::BIV_LY:   return computeDim(dc.local_size,  1, 1);
    case init_spec_builtin::BIV_LZ:   return computeDim(dc.local_size,  2, 1);
    case init_spec_builtin::BIV_LXY:  return computeDim(dc.local_size,  0, 2);
    case init_spec_builtin::BIV_LXYZ: return computeDim(dc.local_size,  0, 3);
    default: sc.fatalAt(e->defined_at,"unsupported expression");
    }
    break;
  }
  default:
    sc.fatalAt(e->defined_at,"unsupported expression");
    break;
  }
  return val((uint64_t)0); // unreachable
}

static size_t computeBufferSize(
  script_compiler &sc,
  dispatch_command &dc,
  const type *elem_type,
  const init_spec_memory *ism)
{
  if (ism->dimension) {
    return evalI(sc, dc,ism->dimension).u64;
  } else {
    return dc.global_items() * elem_type->size(dc.pointer_size());
  }
}

void script_compiler::createKernelArgument(
  dispatch_command &dc,
  cl_uint arg_index,
  const init_spec *is,
  const arg_info &ai)
{
  if (std::holds_alternative<type_ptr>(ai.type.var)) {
    if (is->kind != init_spec::IS_MEM) {
      fatalAt(is->defined_at, "pointer argument requires memory initializer");
      // UNLESS it's local, then it must be integral
    }
    const init_spec_memory *ism = (const init_spec_memory *)is;
    const type *elem_type = std::get<type_ptr>(ai.type.var).element_type;
    size_t buffer_size = computeBufferSize(*this,dc,elem_type,ism);

    surface_object *so;
    auto itr = csi->surfaces.find(ism);
    if (itr != csi->surfaces.find_end()) {
      // ensure we fit with this object
       so = itr->second;
      if (so->size_in_bytes != buffer_size) {
        fatalAt(
          is->defined_at,
          "buffer/image size differs from uses "
          "(see line ",so->spec->defined_at.line,")");
      }
    } else {
      // creating a new surface
      bool is_r =
        (ism->access_properties & init_spec_memory::INIT_SPEC_MEM_READ);
      bool is_w =
        (ism->access_properties & init_spec_memory::INIT_SPEC_MEM_WRITE);
      cl_mem_flags cl_mfs = 0;
      if (is_r && is_w) {
        cl_mfs |= CL_MEM_READ_WRITE;
      } else if (is_r) {
        cl_mfs |= CL_MEM_READ_ONLY;
      } else if (is_w) {
        cl_mfs |= CL_MEM_WRITE_ONLY;
      }

      cl_int clcb_err = 0;
      cl_mem memobj = clCreateBuffer(
        (*dc.kernel->program->device->context)(),
          cl_mfs,
          buffer_size,
          nullptr,
          &clcb_err);
      if (clcb_err != CL_SUCCESS) {
        fatalAt(is->defined_at,"failed to set argument (",
          status_to_symbol(clcb_err),")");
      }
      so = &csi->surfaces.emplace_back(ism,
        ism,
        surface_object::SO_BUFFER,
        buffer_size,
        memobj);

      arg_setter as(is,this,buffer_size);
      fatalAt(is->defined_at,__FUNCTION__,
        "TODO: initializer initializer buffer");

    }
    auto clska_err = clSetKernelArg(
      (*dc.kernel->kernel)(),
      arg_index,
      sizeof(cl_mem),
      so->memobj);
    if (clska_err != CL_SUCCESS) {
      fatalAt(is->defined_at,"failed to set argument (",
        status_to_symbol(clska_err),")");
    }
  } else {
    arg_setter as(is,this,ai.type.size(dc.pointer_size());
    createNonPointerKernelArgument(dc, arg_index, is, ai.type);
    if (as.num_left() != 0) {
      fatalAt(is->defined_at,"INTERNAL ERROR: failed to set full argument");
    }
    auto err = clSetKernelArg(
      (*dc.kernel->kernel)(),
      arg_index,
      ai.type.size(dc.pointer_size()),
      (const void *)as.ptr());
    if (err != CL_SUCCESS) {
      fatalAt(is->defined_at,"failed to set argument (",
        status_to_symbol(err),")");
    }
  }
}

// we match formal type over actual type
void script_compiler::createNonPointerKernelArgument(
  dispatch_command &dc,
  cl_uint arg_index,
  const init_spec *is,
  const type &t,
  arg_setter &as)
{
  if (std::holds_alternative<type_num>(t.var)) {
    createKernelArgumentNum(arg_index, is, std::get<type_num>(t.var), as);
  } else if (std::holds_alternative<type_struct>(t.var)) {
    createKernelArgumentStruct(
      arg_index, is, std::get<type_struct>(t.var), as);
  // else if (enum) {
  // else if (built-in) {
  } else {
    fatalAt(is->defined_at,"unsupported kernel argument type");
  }
}

void script_compiler::createKernelArgumentNum(
  cl_uint arg_index,
  const init_spec *is,
  const type_num &tn,
  arg_setter &as)
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
          createKernelArgumentIntegral<uint8_t>(
            is->defined_at,arg_index,val,as);
          break;
        case 2:
          createKernelArgumentIntegral<uint16_t>(
            is->defined_at,arg_index,val,as);
          break;
        case 4:
          createKernelArgumentIntegral<uint32_t>(
            is->defined_at,arg_index,val,as);
          break;
        case 8:
          createKernelArgumentIntegral<uint64_t>(
            is->defined_at,arg_index,val,as);
          break;
        default:
          fatalAt(is->defined_at,"INTERNAL ERROR: unexpected primitive size");
        }
      } else { // type_num::SIGNED
        switch (tn.size_in_bytes) {
        case 1:
          createKernelArgumentIntegral<int8_t>(
            is->defined_at,arg_index,val,as);
          break;
        case 2:
          createKernelArgumentIntegral<int16_t>(
            is->defined_at,arg_index,val,as);
          break;
        case 4:
          createKernelArgumentIntegral<int32_t>(
            is->defined_at,arg_index,val,as);
          break;
        case 8:
          createKernelArgumentIntegral<int64_t>(
            is->defined_at,arg_index,val,as);
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
      as.copyIn(float_to_half((float)f64));
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
      as.copyIn((float)f64);
      break;
    }
    case 8:
      as.copyIn(f64);
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
  arg_setter &as)
{
  if (is->kind == init_spec::IS_REC) {
    const init_spec_record *isr = (const init_spec_record *)is;
    if (isr->children.size() != ts.elements_length) {
      fatalAt(is->defined_at,
        "structure initializer has wrong number of elements");
    }
    for (size_t i = 0; i < ts.elements_length; i++) {
      createNonPointerKernelArgument(
        arg_index,
        isr->children[i],
        *ts.elements[i],
        as);
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
  arg_setter &as)
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
