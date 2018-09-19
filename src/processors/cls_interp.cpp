#include "cls_interp.hpp"
#include "../cl_headers.hpp"
#include "../devices.hpp"
#include "../stats.hpp"
#include "../system.hpp"
#include "../text.hpp"
#include "../parser/kargs.hpp"

#include <experimental/filesystem>
namespace fs = std::experimental::filesystem;
#include <functional>
#include <map>
#include <tuple>


struct device_state;

// interpreter dispatch object
struct dispatch_command {
  dispatch_command(const cls::dispatch_spec *d, device_state *ds)
    : dispatch(d), device_state(ds) { }
  ~dispatch_command() {
    delete kernel; // kernel first
    delete program; // then program
    device_state = nullptr;
    dispatch = nullptr;
  }
  const cls::dispatch_spec *dispatch = nullptr;
  device_state *device_state;
  sampler times;

  // move this to some shared program object
  cl::Program *program = nullptr;
  cl::Kernel *kernel = nullptr;
  // std::vector<karg> args_types;
  cl::NDRange local_size = cl::NullRange;
  cl::NDRange global_size;
};

struct device_state {
  cls::loc defined_at; // first definition of this device
  cl::Device device;
  std::string callback_prefix; // e.g. "[1.2: Intel HD]: "

  device_state(cls::loc at, cl::Device dev)
    : defined_at(at), device(dev)
  {
    std::stringstream ss;
    ss << "[" << defined_at.line << "." << defined_at.column << "]: ";
    callback_prefix = ss.str();
  }
  cl::Context *context = nullptr;
  cl::CommandQueue *queue = nullptr;

  void contextNotify(
    const char *errinfo,
    const void *private_info, size_t cb,
    void *user_data)
  {
    if (errinfo) {
      std::cout << text::prefix_lines(callback_prefix, errinfo);
    }
  }
};
static void CL_CALLBACK dispatchContextNotify(
  const char *errinfo,
  const void *private_info, size_t cb,
  void *user_data)
{
  device_state *ds = (device_state *)user_data;
  ds->contextNotify(errinfo, private_info, cb, user_data);
}

template <typename T>
struct interp_memory_define {
  cls::init_spec_memory                   *spec;
  T                                       *memory;
  // std::vector<dispatch_command*>          read_by, written_by;
  interp_memory_define(cls::init_spec_memory *_spec, T *_memory)
    : spec(_spec), memory(_memory) { }
};
using interp_buffer_define = interp_memory_define<cl::Buffer>;
using interp_image2d_define = interp_memory_define<cl::Image2D>;
// TODO: other image types


namespace std {
  bool operator<(cl::Device d1, cl::Device d2) {return d1() < d2();}
}

struct compiled_script_impl {
  const cls::script                             &s;
  std::map<cl::Device,device_state*>             device_to_state;
  std::map<const cls::spec*,device_state*>       spec_to_device_state; // includes both dispatch_specs and let_specs

  std::vector<dispatch_command>                  dispatch_commands;

  /// OLD
  std::vector<interp_buffer_define>              buffer_defines;
  std::vector<interp_image2d_define>             image2d_defines;

  compiled_script_impl(const cls::script &_s) : s(_s) { }
};

struct script_compiler : public cls::fatal_handler {
  const cls::script &s;
  const cls::Opts &os;
  compiled_script_impl *csi = nullptr;

  script_compiler(
    const cls::Opts &_os, const cls::script &_s, compiled_script_impl *_csi)
    : cls::fatal_handler(*_s.source)
    , os(_os)
    , s(_s)
    , csi(_csi)
  {
  }
  void compile();
private:
  device_state *instantiateDeviceSpec(const cls::device_spec &ds);
  void dispatchCompileProgram(dispatch_command &dc);
  void dispatchCreateKernel(dispatch_command &dc,const cls::program_source &ps);
  void dispatchParseKernelArgs(dispatch_command &dc,const cls::program_source &ps);
};



/*
====
#0`prog1.cl`kernel<...>(...)
let X=#0`prog2.cl`kernel2
#2`prog1.cl
#GTX`prog1.cl...
barrier()
#0`prog1.cl...
print(...)
======
== construct contexts and command queues for each unique device in the system
#0`prog1.cl`kernel<...>(...)
 ^  device_state(0)
let X=#0`prog2.cl`kernel2
      ^  device_state(0)
#2`prog1.cl
#GTX`prog1.cl...
barrier()
#0`prog1.cl...
print(...)
=========
 */
void script_compiler::compile()
{
  // Construct contexts and command queues for all device_spec's that
  // appear in the script; note, we share common ones
  //
  // Compile all programs.  Parse the kernel argument types for
  // argument inference.
  //
  // Initialize any dispatch commands needed
  //
  // Ensure all LET bindings are unique
  std::map<std::string,const cls::let_spec*> lets_by_name;
  for (const cls::statement_spec *st : s.statements) {
    if (st->kind == cls::statement_spec::DISPATCH) {
      const cls::dispatch_spec *ds = (const cls::dispatch_spec *)st;
      device_state *dstt = instantiateDeviceSpec(ds->device);
      csi->spec_to_device_state[ds] = dstt;
      csi->dispatch_commands.emplace_back(ds,dstt);

      dispatchCompileProgram(csi->dispatch_commands.back());

      // TODO: handle WHERE and AS statements when supported
      //       treat them like let's bindings

    } else if (st->kind == cls::statement_spec::LET) {
      const cls::let_spec *ls = (const cls::let_spec *)st;
      auto lbn = lets_by_name.find(ls->identifier);
      if (lbn != lets_by_name.end()) {
        auto orig = lbn->second->defined_at;
        fatalAt(
          ls->defined_at,
          "duplicate identifier binding "
          "(conflicts with definition at ", orig.line, "." , orig.column, ")");
      } else {
        lets_by_name[ls->identifier] = ls;
      }

      // TODO: handle program and kernel let's
      // EXAMPLES:
      //   let P1=#1`prog.cl[-opts]
      //   let P2=#1`prog.cl
      //   let P3=P2[-different-opts]
      //   let K1=#1`prog.cl[-opts]`kernel
    } else {
      // ignore??
    }
  }

  // Ensure all symbolic bindings map to a LET
  //
  // Map all let buffer definitions to their owning device_state
  // Ensures that buffers don't cross device boundaries
  // E.g. let A = 0:rw
  //     #1`...(A)
  //     #2`...(A)
  // is illegal since #1 and #2 have different contexts.
  for (const cls::statement_spec *st : s.statements) {
    if (st->kind == cls::statement_spec::DISPATCH) {
      const cls::dispatch_spec *ds = (const cls::dispatch_spec *)st;
      for (const cls::init_spec *arg : ds->arguments) {
        if (arg->kind == cls::init_spec::IS_SYM) {
          const cls::init_spec_symbol *iss = (const cls::init_spec_symbol *)st;
          if (lets_by_name.find(iss->identifier) == lets_by_name.end()) {
            fatalAt(iss->defined_at, "undefined symbolic argument");
          }
        } else if  (arg->kind == cls::init_spec::IS_MEM) {
          // guaranteed to be a unique buffer
          // e.g. #1`...(0:w)
          //             ^^^ this is safe because it's unbound
          csi->spec_to_device_state[arg] =
            csi->spec_to_device_state[st];
        }
      }
    } else if (st->kind == cls::statement_spec::LET) {
      // e.g. let A = 0:w
      // ensure that all dispatch_spec's that use the let share
      // the same device state
      const cls::let_spec *ls = (const cls::let_spec *)st;
      const device_state *last_dstt = nullptr;
      const cls::dispatch_spec *last_ds = nullptr;
      // TODO:
      std::vector<cls::dispatch_spec> all_uses;

      for (const cls::statement_spec *st : s.statements) {
        if (st->kind == cls::statement_spec::DISPATCH) {
          const cls::dispatch_spec *ds = (const cls::dispatch_spec *)st;
          device_state *ds_stt = csi->spec_to_device_state.find(st)->second;

          for (const cls::init_spec *arg : ds->arguments) {
            if (arg->kind == cls::init_spec::IS_SYM) {
              const cls::init_spec_symbol *iss = (const cls::init_spec_symbol *)arg;
              if (iss->identifier == ls->identifier) {
                // E.g. #1`....(...,A,...)
                if (last_dstt == nullptr) {
                  last_ds = ds;
                  last_dstt = ds_stt;
                } else if (last_dstt != ds_stt) {
                  fatalAt(ls->defined_at,
                    "memory object definition shared across contexts (",
                    last_ds->defined_at.line, ".", last_ds->defined_at.column,
                    " and ",
                    ds->defined_at.line, ".", ds->defined_at.column,
                    ")");
                } // otherwise it's a safe share (same context)
              }
            }
          } // for args
        } // if dispatch
      } // for all other statements (checking let_spec *ls)
      // ls is safe and valid
      //
      // TODO: memory size inference... (construct it)
    } else {
      // some other statement (not a dispatch and not a let)
    }
  }

  /*
  // OLD (see above)
  // Create all objects (buffers, images, etc..)
  //   Buffers and images need to ensure they exist only on the
  //   same devices and need to ensure that their sizes match up
  for (const cls::statement_spec *st : s.statements) {
    if (st->kind == cls::statement_spec::DISPATCH) {
      const cls::dispatch_spec *ds = (const cls::dispatch_spec *)st;
      for (const cls::init_spec *arg : ds->arguments) {

      }
    } else if (st->kind == cls::statement_spec::LET) {
      const cls::let_spec *ls = (const cls::let_spec *)st;
    }
  }
  */

  //
  // Initialize any dispatch commands
  //
  // Create the initializer functions
  //
  // Set kernel arguments for all programs
}

device_state *script_compiler::instantiateDeviceSpec(
  const cls::device_spec &ds)
{
  cl::Device dev;
  switch (ds.kind) {
  case cls::device_spec::BY_DEFAULT:
    dev = getDeviceDefault(os);
    break;
  case cls::device_spec::BY_INDEX:
    if (!getDeviceByIndex(os, ds.by_index_value,dev)) {
      fatalAt(ds.defined_at,"invalid device index");
    }
    break;
  case cls::device_spec::BY_NAME: {
    std::string err_msg;
    if (!getDeviceByName(os, ds.by_name_value, dev, err_msg))
      fatalAt(ds.defined_at, "invalid device specification ", err_msg);
    break;
  }
  default:
    fatalAt(ds.defined_at, "invalid device spec");
    break;
  }
  // dev is set to the cl::Device used by this device_spec statement
  auto dev_st_itr = csi->device_to_state.find(dev);
  device_state *dstt = nullptr;
  if (dev_st_itr != csi->device_to_state.end()) {
    // already exists
    dstt = dev_st_itr->second;
    if (os.verbosity >= 2) {
      auto def_loc = dstt->defined_at;
      cls::formatMessageWithContext(
        std::cout, ds.defined_at, input(),
        "re-using device state for ", dev.getInfo<CL_DEVICE_NAME>().c_str(),
        " (defined at ", def_loc.line, ".", def_loc.column, ")");
    }
  } else {
    dstt = new device_state(ds.defined_at,dev);

    if (os.verbosity >= 2) {
      cls::formatMessageWithContext(
        std::cout,
        ds.defined_at, input(),
        "creating new device state for "
        "[", dev.getInfo<CL_DEVICE_NAME>().c_str(), "]");
    }
    std::vector<cl::Device> devs{dev};
    dstt->context = new cl::Context(
      devs,
      nullptr,
      dispatchContextNotify,
      (void *)dstt);

    cl_command_queue cq;
    auto cl_err = makeCommandQueue(os.prof_time, dev(), (*(dstt->context))(), cq);
    if (cl_err != CL_SUCCESS) {
      fatalAt(
        ds.defined_at,
        "failed to create command queue for device "
        "(", cls::status_to_symbol(cl_err), ")");
    }
    dstt->queue = new cl::CommandQueue(cq);
    csi->device_to_state[dev] = dstt;
  }
  return dstt;
}

static std::string getLabeledBuildLog(cls::loc at, cl::Program &prog) {
  auto logs = prog.getBuildInfo<CL_PROGRAM_BUILD_LOG>();
  std::stringstream ss;
  ss << "[" << at.str() << "]: ";
  return text::prefix_lines(
    ss.str(),
    logs.front().second.c_str());
}

void script_compiler::dispatchCompileProgram(dispatch_command &dc)
{
  auto file = dc.dispatch->program.program_path;
  if (!sys::file_exists(file)) {
    fatalAt(dc.dispatch->program.defined_at,
      "file not found");
  }
  bool is_bin =
    strsfx(".bin",file) ||
    strsfx(".ptx",file) ||
    strsfx(".obj",file);
  bool is_clc =
    strsfx(".cl",file) ||
    strsfx(".clc",file);

  if (!is_bin && !is_clc) {
    // look at the file contents
    auto bs = sys::read_file_binary(file);
    for (uint8_t c : bs) {
      if (!::isprint(c)) {
        is_bin = true;
        break;
      }
    }
    os.verbose() << file << ": unable to infer program type from extension\n"
      << "based on contents assuming " << (is_bin ? "binary" : "text");
  }
  auto fatalHere = [&](const char *do_what, const char *with_what, cl_int err) {
    auto dev_nm = dc.device_state->device.getInfo<CL_DEVICE_NAME>().c_str();
    fatalAt(dc.dispatch->program.defined_at,
      file, ": failed to ", do_what, " with ", with_what,
      " (", cls::status_to_symbol(err), ") on device [",dev_nm,"]");
  };
  std::string build_opts = dc.dispatch->program.build_opts;
  std::string build_opts_with_arg_info = build_opts;
  if (build_opts.empty()) {
    build_opts_with_arg_info = "-cl-kernel-arg-info";
  } else if (build_opts.find("-cl-kernel-arg-info") == std::string::npos) {
    build_opts_with_arg_info += " -cl-kernel-arg-info";
  }

  cls::program_source src;
  src.path = file;
  src.build_opts = build_opts;
  src.is_binary = is_bin;

  if (is_bin) {
    std::vector<cl::Device> devs{dc.device_state->device};
    cl::Program::Binaries bins{sys::read_file_binary(file)};
    try {
      dc.program = new cl::Program(
        *(dc.device_state->context),
        devs,
        bins); // clCreateProgramWithBinary
    } catch (const cl::Error &err) {
      fatalHere("create","binary",err.err());
    }
    try {
      if (!build_opts.empty()) {
        os.warning() << "non-empty options to clBuildProgram with binary program";
      }
      dc.program->build( // clBuildProgram (yep, it's still required for binary)
        build_opts.empty() ?
        nullptr : build_opts.c_str());
    } catch (const cl::Error &err) {
      fatalHere("build","binary",err.err());
    }
  } else {
    // is_clc (text)
    std::string inp = sys::read_file_text(file);
    try {
      dc.program = new cl::Program(*(dc.device_state->context), inp); // clCreateProgramWithSource
    } catch (const cl::Error &err) {
      fatalHere("create","source",err.err());
    }
    try {
      dc.program->build(build_opts_with_arg_info.c_str()); // clBuildProgram
      if (os.verbosity >= 2) {
        os.debug() <<
          getLabeledBuildLog(dc.dispatch->device.defined_at, *dc.program);
      }
      if (os.save_binaries) {
        auto vend = getDeviceVendor(dc.device_state->device);
        std::string bin_ext;
        if (vend == cl_vendor::CL_NVIDIA)
          bin_ext = ".ptx";
        else
          bin_ext = ".bin";
        auto bin_path =
          fs::path(".") / fs::path(file).filename().replace_extension(bin_ext);
        auto bin = dc.program->getInfo<CL_PROGRAM_BINARIES>().front();
        sys::write_bin_file(bin_path.string(),bin.data(),bin.size());
      }
    } catch (const cl::Error &err) {
      if (err.err() == CL_BUILD_PROGRAM_FAILURE) {
        std::stringstream ss;
        ss << "failed to build source:\n";
        ss << getLabeledBuildLog(
          dc.dispatch->device.defined_at, *dc.program) << "\n";
        fatalAt(dc.dispatch->program.defined_at,ss.str());
      } else {
        fatalHere("build","source",err.err());
      }
    } // catch
  }
  dispatchCreateKernel(dc,src);
}

void script_compiler::dispatchCreateKernel(
  dispatch_command &dc,
  const cls::program_source &src)
{
  // create the kernel from the program
  try {
    dc.kernel = new cl::Kernel(
      *dc.program,
      dc.dispatch->kernel.kernel_name.c_str());
  } catch (const cl::Error &err) {
    if (err.err() == CL_INVALID_KERNEL_NAME) {
      std::stringstream ss;
      try {
        std::vector<cl::Kernel> all_kernels;
        dc.program->createKernels(&all_kernels);
        ss << "\n" << "kernels in the program are:\n";
        for (const cl::Kernel &k : all_kernels) {
          ss << " * " << k.getInfo<CL_KERNEL_FUNCTION_NAME>().c_str() << "\n";
        }
      } catch (const cl::Error &) {
        // ignore it (live without the extra ouput)
      }
      fatalAt(dc.dispatch->kernel.defined_at,
        dc.dispatch->kernel.kernel_name, ": unable to find kernel in program",
        ss.str());
    } else {
      fatalAt(dc.dispatch->kernel.defined_at,
        dc.dispatch->kernel.kernel_name,
        " unable to create kernel (", cls::status_to_symbol(err.err()), ")");
    }
  }
  dispatchParseKernelArgs(dc,src);
}

void script_compiler::dispatchParseKernelArgs(
  dispatch_command &dc,
  const cls::program_source &src)
{
  /////////////////////////////////////////////////////////////////////////////
  // parse and bind the arguments
  if (getDeviceSpec(dc.device_state->device) < cl_spec::CL_1_2) {
    fatalAt(dc.dispatch->program.defined_at,
      " manual argument parsing not supported yet!");
  }
  cl_uint num_args = dc.kernel->getInfo<CL_KERNEL_NUM_ARGS>();
  if ((size_t)num_args != dc.dispatch->arguments.size()) {
    fatalAt(dc.dispatch->kernel.defined_at,
      " wrong number of arguments (expected ",num_args,")");
  }
  auto p = cls::k::parseProgramInfo(this, dc.dispatch->program.defined_at, src);

  // TODO: remove
  for (cl_uint i = 0; i < num_args; i++) {
    std::string arg_type;
    try {
      arg_type = dc.kernel->getArgInfo<CL_KERNEL_ARG_TYPE_NAME>(i).c_str();
    } catch (cl::Error err) {
      std::cout << err.what() << ": " << cls::status_to_symbol(err.err());
    }
    std::string arg_name = dc.kernel->getArgInfo<CL_KERNEL_ARG_NAME>(i).c_str();
    std::cout << "ARG[" << i << "]: " << arg_type << "  " << arg_name << "\n";
  }
}

cls::compiled_script cls::compile(const cls::Opts &os, const cls::script &s)
{
  cls::compiled_script cs;
  auto *csi = new compiled_script_impl(s);
  cs.impl = csi;

  script_compiler sc(os,s,csi);
  sc.compile();

  return cs;
}


void cls::compiled_script::setup(const cls::Opts &os, int)
{
  std::cout << "compiled_script::setup\n";
}

void cls::compiled_script::execute(const cls::Opts &os, int)
{
  std::cout << "compiled_script::execute\n";
  compiled_script_impl *csi = (compiled_script_impl *)impl;
  for (const dispatch_command &dc : csi->dispatch_commands) {
    std::cout << "  we would execute " <<
      text::str_extract(*dc.dispatch) << "\n";
  }
}

cls::times cls::compiled_script::get_times() const
{
  cls::times ts;
  const compiled_script_impl *csi = (const compiled_script_impl *)impl;
  for (const dispatch_command &dc : csi->dispatch_commands)
    ts.emplace_back(dc.dispatch,dc.times);
  return ts;
}
