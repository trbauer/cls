#include <chrono>
#include <cstdlib>
#include <cstring>
#include <functional>
#include <fstream>
#include <streambuf>
#include <string>
#include <sstream>

#include "cls.hpp"
#include "ir/cls_ir.hpp"
#include "parser/cls_parser.hpp"
#include "processors/cls_interp.hpp"
#include "devices.hpp"
#include "opts.hpp"
#include "stats.hpp"
#include "system.hpp"
#include "text.hpp"

#if CLS_HOST_POINTER_SIZE == 32
#define EXE_NAME "cls32"
#elif CLS_HOST_POINTER_SIZE == 64
#define EXE_NAME "cls64"
#else
#error "invalid value for CLS_HOST_POINTER_SIZE"
#endif

enum class units
{
  UNITS_US = 0,
  UNITS_MS,
  UNITS_S,
};
static void runOnDevice(
  struct cls::Opts &opts,
  const cl::Device &dev,
  sampler &wall_time,
  sampler &prof_time);

static void runFile(
  struct cls::Opts &opts,
  std::string file_name,
  std::string file_contents);


static bool readDecInt(const char *str, int &val)
{
  char *end = nullptr;
  val = (int)strtol(str, &end, 10);
  if (end != str + strlen(str)) {
    return false;
  }
  return true;
};

int main(int argc, const char **argv)
{
  cls::Opts opts;
  opts::CmdlineSpec<cls::Opts> cmdspec(
    "CL Script",
    EXE_NAME,
    EXE_NAME " matrix.cl`naive<1024x1024>(0:w,1:r,1:r)\n"
  );
  cmdspec.defineArg(
    "EXPR","a cls expression", "", opts::NONE,
    [] (const char *value, const opts::ErrorHandler &eh, cls::Opts &opts) {
      opts.input_files.push_back(value);
    });
//  cmdspec.defineOpt(
//    "f","file","FILE","pass a script as a file","",opts::NONE,
//    [](const char *value, const opts::ErrorHandler &eh, cls::Opts &opts) {
//      opts.input = sys::read_file_text(value);
//    });
  cmdspec.defineOpt(
    "e","expression","EXPR","pass an expression as an argument","",opts::NONE,
    [] (const char *value, const opts::ErrorHandler &eh, cls::Opts &opts) {
      opts.input_expr = value;
    });
  cmdspec.defineOpt(
    "i","iterations","INT","number of samples to execute","",opts::NONE,
    [] (const char *value, const opts::ErrorHandler &eh, cls::Opts &opts) {
    if (!readDecInt(value, opts.iterations)) {
      eh("malformed iterations");
    }
  });
  cmdspec.defineFlag(
    "E","save-preprocessed","saves the pre-processed source","",opts::NONE,
    [] (const char *value, const opts::ErrorHandler &eh, cls::Opts &opts) {
      opts.save_preprocessed = true;
    });
  cmdspec.defineFlag(
    "B","save-binaries","saves the binaries","",opts::NONE,
    [] (const char *value, const opts::ErrorHandler &eh, cls::Opts &opts) {
      opts.save_binaries = true;
    });
  cmdspec.defineOpt(
    "l","list-devices","list the devices by index or name","DEVICE",
    "Lists devices by index or name.\n"
    "EXAMPLES:\n"
    " -l         lists all devices on the sytstem\n"
    " -l=0       lists device 0\n"
    " -l=GTX     lists the device with \"GTX\" as a substring of its CL_DEVICE_NAME\n"
    "",
    opts::ALLOW_MULTI|opts::FLAG_VALUE,
    [] (const char *value, const opts::ErrorHandler &eh, cls::Opts &opts) {
      opts.list_devices = true;
      if (*value == 0)
        return;
      int dev_ix = 0;
      if (readDecInt(value, dev_ix)) {
        opts.list_devices_specific.push_back(getDeviceByIndex(opts, dev_ix));
      } else {
        opts.list_devices_specific.push_back(getDeviceByName(opts, value));
      }
  });
  auto &g = cmdspec.defineGroup("t", "profiling options");
  g.defineFlag("W",nullptr,"profiles with wall timers", "", opts::NONE,
    [](const char *value, const opts::ErrorHandler &eh, cls::Opts &opts) {
    opts.wall_time = true;
  });
  g.defineFlag("CL",nullptr,"profiles with OCL prof timers", "", opts::NONE,
    [](const char *value, const opts::ErrorHandler &eh, cls::Opts &opts) {
    opts.prof_time = true;
  });
  cmdspec.defineOpt(
    "v","verbosity","INT","sets the output level","",opts::FLAG_VALUE,
    [](const char *value, const opts::ErrorHandler &eh, cls::Opts &opts) {
      if (*value == 0) { // -v
        opts.verbosity = 1;
      } else if (!readDecInt(value, opts.verbosity)) {
        eh("malformed verbosity value");
      }
      sys::desired_message_verbosity = opts.verbosity;
    });

  if (!cmdspec.parse(argc, argv, opts)) {
    exit(EXIT_FAILURE);
  }

  if (opts.list_devices) {
    listDeviceInfo(opts);
    return EXIT_SUCCESS;
  }
  if (opts.input_expr.size() == 0 && opts.input_files.empty()) {
    std::cerr << "expected input -e or file arguments\n";
    return EXIT_FAILURE;
  }
  if (opts.input_expr.size() > 0) {
    runFile(opts,"<interactive>",opts.input_expr);
  }
  for (std::string file : opts.input_files) {
    if (!sys::file_exists(file)) {
      std::cerr << file << ": file not found\n";
      return EXIT_FAILURE;
    }
    auto file_text = sys::read_file_text(file);
    runFile(opts,file,file_text);
  }
}

static void runFile(
  struct cls::Opts &os,
  std::string file_name,
  std::string file_contents)
{
  cls::script s;
  try {
    os.verbose() << "parsing script\n";
    s = cls::parse_script(os, file_contents, file_name);
    s.str(std::cout);
    std::cout << "\n";
  } catch (cls::diagnostic &d) {
    d.str(std::cerr);
    exit(EXIT_FAILURE);
  }

  cls::compiled_script cs;
  try {
    os.verbose() << "compiling script\n";
    cs = cls::compile(os, s);
  } catch (const cls::diagnostic &d) {
    d.str(std::cerr);
    exit(EXIT_FAILURE);
  }

  sampler setup_times, execute_times;
  for (int i = 0; i < os.iterations; i++) {
    os.verbose() << "starting iteration " << i << "\n";
    auto start_setup = std::chrono::high_resolution_clock::now();
    cs.setup(os,i);
    auto duration_setup =
      std::chrono::duration_cast<std::chrono::microseconds>(
        std::chrono::high_resolution_clock::now() - start_setup);

    auto start_execute = std::chrono::high_resolution_clock::now();
    cs.execute(os,i);
    auto duration_exec =
      std::chrono::duration_cast<std::chrono::microseconds>(
        std::chrono::high_resolution_clock::now() - start_execute);
    execute_times.add(duration_exec.count()/1000.0/1000.0);
  }

  text::table t;
  auto &ckl_col = t.define_col("CLOCK", false, 1);
  auto &med_col = t.define_col("med", true, 1);
  auto &avg_col = t.define_col("avg", true, 1);
  t.define_spacer("+-");
  auto &cfv_col = t.define_col("cfv%", true, 1);
  auto &min_col = t.define_col("min", true, 1);
  t.define_spacer("/");
  auto &max_col = t.define_col("max", true, 1);

  auto emitStats = [&](
    const std::string &clk,
    const sampler &s)
  {
    ckl_col.emit(clk);
    med_col.emitFloating(s.med(), 5);
    avg_col.emitFloating(s.avg(), 5);
    cfv_col.emitFloating(100*s.cfv(), 1);
    min_col.emitFloating(s.min(), 5);
    max_col.emitFloating(s.max(), 5);
  };
  if (os.verbosity > 0) {
    emitStats("Setup",setup_times);
  }
  emitStats("Execute",execute_times);
  std::cout << "=================================================\n";
  auto dispatch_times = cs.get_times();
  for (const auto &p_ds : dispatch_times) {
    const cls::dispatch_spec &ds = *std::get<0>(p_ds);
    const sampler &ts = std::get<1>(p_ds);
    std::string str = text::str_extract(ds); // call ds.str(ss); return ss.str();
    emitStats(str,ts);
  }
}


#if 0
... main () {
  ...
  text::table t;
  auto &dev_col = t.define_col("DEVICE", false);
  auto &ckl_col = t.define_col("CLOCK", false, 1);
  auto &med_col = t.define_col("med", true, 1);
  auto &avg_col = t.define_col("avg", true, 1);
  t.define_spacer("+-");
  auto &cfv_col = t.define_col("cfv%", true, 1);
  auto &min_col = t.define_col("min", true, 1);
  t.define_spacer("/");
  auto &max_col = t.define_col("max", true, 1);
  // TODO: -q suppresses header
  // --csv creates CSV
  auto emitStats = [&](
    const std::string &dev,
    const std::string &clk,
    sampler &s)
  {
    dev_col.emit(dev);
    ckl_col.emit(clk);
    med_col.emitFloating(s.med(), 5);
    avg_col.emitFloating(s.avg(), 5);
    cfv_col.emitFloating(100*s.cfv(), 1);
    min_col.emitFloating(s.min(), 5);
    max_col.emitFloating(s.max(), 5);
  };

  for (auto &d : opts.list_devices_specific) {
    sampler wall_times, prof_times;
    runOnDevice(opts, d, wall_times, prof_times);
    auto dev_name = d.getInfo<CL_DEVICE_NAME>();
    if (opts.wall_time)
      emitStats(dev_name, "Wall", wall_times);
    if (opts.prof_time)
      emitStats(dev_name, "Prof", prof_times);
  }

  if (opts.prof_time || opts.prof_time) {
    t.str(std::cout);
  }

  return 0;
}

void CL_CALLBACK ContextErrorCallback(
    const char *errinfo,
    const void *priv_info,
    size_t cb,
    void *env)
{
  std::cerr << "[from context] " << errinfo << "\n";
}

static cl_command_queue createCommandQueue(
  struct cls::Opts &opts,
  cl_context ctx,
  const cl::Device &dev)
{
  cl_command_queue_properties props = 0;
  if (opts.prof_time) {
    props |= CL_QUEUE_PROFILING_ENABLE;
  }

  cl_int err;
  cl_command_queue q;
  /*
  TODO: fix by linking against a 2.0 OpenCL library;
        the ICD loader will do the right thing
  OR
   I could include cl via a separate path?

  if (getDeviceSpec(dev) >= cl_spec::CL_2_0) {
    cl_command_queue_properties props_arr[4];
    props_arr[0] = CL_QUEUE_PROPERTIES;
    props_arr[1] = props;
    props_arr[2] = props_arr[3] = 0;

    q = clCreateCommandQueueWithProperties(ctx, dev(), props_arr, &err);
  } else {
    q = clCreateCommandQueue(ctx, dev(), props, &err);
  }
  */
  q = clCreateCommandQueue(ctx, dev(), props, &err);

  if (err != CL_SUCCESS) {
    FATAL("failed to create command queue on device");
  }
  return q;
}

static void enqueueKernel(
  cls::ndr &call,
  struct cls::Opts &opts,
  cl::CommandQueue &cq,
  cl::Event &evt)
{
  if (true) {
    cq.enqueueNDRangeKernel(
      *call.entry->cl_kernel,
      cl::NullRange,
      call.global_size,
      call.local_size,
      nullptr, // evt lists
      &evt);
  } else {
    FATAL("call metrics API here");
  }
}


static void runOnDevice(
  struct cls::Opts &opts,
  const cl::Device &dev,
  sampler &wall_time,
  sampler &prof_time)
{
  sampler s;
  cls::ndr *c = nullptr;
  try {
    cl::Context context(dev, nullptr, ContextErrorCallback);
    cl::CommandQueue cq(createCommandQueue(opts, context(), dev));

    try {
      c = cls::ParseCLS(context, dev, opts.input_expr);
      if (opts.verbosity > 0) {
        c->str(std::cout);
      }
    } catch (const cls::diag &d) {
      FATAL("%s",d.toString().c_str());
    }

    for (int i = 0; i < opts.iterations; i++) {
      c->initBuffers(cq);

      // might need to call the other function here
      cl::Event evt;
      auto start = std::chrono::high_resolution_clock::now();
      enqueueKernel(*c, opts, cq, evt);
      evt.wait();
      auto duration =
        std::chrono::duration_cast<std::chrono::microseconds>(
          std::chrono::high_resolution_clock::now() - start);
      if (opts.prof_time) {
        auto ptime = evt.getProfilingInfo<CL_PROFILING_COMMAND_END>() -
                     evt.getProfilingInfo<CL_PROFILING_COMMAND_START>();
        prof_time.add(ptime/1000000000.0);
      }
      wall_time.add(duration.count()/1000.0/1000.0);

      c->readDevMem(cq);
    } // for args
  } catch (const cl::Error &e) {
    std::stringstream ss;
    ss << "INTERNAL ERROR: " << e.what() << ": " << e.err() <<
      " (" << cls::status_to_symbol(e.err()) << ")";
    FATAL("%s\n",ss.str().c_str());
  } catch (const cls::diag &d) {
    std::cerr << d.toString() << "\n";
    exit(EXIT_FAILURE);
  }
  if (c) {
    delete c;
  }
}
#endif
