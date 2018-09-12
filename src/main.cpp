#include <chrono>
#include <cstdlib>
#include <cstring>
#include <functional>
#include <fstream>
#include <streambuf>
#include <string>
#include <sstream>

#include "cls.hpp"
#include "devices.hpp"
#include "opts.hpp"
#include "stats.hpp"
#include "system.hpp"
#include "text.hpp"

#ifdef _WIN64
#define EXE_NAME "cls64"
#else
#define EXE_NAME "cls32"
#endif

enum class units
{
  UNITS_US = 0,
  UNITS_MS,
  UNITS_S,
};


bool readDecInt(const char *str, int &val)
{
  char *end = nullptr;
  val = (int)strtol(str, &end, 10);
  if (end != str + strlen(str)) {
    return false;
  }
  return true;
}

static void runOnDevice(
  struct cls::Opts &opts,
  const cl::Device &dev,
  sampler &wall_time,
  sampler &prof_time);

int main(int argc, const char **argv)
{
  cls::Opts opts;
  opts::CmdlineSpec<cls::Opts> cmdspec(
    "CL Script",
    EXE_NAME,
    EXE_NAME " matrix.cl`naive<1024x1024>(0:w,1:r,1:r)\n");
  cmdspec.defineArg(
    "EXPR","a cls expression", "", opts::NONE,
    [](const char *value, const opts::ErrorHandler &eh, cls::Opts &opts) {
      opts.input = value;
    });
  cmdspec.defineOpt(
    "f","file","FILE","pass a script as a file","",opts::NONE,
    [](const char *value, const opts::ErrorHandler &eh, cls::Opts &opts) {
      opts.input = sys::read_file_text(value);
    });
  cmdspec.defineOpt(
    "d","device","DEV","a device name or index","",opts::ALLOW_MULTI,
    [](const char *value, const opts::ErrorHandler &eh, cls::Opts &opts) {
      // optional gpu or cpu prefix
      cl_device_type dt = CL_DEVICE_TYPE_ALL;
      if (strncmp(value,"gpu:",strlen("gpu:")) == 0) {
        dt = CL_DEVICE_TYPE_GPU;
        value += strlen("gpu:");
      } else if (strncmp(value,"cpu:",strlen("cpu:")) == 0) {
        dt = CL_DEVICE_TYPE_CPU;
        value += strlen("cpu:");
      }
      int dev_ix = 0;
      if (readDecInt(value, dev_ix)) {
        opts.devices.push_back(getDeviceByIndex(opts, dt, dev_ix));
      } else {
        opts.devices.push_back(getDeviceByName(opts, dt, value));
      }
    });
  cmdspec.defineOpt(
    "i","iterations","INT","number of samples to execute","",opts::NONE,
    [](const char *value, const opts::ErrorHandler &eh, cls::Opts &opts) {
    if (!readDecInt(value, opts.iterations)) {
      eh("malformed iterations");
    }
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
  cmdspec.defineFlag(
    "L","list-devices","list the devices by index", "", opts::NONE,
    [](const char *value, const opts::ErrorHandler &eh, cls::Opts &opts) {
      opts.list_devices = true;
    });
  auto &g = cmdspec.defineGroup("p", "profiling options");
  g.defineFlag("W",nullptr,"profiles with wall timers", "", opts::NONE,
    [](const char *value, const opts::ErrorHandler &eh, cls::Opts &opts) {
    opts.wall_time = true;
  });
  g.defineFlag("CL",nullptr,"profiles with OCL prof timers", "", opts::NONE,
    [](const char *value, const opts::ErrorHandler &eh, cls::Opts &opts) {
    opts.prof_time = true;
  });

  if (!cmdspec.parse(argc, argv, opts)) {
    exit(EXIT_FAILURE);
  }

  if (opts.list_devices) {
    listDeviceInfo(opts);
    return EXIT_SUCCESS;
  }

  if (opts.devices.empty()) {
    auto plat = cl::Platform::getDefault();
    std::vector<cl::Device> ds;
    plat.getDevices(CL_DEVICE_TYPE_DEFAULT, &ds);
    if (ds.empty()) {
      FATAL("no devices on default platform");
    }
    opts.devices.push_back(ds.back());
  }

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

  for (auto &d : opts.devices) {
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
  cl_device_id dev)
{
  cl_command_queue_properties props = 0;
  if (opts.prof_time) {
    props |= CL_QUEUE_PROFILING_ENABLE;
  }
  cl_int err;
  auto q = clCreateCommandQueue(ctx, dev, props, &err);
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
    cl::CommandQueue cq(createCommandQueue(opts, context(), dev()));

    // double to_s = 1.0 / sys::TicksFreq();
    try {
      c = cls::ParseCLS(context, dev, opts.input);
        c->str(std::cout);
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
      // auto st = sys::TicksNow();
      auto start = std::chrono::high_resolution_clock::now();
      enqueueKernel(*c, opts, cq, evt);
      evt.wait();
      // auto en = sys::TicksNow();
      auto duration =
        std::chrono::duration_cast<std::chrono::microseconds>(
          std::chrono::high_resolution_clock::now() - start);
      if (opts.prof_time) {
        auto ptime = evt.getProfilingInfo<CL_PROFILING_COMMAND_END>() -
                     evt.getProfilingInfo<CL_PROFILING_COMMAND_START>();
        prof_time.add(ptime / 1000000000.0);
      }
      wall_time.add(duration.count()/1000.0/1000.0);

      c->readDevMem(cq);
    } // for args
  } catch (const cl::Error &e) {
    std::stringstream ss;
    ss << "INTERNAL ERROR: " << e.what() << ": " << e.err() <<
      " (" << cls::ErrorToString(e.err()) << ")";
    FATAL("%s\n",ss.str().c_str());
  } catch (const cls::diag &d) {
    std::cerr << d.toString() << "\n";
    exit(EXIT_FAILURE);
  }
  if (c) {
    delete c;
  }
}
