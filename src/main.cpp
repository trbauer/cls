#include <chrono>
#include <cstdlib>
#include <cstring>
#include <functional>
#include <optional>
#include <sstream>
#include <string>
#include <tuple>

#include "cl_lib.hpp"
#include "devices.hpp"
#include "ir/cls_ir.hpp"
#include "opts.hpp"
#include "parser/cls_parser.hpp"
#include "processors/cls_interp.hpp"
#include "stats.hpp"
#include "system.hpp"
#include "text.hpp"
#include "../deps/mdapi/mdapi_wrapper.hpp"

static void run_file(
  struct cls::opts &os,
  std::string file_name,
  std::string file_contents);

// main-format-metrics.cpp
void emit_metrics(const cls::opts &os, const cls::mdapi_ctrs &mdcs);
// main-list-sysinfo.cpp
void list_system_info(const cls::opts &os);
// main-list-device-info.cpp
void list_device_info(const cls::opts &os);

int main(int argc, const char **argv)
{
  cls::opts os;
  opts::CmdlineSpec<cls::opts> cmdspec(
    "CL Script " CLS_VERSION_STRING " (" __DATE__ ")",
    CLS_EXE,
    "  % " CLS_EXE " -e \"matrix.cl`naive<1024x1024>(0:w,1:r,1:r)\"\n"
    "    run a script given as an argument\n"
    "  % " CLS_EXE " script.cls\n"
    "    run the script given in script.cls\n"
    "  % " CLS_EXE " -l=0\n"
    "    lists devices\n"
  );
  cmdspec.defineArg(
    "FILE", "a cls script file",
    "An OpenCL script file.  Use -e to specify an expression "
    "on the command line.  Use -h=syntax for cls syntax.", opts::NONE,
    os.input_files);

  cmdspec.defineOpt(
    "D", nullptr,
    "OPT", "set a preprocessor option (e.g. -DN=4)",
    "Given -DN=4, the parser will expand ...$N... to 4",
    opts::FUSED_VALUE|opts::ALLOW_MULTI,
    [](const char *c_opt, const opts::ErrorHandler &eh, cls::opts &os) {
      std::string opt(c_opt);
      auto ix = opt.find('=');
      if (ix == std::string::npos) {
        eh("expected format -Dkey=val");
      }
      auto key = opt.substr(0, ix);
      if (key.empty())
        eh("expected format -Dkey=val (empty key)");
      auto val = opt.substr(ix + 1);
      for (const auto &iv : os.input_vars) {
        if (iv.first == key)
          eh("input variable redefinition");
      }
      os.input_vars.emplace_back(key, val);
    });
  cmdspec.defineOpt(
    "e", "expression",
      "EXPR", "pass an expression as an argument",
      "Execute the given expression on the command line "
      "(instead of using the script file argument).  "
      "Use -h=syn for a cls syntax cheatsheet.",
      opts::NONE,
    os.input_expr);
  cmdspec.defineOpt(
    "i", "iterations", "INT", "number of samples to execute", "", opts::NONE,
    os.iterations);
  cmdspec.defineFlag(
    "E","save-preprocessed","saves the pre-processed source", "", opts::NONE,
    os.save_preprocessed);
  cmdspec.defineFlag(
    "B", "save-binaries",
    "saves program binaries after compile",
    "This uses clGetProgramInfo(...CL_PROGRAM_BINARIES...).  "
    "The file is saved to the current working directory with the "
    "same filename with a replaced device-specific extension.  Hence, input "
    "programs with the same file name will conflict and the last will stomp "
    "the earlier program binaries of the same name.",
    opts::NONE,
    os.save_binaries);
  cmdspec.defineFlag(
    "P", "parse-only",
    "parses the script only and pretty prints it",
    "",
    opts::NONE,
    os.parse_only);
  cmdspec.defineOpt(
    "l", "list-devices", "DEV?", "list the devices by index or name",
    "Lists devices by index or name.\n"
    "EXAMPLES:\n"
    " -l         lists all devices on the sytstem\n"
    " -l=0       lists device 0\n"
    " -l=RTX     lists the device with \"RTX\" as a substring of "
    "its CL_DEVICE_NAME\n"
    "",
    opts::ALLOW_MULTI|opts::FLAG_VALUE,
    [] (const char *value, const opts::ErrorHandler &eh, cls::opts &os) {
      os.list_devices = true;
      if (*value == 0)
        return;
      int dev_ix = 0;
      if (opts::readDecInt(value, dev_ix)) {
        os.list_devices_specific.push_back(get_device_by_index(os, dev_ix));
      } else {
        os.list_devices_specific.push_back(get_device_by_name(os, value));
      }
    });
  cmdspec.defineOpt(
      "lm",
      "list-metrics",
      "METRIC?",
      "list the metric sets by name",
      "Lists metric sets.\n"
      "EXAMPLES:\n"
      " -l           list all metrics\n"
      " -l=compute   lists all metrics with compute in the name\n"
      "",
      opts::ALLOW_MULTI | opts::FLAG_VALUE,
      [] (const char *value, const opts::ErrorHandler &eh, cls::opts &os) {
        os.list_metrics.push_back(value);
      });
  cmdspec.defineOpt(
      "ls",
      "list-sysinfo",
      "FILTER?",
      "list system info",
      "Lists various system info.\n"
      "EXAMPLES:\n"
      " -ls           list all system info\n"
      " -ls=FILTER    ...implementation defined\n"
      "",
      opts::ALLOW_MULTI | opts::FLAG_VALUE,
      [] (const char *value, const opts::ErrorHandler &eh, cls::opts &os) {
        os.list_sysinfo.push_back(value);
      });
  cmdspec.defineOpt(
      "mf",
      "metric-format",
      "FORMAT",
      "specifies the output format for metrics",
      "The outputs can be (n)aturally ordered, (t)ransposed, or (c)sv.\n",
      opts::NONE,
      [&](const char *value, const opts::ErrorHandler &eh, cls::opts &os) {
        std::string s = value;
        if (s == "n" || s == "nat" || s == "natural") {
          os.metric_format = cls::opts::NAT;
        } else if (s == "t" || s == "trans" || s == "transposed") {
          os.metric_format = cls::opts::TRANS;
        } else if (s == "c" || s == "csv" || s == "csv") {
          os.metric_format = cls::opts::CSV;
        } else {
          eh("invalid format (must be n, t, or c)");
        }
      });

  cmdspec.defineFlag(
      "q", "quiet", "lower verbosity", "generate minimal output", opts::NONE,
      [] (const char *value, const opts::ErrorHandler &eh, cls::opts &os) {
          os.verbosity = -1;
      });
  cmdspec.defineExtraHelpSection("syn", "all syntax information",
    cls::CLS_SYNTAX_ALL());
  cmdspec.defineExtraHelpSection("syn-sc", "syntax script layout",
    cls::CLS_SYN_SC());
  cmdspec.defineExtraHelpSection("syn-st", "script statement syntax",
    cls::CLS_SYN_ST());
  cmdspec.defineExtraHelpSection("syn-pex", "primitive expressions syntax",
    cls::CLS_SYN_PEX());
  cmdspec.defineExtraHelpSection("syn-sex", "surface initializers expressions",
    cls::CLS_SYN_SEX());

  auto &g = cmdspec.defineGroup("t", "profiling options");
  g.defineFlag(
      "W", nullptr, "profiles with wall timers", "", opts::NONE, os.wall_time);
  g.defineFlag(
      "CL",
      nullptr,
      "profiles with OCL prof timers",
      "",
      opts::NONE,
      os.prof_time);
  g.defineOpt(
      "MD",
      nullptr,
      "MetricSet",
      "profiles with MDAPI counters (Intel Only)",
      "",
      opts::NONE,
      [&](const char *value, const opts::ErrorHandler &eh, cls::opts &opts) {
          opts.metric_counter_set = value;

          mdapi_lib *md_lib = new mdapi_lib();
          if (!md_lib->is_loaded()) {
            eh("failed to load MDAPI library");
            delete md_lib;
            return;
          }
          const auto mss = md_lib->list_metric_sets();
          if (opts.metric_counter_set == "") {
            std::stringstream ss;
            ss << "MDAPI Versions " << md_lib->get_version() << "\n";
            ss << "expected metric set name; one of:\n";
            for (const ms_info &mis : mss) {
              ss << " * " << mis.set << "\n";
              if (opts.verbosity >= 1) {
                for (const auto &m : mis.metrics) {
                  ss << "   - " << m << "\n";
                }
              }            }
            delete md_lib;
            eh(ss.str());
          } else {
            bool found = false;
            for (const ms_info &mis : mss) {
              if (mis.set == opts.metric_counter_set) {
                found = true;
              }
            }
            if (!found) {
              std::stringstream ss;
              ss << "invalid metric set name (-tMD= to list)\n";
              for (const ms_info &mis : mss) {
                ss << " * " << mis.set << "\n";
              }
              eh(ss.str());
            }
          }
          delete md_lib;
      });

  cmdspec.defineFlag(
      nullptr,
      "use-kernel-arg-info",
      "uses OpenCL 1.2+ kernel arg info in argument inference",
      "This can fail if there are typedefs or custom types.  "
      "By default we attempt to parse the source."
      "",
      opts::NONE,
      os.use_kernel_arg_info);

  cmdspec.defineOpt(
      "v",
      "verbosity",
      "INT",
      "sets the output level",
      "",
      opts::FLAG_VALUE,
    [] (const char *value, const opts::ErrorHandler &eh, cls::opts &opts) {
      if (*value == 0) { // -v
        opts.verbosity = 1;
      } else if (!opts::readDecInt(value, opts.verbosity)) {
        eh("malformed verbosity value");
      }
      sys::desired_message_verbosity = opts.verbosity;
    });
  opts::Group<cls::opts> &xGrp =
    cmdspec.defineGroup("X", "Experimental Options");
  xGrp.defineFlag(
    "all-times",
    nullptr,
    "include times for non-kernel operations",
    "This option enables times for non-kernel dispatches.  "
    "For instances memory object initialization will be included here.",
    opts::OptAttrs::NONE,
    os.show_all_times);
  xGrp.defineOpt(
    "cpp-path",
    nullptr,
    "PATH",
    "path to the GNU C-Preprocessor",
    "This is the full path to the GNU C Preprocessor.  "
    "During kernel analysis we use this path.",
    opts::OptAttrs::NONE,
    os.cpp_override_path);
  xGrp.defineFlag(
    "no-cleanup",
    nullptr,
    "do no deallocate OpenCL driver objects before exit",
    "Do not explicitly release OpenCL driver objects created before exit.",
    opts::OptAttrs::NONE,
    os.no_cleanup);
  xGrp.defineFlag(
    "no-device-check",
    nullptr,
    "disables device checks for various system-specific uses",
    "Ignores device checks for things like MDAPI and other device-"
    "specific tasks.",
    opts::OptAttrs::NONE,
    os.no_device_check);
  xGrp.defineFlag(
    "no-exit-on-diff-fail",
    nullptr,
    "diff commands will not trigger an exit failure",
    "Normally a memory object diff mismatch will exit immediately.  "
    "This overrides that behavior.",
    opts::OptAttrs::NONE,
    os.no_exit_on_diff_fail);
  if (!cmdspec.parse(argc, argv, os)) {
    exit(EXIT_FAILURE);
  }
  if (os.list_devices && !os.list_metrics.empty()) {
    std::cerr << "-l mutually exclusive with -lm\n";
    return EXIT_FAILURE;
  }
  if (os.list_devices && !os.list_sysinfo.empty()) {
    std::cerr << "-l mutually exclusive with -ls\n";
    return EXIT_FAILURE;
  }
  if (!os.list_metrics.empty() && !os.list_sysinfo.empty()) {
    std::cerr << "-ls mutually exclusive with -lm\n";
    return EXIT_FAILURE;
  }

  if (os.list_devices) {
    list_device_info(os);
    return EXIT_SUCCESS;
  } else if (!os.list_metrics.empty()) {
    list_mdapi_metrics(os.verbosity, os.list_metrics);
    return EXIT_SUCCESS;
  } else if (!os.list_sysinfo.empty()) {
    list_system_info(os);
    return EXIT_SUCCESS;
  } else if (os.input_expr.size() == 0 && os.input_files.empty()) {
    std::cerr << "expected input -e or file arguments\n";
    return EXIT_FAILURE;
  }
  if (os.input_expr.size() > 0) {
    run_file(os, "<interactive>", os.input_expr);
  }
  for (std::string file : os.input_files) {
    if (!sys::file_exists(file)) {
      std::cerr << file << ": file not found\n";
      return EXIT_FAILURE;
    }
    auto file_text = sys::read_file_text(file);
    run_file(os, file, file_text);
  }

  // funky stuff happens with ANSI coloring without explicit flushing
  std::cout.flush();
  std::cerr.flush();

  return EXIT_SUCCESS;
}

static void run_file(
  struct cls::opts &os,
  std::string file_name,
  std::string file_contents)
{
  auto start_setup = std::chrono::high_resolution_clock::now();
  cls::script s(file_contents);
  cls::diagnostics ds(os.verbosity, file_contents);

  try {
    if (os.verbose_enabled())
      std::cout << "============ parsing script\n";
    file_contents =
      cls::expand_input_variables(os, file_contents, ds);
    if (os.verbose_enabled())
      std::cout << "EXPANDS TO==\n" << file_contents << "\n==\n";
    cls::parse_script(os, file_contents, file_name, s, ds);

    ds.flush_warnings(std::cerr);

    if (os.parse_only) {
      cls::format_opts fopts;
      fopts.opts = cls::format_opts::USE_COLOR;
      s.str(std::cout, fopts);
      return;
    }
  } catch (const cls::diagnostic &d) {
    d.emit_and_exit_with_error();
  }
  const auto duration_setup =
    std::chrono::duration_cast<std::chrono::microseconds>(
      std::chrono::high_resolution_clock::now() - start_setup);
  const double duration_setup_s = duration_setup.count() / 1000.0 / 1000.0;

  auto start_compile = std::chrono::high_resolution_clock::now();
  cls::compiled_script cs;
  try {
    if (os.verbose_enabled())
      std::cout << "============ compiling script\n";
    //
    cs = cls::compile(os, s, ds);
    //
    ds.flush_warnings(std::cerr);
  } catch (const cls::diagnostic &d) {
    d.emit_and_exit_with_error();
  }
  const auto duration_compile =
    std::chrono::duration_cast<std::chrono::microseconds>(
      std::chrono::high_resolution_clock::now() - start_compile);
  const auto compile_time_s = duration_compile.count() / 1000.0 / 1000.0;

  sampler execute_times;
  for (int i = 0; i < os.iterations; i++) {
    if (os.verbose_enabled())
      std::cout << "============ starting iteration " << i << "\n";
    try {
      auto start_execute = std::chrono::high_resolution_clock::now();

      cs.execute(i);

      auto duration_exec =
        std::chrono::duration_cast<std::chrono::microseconds>(
          std::chrono::high_resolution_clock::now() - start_execute);
      execute_times.add(duration_exec.count()/1000.0/1000.0);
    } catch (const cls::diagnostic &d) {
      d.emit_and_exit_with_error();
    }
  }

  text::table t;
  auto &ckl_col = t.define_col("CLOCK (all times in s.)", false, 1);
  auto &itr_col = t.define_col("itrs", true, 1);
  auto &med_col = t.define_col("med", true, 1);
  auto &avg_col = t.define_col("avg", true, 1);
  t.define_spacer("+-");
  auto &cfv_col = t.define_col("cfv%", true, 1);
  auto &min_col = t.define_col("min", true, 1);
  t.define_spacer("/");
  auto &max_col = t.define_col("max", true, 1);
  t.define_spacer("  ");
  auto &pct_total_col = t.define_col("overall%", true, 1);

  auto emit_stats = [&](
    const std::string &clk,
    const sampler &s,
    std::optional<double> avg_total)
  {
    ckl_col.emit(clk);
    itr_col.emit(s.size());
    med_col.emit(s.med(), 5);
    avg_col.emit(s.avg(), 5);
    cfv_col.emit(100*s.cfv(), 1);
    min_col.emit(s.min(), 5);
    max_col.emit(s.max(), 5);
    if (avg_total) {
      pct_total_col.emit(100.0 * s.avg() / *avg_total, 1);
    } else {
      pct_total_col.emit("");
    }
    if (os.verbosity > 0) {
      int i = 0;
      for (double d : s.samples()) {
        ckl_col.emit("");
        itr_col.emit(i++);
        med_col.emit("=>");
        avg_col.emit(d, 5);
        cfv_col.emit("");
        min_col.emit("");
        max_col.emit("");
        pct_total_col.emit("");
      }
    }
  };
  if (os.show_all_times)
    emit_stats("TOTAL", execute_times, std::nullopt);
  if (os.verbosity >= 0) {
    if (os.prof_time) {
        std::cout << "PROF=================================================\n";
    } else {
        std::cout << "WALL=================================================\n";
    }
  }
  auto dispatch_times =
    os.prof_time ? cs.get_prof_times() : cs.get_wall_times();
  double total = execute_times.avg();
  if (os.show_all_times) {
    total += duration_setup_s;
    total += compile_time_s;
    for (const auto &dt : cs.get_init_times()) {
      const cls::init_spec_mem &im = *std::get<0>(dt);
      total += std::get<1>(dt).avg();
    }
  }

  for (const auto &p_ds : dispatch_times) {
    const cls::dispatch_spec &ds = *std::get<0>(p_ds);
    const sampler &ts = std::get<1>(p_ds);
    emit_stats(ds.spec::str(), ts, std::make_optional(total));
  }
  if (os.show_all_times) {
    sampler start;
    start.add(duration_setup_s);
    emit_stats("<startup>", start, std::make_optional(total));
    //
    sampler comps;
    comps.add(compile_time_s);
    emit_stats("<compile>", comps, std::make_optional(total));
    //
    for (const auto &dt : cs.get_init_times()) {
      const cls::init_spec_mem &im = *std::get<0>(dt);
      const sampler &ts = std::get<1>(dt);
      emit_stats(
        "<init>(" + im.spec::str() + ")",
        ts,
        std::make_optional(total));
    }
  }

  if (!os.metric_counter_set.empty()) {
    const auto &mdcs = cs.get_mdapi_ctrs();
    emit_metrics(os, mdcs);
  }


  t.str(std::cout);
  std::cout.flush();
  std::cerr.flush();
  if (!os.no_cleanup)
    cs.destroy();
} // run_file

