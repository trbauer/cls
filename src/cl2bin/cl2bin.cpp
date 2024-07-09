#include <cstdlib>
#include <iostream>

#include "cls_opts.hpp"
#include "cl_lib.hpp"
#include "fatal.hpp"
#include "devices.hpp"
#include "system.hpp"
#include "text.hpp"

using namespace cls;
using namespace text;

struct cl2bin_opts {
  int verbosity = 0;
  std::string input_file;
  std::string build_options;
  std::string device_filter;
  std::string output_file;

  bool normal() const {return verbosity >= 0;}
  bool verbose() const {return verbosity >= 1;}
  bool debug() const {return verbosity >= 2;}
  bool verbose_debug() const {return verbosity >= 3;}
};

int main(int argc, const char **argv)
{
  struct cl2bin_opts os;

  for (int i = 1; i < argc; i++) {
    std::string arg = argv[i];
    std::string key = arg, val;
    auto eq = arg.find('=');
    if (eq != std::string::npos) {
      key = arg.substr(0, eq + 1); // include the =
      val = arg.substr(eq + 1);
    }
    auto bad_opt = [&](std::string msg) {
      fatal(arg, ": ", msg);
    };
    // matches start of a key-value pair option (e.g. -k=...)
    // checks to ensure the option is not given as a flag (e.g. -k is illegal)
    // and gives a good error diagnostic in that case
    auto arg_is_key_eq = [&](const char *k_pfx0, const char *k_pfx1 = nullptr) {
      if (k_pfx0 + std::string("=") == key) {
        return true;
      }
      if (k_pfx1 && k_pfx1 + std::string("=") == key) {
        return true;
      }
      if (key == k_pfx0) {
        bad_opt(format(k_pfx0, ": expects a value (", k_pfx0, "=...)"));
      } else if (k_pfx1 && key == k_pfx1) {
        bad_opt(format(k_pfx1, ": expects a value (", k_pfx1, "=...)"));
      }
      return false;
    };
    // similar to above, but as a flag
    auto arg_is_flag = [&](const char *k_pfx0, const char *k_pfx1 = nullptr) {
      if (key == k_pfx0 || (k_pfx1 && key == k_pfx1)) {
        return true;
      }
      if (k_pfx0 + std::string("=") == key) {
        bad_opt(format(k_pfx0, ": should be given as a flag"));
      }
      if (k_pfx1 && k_pfx1 + std::string("=") == key) {
        bad_opt(format(k_pfx1, ": should be given as a flag"));
      }
      return false;
    };
    ///////////////////////
    if (arg == "-h" || arg == "--help") {
      std::stringstream uss;
      uss <<
        "usage: cl2bin [OPTS] INPUT\n"
        "where [OPTS]:\n"
        "  -o/--output=PATH     sets output file\n"
        "                        (defaults same file with .bin or .ptx ext)\n"
        "                        - uses stdout\n"
        "  -v/-v2/-v3            verbosity/debug\n"
        "and INPUT is:\n"
        "          DEV`FILE`[BOPTS]\n"
        ""
        "EXAMPLES:\n"
        " % cl2bin file.cl\n"
        "   using default device\n"
        " % cl2bin file.cl[-cl-kernel-arg-info]\n"
        "   setting build options\n"
        " % cl2bin #0`file.cl\n"
        " % cl2bin #0`file.cl[-cl-std=CL3.0]\n"
        "   selecting device by index\n"
        " % cl2bin \"#RTX`file.cl[-cl-std=CL3.0 -g]\"\n"
        "   selecting device by substring\n"
        " % cl2bin \"#RTX`path/file.cl[-cl-nv-arch sm_89 -nv-line-info]\" -o=-\n"
        "   output to stdout"
        "";
      std::cout << uss.str();
      return EXIT_SUCCESS;
    } else if (arg_is_flag("-v")) {
      os.verbosity = 1;
    } else if (arg_is_flag("-v2")) {
      os.verbosity = 2;
    } else if (arg_is_flag("-v3")) {
      os.verbosity = 3;
    } else if (arg_is_key_eq("-o", "--output")) {
      os.output_file = val;
    } else if (arg.substr(0, 1) == "-") {
      bad_opt("invalid option");
    } else {
      if (!os.input_file.empty()) {
        bad_opt("input file already set");
      }
      // #0`path/file.cl[BOPTS]
      auto s = arg;
      auto tick_off = s.find('`');
      if (tick_off != std::string::npos) {
        // e.g. #0 or #Intel Graph
        auto d = s.substr(0, tick_off);
        if (d.empty() || d[0] != '#') {
          bad_opt("device filter should start with #");
        }
        s = s.substr(tick_off + 1);
        os.device_filter = d.substr(1);
      }
      // path/file.cl[BOPTS]
      os.input_file = arg;
      auto lbrack_off = s.find('[');
      if (lbrack_off == std::string::npos) {
        os.input_file = s;
      } else {
        // "file.cl[BOPTS]"
        if (s[s.size() - 1] != ']') {
          bad_opt("malformed build options clause");
        }
        os.input_file = s.substr(0, lbrack_off);
        os.build_options = s.substr(lbrack_off + 1, s.size() - lbrack_off - 2);
      }
    }
  } // for
  if (os.input_file.empty()) {
    fatal("expected input file and options (try -h)");
  }

  if (os.debug()) {
    std::cout << "dev  = [" << os.device_filter << "]\n";
    std::cout << "file = [" << os.input_file << "]\n";
    std::cout << "opts = [" << os.build_options << "]\n";
  }

  cl_device_id dev;
  cls::opts cls_os;
  cls_os.verbosity = os.verbosity;
  if (os.device_filter.empty()) {
    dev = get_device_default();
  } else if (std::all_of(os.device_filter.begin(),
                         os.device_filter.end(), std::isdigit)) {
    dev = get_device_by_index(cls_os, std::atoi(os.device_filter.c_str()));
  } else {
    std::string err;
    if (!get_device_by_name(cls_os, os.device_filter, dev, err)) {
      fatal("get_device_by_name: ", err);
    }
  }
  if (os.verbose()) {
    std::cout << "using device: " << get_device_name(dev) << "\n";
  }
  //
  cl_lib cl {os.verbosity, dev, false};
  cl_int err = 0;
  cl_context ctx = cl.clCreateContext(
      nullptr,
      1,
      &dev,
      nullptr,
      nullptr, &err);
  if (err != 0) {
    fatal("clCreateContext: ", err);
  }
  /////////////////////
  // TODO: search cls for clCreateProgramWithIL to see how to support that
  //
  std::string inp = sys::read_file_text(os.input_file);
  const char *src = inp.c_str();
  size_t len = inp.size();
  cl_program p = cl.clCreateProgramWithSource(ctx, 1, &src, &len, &err);
  if (err != 0) {
    fatal("clCreateProgramWithSource: ", err);
  }
  auto get_build_log = [&]() -> std::string {
      size_t len = 0;
      err = cl.clGetProgramBuildInfo(p, dev, CL_PROGRAM_BUILD_LOG,
                                     0, nullptr, &len);
      if (err != 0) {
        fatal("clGetProgramBuildInfo(query): ", err);
      }
      std::vector<unsigned char> log;
      log.resize(len + 1);
      err = cl.clGetProgramBuildInfo(p, dev, CL_PROGRAM_BUILD_LOG,
                                     len, log.data(), nullptr);
      if (err != 0) {
        fatal("clGetProgramBuildInfo(fetch): ", err);
      }
      log[len] = 0;
      return std::string((char *)log.data());
    }; // get_build_log

  //
  const auto bp_err = cl.clBuildProgram(p, 1, &dev, os.build_options.c_str(),
                                        nullptr, nullptr);
  if (bp_err == CL_BUILD_PROGRAM_FAILURE) {

    fatal("clBuildProgram failed: ", bp_err, "\n", get_build_log());
  } else if (bp_err != 0) {
    fatal("clBuildProgram unknown error: ", bp_err);
  } else if (os.verbose()) {
    std::cout << "clGetProgramBuildInfo:\n" << get_build_log() << "\n";
  }

  // foo/bar.cl -> bar.bin
  std::string output_file;
  if (os.output_file.empty()) {
    std::stringstream ss;
    ss << sys::drop_extension(sys::take_file(os.input_file));
    const auto vend = get_device_vendor(dev);
    if (vend == vendor::NVIDIA)
      ss << ".ptx";
    else // Intel is ELF; I don't know what others do.
      ss << ".bin";
    output_file = ss.str();
  } else if (os.output_file != "-") {
    output_file = os.output_file;
  }
  size_t bits_len = 0;
  err = cl.clGetProgramInfo(p,
                            CL_PROGRAM_BINARY_SIZES,
                            sizeof(bits_len),
                            &bits_len,
                            nullptr);
  if (err != 0) {
    fatal("clGetProgramInfo(query len.): ", err);
  }
  unsigned char *bits = (unsigned char *)alloca(bits_len);
  err = cl.clGetProgramInfo(p, CL_PROGRAM_BINARIES,
                            bits_len, &bits, nullptr);
  if (err != 0) {
    fatal("clGetProgramInfo(fetch bits)", err);
  }
  if (output_file.empty()) {
    std::cout << bits << "\n";
  } else {
    sys::write_bin_file(output_file, bits, bits_len);
  }

  return EXIT_SUCCESS;
}