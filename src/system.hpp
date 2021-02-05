#ifndef SYSTEM_HPP
#define SYSTEM_HPP

#include <cstdarg>
#include <cstdint>
#include <cstring>
#include <initializer_list>
#include <ostream>
#include <string>
#include <vector>

///////////////////////////////////////////////////////////////////////////////
// FUNCTION ATTRIBUTES AND FEATURES
#define FATAL(...) \
  do { \
    sys::message_for_level(2, __VA_ARGS__); \
    sys::fatal_exit(); \
  } while (0)

#define WARNING(...) \
  sys::message_for_level(1,__VA_ARGS__)
#define VERBOSE(...) \
  sys::message_for_level(-1,__VA_ARGS__)
#define DEBUG(...) \
  sys::message_for_level(-2,__VA_ARGS__)

namespace sys {
  extern int desired_message_verbosity;
}

#ifdef _WIN32
#define STRNCPY(D,DL,S,SL) \
  strncpy_s(D,DL,S,SL)
#else
#define STRNCPY(D,DL,S,SL) \
  strncpy(D,S,SL)
#endif

///////////////////////////////////////////////////////////////////////////////
// STRING COMPARISON HELPERS
static bool streq(const char *s1, const char *s2) {
  return
    s1 == s2 ||
    s1 != nullptr && s2 != nullptr &&
      strlen(s1) == strlen(s2) &&
      strncmp(s1,s2,strlen(s1)) == 0;
}
static bool strpfx(const char *pfx, const char *str) {
  return strncmp(pfx,str,strlen(pfx)) == 0;
}
static bool strpfx(const char *pfx, std::string str) {
  return strpfx(pfx, str.c_str());
}
static bool strsfx(const char *sfx, std::string str) {
  return str.rfind(sfx) == str.size() - strlen(sfx);
}


///////////////////////////////////////////////////////////////////////////////
// MISCELLANEOUS
namespace sys {
  template <typename T>
  static T align_round_up(T x, T a) {
    return (x + a - 1) - ((x + a - 1) % a);
  }
}


namespace sys
{
  using bits = std::vector<uint8_t>;

  //////////////////////////////////////////////////////////////////////////////
  // SYSTEM ERROR SPECIFICS
  //
  // returns GetLastError() or errno
  int last_error();
  // strerror or FormatMessage
  std::string format_last_error(int e);

  // a status value for various operations that lead to system call
  struct status {
    // a status that will fatal out in the destructor
    bool fatal_on_error = false;
    //
    // post-construction state
    bool          failed = false;
    std::string   context; // e.g. sys::get_temp_path
    const char   *api = nullptr; // system call or whatever
    int           api_code = 0; // GetLastError or errno()
    std::string   extended;

    // will not fatal on an eror
    status() : status(nullptr) { }
    status(const char *_in) : status(false, _in) { }
    status(bool fatal, const char *_in)
      : fatal_on_error(fatal), context(_in == nullptr ? "" : _in) { }

    status(const status &s) = default;
    status &operator =(const status &) = default;

    // if so configured, this can trigger a fatal error
    ~status();

    // status st;
    // ...
    // if (st) something_went_wrong();
    //
    operator bool() const {return has_error();}
    bool has_error() const {return failed;}

    // result = call1 | call2 | call3;
    status operator|(const status &rhs) const {
      if (has_error())
        return *this;
      else
        return rhs;
    }

    void sys_error(const char *api, int api_code = last_error());
    void other_error(const std::string &msg);

    void chain_error(const char *pfx) {
      if (context.empty())
        context = pfx;
      else
        context = pfx + std::string(": ") + context;
    }

    void        str(std::ostream &os) const;
    std::string str() const;

    static status FATAL_ON_ERROR(const char *in = nullptr) {
      return status(true, in);
    }
  };

  // fatal_exit will terminate the process with a with an non-zero exit code
  // or a signal some sort depending on platform.  If a debugger is attached,
  // this will trigger a breakpoint first.
  [[noreturn]]
  void               fatal_exit();
  void               debug_break();

  //////////////////////////////////////////////////////////////////////////////
  // LOGGING AND EXIT
  //
  // void               fatal_message(const char *file, int line, const char *patt, ...);
  // void               warning_message(const char *patt, ...);
  // void               verbose_message(const char *patt, ...);
  // void               debug_message(const char *patt, ...);
  void               message_for_level(int this_level, const char *patt, ...);
  bool               is_tty(std::ostream &os);
  size_t             get_terminal_width();


  //////////////////////////////////////////////////////////////////////////////
  // FILE SYSTEM
  static
  const char         path_separator =
#ifdef _WIN32
    '\\'
#else
    '/'
#endif
    ;

  // detects if a file or directory exists (a path matches either)
  // we detect this via S_ISREG and S_ISDIR, respectively; and in
  // Windows !FILE_ATTRIBUTE_DIRECTORY and FILE_ATTRIBUTE_DIRECTORY,
  // respecitvely
  bool               path_exists(const std::string &path);
  bool               file_exists(const std::string &path);
  bool               directory_exists(const std::string &path);

  std::vector<std::string>  list_directory(const std::string &path);
  std::vector<std::string>  list_directory_full_paths(const std::string &path);
  uint64_t           file_size(const std::string &path);
  //
  std::string        drop_extension(std::string file); // foo/bar.txt -> foo/bar
  std::string        take_extension(std::string file); // foo/bar.txt -> .txt
  std::string        take_file(std::string path); // a/foo/bar.txt -> bar.txt
  // replace_extension(foo,"txt") => a/foo.xxx -> a/foo.txt
  std::string        replace_extension(std::string file, std::string ext);

  // if not found, the first fatals, the second variant returns ""
  // if found, then it always ends with a path separator
  std::string        get_temp_dir();
  std::string        get_temp_dir(status &error);
  //
  // creates a path to a temporary file
  // the optional suffix not necessarily an exension, so include the
  // dot if that's the intent (e.g. "txt")
  //
  // the first version fatals when something goes wrong
  // the latter does not fatal, but returns the empty string and sets
  // an error message (if the error pointer is non-null)
  std::string        get_temp_path(const char *sfx);
  std::string        get_temp_path(const char *sfx, status &error);
  //
  // gets the main exe filename of this program
  std::string        get_main_executable();
  std::string        get_main_executable(status &error);

  // searches $PATH for an executable name
  // returns "" if not found
  //
  // NOTE: We will add the ".exe" suffix for Windows
  std::string        find_exe(const char *exe);

  void               read_file_binary(std::string fname, bits &cs);
  bits               read_file_binary(std::string fname);
  std::string        read_file_text(std::string fname);
  std::string        char_file_to_str(const bits &cs);

  void               write_bin_file(std::string f, const void *b, size_t blen);

  /////////////////////////////////////////////////////////////////////////////
  // DYNAMIC LINKING
  void              *load_library(const char *name);
  void               close_library(void *lib);
  void              *get_loaded_library(const char *name);
  void              *get_symbol_address(void *lib,const char *name);

  /////////////////////////////////////////////////////////////////////////////
  // PROCESS CREATION
  struct process_result {
    // the status of running this command
    status          st;

    /////////////////////////////////////////////////////////////////////////////
    // child output
    // results only value if we started up
    enum class process_state {
      NOT_STARTED,
      EXITED,
      SIGNALED,
      OTHER
    };
    process_state   state = process_state::NOT_STARTED;
    int             code = 0; // signal or exit code

    // stderr
    std::string     err;

    // stdout
    std::string     out;

    // this indicates the process at least started (it might or might not
    // have exited)
    bool started() const {return state != process_state::NOT_STARTED;}
    // the process exited 0
    bool succeeded() const {return exited(0);}
    // ran and exited 0
    bool exited(int v) const {return exited() && code == 0;}
    // the process exited (maybe not successfully)
    bool exited() const {return state == process_state::EXITED;}
    // the process signaled
    bool signaled() const {return state == process_state::SIGNALED;}
  };
  // returns the exit code, output, and error streams
  process_result     process_read(
    const std::string &exe,
    const std::vector<std::string> &args,
    const std::string &input = "");
  // allows one to write: sys::process_read("ls","-la","file");
  process_result     process_read(
    const std::string &exe,
    std::initializer_list<std::string> args);
  // return stdout only; fatal if non-zero exit; stderr goes to our stderr
  // std::string process_read(std::string exe, std::vector<std::string> args, std::string input);
  // use this process's standard input and output streams
  // void process_fork(...)

  /////////////////////////////////////////////////////////////////////////////
  // REGISTRY
  bool               lookup_registry_key(
                        const char *path,
                        uint32_t &out_value);
  bool               lookup_registry_key64(
                        const char *path,
                        uint32_t &out_value);
  bool               lookup_registry_key(
                        const char *path,
                        std::string &out_value);
  bool               lookup_registry_key64(
                        const char *path,
                        std::string &out_value);

  /////////////////////////////////////////////////////////////////////////////
  // GEN DEVICE INFO

  // returns a string "skl_gt2"
  std::string        lookup_gen_device_info();

  // returns a string of all the display devices
  // E.g "[\\.\DISPLAY1] ... (active, pruned)"
  std::string        display_device();
} // sys::
#endif