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
#ifdef _WIN32
#define NORETURN_DECLSPEC __declspec(noreturn)
#define NORETURN_ATTRIBUTE
#else
#define NORETURN_DECLSPEC
#define NORETURN_ATTRIBUTE __attribute__((noreturn))
#endif

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

  ///////////////////////////////////////////////////////////////////////////////
  // LOGGING AND EXIT
  NORETURN_DECLSPEC
  void               fatal_exit() NORETURN_ATTRIBUTE;
  void               debug_break();
  // void               fatal_message(const char *file, int line, const char *patt, ...);
  // void               warning_message(const char *patt, ...);
  // void               verbose_message(const char *patt, ...);
  // void               debug_message(const char *patt, ...);
  void               message_for_level(int this_level, const char *patt, ...);
  bool               is_tty(std::ostream &os);
  size_t             get_terminal_width();


  ///////////////////////////////////////////////////////////////////////////////
  // FILE SYSTEM
  static
  const char         path_separator =
#ifdef _WIN32
    '\\'
#else
    '/'
#endif
    ;

  bool               file_exists(const char *path);
  bool               file_exists(const std::string &path);
  bool               directory_exists(const std::string &path);
  std::vector<std::string>  list_directory(const std::string &path);
  std::vector<std::string>  list_directory_full_paths(const std::string &path);
  //
  std::string        drop_extension(std::string file);
  std::string        take_extension(std::string file);
  std::string        replace_extension(std::string file, std::string ext);
  // if not found, then returns ""
  // if found, then it always ends with a / or backslash
  std::string        get_temp_dir();
  // creates a path to a temporary file
  std::string        get_temp_path(const char *sfx);
  // gets the main exe filename of this program
  std::string        get_main_executable();

  // searches $PATH for an executable name
  // returns "" if not found
  //
  // NOTE: We will add the ".exe" suffix for Windows
  std::string        find_exe(const char *exe);

  void               read_file_binary(std::string fname, bits &cs);
  bits               read_file_binary(std::string fname);
  std::string        read_file_text(std::string fname);
  std::string        char_file_to_str(const bits &cs);

  void               write_bin_file(std::string fname, const void *buf, size_t buflen);

  /////////////////////////////////////////////////////////////////////////////
  // DYNAMIC LINKING
  void              *load_library(const char *name);
  void               close_library(void *lib);
  void              *get_loaded_library(const char *name);
  void              *get_symbol_address(void *lib,const char *name);

  /////////////////////////////////////////////////////////////////////////////
  // PROCESS CREATION
  struct process_result {
    // 0 on success GetLastError() or errno otherwise (e.g. file not found)
    int             startup_status;

    // results only value if we started up
    int             exit_code;

    // stderr
    std::string     err;

    // stdout
    std::string     out;

    // did it successfully execute (e.g. the exe was found etc...)
    // executed() && exit_code != 0 implies the program ran, but failed
    bool executed() const {return startup_status == 0;}
    // ran and exited 0
    bool succeeded() const {return executed() && exit_code == 0;}
  };
  // returns the exit code, output, and error streams
  process_result     process_read(
    std::string exe,
    std::vector<std::string> args,
    std::string input = "");
  // allows one to write: sys::process_read("ls","-la","file");
  process_result     process_read(
    std::string exe,
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