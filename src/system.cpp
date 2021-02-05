#include "system.hpp"
#include "text.hpp"

#include <atomic>
#include <fstream>
#include <iostream>
#include <iomanip>
#include <sstream>
#include <thread>

#ifndef _WIN32
// Force Linux to use STL file system (or implement dirent)
#define USE_CPP17_STD_FILESYSTEM
#endif

#ifndef _WIN32
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/wait.h>
#include <unistd.h>
#include <signal.h>
#include <errno.h>
#include <string.h> // strerror
#endif

#ifdef USE_CPP17_STD_FILESYSTEM
// #include <experimental/filesystem>
// namespace fs = std::experimental::filesystem;
#if __has_include(<filesystem>)
#include <filesystem>
// different versions of VS2017 have this in different namespaces
// even with the top-level header
namespace fs = std::filesystem;
// namespace fs = std::experimental::filesystem;
#elif __has_include(<experimental/filesystem>)
#include <experimental/filesystem>
namespace fs = std::experimental::filesystem;
#else
#error "#include <filesystem> not found"
#endif
#else
// both Win32 and Unix
#include <sys/types.h>
#include <sys/stat.h>
#ifdef _WIN32
#define STAT64_T __stat64
#define STAT64    _stat64
#else
#define STAT64_T stat64
#define STAT64   stat64
#endif
#endif

#ifdef _WIN32
#include <Windows.h>
#include <io.h>
#define IS_STDERR_TTY (_isatty(_fileno(stderr)) != 0)
#define IS_STDOUT_TTY (_isatty(_fileno(stdout)) != 0)
#else
#include <dlfcn.h>
#include <unistd.h>
#include <sys/ioctl.h>
#define IS_STDERR_TTY (isatty(STDERR_FILENO) != 0)
#define IS_STDOUT_TTY (isatty(STDOUT_FILENO) != 0)
#endif


using namespace sys;

///////////////////////////////////////////////////////////////////////////////
// SYSTEM ERROR HANDLING
#ifdef _WIN32

int sys::last_error() {return (int)GetLastError();}
std::string sys::format_last_error(int e = last_error())
{
  LPVOID msgBuf;
  FormatMessageA(
    FORMAT_MESSAGE_ALLOCATE_BUFFER |
      FORMAT_MESSAGE_FROM_SYSTEM |
      FORMAT_MESSAGE_IGNORE_INSERTS,
    nullptr,
    (DWORD)e,
    MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT),
    (LPCSTR)&msgBuf,
    0, nullptr);
  std::string s((const char *)msgBuf);
  LocalFree(msgBuf);
  return s;
}
#else // !_WIN32

int sys::last_error() {return (int)errno;}

std::string sys::format_last_error(int e)
{
  char err_buf[64];
  const char *err_cstr = strerror_r(e, err_buf, sizeof(err_buf) - 1);
  if (err_cstr != nullptr)
    return err_cstr;
  return "?";
}
#endif // !_WIN32

status::~status()
{
  if (fatal_on_error && has_error()) {
    str(std::cerr);
    fatal_exit();
  }
}

void sys::status::sys_error(const char *_api, int _code)
{
  failed = true;
  api = _api;
  api_code = _code;
}

void sys::status::other_error(const std::string &msg)
{
  failed = true;
  extended = msg;
}

static std::string fmt_err_hex(int x) {
  std::stringstream ss;
  ss << "0x" << std::hex << std::uppercase << x;
  return ss.str();
}

void sys::status::str(std::ostream &os) const
{
  bool wrote_something = false;
  if (!context.empty()) {
    os << context << ": ";
    wrote_something = true;
  }
  if (api) {
    wrote_something = true;
    os << api << " returned " << fmt_err_hex(api_code) <<
      " (" << format_last_error(api_code) << ")";
  }
  if (!extended.empty()) {
    if (wrote_something)
      os << "; ";
    os << extended;
  }
}
std::string sys::status::str() const
{
  std::stringstream ss; str(ss); return ss.str();
}

///////////////////////////////////////////////////////////////////////////////
// COLORED TEXT
bool sys::is_tty(std::ostream &os) {
  return &os == &std::cerr ? IS_STDERR_TTY :
         &os == &std::cout ? IS_STDOUT_TTY :
        false;
}

size_t sys::get_terminal_width()
{
#ifdef _WIN32
  CONSOLE_SCREEN_BUFFER_INFO csbi = {0};
  if (!GetConsoleScreenBufferInfo(GetStdHandle(STD_OUTPUT_HANDLE), &csbi)) {
    return 0;
  }
  return csbi.srWindow.Right - csbi.srWindow.Left + 1; // X-size
      // csbi.srWindow.Bottom - csbi.srWindow.Top + 1; // Y-size
#else
  struct winsize ws{0};
  ioctl(0, TIOCGWINSZ , &ws);
  return ws.ws_col;
#endif
}

///////////////////////////////////////////////////////////////////////////////
// LOGGING AND EXIT
void sys::debug_break() {
#ifdef _WIN32
  DebugBreak();
#else
  raise(SIGTRAP);
#endif
}

// the desired message verbosity
int sys::desired_message_verbosity = 0;

// this_level =  2 => fatal_message
// this_level =  1 => warning_message
// this_level =  0 => normal_message
// this_level = -1 => verbose_message
// this_level = -2 => debug_message
void sys::message_for_level(int this_level, const char *patt, ...)
{
  if (this_level < desired_message_verbosity) {
    return;
  }

  va_list va;
  va_start(va, patt);
#ifdef _WIN32
  size_t elen = _vscprintf(patt, va) + 1;
#else
  char dummy[1];
  size_t elen = vsnprintf(&dummy[0], 0, patt, va) + 1;
#endif
  va_end(va);
#ifdef _WIN32
  char *ebuf = (char *)_alloca(elen + 1);
#else
  char *ebuf = (char *)alloca(elen + 1);
#endif
  va_start(va, patt);
  vsnprintf(ebuf, elen, patt, va);
  ebuf[elen - 1] = 0;
  if (elen - 1 > 0 && ebuf[elen - 2] != '\n')
    ebuf[elen - 1] = '\n';
  ebuf[elen - 0] = 0;
  va_end(va);

#ifdef _WIN32
  if (this_level > 0)
   OutputDebugStringA(ebuf);
#endif

  if (is_tty(std::cerr)) {
    if (this_level >= 2)
      std::cerr << text::ANSI_RED;
    else if (this_level == 1)
      std::cerr << text::ANSI_YELLOW;
    else if (this_level < 0)
      std::cerr << text::ansi_literal("\033[38;2;64;64;64m");
    std::cerr << ebuf;
    std::cerr << text::ANSI_RESET;
  } else {
    std::cerr << ebuf;
  }
}

void sys::fatal_exit()
{
#ifdef _WIN32
  if (IsDebuggerPresent()) {
      sys::debug_break();
  }
  exit(-1);
#else
  if (sys::file_exists("/proc/self/status")) {
    // if there's a tracing PID we can trap
    // NOTE: sometimes the field is there, but mapped to 0, meaning no trace
    auto status = sys::read_file_text("/proc/self/status");
    // std::cout << status << "\n";
    size_t tp = status.find("TracerPid:");
    if (tp != std::string::npos) {
      while (tp < status.size() && !isdigit(status[tp]))
        tp++;
      if (tp < status.size() && status[tp] != '0') {
        sys::debug_break();
      }
    }
  }
  abort();
#endif
}

///////////////////////////////////////////////////////////////////////////////
// FILE SYSTEM
bool sys::path_exists(const std::string &path)
{
#ifdef _WIN32
  //  WIN32_FIND_DATAA ffd;
  //  HANDLE h = FindFirstFileA(tmp_daf.c_str(), &ffd);
  //  if (h == INVALID_HANDLE_VALUE) {
  //    FATAL("kdc -off failed to generate .daf file (is DafEnable set?)\n");
  //  }
  //  FindClose(h);
  DWORD dwAttrib = GetFileAttributesA(path.c_str());
  return (dwAttrib != INVALID_FILE_ATTRIBUTES);
#else
  struct stat sb = {0};
  if (stat(path.c_str(), &sb)) {
      return false;
  }
  return S_ISDIR(sb.st_mode) || S_ISREG(sb.st_mode);
#endif
}

bool sys::file_exists(const std::string &path)
{
#ifdef _WIN32
  DWORD dwAttrib = GetFileAttributesA(path);
  return (dwAttrib != INVALID_FILE_ATTRIBUTES &&
         !(dwAttrib & FILE_ATTRIBUTE_DIRECTORY));
#else
  // TODO: we might be able to use this for Win32 as well on newer compilers
  struct stat sb = {0};
  if (stat(path.c_str(), &sb)) {
      return false;
  }
  return S_ISREG(sb.st_mode);
#endif
}

bool sys::directory_exists(const std::string &path)
{
#ifdef _WIN32
  DWORD dwAttrib = GetFileAttributesA(path.c_str());
  return (dwAttrib != INVALID_FILE_ATTRIBUTES &&
         (dwAttrib & FILE_ATTRIBUTE_DIRECTORY));
#else
  struct stat sb = {0};
  if (stat(path.c_str(), &sb)) {
      return false;
  }
  return S_ISDIR(sb.st_mode);
#endif
}

static std::string add_trailing_slash_if_absent(std::string path)
{
  if (!path.empty()) {
    if (path[path.size() - 1] != '\\' && path[path.size() - 1] != '/') {
      path += sys::path_separator;
    }
  }
  return path;
}

static std::vector<std::string> list_dir_elems(
  const std::string &path, bool return_full_paths)
{
  std::vector<std::string> elems;
#if defined(USE_CPP17_STD_FILESYSTEM)
  for (auto &p : fs::directory_iterator(path)) {
    if (return_full_paths)
      elems.emplace_back(p.path().string());
    else
      elems.emplace_back(p.path().filename().string());
  }
#else
#ifdef _WIN32
  std::vector<std::string> paths;
  auto dir_with_slash = add_trailing_slash_if_absent(path);
  auto find_spec = dir_with_slash + "*";

  WIN32_FIND_DATAA ffd;
  HANDLE h = FindFirstFileA(find_spec.c_str(), &ffd);
  if (h == INVALID_HANDLE_VALUE)
    return elems;
  do {
    if (!streq(ffd.cFileName,".") && !streq(ffd.cFileName,".."))
      if (return_full_paths)
        elems.emplace_back(dir_with_slash + ffd.cFileName);
      else
        elems.emplace_back(ffd.cFileName);
  } while (FindNextFileA(h, &ffd) != 0);
  (void)FindClose(h);
#else
#error "need to implement this via dirent.h"
#endif
#endif
  return elems;
}
std::vector<std::string> sys::list_directory(const std::string &path)
{
  return list_dir_elems(path, false);
}
std::vector<std::string> sys::list_directory_full_paths(const std::string &path)
{
  return list_dir_elems(path, true);
}

uint64_t sys::file_size(const std::string &path)
{
#ifdef USE_CPP17_STD_FILESYSTEM
  return (uint64_t)fs::file_size(fs::path(path));
#else
  struct STAT64_T sb;
  if (STAT64(path.c_str(), &sb) != 0)
    return (uint64_t)-1;
  return (uint64_t)sb.st_size;
#endif
}

std::string sys::drop_extension(std::string file)
{
  auto dot_ix = file.rfind('.');
  return file.substr(0, dot_ix);
}
std::string sys::take_extension(std::string file)
{
  auto dot_ix = file.rfind('.');
  return file.substr(dot_ix);
}

std::string sys::take_file(std::string path)
{
  // a/foo/bar.txt -> bar.txt
  if (path.empty())
    return path;
  int off = (int)path.size();
  while (off > 0) {
    if (path[off-1] == '/' || path[off-1] == '\'')
      break;
    off--;
  }
  return path.substr(off);
}

std::string sys::replace_extension(std::string file, std::string new_ext)
{
  auto dot_ix = file.rfind('.');
  // if (dot_ix != std::string::npos)
  //  return file;
  return file.substr(0, dot_ix) + "." + new_ext;
}
std::string sys::get_temp_dir()
{
  status st = status::FATAL_ON_ERROR();
  return get_temp_dir(st);
}
std::string sys::get_temp_dir(status &st)
{
  st.context = "sys::get_temp_dir";
  std::string path;
#ifdef _WIN32
  // e.g. C:/Users/%USERNAME%/AppData/Local/Temp/
  DWORD tmp_dir_len = GetTempPathA(0, NULL);
  if (tmp_dir_len == 0) {
    st.error("GetTempPathA");
    return "";
  }
  char *tmp_path = (char *)alloca(tmp_dir_len + 1);
  if (GetTempPathA(tmp_dir_len, tmp_path) == 0) {
    st.error("GetTempPathA");
    return "";
  }
  tmp_path[tmp_dir_len] = 0;
  path = tmp_path;
#else
  const char *e = getenv("TEMP");
  if (e == nullptr) {
    e = getenv("TMP");
  }
  if (e == nullptr && sys::directory_exists("/tmp/")) {
    e = "/tmp/";
  }
  if (e == nullptr) {
    st.other_error("unable to find temporary directory");
    return "";
  }
  path = e;
#endif
  if (!path.empty() && path[path.size() - 1] != path_separator) {
    path += path_separator;
  }
  return path;
}

std::string sys::get_temp_path(const char *sfx)
{
  status st = status::FATAL_ON_ERROR();
  return get_temp_path(sfx, st);
}

std::string sys::get_temp_path(const char *c_sfx, status &st)
{
  st.context = "sys::get_temp_path";
  // normalize the suffix
  std::string sfx = c_sfx ? c_sfx : "";
#ifdef _WIN32
  int pid = (int)GetCurrentProcessId();
#else
  int pid = (int)getpid();
#endif

  status err;
  auto exe = get_main_executable(err);
  if (err) {
    st = err;
    st.chain_error("sys::get_temp_path");
    return "";
  }

  // foo/bar/baz => baz
  // foo\bar\baz.exe => baz.exe
  size_t last_slash = exe.size() - 1;
  while (last_slash > 0) {
    if (exe[last_slash] == '\\' || exe[last_slash] == '/') {
      exe = exe.substr(last_slash + 1);
      break;
    } else {
      last_slash--;
    }
  }
#ifdef _WIN32
  // baz.exe => baz
  auto ext = exe.rfind('.');
  if (ext != std::string::npos) {
    exe = exe.substr(0, ext);
  }
#endif
  std::stringstream ss;
  ss << exe << "_" << pid;

  auto temp_dir = get_temp_dir(err);
  if (err) {
    err.chain_error("sys::get_main_executable");
    return "";
  }

  std::string pfx = temp_dir + ss.str() + "_";

  // This should be fairly safe, even with conurrent processes.
  // 1. we use our PID in the temp name, so the only collisions are from
  //    this process
  // 2. we atomic increment a unique suffix until we find a file
  static std::atomic_llong next_suffix_id(0);
  while (1) {
    auto next_id = next_suffix_id.fetch_add(1);
    std::stringstream ss;
    ss << pfx << std::setw(4) << std::setfill('0') << next_id << sfx;
    std::string file = ss.str();
    if (!file_exists(file) && !directory_exists(file)) {
      return file;
    }
  }
}

std::string sys::get_main_executable()
{
  status st = status::FATAL_ON_ERROR();
  return get_main_executable(st);
}
std::string sys::get_main_executable(status &st)
{
  st.context = "sys::get_main_executable";
#ifdef _WIN32
  char buffer[MAX_PATH + 1] = {0};
  if (GetModuleFileNameA(NULL, buffer, sizeof(buffer)) == sizeof(buffer)) {
    st.sys_error("GetModuleFileName");
    return "";
    // overflow
  }
  return std::string(buffer);
#else
  // TODO: readlink /proc/self/exe
  std::vector<char> buf;
  buf.resize(1);
  ssize_t nr;
  while (true) {
    nr = readlink("/proc/self/exe", buf.data(), buf.size());
    if (nr < 0) {
      st.sys_error("readlink");
      return "";
    } else if (nr == buf.size()) {
      buf.resize(buf.size() + 1);
      continue;
    } else {
      break;
    }
  }
//  buf.push_back(0); // readlink doesn't append a 0
  return std::string(buf.data(), nr);
#endif
}

std::string sys::find_exe(const char *exe)
{
  std::string filename_exe = exe;
#ifdef _WIN32
  // "foo" -> "foo.exe"
  if (strlen(exe) < 4 && strncmp(".exe", exe + strlen(exe) - 4,4) != 0) {
    filename_exe += ".exe";
  }
#endif
  const char *path_value = nullptr;

#ifdef _MSC_VER
  char *msc_buf = nullptr;
  size_t msc_buf_len = 0;
  std::string msc_dummy;
  if (_dupenv_s(&msc_buf, &msc_buf_len, "PATH") == 0 && msc_buf != nullptr) {
    msc_dummy = msc_buf;
    path_value = msc_dummy.c_str();
    free(msc_buf);
  }
#else
  path_value = getenv("PATH");
#endif
  std::stringstream stream(path_value);
#ifdef _WIN32
  const char path_sep = ';';
#else
  const char path_sep = ':';
#endif
  std::string path_component;
  while(std::getline(stream, path_component, path_sep)) {
    if (path_component.empty())
      continue;
    std::string possible_path = path_component;
    if (path_component[path_component.size()-1] != sys::path_separator) {
      possible_path += sys::path_separator;
    }
    possible_path += filename_exe;
    if (sys::file_exists(possible_path))
      return possible_path;
  }
  return "";
}

bits sys::read_file_binary(std::string fname)
{
  bits bytes;
  read_file_binary(fname, bytes);
  return bytes;
}

void sys::read_file_binary(std::string fname, bits &cs)
{
  std::ifstream file(fname, std::ios::binary);
  if (!file.good()) {
    FATAL("%s: file not found", fname.c_str());
  }
  while (true) {
    int c = file.get();
    if (c == EOF)
      break;
    cs.push_back((char)c);
  }
}

std::string sys::char_file_to_str(const bits &cs)
{
  std::string text;
  for (char c : cs)
    text += c;
  return text;
}

std::string sys::read_file_text(std::string fname)
{
  std::string s;
  std::ifstream file(fname);
  if (!file.good()) {
    FATAL("%s: file not found", fname.c_str());
  }
  s.append(std::istreambuf_iterator<char>(file),
            std::istreambuf_iterator<char>());
  return s;
}

void sys::write_bin_file(
    std::string fname, const void *buf, size_t buflen)
{
  std::ofstream of(fname, std::ios::binary);
  if (!of.good())
    FATAL("%s: failed to open output buffer file for writing",
      fname.c_str());
  of.write((const char *)buf, buflen);
  if (!of.good())
    FATAL("%s: error writing file", fname.c_str());
}

/////////////////////////////////////////////////////////////////////////////
// DYNAMIC LINKIN
void *sys::load_library(const char *name) {
#ifdef _WIN32
  return (void *)LoadLibraryA(name);
#else
  return dlopen(name, RTLD_LAZY); // RTLD_NOW
#endif
}

void sys::close_library(void *lib)
{
#ifdef _WIN32
  (void)FreeLibrary((HMODULE)lib);
#else
  (void)dlclose(lib);
#endif
}

void *sys::get_loaded_library(const char *name) {
#ifdef _WIN32
  return (void *)GetModuleHandleA(name);
#else
  // TODO: dl???()
  return nullptr;
#endif
}

void *sys::get_symbol_address(void *lib, const char *name) {
#ifdef _WIN32
  return (void *)GetProcAddress((HMODULE)lib, name);
#else
  return dlsym(lib,name);
#endif
}



///////////////////////////////////////////////////////////////////////////////
// PROCESS CREATION
//
#ifdef _WIN32
// TODO: remove this and unify via process_pipe and other abstractions
//
// NEEDED because GCC's std::thread isn't allowing more than one argument
//  (could be older headers on MinGW:GCC 7.2)
//
// static void process_reader(HANDLE read_handle, std::string &save_target)
struct process_io_args {
  HANDLE        handle;
  std::string  &data;
  process_io_args(HANDLE _handle, std::string &_save_target)
    : handle(_handle), data(_save_target)
  {}
};

static void process_reader(process_io_args args)
{
  std::stringstream ss;
  while (true) {
    OVERLAPPED o;
    ZeroMemory(&o, sizeof(0));

    char buf[513];
    DWORD nr = 0;
    if (!ReadFile(args.handle, buf, sizeof(buf) - 1, &nr, NULL)) {
      DWORD err = GetLastError();
      if (err = ERROR_BROKEN_PIPE) {
        if (nr > 0) {
          buf[nr] = 0;
          ss << buf;
        }
        break;
      }
      FATAL("process_reader failed %d\n", (int)err);
    } else if (nr == 0) {
      break;
    } else {
      buf[nr] = 0;
      ss << buf;
    }
  }
  args.data = ss.str();
}

// static void process_writer(HANDLE write_handle, std::string data)
static void process_writer(process_io_args args)
{
  const char *buf = args.data.c_str();
  size_t off = 0;
  while (off < args.data.size()) {
    DWORD nw = 0;
    if (!WriteFile(
      args.handle,
      buf + off,
      (DWORD)(args.data.size() - off),
      &nw,
      NULL))
    {
      DWORD err = GetLastError();
      if (err = ERROR_BROKEN_PIPE) {
        break;
      }
      FATAL("process_writer failed %d\n", (int)err);
    }
  }
}
#endif


#ifdef _WIN32
using handle_t = HANDLE;
static const handle_t CLOSED_HANDLE = nullptr;
#else // !_WIN32
using handle_t = int; // fd
static const handle_t CLOSED_HANDLE = -1;
#endif // !_WIN32

struct process_pipe {
  status st;
  handle_t rd = CLOSED_HANDLE, wr = CLOSED_HANDLE;
  enum class which_type {STDIN, STDOUT, STDERR} which;

  process_pipe(const status &s, which_type w) : st(s), which(w)
  {
    if (which == which_type::STDIN)
      st.context += ": stdin pipe writer";
    else if (which == which_type::STDOUT)
      st.context += ": stdout pipe reader";
    else if (which == which_type::STDERR)
      st.context += ": stderr pipe reader";
    else
      st.context += ": unknown pipe accessor";
    open_pipe();
  }
  ~process_pipe() {close_both();}

  void open_pipe();

  bool valid() const {return rd != CLOSED_HANDLE || wr != CLOSED_HANDLE;}

  void close_both() {close_rd(); close_wr();}
  void close_rd() {close_one(rd);}
  void close_wr() {close_one(wr);}

  void read(std::string &out);
  void write(const std::string &out);

private:
  void close_one(handle_t &h) {
    if (h != CLOSED_HANDLE) {
      close_impl(h);
      h = CLOSED_HANDLE;
    }
  }
  void close_impl(handle_t h);
};

#ifdef _WIN32
void process_pipe::open_pipe() {
  SECURITY_ATTRIBUTES sa { };
  sa.nLength = sizeof(sa);
  sa.bInheritHandle = TRUE;
  sa.lpSecurityDescriptor = NULL;

  if (!CreatePipe(&rd, &wr, &sa, 64*1024)) {
    st.sys_error("CreatePipe");
    return;
  }
  /*
  // why should this be needed?
  // was RD (stdout), RD (stderr), WR (stdin)
  HANDLE i = w == which_type::STDIN ? wr : rd;
  if (!SetHandleInformation(i, HANDLE_FLAG_INHERIT, 0)) {
    error_api = "SetHandleInformation";
    error_code = last_error();
    CloseHandle(rd);
    CloseHandle(wr);
  }*/
}
void pipe_t::close_impl(handle_t h) {CloseHandle(h);}
#else // !_WIN32
void process_pipe::open_pipe() {
  int fds[2];
  if (pipe(fds) == 0) {
    rd = fds[0]; // 0 is the read end
    wr = fds[1];
  } else {
    st.sys_error("pipe");
  }
}
void process_pipe::close_impl(handle_t h) {close(h);}
#endif // !_WIN32

#ifdef _WIN32
void process_pipe::read(std::string &out)
{
  std::stringstream ss;
  while (true) {
    OVERLAPPED o;
    ZeroMemory(&o, sizeof(0));

    char buf[512];
    DWORD nr = 0;
    if (!ReadFile(rd, buf, sizeof(buf) - 1, &nr, NULL)) {
      DWORD err = GetLastError();
      if (err == ERROR_BROKEN_PIPE) {
        if (nr > 0) {
          buf[nr] = 0;
          ss << buf;
        }
      }
      error("ReadFile", (int)err);
      break;
    } else if (nr == 0) {
      break;
    } else {
      buf[nr] = 0;
      ss << buf;
    }
  }
}

void process_pipe::write(const std::string &out)
{
  const char *buf = out.c_str();
  size_t off = 0, len = out.size();
  while (off < len) {
    DWORD nw = 0;
    if (!WriteFile(
      wr,
      buf + off,
      (DWORD)(len - off),
      &nw,
      NULL))
    {
      sys_error("WriteFile");
    }
  }
}

#else // !_WIN32
void process_pipe::read(std::string &out)
{
  std::stringstream ss;
  while (true) {
    char buf[512];
    auto nr = ::read(rd, buf, sizeof(buf) - 1);
    if (nr == 0) {
      break; // eof
    } else if (nr < 0) { // error
      st.sys_error("read");
      break;
    } else {
      buf[nr] = 0;
      ss << buf;
    }
  }
  out = ss.str();
}
void process_pipe::write(const std::string &in)
{
  const char *buf = in.c_str();
  size_t off = 0, len = in.size();
  while (off < len) {
    auto nw = ::write(wr, buf + off, len - off);
    if (nw < 0) {
      st.sys_error("write");
      break;
    }
    off += nw;
  } // end while
}
#endif // !_WIN32

process_result sys::process_read(
  const std::string &exe,
  const std::vector<std::string> &args,
  const std::string &input)
{
  process_result pr;
  pr.st.context = "sys::process_read";

#ifdef _WIN32
  // https://docs.microsoft.com/en-us/windows/desktop/ProcThread/
  //        creating-a-child-process-with-redirected-input-and-output
  // surround in " if there's a space (and it's not already surrounded)
  if (exe.find(' ') != std::string::npos && exe.find('"') != 0)
    exe = '\"' + exe + '\"';

  std::string cmdline = exe;
  for (std::string a : args)
    cmdline += " " + a;

  // has to be writable...
  char *cmdline_buffer = (char *)alloca(cmdline.size() + 1024);
  memset(cmdline_buffer, 0, cmdline.size() + 1024);
  STRNCPY(cmdline_buffer,
    cmdline.size() + 1024, cmdline.c_str(), cmdline.size());

  STARTUPINFOA si { };
  si.cb = sizeof(si);
  PROCESS_INFORMATION pi { };

  SECURITY_ATTRIBUTES sa { };
  sa.nLength = sizeof(sa);
  sa.bInheritHandle = TRUE;
  sa.lpSecurityDescriptor = NULL;

  HANDLE stdout_read_handle, stdout_write_handle;
  if (!CreatePipe(&stdout_read_handle, &stdout_write_handle, &sa, 64*1024)) {
    pr.sys_error("CreatePipe(stdout_write)");
    return pr;
  }
  if (!SetHandleInformation(stdout_read_handle, HANDLE_FLAG_INHERIT, 0)) {
    pr.sys_error("SetHandleInformation(stdout_read)")
    return pr;
  }
  si.hStdOutput = stdout_write_handle;

  HANDLE stderr_read_handle, stderr_write_handle;
  if (!CreatePipe(&stderr_read_handle, &stderr_write_handle, &sa, 0)) {
    pr.sys_error("CreatePipe(stderr_write)")
    return pr;
  }
  if (!SetHandleInformation(stderr_read_handle, HANDLE_FLAG_INHERIT, 0)) {
    pr.sys_error("SetHandleInformation(stderr_read)")
    return pr;
  }
  si.hStdError = stderr_write_handle;

  HANDLE stdin_write_handle, stdin_read_handle;
  if (!CreatePipe(&stdin_read_handle, &stdin_write_handle, &sa, 0)) {
    pr.sys_error("CreatePipe(stdin_write)")
    return pr;
  }
  if (!SetHandleInformation(stdin_write_handle, HANDLE_FLAG_INHERIT, 0)) {
    pr.sys_error("SetHandleInformation(stdin_write)")
    return pr;
  }
  si.hStdInput = stdin_read_handle;
  si.dwFlags = STARTF_USESTDHANDLES;

  if (!CreateProcessA(
      NULL,
      cmdline_buffer,
      NULL,
      NULL,
      FALSE,
      0,
      NULL,
      NULL,
      &si,
      &pi))
  {
    // ours
    (void)CloseHandle(stdout_read_handle);
    (void)CloseHandle(stderr_read_handle);
    (void)CloseHandle(stdin_write_handle); // TODO: this needs to stay open so we can write out the input
    // theirs
    (void)CloseHandle(stdout_write_handle);
    (void)CloseHandle(stderr_write_handle);
    (void)CloseHandle(stdin_read_handle);
    // bail
    pr.st.sys_error("CreateProcessA");
    return pr;
  }
  // CreateProcess at least succeeded
  pr.state = process_result::process_state::OTHER;

  // we don't need the child's main thread handle
  (void)CloseHandle(pi.hThread);

  // close our copy of the pipes so that reader threads get an EOF
  (void)CloseHandle(stdout_write_handle);
  (void)CloseHandle(stderr_write_handle);
  (void)CloseHandle(stdin_read_handle);

  // spawn threads for IO
  //  std::thread thr_out(process_reader, stdout_read_handle, pr.out); // see the note above by process_io_args
  //  std::thread thr_err(process_reader, stderr_read_handle, pr.err);
  //  std::thread thr_in(process_writer, stdin_write_handle, input);
  std::thread thr_out(process_reader, process_reader_args(stdout_read_handle, pr.out));
  std::thread thr_err(process_reader, process_reader_args(stderr_read_handle, pr.err));
  std::thread thr_in(process_writer, process_writer_args(stdin_write_handle, input));

  // fetch the exit code and release the process object
  auto exit_code = WaitForSingleObject(pi.hProcess, INFINITE);
  if ((exit_code == WAIT_FAILED) || (exit_code == WAIT_ABANDONED)) {
    pr.sys_error("WaitForSingleObject:return", exit_code);
  } else {
    DWORD exit_code = 0;
    if (!GetExitCodeProcess(pi.hProcess, &exit_code)) {
      pr.sys_error("GetExitCodeProcess");
    } else {
      pr.state = process_result::process_state::EXITED;
    }
    pr.exit_code = (int)exit_code;
  }
  (void)CloseHandle(pi.hProcess);
  // wait for the IO threads to complete
  thr_in.join();
  thr_out.join();
  thr_err.join();
  // close our side of the pipes
  (void)CloseHandle(stdin_write_handle);
  (void)CloseHandle(stdout_read_handle);
  (void)CloseHandle(stderr_read_handle);
#else // Linux
  // create pipe with the various appropriate inheritence properties
  process_pipe p_in(pr.st, process_pipe::which_type::STDIN);
  if (!p_in.valid()) {
    pr.st = p_in.st;
    return pr;
  }
  process_pipe p_ou(pr.st, process_pipe::which_type::STDOUT);
  if (!p_ou.valid()) {
    pr.st = p_in.st;
    return pr;
  }
  process_pipe p_er(pr.st, process_pipe::which_type::STDERR);
  if (!p_er.valid()) {
    pr.st = p_in.st;
    return pr;
  }

  // flush output streams before the fork
  std::cout.flush();
  std::cerr.flush();

  pid_t pid = fork();
  if (pid == -1) {
    // fork error
    pr.st.sys_error("fork");
    return pr;
  } else if (pid != 0) {
    // parent
    pr.state = process_result::process_state::OTHER;
    //
    // close the pipes we don't need
    p_ou.close_wr();
    p_er.close_wr();
    p_in.close_rd();
    //
    // spawn off reader threads and writer threads
    std::thread thr_in([&] {
        p_in.write(input);
        p_in.close_wr();
        std::atomic_thread_fence(std::memory_order_release);
    });
    std::thread thr_out([&] {
        p_ou.read(pr.out);
        std::atomic_thread_fence(std::memory_order_release);
    });
    std::thread thr_err([&] {
        p_er.read(pr.err);
        std::atomic_thread_fence(std::memory_order_release);
    });

    int st = -1;
    if (waitpid(pid, &st, 0) < 0) {
      pr.st.sys_error("wait");
      // The waitpid call should never fail here, but here we are.
      // Threads are still running and are using our memory
      // (the std::thread, writing to pr).  We must not return until
      // they are definitely dead.
      //
      // It's tempting to close the pipes and hope the things come toppling down
      // so everyone can get error codes and exit, but that's unsafe.
      // Another, thread could swoop in and open a new file or socket using
      // that same descriptor.  The child's next read would be from the wrong
      // source.  Massive chaos.
      // p_in.close_both(); << DON'T DO IT BEFORE JOIN
      // p_ou.close_both(); << DON'T DO IT BEFORE JOIN
      // p_er.close_both(); << DON'T DO IT BEFORE JOIN
      std::cerr << "  [WARNING]: cls: waitpid failed: " <<
        format_last_error(pr.st.api_code) << "\n";
      // might hang, but it's the best we can do
      thr_in.join();
      thr_out.join();
      thr_err.join();
      std::cerr << "  [WARNING] joined children; we can continue\n";
      // the threads are definitely dead
    } else {
      // successful waitpid
      if (WIFEXITED(st)) {
        pr.state = process_result::process_state::EXITED;
        pr.code = WEXITSTATUS(st);
      } else if (WIFSIGNALED(st)) {
        pr.state = process_result::process_state::SIGNALED;
        pr.code = WTERMSIG(st);
      } else {
        // WIFSTOPPED/WIFCONTINUED
        pr.st.sys_error("waitpid", 0);
        pr.st.extended = "status not an exit or signal";
      }
      thr_in.join();
      thr_out.join();
      thr_err.join();
      // threads close their own pipe ends; presumably in this order
      // 1. the stdin writer finishes close stdin and begins exiting;
      //    the child's stdin closes; they get EOF and finish processing
      //    closing stdout and stderr and exiting peacefully
      // 2. the reader threads gets the EOF and closes the output pipes
      //    and exits
      // 2. waitpid returns
      // 3. we join the reader/writer threads (they are exited)
      //
      // check for errors in the child threads (only take the first)
      // fence error_code/error_api
      std::atomic_thread_fence(std::memory_order_acquire);
      if (p_in.st.has_error()) {
        pr.st = p_in.st;
        return pr;
      }
      if (p_ou.st.has_error()) {
        pr.st = p_ou.st;
        return pr;
      }
      if (p_er.st.has_error()) {
        pr.st = p_er.st;
        return pr;
      }
    }
  } else {
    // child process
    //
    // redirect file descriptors
    dup2(p_in.rd, STDIN_FILENO);
    dup2(p_ou.wr, STDOUT_FILENO);
    dup2(p_er.wr, STDERR_FILENO);
    //
    // close aliases
    p_in.close_both();
    p_ou.close_both();
    p_er.close_both();

    // create the argv array: no need to free memory
    std::vector<char *> argv;
    auto add_arg = [&](const std::string &s) {
      char* ptr = new char[s.size() + 1];
      if (ptr == nullptr) {
        std::cerr << "forked child failed to allocate memory\n";
        raise(SIGSEGV);
      }
      memcpy(ptr, s.c_str(), s.size());
      ptr[s.size()] = 0;
      argv.push_back(ptr);
    };
    add_arg(exe);
    for (const std::string &s : args) {
      add_arg(s);
    }
    argv.push_back(nullptr);

    execv(exe.c_str(), argv.data());
    // should not return
    std::cerr << "execv(" << exe << "): " <<
      format_last_error(last_error()) << "\n";
    _exit(-1);
  }
#endif
  return pr;
}

process_result sys::process_read(
  const std::string &exe,
  std::initializer_list<std::string> args)
{
  std::vector<std::string> argv;
  for (auto a : args)
    argv.push_back(a);
  return process_read(exe, argv, "");
}


///////////////////////////////////////////////////////////////////////////////
// REGISTRY
#ifdef WIN32
static bool open_reg_key(
  const char *path,
  REGSAM sec,
  HKEY &key,
  std::string &out_value)
{
  auto strip_prefix = [](const char *&str, const char *pfx) {
    size_t plen = strlen(pfx);
    if (strncmp(str, pfx, plen) == 0) {
      str += plen;
      return true;
    }
    return false;
  };

  HKEY root;
  if (strip_prefix(path,"HKLM") ||
    strip_prefix(path,"HKEY_LOCAL_MACHINE"))
  {
    root = HKEY_LOCAL_MACHINE;
  } else if (strip_prefix(path,"HKCU") ||
    strip_prefix(path,"HKEY_CURRENT_USER"))
  {
    root = HKEY_CURRENT_USER;
  } else if (strip_prefix(path,"HKCR") ||
    strip_prefix(path,"HKEY_CLASSES_ROOT"))
  {
    root = HKEY_CLASSES_ROOT;
  } else if (strip_prefix(path,"HKCC") ||
    strip_prefix(path,"HKEY_CURRENT_CONFIG"))
  {
    root = HKEY_CURRENT_CONFIG;
  } else if (strip_prefix(path,"HKU") ||
    strip_prefix(path,"HKEY_USERS"))
  {
    root = HKEY_USERS;
  } else {
    return false;
  }
  if (*path == '\\' || *path == '/')
    path++;

  std::string key_path;
  const char *value = path + strlen(path) - 1;
  while (value > path && *value != '\\' && *value != '/') {
    value--;
  }
  if (value == path)
    return false;
  value++;

  key_path = "";
  const char *k = path;
  while (k < value) {
    key_path += *k++;
  }
  if (RegOpenKeyExA(root, key_path.c_str(), 0, sec, &key) != ERROR_SUCCESS) {
    return false;
  }
  out_value = value;
  return true;
}

// looks up a dword registry key
// E.g. "HKLM/Software/Intel/KMD/DevId"
// Input is the form of
// HKLM/[path]
static bool lookup_registry_key_impl(
  const char *path, REGSAM wow3264, uint32_t &out_value)
{
  HKEY key;
  std::string value;
  if (!open_reg_key(path, KEY_READ | wow3264, key, value)) {
    return false;
  }
  DWORD type;
  DWORD tmp_out_value;
  DWORD nbytes = sizeof(tmp_out_value);
  if (RegQueryValueExA(key,
    value.c_str(),
    NULL,
    &type,
    (unsigned char *)&tmp_out_value,
    &nbytes) != ERROR_SUCCESS)
  {
    RegCloseKey(key);
    return false;
  }
  if (type != REG_DWORD) {
    WARNING("lookup_registry_key<DWORD>: %s is not a DWORD", path);
    RegCloseKey(key);
    return false;
  }
  RegCloseKey(key);
  out_value = tmp_out_value;
  return true;
}
static bool lookup_registry_key_impl(
  const char *path, DWORD wow3264, std::string &out_value)
{
  HKEY key;
  std::string value;
  if (!open_reg_key(path, KEY_READ | wow3264, key, value)) {
    return false;
  }
  DWORD type;
  DWORD nbytes;
  if (RegQueryValueExA(key, value.c_str(), NULL, &type, NULL, &nbytes) !=
    ERROR_SUCCESS)
  {
    RegCloseKey(key);
    return false;
  }
  if (type != REG_SZ) {
    WARNING("lookup_registry_key<REG_SZ>: %s is not a REG_SZ", path);
    RegCloseKey(key);
    return false;
  }
  unsigned char *data = (unsigned char *)alloca(nbytes);
  if (RegQueryValueExA(key, value.c_str(), NULL, &type, data, &nbytes) !=
    ERROR_SUCCESS)
  {
    RegCloseKey(key);
    return false;
  }
  out_value = (char *)data;
  RegCloseKey(key);
  return true;
}
#else
using REGSAM=int;
using DWORD=uint32_t;
#define KEY_WOW64_64KEY 0

static bool lookup_registry_key_impl(
  const char *, REGSAM, DWORD &)
{
  return false;
}
static bool lookup_registry_key_impl(
  const char *, DWORD, std::string &)
{
  return false;
}
#endif

bool sys::lookup_registry_key(const char *path, uint32_t &out_value)
{
  return lookup_registry_key_impl(path, 0, out_value);
}
bool sys::lookup_registry_key64(const char *path, uint32_t &out_value)
{
  return lookup_registry_key_impl(path, KEY_WOW64_64KEY, out_value);
}
bool sys::lookup_registry_key(const char *path, std::string &out_value)
{
  return lookup_registry_key_impl(path, 0, out_value);
}
bool sys::lookup_registry_key64(const char *path, std::string &out_value)
{
  return lookup_registry_key_impl(path, KEY_WOW64_64KEY, out_value);
}

std::string sys::lookup_gen_device_info()
{
  std::string str;
  // TODO: do something else for Linux
  if (!sys::lookup_registry_key64("HKLM\\Software\\Intel\\KMD\\DevId", str)) {
    // WARNING("cannot infer device without registry value"
    //  " HKLM\\Software\\Intel\\KMD\\DevId\n");
    return "unk_unk";
  } else {
    // e.g. SKL GT2 ULX MOBILE F0
    std::string dev; // e.g. "skl", "bdw"
    std::string cfg; // e.g. "gt2"

    if (str.find("ICL") != std::string::npos) {
      dev = "icl";
    } else if (str.find("CNL") != std::string::npos) {
      dev = "cnl";
    } else if (str.find("SKL") != std::string::npos) {
      dev = "skl";
    } else if (str.find("BDW") != std::string::npos) {
      dev = "bdw";
    } else if (str.find("CRW") != std::string::npos) {
      dev = "crw";
    } else if (str.find("HSW") != std::string::npos) {
      dev = "hsw";
    } else if (str.find("IVB") != std::string::npos) {
      dev = "ivb";
    } else if (str.find("SNB") != std::string::npos) {
      dev = "snb";
    } else {
      dev = "unk";
    }

    if (str.find("GT1") != std::string::npos) {
      cfg = "gt1";
    } else if (str.find("GT2") != std::string::npos) {
      cfg = "gt2";
    } else if (str.find("GT3") != std::string::npos) {
      cfg = "gt3";
    } else if (str.find("GT4") != std::string::npos) {
      cfg = "gt4";
    } else {
      cfg = "unk";
    }
    return dev + '_' + cfg;
  }
}

std::string sys::display_device()
{
  std::stringstream ss;

#ifdef _WIN32
  DISPLAY_DEVICEA dd = { 0 };
  dd.cb = sizeof(dd);

  for (DWORD ix = 0; EnumDisplayDevicesA(NULL, ix, &dd, 0); ix++) {
    if (!(dd.StateFlags & DISPLAY_DEVICE_ACTIVE)) {
      continue;
    }
    ss << "    [" << dd.DeviceName << "]  " <<
      dd.DeviceString << "; " << dd.DeviceID;

    bool first = true;
    if (dd.StateFlags & DISPLAY_DEVICE_ACTIVE) {
      if (first) {
        ss << " (";
        first = false;
      }
      else {
        ss << ", ";
      }
      ss << "active";
    }
    if (dd.StateFlags & DISPLAY_DEVICE_PRIMARY_DEVICE) {
      if (first) {
        ss << " (";
        first = false;
      }
      else {
        ss << ", ";
      }
      ss << "primary";
    }
    if (dd.StateFlags & DISPLAY_DEVICE_MIRRORING_DRIVER) {
      if (first) {
        ss << " (";
        first = false;
      }
      else {
        ss << ", ";
      }
      ss << "mirroring";
    }
    if (dd.StateFlags & DISPLAY_DEVICE_MODESPRUNED) {
      if (first) {
        ss << " (";
        first = false;
      }
      else {
        ss << ", ";
      }
      ss << "pruned";
    }
    if (!first) {
      ss << ")";
    }
    ss << "\n";
  }
#else
  // TODO: something /proc for this information?
  ss << "???";
#endif
  return ss.str();
}

#if 0
#ifdef _WIN32
void sys::hide_cursor(struct cursor_info &ci)
{
  HANDLE stdout_handle = GetStdHandle(STD_OUTPUT_HANDLE);
  if (_isatty(_fileno(stdin)) &&
    !GetConsoleCursorInfo(GetStdHandle(STD_OUTPUT_HANDLE), &ci.info))
  {
    FATAL("hide_cursor_win32: GetConsoleCursorInfo()");
  }

  CONSOLE_CURSOR_INFO new_ci = ci.info;
  new_ci.dwSize = 1;
  new_ci.bVisible = FALSE;
  if (_isatty(_fileno(stdin)) &&
    !SetConsoleCursorInfo(stdout_handle, &new_ci))
  {
    FATAL("hide_cursor_win32: SetConsoleCursorInfo()");
  }
}
void sys::restore_cursor(const struct cursor_info &ci)
{
  if (_isatty(_fileno(stdin)) &&
    !SetConsoleCursorInfo(GetStdHandle(STD_OUTPUT_HANDLE), &ci.info))
  {
    FATAL("SetConsoleCursorInfo(restore)");
  }
}

#else
void sys::hide_cursor(struct cursor_info &ci)
{
  if (!isatty(fileno(stdin)) {
    return;
  }
  printf("\e[?25l"); // hide cursor
  // \033[?17;0;0c
  // system("setterm -cursor off");
}
void sys::restore_cursor(const struct cursor_info &ci)
 {
  if (!isatty(fileno(stdin)) {
    return;
  }
  printf("\e[?25h"); // show cursor
  // system("setterm -cursor on");
}
#endif


#endif