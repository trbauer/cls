#include "system.hpp"
#include "text.hpp"

#include <fstream>
#include <iostream>
#include <mutex>
#include <sstream>
#include <experimental/filesystem>
namespace fs = std::experimental::filesystem;

#ifdef _WIN32
#include <Windows.h>
#include <io.h>
#define IS_STDERR_TTY (_isatty(_fileno(stderr)) != 0)
#define IS_STDOUT_TTY (_isatty(_fileno(stdout)) != 0)
#else
#include <unistd.h>
#include <sys/ioctl.h>
#define IS_STDERR_TTY (isatty(STDERR_FILENO) != 0)
#define IS_STDOUT_TTY (isatty(STDOUT_FILENO) != 0)
#endif




using namespace sys;

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
  struct winsize max;
  ioctl(0, TIOCGWINSZ , &max);
  return max.ws_col;
#endif
}


///////////////////////////////////////////////////////////////////////////////
// LOGGING AND EXIT
void sys::debug_break() {
  DebugBreak();
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
  size_t elen = _vscprintf(patt, va) + 1;
  va_end(va);
  char *ebuf = (char *)_alloca(elen);
  va_start(va, patt);
  vsnprintf(ebuf, elen, patt, va);
  ebuf[elen - 1] = 0;
  va_end(va);

  if (this_level > 0)
   OutputDebugStringA(ebuf);

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
    if (IsDebuggerPresent()) {
	    sys::debug_break();
    }
    exit(-1);
//  abort();
}

///////////////////////////////////////////////////////////////////////////////
// FILE SYSTEM
bool sys::file_exists(const std::string &path) {
  return file_exists(path.c_str());
}
bool sys::file_exists(const char *path)
{
#ifdef _WIN32
  //  WIN32_FIND_DATAA ffd;
  //  HANDLE h = FindFirstFileA(tmp_daf.c_str(), &ffd);
  //  if (h == INVALID_HANDLE_VALUE) {
  //    FATAL("kdc -off failed to generate .daf file (is DafEnable set?)\n");
  //  }
  //  FindClose(h);
  DWORD dwAttrib = GetFileAttributesA(path);
  return (dwAttrib != INVALID_FILE_ATTRIBUTES &&
         !(dwAttrib & FILE_ATTRIBUTE_DIRECTORY));
#else
  struct stat sb = {0};
  if (stat(path, &sb)) {
      return false;
  }
  return S_ISREG(sb.st_mode);
#endif
}

bool sys::directory_exists(const std::string &path)
{
#ifdef _WIN32
  //  WIN32_FIND_DATAA ffd;
  //  HANDLE h = FindFirstFileA(tmp_daf.c_str(), &ffd);
  //  if (h == INVALID_HANDLE_VALUE) {
  //    FATAL("kdc -off failed to generate .daf file (is DafEnable set?)\n");
  //  }
  //  FindClose(h);
  DWORD dwAttrib = GetFileAttributesA(path.c_str());
  return (dwAttrib != INVALID_FILE_ATTRIBUTES &&
         (dwAttrib & FILE_ATTRIBUTE_DIRECTORY));
#else
  struct stat sb = {0};
  if (stat(path, &sb)) {
      return false;
  }
  return S_ISDIR(sb.st_mode);
#endif
}

std::vector<std::string> sys::get_directory_contents(const std::string &path)
{
  std::vector<std::string> elems;
  for (auto &p : fs::directory_iterator(path))
    elems.push_back(p.path().string());
  return elems;
}

std::string sys::get_temp_dir()
{
  std::string path;
#ifdef _WIN32
  DWORD tmpdirlen = GetTempPathA(0, NULL);
  char *tmp_path = (char *)alloca(tmpdirlen + 1);
  (void)GetTempPathA(tmpdirlen, tmp_path);
  tmp_path[tmpdirlen] = 0;
  path = tmp_path;
#else
  const char *e = getenv("TEMP");
  if (!e) {
    e = getenv("TMP");
  }
  path = e;
#endif
  if (!path.empty() && path[path.size() - 1] != path_separator) {
    path += path_separator;
  }
  return path;
}

static std::mutex temp_path_mutex;

std::string sys::get_temp_path(const char *sfx)
{
#ifdef _WIN32
  int pid = (int)GetCurrentProcessId();
#else
  int pid = (int)getpid();
#endif
  std::stringstream ss;
  auto exe = get_main_executable();
  // foo/bar/baz.exe => foo/bar/baz
  auto ext = exe.rfind('.');
  if (ext != std::string::npos) {
    exe = exe.substr(0,ext);
  }
  // foo/bar/baz => baz
  size_t last_slash = exe.size() - 1;
  while (last_slash > 0) {
    if (exe[last_slash] == '\\' || exe[last_slash] == '/') {
      exe = exe.substr(last_slash+1);
      break;
    } else {
      last_slash--;
    }
  }

  ss << exe << "_" << pid;
  std::string pfx = get_temp_dir() + sys::path_separator + ss.str() + "_";

  // protect this part under a mutex
  std::lock_guard<std::mutex> guard(temp_path_mutex);
  static int i = 0;
  while (1) {
    std::stringstream ss;
    ss << pfx << std::setw(4) << std::setfill('0') << i++;
    ss << "_" << sfx;
    std::string file = ss.str();
    if (!file_exists(file.c_str())) {
      // technically should touch the path to create file, but we take a
      // weak assumption that we're the only process modifying this file
      // currently (pretty safe given that we use a pid in it and a
      // monotonically increasing integer)
      return file;
    }
  }
}

std::string sys::get_main_executable()
{
  char buffer[MAX_PATH + 1] = {0};
  if (GetModuleFileNameA(NULL, buffer, sizeof(buffer)) == sizeof(buffer)) {
    // overflow
  }
  return std::string(buffer);
}

std::string sys::find_exe(const char *exe)
{
  std::string filename_exe = exe;
#ifdef _WIN32
  // "foo" -> "foo.exe"
  if (strlen(exe) > 4 && strncmp(".exe",exe + strlen(exe) - 4,4) != 0) {
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
    if (sys::file_exists(filename_exe))
      return filename_exe;
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


///////////////////////////////////////////////////////////////////////////////
// PROCESS CREATION
static void process_reader(HANDLE read_handle, std::string &save_target)
{
  std::stringstream ss;

  while (true) {
    OVERLAPPED o;
    ZeroMemory(&o, sizeof(0));

    char buf[513];
    DWORD nr = 0;
    if (!ReadFile(read_handle, buf, sizeof(buf) - 1, &nr, NULL)) {
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

  save_target = ss.str();
}
static void process_writer(HANDLE write_handle, std::string data)
{
  const char *buf = data.c_str();
  size_t off = 0;
  while (off < data.size()) {
    DWORD nw = 0;
    if (!WriteFile(
      write_handle,
      buf + off,
      (DWORD)(data.size() - off),
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

process_result sys::process_read(
  std::string exe,
  std::vector<std::string> args,
  std::string input)
{
  // https://docs.microsoft.com/en-us/windows/desktop/ProcThread/creating-a-child-process-with-redirected-input-and-output
  process_result pr;
  memset(&pr, 0, sizeof(pr));

  // surround in " if there's a space (and it's not already surrounded)
  if (exe.find(' ') != std::string::npos && exe.find('"') != 0)
    exe = '\"' + exe + '\"';

  std::string cmdline = exe;
  for (std::string a : args)
    cmdline += " " + a;
	// has to be writable...
	char *cmdline_buffer = (char *)alloca(cmdline.size() + 1024);
	memset(cmdline_buffer, 0, cmdline.size() + 1024);
  STRNCPY(cmdline_buffer, cmdline.size() + 1024, cmdline.c_str(), cmdline.size());

	STARTUPINFOA si;
	PROCESS_INFORMATION pi;
	ZeroMemory(&si, sizeof(si));
	si.cb = sizeof(si);
	ZeroMemory(&pi, sizeof(pi));

  SECURITY_ATTRIBUTES sa;
  ZeroMemory(&sa,sizeof(sa));
  sa.nLength = sizeof(sa);
  sa.bInheritHandle = TRUE;
  sa.lpSecurityDescriptor = NULL;

  HANDLE stdout_read_handle, stdout_write_handle;
  if (!CreatePipe(&stdout_read_handle,&stdout_write_handle,&sa,64*1024)) {
    FATAL("process_read: failed to create pipe");
  }
  if (!SetHandleInformation(stdout_read_handle, HANDLE_FLAG_INHERIT,0)) {
    FATAL("process_read: set inherit flag");
  }
  si.hStdOutput = stdout_write_handle;

  HANDLE stderr_read_handle, stderr_write_handle;
  if (!CreatePipe(&stderr_read_handle,&stderr_write_handle,&sa,0)) {
    FATAL("process_read: failed to create pipe");
  }
  if (!SetHandleInformation(stderr_write_handle, HANDLE_FLAG_INHERIT,0)) {
    FATAL("process_read: set inherit flag");
  }
  si.hStdError = stderr_write_handle;

  HANDLE stdin_write_handle, stdin_read_handle;
  if (!CreatePipe(&stdin_read_handle,&stdin_write_handle,&sa,0)) {
    FATAL("process_read: failed to create pipe");
  }
  if (!SetHandleInformation(stdin_write_handle, HANDLE_FLAG_INHERIT,0)) {
    FATAL("process_read: set inherit flag");
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
		auto err = GetLastError();
	  pr.startup_status =  (int)err;
	  // if (err == ERROR_FILE_NOT_FOUND) {
  	// 	WARNING("exec_process: cpp.exe not found in %%PATH%%\n");
		// } else {
		// 	WARNING("exec_process: CreateProcess failed (GLE: %d)\n", (int)err);
		//  }
    // close the pipes (both sides)
    // ours
    (void)CloseHandle(stdout_read_handle);
    (void)CloseHandle(stderr_read_handle);
    (void)CloseHandle(stdin_write_handle);
    // theirs
    (void)CloseHandle(stdout_write_handle);
    (void)CloseHandle(stderr_write_handle);
    (void)CloseHandle(stdin_read_handle);
    // bail
		return pr;
	}
  // CreateProcess at least succeeded
  pr.startup_status = 0;

  // we don't need the child's main thread handle
  (void)CloseHandle(pi.hThread);

  // close our copy of the pipes so that reader threads get an EOF
  (void)CloseHandle(stdout_write_handle);
  (void)CloseHandle(stderr_write_handle);
  (void)CloseHandle(stdin_read_handle);

  // spawn threads for IO
  std::thread thr_out(process_reader, stdout_read_handle, pr.out);
  std::thread thr_err(process_reader, stderr_read_handle, pr.err);
  std::thread thr_in(process_writer, stdin_write_handle, input);

  // fetch the exit code and release the process object
	auto exit_code = WaitForSingleObject(pi.hProcess, INFINITE);
	if ((exit_code == WAIT_FAILED) || (exit_code == WAIT_ABANDONED)) {
		WARNING("read_process(%s): unable to wait for process\n", exe.c_str());
    pr.exit_code = -1;
	} else {
		DWORD exit_code = 0;
		(void)GetExitCodeProcess(pi.hProcess, &exit_code);
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

  return pr;
}
process_result sys::process_read(
  std::string exe,
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