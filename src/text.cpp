#include "fatal.hpp"
#include "system.hpp"
#include "text.hpp"

#ifdef _WIN32
#include <Windows.h>
#else
#include <sys/ioctl.h>
#endif
#include <cstdint>
#include <cstring>
#include <cctype>
#include <fstream>
#include <functional>
#include <iostream>
#include <iomanip>
#include <sstream>

bool text::is_tty(std::ostream &os)
{
  return sys::is_tty(os);
}

#ifdef _WIN32
static void enable_colored_io()
{
  static bool enabled = false;
  if (enabled)
    return;
  // TODO: should only do this on Windows 10 Threshold 2 (TH2),
  // "November Update": version 1511 and has the build number 10586
#ifndef ENABLE_VIRTUAL_TERMINAL_PROCESSING
#define ENABLE_VIRTUAL_TERMINAL_PROCESSING 0x0004
#endif
  // https://docs.microsoft.com/en-us/windows/console/setconsolemode
  // https://bugs.php.net/bug.php?id=72768
  // https://docs.microsoft.com/en-us/windows/console/console-virtual-terminal-sequences
  auto enable_on_handle = [](DWORD H_CODE) {
    DWORD mode;
    HANDLE h = GetStdHandle(H_CODE);
    GetConsoleMode(h, &mode);
    mode |= ENABLE_VIRTUAL_TERMINAL_PROCESSING;
    SetConsoleMode(h, mode);
  };
  enable_on_handle(STD_ERROR_HANDLE);
  enable_on_handle(STD_OUTPUT_HANDLE);
  enabled = true;
}

// create a static constructor
struct dummy
{
  dummy() {
    enable_colored_io();
  };
};
static dummy _dummy;
#endif



std::vector<std::string> text::to_lines(
  const std::string &str)
{
  std::vector<std::string> lns;
  if (str.empty()) {
    return lns;
  }

  std::string curr;
  for (size_t i = 0; i < str.length(); i++) {
    if (str[i] == '\n') {
      lns.push_back(curr);
      if (i == str.length() - 1) {
        return lns;
      }
    }
    else if (str[i] != '\r') {
      curr += str[i];
    }
  }
  lns.push_back(curr);
  return lns;
}

std::vector<std::string> text::to_words(const std::string &str)
{
  std::vector<std::string> ws;
  size_t off = 0;
  auto skip_spaces = [&]() {
    while (off < str.size() && isspace(str[off]))
      off++;
  };

  while (off < str.size()) {
    skip_spaces();

    size_t start = off;
    while (off < str.size() && !isspace(str[off]))
      off++;
    ws.push_back(str.substr(start, off - start));
  }
  return ws;
}

void text::prefix_lines_to(
  std::ostream &os,
  const std::string &pfx,
  const std::string &str)
{
  if (str.empty())
    return;
  os << pfx;
  for (size_t i = 0; i < str.size(); i++) {
    if (str[i] == '\n') {
      os << '\n';
      if (i == str.size() - 1) {
        return;
      }
      os << pfx;
    } else {
      os << str[i];
    }
  }
}

std::string text::prefix_lines(
  const std::string &pfx,
  const std::string &str)
{
  std::stringstream ss;
  prefix_lines_to(ss, pfx, str);
  return ss.str();
}


void text::printf_to(std::ostream &os, const char *patt, va_list va)
{
  va_list va_tmp;

  va_copy(va_tmp, va);
#ifdef _WIN32
  size_t elen = (size_t)_vscprintf(patt, va_tmp) + 1;
#else
  char dummy;
  size_t elen = (size_t)vsnprintf(&dummy, 0, patt, va_tmp) + 1;
#endif
  va_end(va_tmp);

#ifdef _WIN32
  char *ebuf = (char *)_alloca(elen);
#else
  char *ebuf = (char *)alloca(elen);
#endif
  vsnprintf(ebuf, elen, patt, va);
  ebuf[elen - 1] = 0;

  os << ebuf;
}
void text::printf_to(std::ostream &os, const char *patt, ...)
{
  va_list va;
  va_start(va, patt);
  text::printf_to(os, patt, va);
  va_end(va);
}



std::string text::printf(const char *patt, ...)
{
  std::stringstream ss;

  va_list va;
  va_start(va, patt);
  printf_to(ss, patt, va);
  va_end(va);

  return ss.str();
}

/////////////////////////////////////////////////////////////////////////////
void text::expand_bitset_to(
  std::ostream &os,
  uint64_t val,
  std::initializer_list<std::pair<uint64_t,const char *>> mappings)
{
  // check for a zero symbol or a compound match
  for (auto [v,sym] : mappings) {
    if (val == v) {
      os << sym;
      return;
    }
  }
  // no match and no mapping for 0x0 => emit 0x0
  if (val == 0) {
    os << "0x0";
    return;
  }
  // loop through bits
  bool first = true;
  auto add_sep = [&]() {
    if (first) {
      first = false;
    } else {
      os << '|';
    }
  };
  // main body of symbols
  for (auto [v,sym] : mappings) {
    if (val & v) {
      add_sep();
      os << sym;
      val &= ~v;
    }
  }
  if (val != 0) {
    add_sep();
    os << "0x" << text::hex(val, 0);
  }
}

/////////////////////////////////////////////////////////////////////////////
// integer parsing
template <typename T>
static std::optional<T>
parse_seq_integral(const char *str, const char *seps, const char **sfx) {
  if (!str || !*str)
    return std::nullopt;

  auto is_sep = [seps](char c) {
    // this is delicate
    return c && seps && *seps && std::strchr(seps, c) != nullptr;
  };
  using parse_digit_func = std::function<std::optional<T>(char)>;
  auto parse_loop = [&](T                radix,
                        parse_digit_func parse_dig) -> std::optional<T> {
    T val = 0;
    int ndigits = 0;
    while (true) {
      if (is_sep(*str)) {
#if _DEBUG
        // if seps has digits, then internally fail
        if (parse_dig(*str) != std::nullopt) {
          cls::fatal_internal("parse_seq_integral: seps overlaps with digits");
        }
#endif
        str++;
        continue;
      }
      auto odig = parse_dig(*str);
      if (!odig) {
        break;
      }
      T new_val = radix * val + *odig;
      if (new_val < val)
        return std::nullopt;
      val = new_val;
      ndigits++;
      str++;
    }
    if (ndigits == 0) // e.g. all separators or an empty string
      return std::nullopt;
    if (sfx)
      *sfx = str;
    return std::make_optional<T>(val);
  }; // parse_loop


  if (strlen(str) > 2 && str[0] == '0' && (str[1] == 'x' || str[1] == 'X')) {
    str += 2;
    auto parse_hex_digit = [](char c) -> std::optional<T> {
      T dig = 0;
      if (c >= 'a' && c <= 'f') {
        dig = (c - 'a' + 10);
      } else if (c >= 'A' && c <= 'F') {
        dig = (c - 'A' + 10);
      } else if (c >= '0' && c <= '9') {
        dig = c - '0';
      } else {
        return std::nullopt;
      }
      return dig;
    };
    return parse_loop(T(16), parse_hex_digit);
  } else {
    auto parse_dec_digit = [](char c) -> std::optional<T> {
      return (c >= '0' && c <= '9') ? std::make_optional<T>(c - '0')
                                    : std::nullopt;
    };
    return parse_loop(T(10), parse_dec_digit);
  }
} // parse_integral_seq


template <typename T>
static std::optional<T>
parse_seq_signed_integral(const char *str, const char *seps, const char **sfx)
{
  if (!str)
    return std::nullopt;
  T sign = 1;
  if (*str == '-') {
    str++;
    sign = T(-1);
  }
  if (auto ov = parse_seq_integral<T>(str, seps, sfx)) {
    return *ov * sign;
  } else {
    return std::nullopt;
  }
}

std::optional<uint64_t>
text::parse_seq_uint64(const char *str, const char *seps, const char **sfx)
{
  return parse_seq_integral<uint64_t>(str, seps, sfx);
} // parse_uint64


std::optional<int64_t>
text::parse_seq_sint64(const char *str, const char *seps, const char **sfx)
{
  return parse_seq_signed_integral<uint64_t>(str, seps, sfx);
} // parse_sint64

std::optional<uint64_t>
text::parse_uint64(const char *str, const char *seps)
{
  const char *end = nullptr;
  auto ov = parse_seq_uint64(str, seps, &end);
  if (ov && *end != '\0') {
    return std::nullopt;
  }
  return ov;
}

std::optional<int64_t>
text::parse_sint64(const char *str, const char *seps)
{
  const char *end = nullptr;
  auto ov = parse_seq_sint64(str, seps, &end);
  if (ov && *end != '\0') {
    return std::nullopt;
  }
  return ov;
}

#define TEST_INT_PARSING
#ifdef TEST_INT_PARSING
static std::string to_str(const char *s)
{
  if (!s) return "nullptr";
  std::stringstream ss;
  ss << "\"" << s << "\"";
  return ss.str();
}

template<typename T>
static void test_integral_seq_parse(
    const char *func,
    std::optional<T>(*parse_func)(const char *, const char *, const char **),
    const char      *inp,
    const char      *seps    = nullptr,
    std::optional<T> exp     = std::nullopt,
    std::string      end_exp = "")
{
  std::stringstream ss;
  ss << func << "(" << to_str(inp) << ", " << to_str(seps)
            << ", " << exp << "): ";
  const char *end_act = nullptr;
  std::cout << text::coll(ss.str(), 64);
  auto x0 = parse_func(inp, seps, &end_act);
  // special handling for nullptr case (ignore end)
  if (x0 != exp) {
    cls::fatal("FAILED: value incorrect (", x0, ")\n");
  } else if (exp && (!end_act || end_exp != end_act)) {
    // only check end pointer if parsing succeeded
    cls::fatal("FAILED: end_ptr is incorrect (", to_str(end_act), ")\n");
  }
  auto x1 = parse_func(inp, seps, nullptr);
  if (x0 != x1) {
    cls::fatal("FAILED: parses differently based on end ptr\n");
  } else {
    std::cout << "OK\n";
  }
};


void test_int_parsing()
{
  auto testS = [](const char            *inp,
                  const char            *seps    = nullptr,
                  std::optional<int64_t> exp     = std::nullopt,
                  std::string            end_exp = "") {
    test_integral_seq_parse<int64_t>(
        "parse_seq_sint64", text::parse_seq_sint64, inp, seps, exp, end_exp);
  };

  testS("0x45", "", 0x45); // unused sep is fine

  testS(nullptr, nullptr);
  testS("", nullptr); // fails
  testS("abc");
  testS("-"); // fails
  testS("4", nullptr, 4, "");
  testS("45", nullptr, 45);
  testS("0xF", nullptr, 0xF);
  testS("0xF1", nullptr, 0xF1);
  testS("0xf1aE", nullptr, 0xF1AE);
  testS("-1", nullptr, -1);
  testS("-13", nullptr, -13);
  testS("-0xA", nullptr, -0xA);
  testS("-0xA1", nullptr, -0xA1);
  testS("0xFEDCBA9876543210", nullptr, 0xFEDCBA9876543210i64);
  testS("0xFEDCBA9876543210A");
  testS("0x8000000000000000", nullptr, 0x8000000000000000i64);
  testS("0x7FFFFFFFFFFFFFFF", nullptr, 0x7FFFFFFFFFFFFFFFi64);
  testS("-0x7FFFFFFFFFFFFFFF", nullptr, -0x7FFFFFFFFFFFFFFFi64);
  testS("-0x8000000000000000", nullptr, -0x8000000000000000i64);
  // suffixes
  testS("4 K", nullptr, 4, " K");
  testS("-0x45, abc", nullptr, -0x45, ", abc");
  // separators
  testS("45", "", 45); // empty string seps is same as nullptr
  testS("45", "_", 45); // unused sep is fine
  testS("_", "_"); // only seps
  testS("_4_5___", "_", 45); // used seps (anywhere)
  testS("-_4`5___", "`_", -45); // multiple seps
  testS("0x_4`5___", "`_", 0x45); // multiple seps hex

  auto testU = [](const char            *inp,
                  const char            *seps    = nullptr,
                  std::optional<int64_t> exp     = std::nullopt,
                  std::string            end_exp = "") {
    test_integral_seq_parse<uint64_t>(
        "parse_seq_uint64", text::parse_seq_uint64, inp, seps, exp, end_exp);
  };

  testU(nullptr);
  testU("");
  testU("-");
  testU("-1");
  testU("-13");
  testU("4", nullptr, 4);
  testU("45", nullptr, 45);
  testU("-0xA", nullptr);
  testU("0xA", nullptr, 0xA);
  testU("0xAB1", nullptr, 0xAB1);
  testU("0xFEDCBA9876543210", nullptr, 0xFEDCBA9876543210ull);
  testU("0xFEDCBA9876543210A", nullptr);
  // suffixes
  testU("4 K", nullptr, 4, " K");
  testU("0x4, abc", nullptr, 0x4, ", abc");
  // separators
  testS("45", "", 45); // empty string seps is same as nullptr
  testS("45", "_", 45); // unused sep is fine
  testS("_", "_"); // only seps
  testS("_4_5___", "_", 45); // used seps (anywhere)
  testS("_4`5___", "`_", 45); // multiple seps
  testS("0x_4`5___", "`_", 0x45); // multiple seps hex
} // test_int_parsing
#endif // TEST_INT_PARSING


void text::format_buffer(
  std::ostream &os,
  const void *buf,      // buffer base pointer
  size_t buf_len,       // the total size of the buffer
  size_t elem_wbytes,   // width per work item element in bytes: 8 for cl_float2
  size_t elem_vwidth,   // vector size of each work element: 2 for cl_float2
  bool floating_point,  // if we should show floating-point values
  size_t max_cols,      // preferred size in columns to use in rendering
  size_t elems_per_row) // preferred elements per row
{
  bool using_cols = elems_per_row == 0;
  if (max_cols != 0 && elems_per_row != 0) {
    cls::fatal("text::format_buffer can only accept maxcols xor elems_per_row");
  }
  else if (max_cols == 0 && elems_per_row == 0) {
    max_cols = sys::get_terminal_width();
    if (max_cols <= 1) {
      max_cols = 80;
    }
    else {
      max_cols -= 1;
    }
  }
  const size_t velem_w = elem_wbytes / elem_vwidth;
  if (elem_wbytes % elem_vwidth != 0 ||
    velem_w != 1 && velem_w != 2 &&
    velem_w != 4 && velem_w != 8)
  {
    // we fallback on a dumb hex dump for shapes we don't understand
    text::format_buffer(os, buf, buf_len, 1, 1, floating_point, max_cols);
    return;
  }

  const unsigned char *ptr = (const unsigned char *)buf,
    *base = (const unsigned char *)buf;
  size_t col_ix = 0, elem_col_ix = 0;

  auto emit_off = [&]() {
    os << text::fmt_hex((unsigned)(ptr - base), 4) << ':';
    col_ix += 5;
  };
  auto newline = [&]() {
    os << '\n';
    col_ix = 0;
    elem_col_ix = 0;
    emit_off(); // [addr]: ...
  };
  auto emit_velem = [&] {
    if (ptr >= (const unsigned char *)buf + buf_len)
      return;
    if (velem_w == 1) {
      os << std::setw(2 * velem_w) << (unsigned)(*(uint8_t *)ptr);
    }
    else if (velem_w == 2) {
      os << std::setw(2 * velem_w) << *(uint16_t *)ptr;
    }
    else if (velem_w == 4) {
      if (floating_point) {
        os << std::setw(12) << std::setprecision(6) << *(float *)ptr;
      }
      else {
        os << std::setw(2 * velem_w) << *(uint32_t *)ptr;
      }
    }
    else if (velem_w == 8) {
      if (floating_point) {
        os << std::setw(12) << std::setprecision(7) << *(double *)ptr;
      }
      else {
        os << std::setw(2 * velem_w) << *(uint64_t *)ptr;
      }
    }
    col_ix += 2 * velem_w;
    ptr += velem_w;
  };
  auto emit_work_item = [&] {
    if (elem_vwidth <= 1) {
      if (using_cols && col_ix + 2 * velem_w + 1 > max_cols
        || !using_cols && elem_col_ix == elems_per_row)
      {
        newline();
      }
      os << ' '; col_ix++;
      emit_velem();
    }
    else {
      // " {" + v * elem_width + interspacing + 1
      size_t space_needed = 2 + 2 * velem_w*elem_vwidth + (elem_vwidth - 1) + 1;
      if (using_cols && col_ix + space_needed >= max_cols - 1 ||
        !using_cols && elem_col_ix == elems_per_row) {
        // col_ix + [space used in output + interspacing + {} and space]
        // insufficient space on this line for this element
        newline();
      }
      os << " (";
      emit_velem();
      for (size_t i = 1; i < elem_vwidth; i++) {
        os << ' '; col_ix++;
        emit_velem();
      }
      os << ')';
      col_ix += 4;
    }
  };
  // only if buffer non-zero
  if (ptr < (const unsigned char *)buf + buf_len) {
    emit_off();
    while (ptr < (const unsigned char *)buf + buf_len) {
      emit_work_item();
      elem_col_ix++;
    }
  }
  else {
    os << "*** zero-length buffer ****";
  }
  os << '\n';
}

std::string text::format_buffer_diff(
  const void *b,
  const void *r,
  size_t n_elems,
  size_t elem_w,
  unsigned cols,
  bool show_chars)
{
  std::stringstream ss;
  ss << std::hex << std::setfill('0');
  if (elem_w != 1 && elem_w != 2 && elem_w != 4 && elem_w != 8)
    return format_buffer_diff(b, r, n_elems * elem_w, 1, cols, show_chars);

  cols = cols == 0 ? 16 : cols;
  if (cols == 0)
    cols = (elem_w == 1 || elem_w == 2) ? 16 :
    elem_w == 4 ? 8 : elem_w == 8 ? 4 : 8;
  uint8_t *buf = (uint8_t *)b, *ref = (uint8_t *)r;
  auto emit_elem = [&](uint8_t *arr, size_t i) {
    void *ptr = arr + i * elem_w;
    if (elem_w == 1) {
      ss << std::setw(2 * elem_w) << (unsigned)(*(uint8_t *)ptr);
    }
    else if (elem_w == 2) {
      ss << std::setw(2 * elem_w) << *(uint16_t *)ptr;
    }
    else if (elem_w == 4) {
      ss << std::setw(2 * elem_w) << *(uint32_t *)ptr;
    }
    else if (elem_w == 8) {
      ss << std::setw(2 * elem_w) << *(uint64_t *)ptr;
    }
  };
  auto elems_match = [&](size_t i) {
    return !ref || memcmp(buf + i * elem_w, ref + i * elem_w, elem_w) == 0;
  };

  for (size_t i = 0; i < n_elems; i++) {
    if (!elems_match(i)) {
      ss << ANSI_RED;
      emit_elem(buf, i);
      ss << ANSI_RESET;
    }
    else {
      emit_elem(buf, i);
    }
    if (i % cols == cols - 1 || i == n_elems - 1) {
      if (elem_w == 1 && show_chars) {
        ss << ' ';
        for (size_t k = i - cols + 1; k <= i; k++)
        {
          if (ref && buf[k] != ref[k])
            ss << ANSI_RED;

          if (isprint(buf[k]))
            ss << buf[k];
          else
            ss << '.';

          if (ref && buf[k] != ref[k])
            ss << ANSI_RESET;
        }
      }
      if (ref) {
        ss << " | ";
        for (size_t k = i - cols + 1; k <= i; k++) {
          if (!elems_match(k)) {
            ss << ANSI_GREEN;
            emit_elem(ref, k);
            ss << ANSI_RESET;
          }
          else {
            emit_elem(ref, k);
          }
          ss << " ";
        }
        if (elem_w == 1 && show_chars) {
          for (size_t k = i - cols + 1; k <= i; k++) {
            if (ref && buf[k] != ref[k])
              ss << ANSI_GREEN;

            if (isprint(buf[k]))
              ss << ref[k];
            else
              ss << '.';

            if (ref && buf[k] != ref[k])
              ss << ANSI_RESET;
          }
        }
      }
      ss << '\n';
    }
    else {
      ss << ' ';
    }
  }
  return ss.str();
}


static std::string search_env(const char *exe)
{
  return sys::find_exe(exe);
}

#define RETURN_IF_EXISTS(X) \
  do {std::string s = (X); if (sys::file_exists(s)) return s;} while(0)

static std::string find_gnu_cpp()
{
#ifndef _WIN32
  RETURN_IF_EXISTS("/usr/bin/cpp");
#endif
  return search_env("cpp");
}
#ifdef _WIN32
static std::string find_msvc()
{
  // VS2017 and VS2019 have a versioned toolchain in the link
  // Try for VS2019
  // C:\Program Files (x86)\Microsoft Visual Studio\2019\Professional\VC\Tools\MSVC\14.14.26428\bin\HostX86\x86\cl.exe
  //                                                                                ^^^^^^^^^^^
  const char *msvc_2022_prof_root =
    "C:\\Program Files\\Microsoft Visual Studio\\2022\\Professional\\VC\\Tools\\MSVC\\";
  if (sys::directory_exists(msvc_2022_prof_root)) {
    for (auto &p : sys::list_directory_full_paths(msvc_2022_prof_root)) {
      RETURN_IF_EXISTS(p + "\\bin\\HostX64\\x64\\cl.exe");
      RETURN_IF_EXISTS(p + "\\bin\\HostX86\\x86\\cl.exe");
    }
  }
  const char *msvc_2022_comm_root =
    "C:\\Program Files\\Microsoft Visual Studio\\2022\\Community\\VC\\Tools\\MSVC\\";
  if (sys::directory_exists(msvc_2022_comm_root)) {
    for (auto &p : sys::list_directory_full_paths(msvc_2022_comm_root)) {
      RETURN_IF_EXISTS(p + "\\bin\\HostX64\\x64\\cl.exe");
      RETURN_IF_EXISTS(p + "\\bin\\HostX86\\x86\\cl.exe");
    }
  }
  const char *msvc_2019_prof_root =
    "C:\\Program Files (x86)\\Microsoft Visual Studio\\2019\\Professional\\VC\\Tools\\MSVC\\";
  if (sys::directory_exists(msvc_2019_prof_root)) {
    for (auto &p : sys::list_directory_full_paths(msvc_2019_prof_root)) {
      RETURN_IF_EXISTS(p + "\\bin\\HostX64\\x64\\cl.exe");
      RETURN_IF_EXISTS(p + "\\bin\\HostX86\\x86\\cl.exe");
    }
  }
  const char *msvc_2019_comm_root =
    "C:\\Program Files (x86)\\Microsoft Visual Studio\\2019\\Community\\VC\\Tools\\MSVC\\";
  if (sys::directory_exists(msvc_2019_comm_root)) {
    for (auto &p : sys::list_directory_full_paths(msvc_2019_comm_root)) {
      RETURN_IF_EXISTS(p + "\\bin\\HostX64\\x64\\cl.exe");
      RETURN_IF_EXISTS(p + "\\bin\\HostX86\\x86\\cl.exe");
    }
  }
  //
  // Try VS2017
  const char *msvc_2017_prof_root =
    "C:\\Program Files (x86)\\Microsoft Visual Studio\\2017\\Professional\\VC\\Tools\\MSVC\\";
  if (sys::directory_exists(msvc_2017_prof_root)) {
    for (auto &p : sys::list_directory_full_paths(msvc_2017_prof_root)) {
      RETURN_IF_EXISTS(p + "\\bin\\HostX64\\x64\\cl.exe");
      RETURN_IF_EXISTS(p + "\\bin\\HostX86\\x86\\cl.exe");
    }
  }
  const char *msvc_2017_comm_root =
    "C:\\Program Files (x86)\\Microsoft Visual Studio\\2017\\Community\\VC\\Tools\\MSVC\\";
  if (sys::directory_exists(msvc_2017_comm_root)) {
    for (auto &p : sys::list_directory_full_paths(msvc_2017_comm_root)) {
      RETURN_IF_EXISTS(p + "\\bin\\HostX64\\x64\\cl.exe");
      RETURN_IF_EXISTS(p + "\\bin\\HostX86\\x86\\cl.exe");
    }
  }
  //
  // Try older VS versions
  RETURN_IF_EXISTS("C:\\Program Files (x86)\\Microsoft Visual Studio 14.0\\VC\\BIN\\cl.exe");
  RETURN_IF_EXISTS("C:\\Program Files (x86)\\Microsoft Visual Studio 12.0\\VC\\BIN\\cl.exe");
  // VS2012 requires .bat setup script (missing DLL otherwise)
  // RETURN_IF_EXISTS("C:\\Program Files (x86)\\Microsoft Visual Studio 11.0\\VC\\BIN\\cl.exe");

  return search_env("cl");
}
#endif
static std::string find_clang()
{
#ifdef _WIN32
  RETURN_IF_EXISTS("C:\\Program Files\\LLVM\\bin\\clang++.exe");
  RETURN_IF_EXISTS("C:\\Program Files\\LLVM\\bin\\clang.exe");
  RETURN_IF_EXISTS("C:\\Program Files (x86)\\LLVM\\bin\\clang++.exe");
  RETURN_IF_EXISTS("C:\\Program Files (x86)\\LLVM\\bin\\clang.exe");
  //  RETURN_IF_EXISTS("C:\\Progra~1\\LLVM\\bin\\clang.exe");
  //  RETURN_IF_EXISTS("C:\\Progra~2\\LLVM\\bin\\clang.exe");
#else
  RETURN_IF_EXISTS("/usr/bin/clang++");
  RETURN_IF_EXISTS("/usr/bin/clang");
#endif
  return search_env("clang");
}

static std::string ppc_exe;

static const std::string &find_cpp()
{
  if (!ppc_exe.empty())
    return ppc_exe;
#ifdef _WIN32
  if (ppc_exe.empty())
    ppc_exe = find_msvc();
#endif
  if (ppc_exe.empty())
    ppc_exe = find_clang();
  if (ppc_exe.empty()) // even Win32, we might find it in %PATH%
    ppc_exe = find_gnu_cpp();

  return ppc_exe;
}

std::string text::cpp_result::status_to_string() const {
  switch (result) {
  case status::SUCCESS: return "success";
  case status::FILE_NOT_FOUND: return "failed to find the input file";
  case status::CPP_NOT_FOUND: return "was not found";
  case status::CPP_FAILED: return "failed";
  default: return "???";
  }
}

text::cpp_result text::load_c_preprocessed(
  const std::string &inp_path,
  const std::string &args)
{
  auto pp_exe = find_cpp();
  // start searching $PATH
  if (pp_exe.empty())
    pp_exe = sys::find_exe("cpp");
  if (pp_exe.empty())
    pp_exe = sys::find_exe("clang");
  if (pp_exe.empty())
    pp_exe = sys::find_exe("clang++");
  if (pp_exe.empty())
    return cpp_result(cpp_result::status::CPP_NOT_FOUND, "cpp");

  return load_c_preprocessed_using(pp_exe, inp_path, args);
}

text::cpp_result text::load_c_preprocessed_using(
  const std::string &cpp_path,
  const std::string &inp_path,
  const std::string &args)
{
  // MSVC
  // this works all the way back to VS2013 at least
  // % cl -E examples\vec.cl -D TYPE=int
  // to a file
  // % cl /E examples\vec.cl /P /Fi:file.ppc -D TYPE=int
  //
  // CLANG/CPP
  // % cpp -E file.cl
  // % cpp -E file.cl -DTYPE=int -o out.ppc
  std::vector<std::string> p_args {
    inp_path,
    "-E", // works for MSVC too
  };
  // add all preprocessor argument tokens
  //    -D foo=bar
  // this works on all preprocessors including cl.exe
  for (auto &w : text::to_words(args)) {
    p_args.push_back(w);
  }

  // using stdout doesn't seem to work; so we create a file and cpp into that
  sys::status st;
  auto tmp_file = sys::get_temp_path(".ppc", st);
  if (st) {
    return cpp_result(cpp_result::status::CPP_FAILED, cpp_path,
      text::format("failed to create temporary file for cpp: " + st.str()));
  }
  if (cpp_path.find("cl.exe") != std::string::npos) {
    p_args.push_back("/P");
    p_args.push_back("/Fi:" + tmp_file);
  } else {
    p_args.push_back("-o");
    p_args.push_back(tmp_file);
  }

  auto pr = sys::process_read(cpp_path, p_args);
  if (!pr.started()) {
    return cpp_result(cpp_result::status::CPP_FAILED, cpp_path,
      text::format("failed to start child process: ", pr.st.str()));
  } else if (!pr.succeeded()) {
    if (pr.exited() || pr.signaled()) {
      return cpp_result(cpp_result::status::CPP_FAILED, cpp_path,
        text::format("child process exited/signaled: ", pr.code, "\n",
          pr.err));
    } else {
      return cpp_result(cpp_result::status::CPP_FAILED, cpp_path,
        text::format("error reading child process output: ", pr.st.str(), "\n",
          pr.err));
    }
  } else {
    // the file read could fail here; we're gonna cross our fingers for now
    auto out = sys::read_file_text(tmp_file);
    auto st = remove(tmp_file.c_str());
    if (st != 0) {
      WARNING("text::load_c_preprocessed_using: failed to remove temp file %s",
        tmp_file.c_str());
    }
    return cpp_result(cpp_result::status::SUCCESS, cpp_path, out);
  }
}

void text::table::col::emit(double f, int prec) {
  std::stringstream ss;
  if (numeric) { ss << std::left; }
  else { ss << std::right; }
  for (int i = 0; i < lpad; i++)
    ss << ' ';
  ss << std::fixed << std::setprecision(prec) << f;
  for (int i = 0; i < rpad; i++)
    ss << ' ';
  rows.push_back(ss.str());
  max_width = std::max<size_t>(max_width, rows.back().size());
}

void text::table::str(std::ostream &os, const char *delim) const {
  os << std::setfill(' ');
  size_t max_rows = 0;
  for (const auto *c : cols) {
    max_rows = std::max<size_t>(max_rows, c->rows.size());
  }
  for (size_t i = 0; i < max_rows; i++) {
    if (i > 0)
      os << delim;
    for (const auto *c : cols) {
      auto align = c->numeric ? std::right : std::left;
      if (i >= c->rows.size()) {
        os << align << std::setw(c->max_width) << c->dft;
      }
      else {
        os << align << std::setw(c->max_width) << c->rows[i];
      }
      os << delim;
    }
    os << '\n';
  }
}

std::ostream &operator <<(std::ostream &os, text::hex h) {
  std::stringstream ss;
  ss << std::setw(h.columns) <<
    std::setfill('0') << std::hex << std::uppercase << h.value;
  os << ss.str();
  return os;
}
std::ostream &operator <<(std::ostream &os, text::frac f) {
  std::stringstream ss;
  if (f.columns >= 0)
    ss << std::setw(f.columns);
  ss << std::setprecision(f.prec) << std::fixed;
  if (f.tag == text::frac::F32)
    ss << f.f32;
  else if (f.tag == text::frac::F64)
    ss << f.f64;
  else
    ss << "??";
  os << ss.str();
  return os;
}
std::ostream &operator <<(std::ostream &os, const text::ansi &e)
{
  if (sys::is_tty(os))
    os << e.esc;
  return os;
}
std::ostream &operator <<(std::ostream &os, const text::ansi_literal &e)
{
  if (e.esc && sys::is_tty(os))
    os << e.esc;
  return os;
}
