#include "system.hpp"
#include "text.hpp"

#ifdef _WIN32
#include <Windows.h>
#else
#include <sys/ioctl.h>
#endif
#include <cstdint>
#include <cctype>
#include <fstream>
#include <iostream>
#include <iomanip>
#include <sstream>

std::ostream &operator <<(std::ostream &os, text::ansi e)
{
  if (sys::is_tty(os))
    os << e.esc;
  return os;
}
std::ostream &operator <<(std::ostream &os, text::ansi_literal e)
{
  if (sys::is_tty(os))
    os << e.esc;
  return os;
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
  auto enableOnHandle = [](DWORD H_CODE) {
    DWORD mode;
    HANDLE h = GetStdHandle(H_CODE);
    GetConsoleMode(h, &mode);
    mode |= ENABLE_VIRTUAL_TERMINAL_PROCESSING;
    SetConsoleMode(h, mode);
  };
  enableOnHandle(STD_ERROR_HANDLE);
  enableOnHandle(STD_OUTPUT_HANDLE);
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
    } else if (str[i] != '\r') {
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
  auto skipSpaces = [&]() {
    while (off < str.size() && isspace(str[off]))
      off++;
  };

  while (off < str.size()) {
    skipSpaces();

    size_t start = off;
    while (off < str.size() && !isspace(str[off]))
      off++;
    ws.push_back(str.substr(off,off - start));
  }
  return ws;
}


std::string text::prefix_lines(
  const std::string &pfx,
  const std::string &str)
{
  std::stringstream ss;
  if (str.empty())
    return "";

  ss << pfx;
  for (size_t i = 0; i < str.length(); i++) {
    if (str[i] == '\n') {
      ss << std::endl;
      if (i == str.length() - 1) {
        return ss.str();
      }
      ss << pfx;
    } else {
      ss << str[i];
    }
  }
  return ss.str();
}


void text::printf_to(std::ostream &os, const char *patt, va_list va)
{
  va_list va_tmp;

  va_copy(va_tmp,va);
  size_t elen = _vscprintf(patt, va_tmp) + 1;
  va_end(va_tmp);

  char *ebuf = (char *)_alloca(elen);

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

std::string text::printf(std::ostream &os, const char *patt, ...)
{
  std::stringstream ss;

  va_list va;
  va_start(va, patt);
  printf_to(os, patt,va);
  va_end(va);

  return ss.str();
}

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
    FATAL("FormatBinary can only accept maxcols xor elems_per_row");
  } else if (max_cols == 0 && elems_per_row == 0) {
    max_cols = sys::get_terminal_width();
    if (max_cols <= 1) {
      max_cols = 80;
    } else {
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
    os << std::hex << std::setfill('0');
    os << std::setw(4) << (unsigned)(ptr - base) << ':';
    col_ix += 5;
    if (floating_point) {
      os << std::setfill(' ');
    }
  };
  auto newline = [&]() {
    os << '\n';
    col_ix = 0;
    elem_col_ix = 0;
    emit_off(); // [addr]: ...
  };
  auto emit_velem = [&]() {
    if (ptr >= (const unsigned char *)buf + buf_len)
      return;
    if (velem_w == 1) {
      os << std::setw(2 * velem_w) << (unsigned)(*(uint8_t *)ptr);
    } else if (velem_w == 2) {
      os << std::setw(2 * velem_w) << *(uint16_t *)ptr;
    } else if (velem_w == 4) {
      if (floating_point) {
        os << std::setw(12) << std::setprecision(6) << *(float *)ptr;
      } else {
        os << std::setw(2 * velem_w) << *(uint32_t *)ptr;
      }
    } else if (velem_w == 8) {
      if (floating_point) {
        os << std::setw(12) << std::setprecision(7) << *(double *)ptr;
      } else {
        os << std::setw(2 * velem_w) << *(uint64_t *)ptr;
      }
    }
    col_ix += 2 * velem_w;
    ptr += velem_w;
  };
  auto emit_work_item = [&]() {
    if (elem_vwidth <= 1) {
      if (using_cols && col_ix + 2*velem_w + 1 > max_cols
        || !using_cols && elem_col_ix == elems_per_row)
      {
        newline();
      }
      os << ' '; col_ix++;
      emit_velem();
    } else {
      // " {" + v * elem_width + interspacing + 1
      size_t space_needed = 2 + 2*velem_w*elem_vwidth + (elem_vwidth - 1) + 1;
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
  } else {
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
  bool showChars)
{
  std::stringstream ss;
  ss << std::hex << std::setfill('0');
  if (elem_w != 1 && elem_w != 2 && elem_w != 4 && elem_w != 8)
    return format_buffer_diff(b, r, n_elems * elem_w, 1, cols, showChars);
  //    FATAL("invalid element width");
  cols = cols == 0 ? 16 : cols;
  if (cols == 0)
    cols = (elem_w == 1 || elem_w == 2) ? 16 :
    elem_w == 4 ? 8 : elem_w == 8 ? 4 : 8;
  uint8_t *buf = (uint8_t *)b, *ref = (uint8_t *)r;
  auto emit_elem = [&](uint8_t *arr, size_t i) {
    void *ptr = arr + i * elem_w;
    if (elem_w == 1) {
      ss << std::setw(2 * elem_w) << (unsigned)(*(uint8_t *)ptr);
    } else if (elem_w == 2) {
      ss << std::setw(2 * elem_w) << *(uint16_t *)ptr;
    } else if (elem_w == 4) {
      ss << std::setw(2 * elem_w) << *(uint32_t *)ptr;
    } else if (elem_w == 8) {
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
      if (elem_w == 1 && showChars) {
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
          } else {
            emit_elem(ref, k);
          }
          ss << " ";
        }
        if (elem_w == 1 && showChars) {
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
    } else {
      ss << ' ';
    }
  }
  return ss.str();
}



std::string text::load_c_preprocessed(
  const std::string &inp_path,
  const std::string &args)
{
  auto temp_out_path = sys::get_temp_path("pp.cl");

  auto findClangExe = [] {
    static const char *paths [] = {
      "C:\\Progra~2\\LLVM\\bin\\clang.exe", // 32-bit Program Files (x86)
      "C:\\Progra~1\\LLVM\\bin\\clang.exe", // 64-bit
    };
    static const char *good_clang;
    if (good_clang == nullptr) {
      for (size_t i = 0; i < sizeof(paths)/sizeof(paths[0]); i++) {
        if (sys::file_exists(paths[i])) {
          good_clang = paths[i];
          break;
        }
      }
      if (good_clang == nullptr) {
        good_clang = "clang";
      }
    }
    return good_clang;
  };
  auto clang_exe = findClangExe();

  std::vector<std::string> p_args {
    "-E",
    "-o",
    temp_out_path
  };
   for (auto &w : text::to_words(args)) {
     p_args.push_back(w);
   }

  auto pr = sys::process_exec(clang_exe, p_args);

  std::string outp;
  if (!pr.succeeded()) {
    WARNING("text::PreProcess: unable to preprocess with clang.exe\n");
    // punt, and just return the .cl contents
    outp =sys::read_file_text(inp_path);
  } else {
    outp = sys::read_file_text(temp_out_path);
  }

  (void)remove(temp_out_path.c_str());

  return outp;

//  std::stringstream ss;
//  ss << clang_exe << " -E -o " <<
//    temp_out_path << " " << args << " " << inp_path;
//  int e = system(ss.str().c_str());
//  std::string outp;
//  if (e != 0) {
//    WARNING("text::PreProcess: unable to preprocess with clang.exe\n");
//    // punt, and just return the .cl contents
//    outp = sys::read_file_text(inp_path);
//  } else {
//    outp = sys::read_file_text(temp_out_path);
//  }
//  (void)remove(temp_out_path.c_str());
//  return outp;
}


