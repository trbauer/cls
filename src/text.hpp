#ifndef _TEXT_HPP
#define _TEXT_HPP

#include <algorithm>
#include <cstdarg>
#include <iomanip>
#include <ostream>
#include <sstream>
#include <string>
#include <vector>

// TODO:
// text::col(FOO,16)  places a std::setw and std::right or std::right
//     depending on FOO's type (integer or not)
//


namespace text {
  /////////////////////////////////////////////////////////////////////////////
  // COLORED TEXT
  //
  // example:
  //   std::cout << "Here is some " << ANSI_RED << "red" << ANSI_RESET << " text.";
  //   std::cout << "Here is some " <<
  //             text::ansi_literal("\033[2;34m") << "dark blue" <<
  //             ANSI_RESET << " text.";
  //
  #define ANSI_RESET          text::ansi_literal("\033[0m")

  // foreground colors
  //
  // The "default" color
  #define ANSI_DEFAULT        text::ansi_literal("\033[1;39m")
  // #define ANSI_BLACK          "\033[1;30m"
  #define ANSI_RED            text::ansi_literal("\033[1;31m")
  #define ANSI_DRED           text::ansi_literal("\033[2;31m")
  #define ANSI_GREEN          text::ansi_literal("\033[1;32m")
  #define ANSI_DGREEN         text::ansi_literal("\033[2;32m")
  #define ANSI_YELLOW         text::ansi_literal("\033[1;33m")
  #define ANSI_DYELLOW        text::ansi_literal("\033[2;33m")
  #define ANSI_BLUE           text::ansi_literal("\033[1;34m")
  #define ANSI_DBLUE          text::ansi_literal("\033[2;34m")
  #define ANSI_MAGENTA        text::ansi_literal("\033[1;35m")
  #define ANSI_DMAGENTA       text::ansi_literal("\033[2;35m")
  #define ANSI_CYAN           text::ansi_literal("\033[1;36m")
  #define ANSI_DCYAN          text::ansi_literal("\033[2;36m")
  #define ANSI_WHITE          text::ansi_literal("\033[1;37m")
  #define ANSI_DWHITE         text::ansi_literal("\033[2;37m")

  // #define ANSI_INTEL_BLUE     "\033[38;2;0;113;197m"
  // better on black background
  #define ANSI_COLOR_INTEL_BLUE     text::ansi("\033[38;2;10;153;245m")
  // #define ANSI_COLOR_INTEL_BLUE_ON_WHITE     "\033[38;2;0;113;197m\033[47m"

  // #define ANSI_COLOR_NVIDIA_GREEN   "\033[38;2;118;185;0m"
  // better on black background
  #define ANSI_COLOR_NVIDIA_GREEN   text::ansi("\033[38;2;94;182;0m")

  // my crappy approximation (I couldn't find one to reference)
  #define ANSI_COLOR_AMD_ORANGE     text::ansi("\033[38;2;236;66;57m")

  struct ansi {
    std::string esc;
    ansi(std::string _esc) : esc(_esc) { }
  };
  struct ansi_literal {
    const char *esc;
    constexpr ansi_literal(const char *_esc) : esc(_esc) { }
  };

  /////////////////////////////////////////////////////////////////////////////
  // TEXT MANIPULATION
  std::vector<std::string>
                to_lines(const std::string &str);
  std::vector<std::string>
                to_words(const std::string &str);

  std::string   prefix_lines(const std::string &pfx, const std::string &str);
  void          printf_to(std::ostream &os, const char *patt, va_list va);
  void          printf_to(std::ostream &os, const char *patt, ...);
  std::string   printf(const char *patt, ...);

  template <typename...Ts>
  void          format_to(std::ostream &os) { }
  template <typename T, typename...Ts>
  void          format_to(std::ostream &os, T t, Ts...ts) {os << t; format_to(os, ts...);}

  template <typename...Ts>
  std::string   format(Ts...ts) {
    std::stringstream ss;
    format_to(ss, ts...);
    return ss.str();
  }

  /////////////////////////////////////////////////////////////////////////////
  // runs the C preprocessor
  template <typename F>
  std::string  str_extract(F f) {
    std::stringstream ss;
    f.str(ss);
    return ss.str();
  }


  /////////////////////////////////////////////////////////////////////////////
  // formats buffers
  void          format_buffer(
    std::ostream &os,
    const void *buf,           // buffer base pointer
    size_t buf_len,            // the total size of the buffer in bytes
    size_t elem_wbytes,        // width per element in bytes: 8 for cl_float2
    size_t elem_vwidth,        // vector size of each work element: 2 for cl_float2
    bool floating_point,       // if we should show floating-point values
    size_t max_cols = 0,       // preferred size in columns to use in rendering
    size_t elems_per_row = 0); // preferred elements per row

  std::string   format_buffer_diff(
    const void *b, // buffer
    const void *r, // reference
    size_t n_elems,
    size_t elem_w = 1,
    unsigned cols = 0,
    bool showChars = false);

  /////////////////////////////////////////////////////////////////////////////
  // runs the C preprocessor
  std::string   load_c_preprocessed(
    const std::string &inp,
    const std::string &args);

  /////////////////////////////////////////////////////////////////////////////
  // TEXT TABLES
  struct table {
    struct col {
      std::string label;
      bool numeric = true;
      int lpad = 0, rpad = 0;
      std::string dft; // default filler value

      std::vector<std::string> rows;
      size_t max_width = 0;

      col() { }
      col(const std::string &lab, bool num, int l, int r)
        : label(lab), numeric(num), lpad(l), rpad(r)
      {
        emit(lab);
      }

      template <typename T>
      void emit(const T &t) {
        std::stringstream ss;
        if (numeric) {ss << std::left;} else {ss << std::right;}
        for (int i = 0; i < lpad; i++)
          ss << ' ';
        ss << t;
        for (int i = 0; i < rpad; i++)
          ss << ' ';
        rows.push_back(ss.str());
        max_width = std::max<size_t>(max_width, rows.back().size());
      }
      void emitFloating(double f, int prec) {
        std::stringstream ss;
        if (numeric) {ss << std::left;} else {ss << std::right;}
        for (int i = 0; i < lpad; i++)
          ss << ' ';
        ss << std::fixed << std::setprecision(prec) << f;
        for (int i = 0; i < rpad; i++)
          ss << ' ';
        rows.push_back(ss.str());
        max_width = std::max<size_t>(max_width, rows.back().size());
      }
      //  private:
      //    col(const col &) = delete;
      //    col &operator=(const col &t) = delete;
      // col() = default;
    }; // col
    std::vector<col> cols;

    table() { }


    col &define_col(
      const std::string &label,
      bool numeric = true,
      int lpad = 0,
      int rpad = 0)
    {
      cols.emplace_back(label, numeric, lpad, rpad);
      return cols.back();
    }
    void define_spacer(
      const std::string &label,
      const std::string &val = "")
    {
      auto &c = define_col(label, false, 0, 0);
      c.dft = val;
    }

    std::string str() const {
      std::stringstream ss;
      str(ss, "");
      return ss.str();
    }

    void str(std::ostream &os,const char *delim = "") const {
      os << std::setfill(' ');
      size_t max_rows = 0;
      for (auto &c : cols) {
        max_rows = std::max<size_t>(max_rows, c.rows.size());
      }
      for (size_t i = 0; i < max_rows; i++) {
        os << delim;
        for (auto &c : cols) {
          if (i >= c.rows.size()) {
            os << std::setw(c.max_width) << c.dft;
          } else {
            os << std::setw(c.max_width) << c.rows[i];
          }
          os << delim;
        }
        os << '\n';
      }
    }
  private:
    table(const table &) = delete;
    table &operator=(const table &t) = delete;
  }; // table
} // namespace

std::ostream &operator <<(std::ostream &os, text::ansi e);
std::ostream &operator <<(std::ostream &os, text::ansi_literal e);


#endif