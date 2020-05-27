#ifndef _TEXT_HPP
#define _TEXT_HPP

#include <algorithm>
#include <cstring>
#include <iomanip>
#include <ostream>
#include <sstream>
#include <string>
#include <vector>

// TODO:
// text::col(FOO,16)  places a std::setw and std::right or std::right
//     depending on FOO's type (integer or not)
//


namespace text
{
  static bool streq(const char *str1, const char *str2) {
    return str1 == str2 || (str1 && str2 && strcmp(str1, str2) == 0);
  }
  static bool strpfx(const char *pfx, const char *str) {
    return strncmp(str, pfx, strlen(pfx)) == 0;
  }

  template <typename T>
  static std::string fmtHex(T val, int w) {
    std::stringstream ss;
    ss << std::uppercase << std::hex << std::setfill('0') << std::setw(w) <<
      val;
    return ss.str();
  }
  template <typename T>
  static std::string fmtHex(T val) {
    return fmtHex(val, 2*sizeof(val));
  }

  /////////////////////////////////////////////////////////////////////////////
  // COLORED TEXT
  //
  // example:
  //   std::cout << "Here is some " << ANSI_RED << "red" << ANSI_RESET << " text.";
  //   std::cout << "Here is some " <<
  //             text::ansi_literal("\033[2;34m") << "dark blue" <<
  //             ANSI_RESET << " text.";
  //
  // The normal case
  struct ansi_literal {
    const char *esc;
    constexpr ansi_literal(const char *_esc) : esc(_esc) { }
  };
  // uses a backing string so it can be constructed via variable (rarely needed)
  struct ansi {
    std::string esc;
    ansi(std::string _esc) : esc(_esc) { }
  };
  constexpr ansi_literal ANSI_NOP(nullptr);
  constexpr ansi_literal ANSI_RESET("\033[0m");

  // foreground colors
  //
  // The "default" color
  constexpr ansi_literal ANSI_DEFAULT("\033[1;39m");
  constexpr ansi_literal ANSI_BLACK("\033[1;30m");
  constexpr ansi_literal ANSI_RED("\033[1;31m");
  constexpr ansi_literal ANSI_DRED("\033[2;31m");
  constexpr ansi_literal ANSI_GREEN("\033[1;32m");
  constexpr ansi_literal ANSI_DGREEN("\033[2;32m");
  constexpr ansi_literal ANSI_YELLOW("\033[1;33m");
  constexpr ansi_literal ANSI_DYELLOW("\033[2;33m");
  constexpr ansi_literal ANSI_BLUE("\033[1;34m");
  constexpr ansi_literal ANSI_DBLUE("\033[2;34m");
  constexpr ansi_literal ANSI_MAGENTA("\033[1;35m");
  constexpr ansi_literal ANSI_DMAGENTA("\033[2;35m");
  constexpr ansi_literal ANSI_CYAN("\033[1;36m");
  constexpr ansi_literal ANSI_DCYAN("\033[2;36m");
  constexpr ansi_literal ANSI_WHITE("\033[1;37m");
  constexpr ansi_literal ANSI_DWHITE("\033[2;37m");

  constexpr ansi_literal ANSI_FADED("\033[38;2;120;120;120m");
  constexpr ansi_literal ANSI_FADED_YELLOW("\033[38;2;120;120;0m");

  // #define ANSI_INTEL_BLUE     "\033[38;2;0;113;197m"
  // better on black background
  constexpr ansi_literal ANSI_COLOR_INTEL_BLUE("\033[38;2;10;153;245m");
  // #define ANSI_COLOR_INTEL_BLUE_ON_WHITE     "\033[38;2;0;113;197m\033[47m"

  // #define ANSI_COLOR_NVIDIA_GREEN   "\033[38;2;118;185;0m"
  // better on black background
  constexpr ansi_literal ANSI_COLOR_NVIDIA_GREEN("\033[38;2;94;182;0m");

  // my crappy approximation (I couldn't find one to reference)
  constexpr ansi_literal ANSI_COLOR_AMD_ORANGE("\033[38;2;236;66;57m");

  // redirects to sys::is_tty, but we don't want to expose the header here
  bool is_tty(std::ostream &os);

  namespace spans {
    // this enables us to avoid an explicit reset
    //   os << ansi_span(...,"foo") << ...
    // or more reasonably
    //   os << RED("foo") << " = " << GREEN(44) << "\n";
    template<typename T>
    struct ansi_span {
      const char   *esc;
      T             element;
      ansi_span(
        const char *_esc,
        const T & _element)
        : esc(_esc) , element(_element) { }
    };

    template<typename T>
    ansi_span<T> RED(T t) {return ansi_span<T>(ANSI_RED.esc,t);}
    template<typename T>
    ansi_span<T> GREEN(T t) {return ansi_span<T>(ANSI_GREEN.esc,t);}
    template<typename T>
    ansi_span<T> BLUE(T t) {return ansi_span<T>(ANSI_RED.esc,t);}
    template<typename T>
    ansi_span<T> YELLOW(T t) {return ansi_span<T>(ANSI_YELLOW.esc,t);}
    template<typename T>
    ansi_span<T> CYAN(T t) {return ansi_span<T>(ANSI_CYAN.esc,t);}
    template<typename T>
    ansi_span<T> MAGENTA(T t) {return ansi_span<T>(ANSI_MAGENTA.esc,t);}

    template<typename T>
    std::ostream &operator <<(std::ostream &os, const ansi_span<T> &e) {
      if (e.esc && text::is_tty(os)) {
        os << e.esc;
        os << e.element;
        os << ANSI_RESET.esc;
      } else {
        os << e.element;
      }
      return os;
    }
  } // namespace text::spans

  /////////////////////////////////////////////////////////////////////////////
  // TEXT MANIPULATION
  std::vector<std::string>
                to_lines(const std::string &str);
  std::vector<std::string>
                to_words(const std::string &str);

  std::string   prefix_lines(const std::string &pfx, const std::string &str);
  void          prefix_lines_to(
      std::ostream &os, const std::string &pfx, const std::string &str);
  void          printf_to(std::ostream &os, const char *patt, va_list va);
  void          printf_to(std::ostream &os, const char *patt, ...);
  std::string   printf(const char *patt, ...);

  template <typename...Ts>
  void          format_to(std::ostream &) { }
  template <typename T, typename...Ts>
  void          format_to(std::ostream &os, T t, Ts...ts) {
      os << t; format_to(os, ts...);
  }

  template <typename...Ts>
  std::string   format(Ts...ts) {
    std::stringstream ss; format_to(ss, ts...); return ss.str();
  }

  /////////////////////////////////////////////////////////////////////////////
  // calls the str() function on a type T and returns the result as a string
  template <typename T>
  std::string   str_extract(T t) {
    std::stringstream ss; t.str(ss); return ss.str();
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
  // Run the C preprocessor on an input file.
  //
  // This searches the system for a recognized tool and executes it.
  // (including clang, cpp, or even cl.exe on Windows)
  // If we can't find it or executing it fails, we just return the string
  // we loaded (emitting a warning) and hope for the best.
  std::string   load_c_preprocessed(
    const std::string &inp,
    const std::string &args);
  std::string   load_c_preprocessed_using(
    const std::string &cpp_path,
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
      void emit(double f, int prec);
      void emit(float f, int prec) {emit((double)f, prec);}
      //  private:
      //    col(const col &) = delete;
      //    col &operator=(const col &t) = delete;
      // col() = default;
    }; // col
    std::vector<col*> cols; // has to be ptr because we return refs

    table() { }
    ~table() {for (auto *c : cols) {delete c;}}

    col &define_col(
      const std::string &label,
      bool numeric = true,
      int lpad = 0,
      int rpad = 0)
    {
      cols.push_back(new col(label, numeric, lpad, rpad));
      return *cols.back();
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

    void str(std::ostream &os,const char *delim = "  ") const;
  private:
    table(const table &) = delete;
    table &operator=(const table &t) = delete;
  }; // table
} // namespace

std::ostream &operator <<(std::ostream &os, text::ansi e);
std::ostream &operator <<(std::ostream &os, text::ansi_literal e);


#endif