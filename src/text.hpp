#ifndef _TEXT_HPP
#define _TEXT_HPP

#include <algorithm>
#include <cstring>
#include <initializer_list>
#include <iomanip>
#include <ostream>
#include <optional>
#include <sstream>
#include <string>
#include <vector>


namespace text
{
  ///////////////////////////////////////////////////////
  // text tools
  // EXAMPLES:
  //   auto s = format("foo=", foo_val, "; bar=", bar_val);
  //
  // extra ostream decorators:
  // a hex number of with fill of 8
  //   std::cout << hex(x, 8)
  // a fractional number with precision 5
  //   std::cout << frac(x, 5)
  // formatting with a fraction inline with 3 digits of precions
  //   auto s = format("foo=", foo_val, "; bar=", frac(bar_val, 3));
  //    or
  //   format_to(std::cout, "foo=", foo_val, "; bar=", frac(bar_val, 3));
  // left-aligned column of 32 rendering x (of any type T):
  //   std::cout << coll(x, 32) << ...
  // right-aligned with dots padding
  //   std::cout << "0x" << colr(x, 8, '.')
  // with variable column setting
  //   std::cout << col(x, pad::L, 32)
  //

  // hex stream decorator (a right-justified column)
  struct hex
  {
    uint64_t value;
    int columns;
    template <typename T>
    explicit hex(T v, int cls = 2 * sizeof(T)) : value((uint64_t)v), columns(cls) { }
  };

  // decimal stream decorator (a right-justified column)
  template <typename T> // could be signed or unsigned
  struct dec
  {
    T value;
    explicit dec(T v) : value(v) { }
  };

  struct frac
  {
    union {
      float f32;
      double f64;
    };
    const enum {F32 = 1, F64} tag;
    const int columns;
    const int prec;
    explicit frac(float v, int _prec = 3, int cols = -1)
        : columns(cols), prec(_prec), tag(F32) {
      f32 = v;
    }
    explicit frac(double v, int _prec = 4, int cols = -1)
        : columns(cols), prec(_prec), tag(F64) {
      f64 = v;
    }
  }; // frac

  enum class pad {L, R};

  template <typename T>
  struct col
  {
    const pad pd;
    const T &value;
    size_t width;
    char pad_fill;
    explicit col(const T &val, pad p, size_t wid, char f = ' ')
      : pd(p), value(val), width(wid), pad_fill(f) { }
  }; // col
  template <typename T>
  struct coll : col<T>
  {
    explicit coll(const T &val, size_t wid, char f = ' ')
      : col<T>(val, pad::L, wid, f) { }
  };
  template <typename T>
  struct colr : col<T>
  {
    explicit colr(const T &val, size_t wid, char f = ' ')
      : col<T>(val, pad::R, wid, f) { }
  };


  static bool streq(const char *str1, const char *str2) {
    return str1 == str2 || (str1 && str2 && strcmp(str1, str2) == 0);
  }
  static bool strpfx(const char *pfx, const char *str) {
    return strncmp(str, pfx, strlen(pfx)) == 0;
  }

  template <typename T>
  static std::string fmt_hex(T val, int w = 2 * sizeof(T)) {
    std::stringstream ss;
    ss << std::uppercase << std::hex << std::setfill('0') << std::setw(w) <<
      val;
    return ss.str();
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
    ansi_span<T> RED(T t) {return ansi_span<T>(ANSI_RED.esc, t);}
    template<typename T>
    ansi_span<T> GREEN(T t) {return ansi_span<T>(ANSI_GREEN.esc, t);}
    template<typename T>
    ansi_span<T> BLUE(T t) {return ansi_span<T>(ANSI_RED.esc, t);}
    template<typename T>
    ansi_span<T> YELLOW(T t) {return ansi_span<T>(ANSI_YELLOW.esc, t);}
    template<typename T>
    ansi_span<T> CYAN(T t) {return ansi_span<T>(ANSI_CYAN.esc, t);}
    template<typename T>
    ansi_span<T> MAGENTA(T t) {return ansi_span<T>(ANSI_MAGENTA.esc, t);}

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

  template<typename S, typename... Ts>
  void intercalate_to_successive(const S &, std::ostream &) { }
  template<typename S, typename T, typename... Ts>
  void intercalate_to_successive(const S &sep, std::ostream &os, T t, Ts... ts)
  {
    os << sep;
    os << t;
    intercalate_to_successive(sep, os, ts...);
  }
  template <typename S, typename...Ts>
  void          intercalate_to(const S &, std::ostream &) { }
  template <typename S, typename T, typename...Ts>
  void          intercalate_to(const S &sep, std::ostream &os, T t, Ts...ts) {
    os << t; intercalate_to_successive(sep, os, ts...);
  }

  template <typename S, typename...Ts>
  std::string   intercalate(const S &sep, Ts...ts) {
    std::stringstream ss; intercalate_to(sep, ss, ts...); return ss.str();
  }

  /////////////////////////////////////////////////////////////////////////////
  // calls the str() function on a type T and returns the result as a string
  template <typename T>
  std::string   str_extract(T t) {
    std::stringstream ss; t.str(ss); return ss.str();
  }

  /////////////////////////////////////////////////////////////////////////////
  // BITSET EXPANSION
  // - expands a bitset to a | separated list of symbols:
  // - exact matches tested first: so 0x0 can have a mapping
  //   as can compound symbols e.g. enum {READ=1, WRITE=2, READ_WRITE=3}
  //   (we render READ_WRITE intead of READ|WRITE)
  // - leftovers printed in hex; e.g. READ|0x8 given 9 or just 0x8 given 0x8
  // - if the value is 0 and no mapping exists, then we render 0x0
  // - chooses first symbols first; so put READ_WRITE ahead of READ; so if
  //   there are other undefined bits you will see READ_WRITE|0x8 rather
  //   than READ|WRITE|0x8
  void expand_bitset_to(
      std::ostream                                            &os,
      uint64_t                                                 bs,
      std::initializer_list<std::pair<uint64_t, const char *>> mappings);
  static inline std::string expand_bitset(
      uint64_t                                                 bs,
      std::initializer_list<std::pair<uint64_t, const char *>> mappings) {
    std::stringstream ss; expand_bitset_to(ss, bs, mappings); return ss.str();
  }


  /////////////////////////////////////////////////////////////////////////////
  // INTEGER PARSING
  // - parses decimal (decimal digits) or hex (0x...)
  // - signed version allows leading negation (-)
  // - separators (e.g. "_") are optional
  // - parse_seq_* allows an end pointer is optional and holds next
  //   non-consumed character (enables suffix handling by caller);
  //   the other functions forbid suffixes (e.g. "123abc" fails)
  //
  // DEVNOTE: strto* allow overflow and saturate; this is dumb.
  // These function return std::nullopt.
  //
  std::optional<uint64_t>
  parse_uint64(const char *str, const char *seps = nullptr);
  std::optional<int64_t>
  parse_sint64(const char *str, const char *seps = nullptr);
  std::optional<uint64_t> parse_seq_uint64(
      const char *str, const char *seps = nullptr, const char **sfx = nullptr);
  std::optional<int64_t> parse_seq_sint64(
      const char *str, const char *seps = nullptr, const char **sfx = nullptr);

  /////////////////////////////////////////////////////////////////////////////
  // formats buffers
  void format_buffer(
      std::ostream &os,
      const void   *buf,         // buffer base pointer
      size_t        buf_len,     // the total size of the buffer in bytes
      size_t        elem_wbytes, // width per element in bytes: 8 for cl_float2
      size_t elem_vwidth, // vector size of each work element: 2 for cl_float2
      bool   floating_point,    // if we should show floating-point values
      size_t max_cols      = 0, // preferred size in columns to use in rendering
      size_t elems_per_row = 0); // preferred elements per row

  std::string format_buffer_diff(
      const void *b, // buffer
      const void *r, // reference
      size_t      n_elems,
      size_t      elem_w     = 1,
      unsigned    cols       = 0,
      bool        show_chars = false);

  /////////////////////////////////////////////////////////////////////////////
  // Run the C preprocessor on an input file.
  //
  // This searches the system for a recognized tool and executes it.
  // (including clang, cpp, or even cl.exe on Windows)
  // If we can't find it or executing it fails, we just return the string
  // we loaded (emitting a warning) and hope for the best.
  struct cpp_result {
    enum class status {
      SUCCESS = 0,
      FILE_NOT_FOUND,
      CPP_NOT_FOUND,
      CPP_FAILED, // failed to start or had an error
    } result;
    std::string cpp_path;
    std::string output; // can contain error output if cpp fails due to #error

    cpp_result() { }
    cpp_result(status s, std::string cpp = "", std::string oup = "")
      : result(s), cpp_path(cpp), output(oup) { }
    std::string status_to_string() const;
    bool succeeded() const {return result == status::SUCCESS;}
  };
  cpp_result load_c_preprocessed(
    const std::string &inp,
    const std::string &args);
  cpp_result load_c_preprocessed_using(
    const std::string &cpp_path,
    const std::string &inp,
    const std::string &args);

  /////////////////////////////////////////////////////////////////////////////
  // TEXT TABLES
  //
  // EXAMPLE:
  //  table t;
  //  auto &c0 = t.define_col("Key", false);
  //  t.define_spacer("|");
  //  auto &c1 = t.define_col("Value", false);
  //  t.define_spacer("|");
  //  auto &c2 = t.define_col("FloatValue", false);
  //  c0.emit(key_row0);
  //  c0.emit(key_row1);
  //  c1.emit(val_row0);
  //  c1.emit(val_row1);
  //  c2.emit(fval_row0, 3); // .000
  //  c2.emit(fval_row1, 3); // .000
  //  t.str(std::cout);
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
      col(const col &) = delete;
      col operator=(const col &) = delete;

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

    void str(std::ostream &os, const char *delim = "  ") const;
  private:
    table(const table &) = delete;
    table &operator=(const table &t) = delete;
  }; // table
} // namespace text

std::ostream &operator <<(std::ostream &os, text::hex h);
template <typename T>
static inline std::ostream &operator <<(std::ostream &os, const text::dec<T> &d) {
  os << text::format(d.value);
  return os;
}
std::ostream &operator <<(std::ostream &os, text::frac f);

std::ostream &operator <<(std::ostream &os, const text::ansi &e);
std::ostream &operator <<(std::ostream &os, const text::ansi_literal &e);

template <typename T>
static inline std::ostream &operator<< (std::ostream &os, const text::col<T> &p) {
  auto s = text::format(p.value);
  std::stringstream ss;
  if (p.pd == text::pad::R) {
    for (size_t i = s.size(); i < p.width; i++)
      ss << p.pad_fill;
  }
  ss << s;
  if (p.pd == text::pad::L) {
    for (size_t i = s.size(); i < p.width; i++)
      ss << p.pad_fill;
  }
  os << ss.str();
  return os;
}


template <typename T>
static inline
std::ostream &operator <<(std::ostream &os, const std::optional<T> &ov) {
  if (ov)
    os << *ov;
  else
    os << "std::nullopt";
  return os;
}

#endif