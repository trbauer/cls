#ifndef DECODER_HPP
#define DECODER_HPP

#include "../fatal.hpp"
#include "../text.hpp"

#include <climits>
#include <cstdint>
#include <functional>

namespace cls {

  class decoder {
    diagnostics          &diags;
    loc                   at;
    const uint8_t        *bits;
    size_t                bits_length;

    size_t                off = 0;

  public:
    decoder(
      diagnostics &_diags,
      loc _at,
      const uint8_t *_bits,
      size_t _bits_length)
      : diags(_diags), at(_at), bits(_bits), bits_length(_bits_length) { }

    ///////////////////////////////////////////////////////////////////////////
    // for chaining
    diagnostics &get_handler() const {return diags;}
    loc get_at() const {return at;}
    const uint8_t *get_bits() const {return bits;}
    size_t get_bits_length() const {return bits_length;}

    ///////////////////////////////////////////////////////////////////////////
    size_t bytes_left() const {return bits_length - off;}
    size_t offset() const {return off;}

    ///////////////////////////////////////////////////////////////////////////
    template <typename...Ts>
    [[noreturn]]
    void fatal(Ts...ts) const {fatal_at(off, ts...);}
    template <typename...Ts>
    [[noreturn]]
    void fatal_at(size_t off, Ts...ts) const {
      std::stringstream ss;
      if (off != (size_t)-1)
        ss << "at binary offset 0x" << text::fmt_hex(off) << ": ";
      text::format_to(ss, ts...);
      diags.fatal_at(at, ss.str());
    }

    template <typename...Ts>
    void warning_at(size_t off, Ts...ts) const {
      std::stringstream ss;
      ss << "at binary offset 0x" << text::fmt_hex(off) << ": ";
      text::format_to(ss, ts...);
      diags.warning_at(at, ss.str());
    }

    void skip(size_t len) {
      seek(off + len);
    }
    void seek(size_t new_off) {
      if (new_off > bits_length)
        fatal("premature end of file (trying to seek to ", new_off, ")");
      off = new_off;
    }

    ///////////////////////////////////////////////////////////////////////////
    // Read without advance
    template <typename T>
    T peek() const {
      T val;
      peek_into(&val, sizeof(T));
      return val;
    }
    template <typename T>
    T peek(size_t at) const {
      T val;
      peek_into(&val, sizeof(T), at);
      return val;
    }
    void peek_into(void *val, size_t len) const {
      peek_into(val, len, off);
    }
    void peek_into(void *val, size_t len, size_t abs_off) const {
      if (abs_off + len > bits_length)
        fatal("premature end of file");
      memcpy(val, bits + abs_off, len);
    }
    ///////////////////////////////////////////////////////////////////////////
    // Read and advance
    void decode_into(void *val, size_t len) {
      peek_into(val, len);
      skip(len);
    }
    template <typename T>
    T decode() {
      T val;
      decode_into(&val, sizeof(T));
      return val;
    }
    template <typename T>
    void decode_eq(T expected, const char *err) {
      auto value = peek<T>();
      if (value != expected) {
        fatal(err);
      }
      skip(sizeof(T));
    }
    //
    template <typename T>
    static T swap_byte_order(T t) {
      static_assert (CHAR_BIT == 8, "CHAR_BIT != 8");

      union { T u; uint8_t u8[sizeof(T)]; } src, dst;
      src.u = t;
      for (size_t k = 0; k < sizeof(T); k++)
        dst.u8[k] = src.u8[sizeof(T) - k - 1];
      return dst.u;
    }
  }; // struct decoder
}



#endif