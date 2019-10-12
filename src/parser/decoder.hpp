#ifndef DECODER_HPP
#define DECODER_HPP

#include "../fatal.hpp"
#include "../text.hpp"

#include <climits>
#include <cstdint>
#include <functional>

namespace cls {

  class decoder {
    fatal_handler        &fh;
    loc                   at;
    const uint8_t        *bits;
    size_t                bits_length;

    size_t                off = 0;

  public:
    decoder(
      fatal_handler &_fh,
      loc _at,
      const uint8_t *_bits,
      size_t _bits_length)
      : fh(_fh), at(_at), bits(_bits), bits_length(_bits_length) { }

    ///////////////////////////////////////////////////////////////////////////
    // for chaining
    fatal_handler &get_handler() const {return fh;}
    loc get_at() const {return at;}
    const uint8_t *get_bits() const {return bits;}
    size_t get_bits_length() const {return bits_length;}

    ///////////////////////////////////////////////////////////////////////////
    size_t bytes_left() const {return bits_length - off;}
    size_t offset() const {return off;}

    ///////////////////////////////////////////////////////////////////////////
    template <typename...Ts>
    void fatal(Ts...ts) const {fatalAt(off, ts...);}
    template <typename...Ts>
    void fatalAt(size_t off, Ts...ts) const {
      std::stringstream ss;
      if (off != (size_t)-1)
        ss << "at binary offset 0x" << text::fmtHex(off) << ": ";
      text::format_to(ss, ts...);
      fh.fatalAt(at, ss.str());
    }

    template <typename...Ts>
    void warningAt(size_t off, Ts...ts) const {
      std::stringstream ss;
      ss << "at binary offset 0x" << text::fmtHex(off) << ": ";
      text::format_to(ss, ts...);
      fh.warningAt(at, ss.str());
    }

    void skip(size_t len) {
      seek(off + len);
    }
    void seek(size_t new_off) {
      if (new_off > bits_length)
        fatal("premature end of file (trying to seek to ",new_off,")");
      off = new_off;
    }

    ///////////////////////////////////////////////////////////////////////////
    // Read without advance
    template <typename T>
    T peek() const {
      T val;
      peekInto(&val, sizeof(T));
      return val;
    }
    template <typename T>
    T peek(size_t at) const {
      T val;
      peekInto(&val, sizeof(T), at);
      return val;
    }
    void peekInto(void *val, size_t len) const {
      peekInto(val, len, off);
    }
    void peekInto(void *val, size_t len, size_t abs_off) const {
      if (abs_off + len > bits_length)
        fatal("premature end of file");
      memcpy(val, bits + abs_off, len);
    }
    ///////////////////////////////////////////////////////////////////////////
    // Read and advance
    void decodeInto(void *val, size_t len) {
      peekInto(val, len);
      skip(len);
    }
    template <typename T>
    T decode() {
      T val;
      decodeInto(&val, sizeof(T));
      return val;
    }
    template <typename T>
    void decodeEq(T expected, const char *err) {
      auto value = peek<T>();
      if (value != expected) {
        fatal(err);
      }
      skip(sizeof(T));
    }
    //
    template <typename T>
    static T swapByteOrder(T t) {
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