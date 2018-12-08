#ifndef DECODER_HPP
#define DECODER_HPP

#include "../fatal.hpp"
#include "../text.hpp"

#include <cstdint>

namespace cls {
  struct decoder {
    const fatal_handler  &fh;
    loc                   at;
    const uint8_t        *bits;
    size_t                bits_length;

    size_t                off = 0;

    decoder(
      const fatal_handler &_fh,
      loc _at,
      const uint8_t *_bits,
      size_t _bits_length)
      : fh(_fh), at(_at), bits(_bits), bits_length(_bits_length) { }

    template <typename...Ts>
    void fatalHere(Ts...ts) const
    {
      std::stringstream ss;
      ss << "at binary offset " << off << ": ";
      text::format_to(ss, ts...);
      fh.fatalAt(at, ss.str());
    }
    template <typename...Ts>
    void fatal(Ts...ts) const
    {
      std::stringstream ss;
      text::format_to(ss, ts...);
      fh.fatalAt(at, ss.str());
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
    void peekInto(void *val, size_t len) const {
      if (len - off < len)
        fatal("premature end of file");
      memcpy(val, bits + off, len);
      std::cout << " ";
    }
    ///////////////////////////////////////////////////////////////////////////
    // Read and advance
    template <typename T> T decode() {
      T val;
      decodeInto(&val, sizeof(T));
      return val;
    }
    void decodeInto(void *val, size_t len) {
      peekInto(val, len);
      skip(len);
    }
  }; // struct decoder
}



#endif