#ifndef DECODER_HPP
#define DECODER_HPP

#include "fatal.hpp"
#include "text.hpp"

#include <cstdint>

namespace cls {
  struct decoder : fatal_handler {
    loc            at;
    const uint8_t *bits;
    size_t         bits_length;

    size_t         off = 0;

    decoder(loc _at, const uint8_t *_bits, size_t _bits_length)
      : at(loc), bits(_bits), bits_length(_bits_length) {}

    template <typename..Ts>
    void fatal(Ts...ts)
    {
      std::stringstream ss;
      ss << "at binary offset " << off << ": ";
      text::format_to(ss, ts...);
      fatalAt(at, ss.str());
    }

    void skip(size_t len) {
      seek(off + len);
    }
    void seek(size_t new_off) {
      if (new_off > bits_length)
        fatal("premature end of file (trying to seek to ",len,"B)");
      off = new_off;
    }

    ///////////////////////////////////////////////////////////////////////////
    // Read without advance
    template <typename T>
    T peek() const {
      T val;
      peekBuf(&val, sizeof(T));
      return val;
    }
    void peekBuf(const void *val, size_t len) const {
      if (len - off < len)
        fatal("premature end of file");
      memcpy(&val, bits + off, len);
    }
    ///////////////////////////////////////////////////////////////////////////
    // Read and advance
    template <typename T> T decode() {
      T val;
      decodeBuf(&val, sizeof(T));
      return val;
    }
    void decodeBuf(const void *val, size_t len) {
      peek(val, len);
      skip(len);
    }
  }
}



#endif