#pragma once
#include <cstdint>

struct pixel_value {
  pixel_value(uint8_t _r, uint8_t _g, uint8_t _b, uint8_t _a)
    : r(_r), g(_g), b(_b), a(_a) {  }

  pixel_value(uint8_t _r, uint8_t _g, uint8_t _b)
    : r(_r), g(_g), b(_b), a(0xFF) {  }

  pixel_value(uint8_t i) : r(i), g(i), b(i), a(i) {  }

  union {
    uint8_t rgba[4];
    struct {uint8_t r, g, b, a;};
  };

  uint8_t intensity() const;
};

struct image {
  // number of bytes in a row of pixels.
  size_t pitch = 0; // in ***bytes***
  size_t alloc_height; // in px (can be > height)
                       // array of image bits
                       // the top-left pixel is bits[0],
                       // the bottom right is bits[n - 1].
  uint8_t *bits = nullptr;
  size_t width = 0, height = 0;  // in px
  enum format {INVALID, I, RGB, BGR, RGBA, ARGB} format;

  static size_t bytes_per_pixel(enum format f);
  static image *load_bmp(const char *file, bool fatal_if_error = false);
  void save_bmp(const char *file) const;

  image();
  image(size_t w, size_t h, enum format f);
  image(const image &rhs) { assign(rhs); }
  ~image();

  // assignment for ownership semantics
  image& operator=(image& rhs) { assign(rhs); return *this; }

  void assign(
    const void *imgbits,
    size_t w,
    size_t h,
    enum format f,
    size_t p,
    size_t ah);
  void assign(const image &rhs);

  void release();

  image convert(enum format to) const;

  // resizes the image padding pixels or truncating if needed.
  void resize(
    size_t new_width,
    size_t new_height,
    struct pixel_value fill = pixel_value(0));

};