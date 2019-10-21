#ifndef IMAGE_HPP
#define IMAGE_HPP

#ifdef _WIN32
// TODO: we could support this on Unix if we made custom BITMAPINFOHEADER, etc...
#define IMAGE_HPP_SUPPORTS_BMP
#endif
#ifdef USE_LODE_PNG
#define IMAGE_HPP_SUPPORTS_PNG
#endif

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
  enum data_format {INVALID, I, RGB, BGR, RGBA, ARGB, BGRA} format;

  static size_t bytes_per_pixel(enum data_format f);

  static image *load_ppm(const char *file, bool fatal_if_error = false);
  void save_ppm(const char *file, bool use_binary = true);
#ifdef IMAGE_HPP_SUPPORTS_BMP
  static image *load_bmp(const char *file, bool fatal_if_error = false);
  void save_bmp(const char *file) const;
#endif
#ifdef IMAGE_HPP_SUPPORTS_PNG
  static image *load_png(const char *file, bool fatal_if_error = false);
  void save_png(const char *file) const;
#endif

  image();
  image(size_t w, size_t h, enum data_format f);
  image(size_t w, size_t h, size_t p, enum data_format f);
  image(const image &rhs) { assign(rhs); }
  ~image();

  // assignment for ownership semantics
  image &operator=(const image &rhs) { assign(rhs); return *this; }

  void assign(
    const void *imgbits,
    size_t w,
    size_t h,
    enum data_format f,
    size_t p,
    size_t ah);
  void assign(const image &rhs);

  void release();

  image convert(enum data_format to) const;

  // resizes the image by padding or truncating as needed.
  void resize(
    size_t new_width,
    size_t new_height,
    struct pixel_value fill = pixel_value(0));
};

#endif
