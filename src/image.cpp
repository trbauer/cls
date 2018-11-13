#include "image.hpp"
#include "system.hpp"


#include <Windows.h>
// #include <Wincodec.h>
#include <cstdlib>
#include <fstream>
#include <cmath>
#include <string>
#include <vector>

uint8_t pixel_value::intensity() const {
  return (uint8_t)std::roundf(((float)r + (float)g + (float)b) / 3.0f);
}

image::image()
  : bits(nullptr)
  , width(0)
  , pitch(0)
  , height(0)
  , alloc_height(0)
  , format(format::INVALID)
{
}
image::image(size_t w, size_t h, size_t p, enum format f)
  : pitch(p)
  , bits(new uint8_t[pitch * h])
  , width(w)
  , height(h)
  , alloc_height(h)
  , format(f)
{
  if (p < bytes_per_pixel(f) * w)
    FATAL("image::image: pitch is too small for width*bytes-per-pixel");
  memset(bits, 0, pitch*alloc_height);
}
image::image(size_t w, size_t h, enum format f)
  : image(w, h, bytes_per_pixel(f) * w, f) { }
image::~image() {release();}

size_t image::bytes_per_pixel(enum format f) {
  switch (f) {
  case image::I:
    return 1;
  case image::RGB:
  case image::BGR:
    return 3;
  case image::RGBA:
    return 4;
  default: FATAL("invalid image format %d", (int)f);
  }
}

void image::assign(
  const void *imgbits,
  size_t w,
  size_t h,
  enum format f,
  size_t p,
  size_t ah)
{
  const size_t bpp = bytes_per_pixel(f);
  if (ah < h) {
    FATAL("invalid allocation height");
  } else if (p < w * bpp) {
    FATAL("invalid pitch");
  }

  release();

  format = f;
  alloc_height = ah;
  width = w;
  height = h;
  format = f;
  pitch = p;
  bits = new uint8_t[p * h];
  memcpy(bits, imgbits, p * h);
}

void image::assign(const image &rhs) {
  assign(
    rhs.bits, rhs.width, rhs.height,
    rhs.format, rhs.pitch, rhs.alloc_height);
}

image image::convert(enum format to) const {
  image copy(width, height, to);
  size_t pitch_slack = pitch - bytes_per_pixel(format) * width;
  size_t copy_pitch_slack = copy.pitch - bytes_per_pixel(to) * copy.width;
  unsigned char *ibits = bits;
  unsigned char *obits = copy.bits;
  for (size_t y = 0; y < height; y++) {
    for (size_t x = 0; x < width; x++) {
      unsigned char r, g, b, a = 0xFF;
      if (format == image::I) {
        r = g = b = *ibits++;
      } else if (format == image::RGB) {
        r = *ibits++;
        g = *ibits++;
        b = *ibits++;
      } else if (format == image::BGR) {
        b = *ibits++;
        g = *ibits++;
        r = *ibits++;
      } else if (format == image::RGBA) {
        r = *ibits++;
        g = *ibits++;
        b = *ibits++;
        a = *ibits++;
      } else if (format == image::RGBA) {
        a = *ibits++;
        r = *ibits++;
        g = *ibits++;
        b = *ibits++;
      } else {
        FATAL("image::convert: invalid format");
      }

      if (copy.format == image::I) {
        uint8_t i =
          (uint8_t)roundf((((float)r + (float)g + (float)b)/3.0f));
        *obits++ = i;
      } else if (copy.format == image::RGB) {
        *obits++ = r;
        *obits++ = g;
        *obits++ = b;
      } else if (copy.format == image::BGR) {
        *obits++ = b;
        *obits++ = g;
        *obits++ = r;
      } else if (copy.format == image::RGBA) {
        *obits++ = r;
        *obits++ = g;
        *obits++ = b;
        *obits++ = a;
      } else if (copy.format == image::ARGB) {
        *obits++ = a;
        *obits++ = r;
        *obits++ = g;
        *obits++ = b;
      } else {
        FATAL("image::convert: invalid format");
      }
    }
    ibits += pitch_slack;
    memset(obits, 0, copy_pitch_slack);
    obits += copy_pitch_slack;
  }
  return copy;
}

void image::release() {
  if (bits)
    delete[] bits;
  bits = nullptr;
  format = image::INVALID;
  width = height = pitch = alloc_height = 0;
}

/*
// PPM format http://netpbm.sourceforge.net/doc/ppm.html
//
// EXAMPLE:
// P3
// # feep.ppm
// 4 4
// 15
//  0  0  0    0  0  0    0  0  0   15  0 15
//  0  0  0    0 15  7    0  0  0    0  0  0
//  0  0  0    0  0  0    0 15  7    0  0  0
// 15  0 15    0  0  0    0  0  0    0  0  0
image *image::load_ppm(const char *file, bool fatal_if_error)
{
  std::ifstream ifs(file);
  if (!ifs.is_open()) 
    FATAL("image::load_ppm: could not open file");
  
  std::string ln;

  while (ifs.good()) {
    std::getline(ifs,ln);
    if (ln.size() == 0 && ln[0] = '#')
      continue;
    else if (ln != "P3") 
      FATAL("image::load_ppm: malformed PPM (expected P3)");
  }

}

void image::save_ppm(const char *file)
{
}
*/


#ifdef IMAGE_HPP_SUPPORTS_BMP  
static void write_bmp_info_header(
  const image &img, BITMAPINFOHEADER &bih)
{
  const size_t row_bytes = sys::align_round_up<size_t>(3 * img.width, 4);
  bih.biSize = sizeof(bih);
  bih.biWidth = (LONG)img.width;
  bih.biHeight = (LONG)img.height;
  bih.biPlanes = 1;
  bih.biBitCount = 24; // BGR
  bih.biCompression = 0;
  bih.biSizeImage = (DWORD)(row_bytes * img.height);
  bih.biXPelsPerMeter = bih.biYPelsPerMeter = 0;
  bih.biClrUsed = 0;
  bih.biClrImportant = 0;
}

// creates and passes the out_size back
static void *create_bmp_image_data(const image &img, size_t &out_size)
{
  const size_t row_bytes = sys::align_round_up<size_t>(3 * img.width, 4);
  static const char zeros[4] = { 0, 0, 0, 0 };
  const uint8_t *img_bits = img.bits;
  const size_t pitch_slack =
    img.pitch - image::bytes_per_pixel(img.format) * img.width;
  out_size = img.height * row_bytes; // (3 * img.width + out_row_slack);
  uint8_t *obuf_base = new uint8_t[out_size];
  // bitmaps reverse the row-order
  for (size_t y = 1; y <= img.height; y++) {
    uint8_t *orow = obuf_base + row_bytes * (img.height - y);
    for (size_t x = 0; x < img.width; x++) {
      uint8_t r, g, b;
      if (img.format == image::I) {
        r = g = b = *img_bits++;
      } else if (img.format == image::RGB) {
        r = *img_bits++;
        g = *img_bits++;
        b = *img_bits++;
      } else if (img.format == image::BGR) {
        b = *img_bits++;
        g = *img_bits++;
        r = *img_bits++;
      } else if (img.format == image::RGBA) {
        r = *img_bits++;
        g = *img_bits++;
        b = *img_bits++;
        img_bits++;
      } else if (img.format == image::ARGB) {
        img_bits++;
        r = *img_bits++;
        g = *img_bits++;
        b = *img_bits++;
      } else {
        FATAL("create_bmp_image_data: invalid format");
      }
      // TODO: support intensity images?
      // copy out in BGR
      *orow++ = b;
      *orow++ = g;
      *orow++ = r;
    }
    // row sizes must be multiple of 4
    for (size_t i = 0; i < row_bytes - 3 * img.width; i++)
      *orow++ = 0;

    // skip over image pitch slack
    img_bits += pitch_slack;
  }
  return obuf_base;
}

void image::save_bmp(const char *file_name) const
{
  // We skip the bmiColors table since we don't use it.
  // Hence we just store a BITMAPINFOHEADER and ignore the color
  // table (included if we correctly used BITMAPINFO)
  BITMAPINFOHEADER bih = { 0 };
  write_bmp_info_header(*this, bih);
  const size_t row_bytes = sys::align_round_up<size_t>(3 * width, 4);
  BITMAPFILEHEADER bfh = { 0 };
  bfh.bfType = 0x4D42; // "BM"
  bfh.bfOffBits = sizeof(bfh)+sizeof(BITMAPINFOHEADER);
  bfh.bfSize = bih.biSizeImage + bfh.bfOffBits;

  std::ofstream file(file_name, std::ios::binary);
  if (!file.is_open()) {
    FATAL("image::save_bmp: could not open file");
  }
  file.write((const char *)&bfh, sizeof(bfh));
  // no color table: using BITMAPINFOHEADER
  file.write((const char *)&bih, sizeof(BITMAPINFOHEADER));
  size_t img_data_bytes;
  void *img_data = create_bmp_image_data(*this, img_data_bytes);
  file.write((const char *)img_data, img_data_bytes);
  delete[] img_data;
}

image *image::load_bmp(const char *file_name, bool fatal_if_error)
{
  std::ifstream file(file_name, std::ios::binary);
  if (!file.is_open()) {
    if (fatal_if_error)
      FATAL("%s: failed to open", file_name);
    return nullptr;
  }
  file.seekg(0, std::ios::end);
  std::streamoff size = file.tellg();
  //  std::streamsize size = file.tellg();
  //  size_t size = file.tellg();
  file.seekg(0, std::ios::beg);

  std::vector<uint8_t> buffer((size_t)size);
  if (!file.read((char *)buffer.data(), size)) {
    if (fatal_if_error)
      FATAL("%s: failed to read", file_name);
    return nullptr;
  }
  BITMAPFILEHEADER& bfh = (BITMAPFILEHEADER&)buffer[0];
  if (bfh.bfType != 0x4D42) {
    if (fatal_if_error)
      FATAL("%s: not a bitmap file", file_name);
    return nullptr;
  }
  BITMAPINFO& bi = (BITMAPINFO&)buffer[sizeof(BITMAPFILEHEADER)];
  BITMAPINFOHEADER& bih = bi.bmiHeader;
  if (bih.biCompression != BI_RGB) {
    if (fatal_if_error)
      FATAL("%s: unsupported format (bih.biCompression != BI_RGB)", file_name);
    return nullptr;
  }
  if (bih.biClrUsed != 0) {
    if (fatal_if_error)
      FATAL("%s: unsupported format (biClrUsed != 0)", file_name);
    return nullptr;
  }
  if (bih.biClrImportant != 0) {
    if (fatal_if_error)
      FATAL("%s: unsupported format (biClrImportant != 0)", file_name);
    return nullptr;
  }

  const size_t W = bih.biWidth;
  const size_t H = bih.biHeight;
  const enum format default_format = image::RGB;
  const size_t bytes_per_px = image::bytes_per_pixel(default_format);
  const size_t row_bytes = sys::align_round_up<size_t>(3 * W, 4);
  const uint8_t* bitmap_bits =
    (const uint8_t *)&buffer[bfh.bfOffBits];

  image *img = new image(W, H, default_format);
  size_t pitch_slack = img->pitch - 3 * img->width;
  uint8_t *imgbits = img->bits;
  // Bitmaps put rows in reverse order.  Hence, row[0] is really row[h - 1].
  // We simply read the rows out backwards to fix this.
  for (size_t y = 1; y <= H; y++) {
    const uint8_t *row_bits = bitmap_bits + (H - y) * row_bytes;
    for (size_t x = 0; x < W; x++) {
      uint8_t b = *row_bits++;
      uint8_t g = *row_bits++;
      uint8_t r = *row_bits++;
      // default format is RGB-24 bit
      *imgbits++ = r;
      *imgbits++ = g;
      *imgbits++ = b;
    }
    // zero the pitch slack and move the ptr
    memset(imgbits, 0, pitch_slack);
    imgbits += pitch_slack;
  }
  return img;
}
#endif

void image::resize(
  size_t new_width,
  size_t new_height,
  struct pixel_value fill)
{
  size_t old_height = height;
  size_t old_width = width;
  width = new_width;
  height = new_height;
  const size_t bytes_per_px = bytes_per_pixel(format);
  if (bytes_per_px * new_width <= pitch && new_height <= alloc_height) {
    // new width fits within the old pitch and allocation height
    const size_t new_pitch_slack = pitch - bytes_per_px * new_width;
    // clear the ends of the rows
    for (size_t y = 0; y < new_height; y++) {
      uint8_t *row_tail = bits + y * pitch + new_width * bytes_per_px;
      memset(row_tail, 0, new_pitch_slack);
    }
    if (new_height <= old_height) {
      // height got smaller, clear the truncated rows
      memset(
        bits + pitch * new_height,
        0,
        (old_height - new_height) * pitch);
    } else {
      // expanded height: initialize the new height to fill
      for (size_t y = old_height; y < new_height; y++) {
        uint8_t *row = bits + pitch * y;
        for (size_t x = 0; x < new_width; x++) {
          if (format == image::I) {
            *row++ = fill.intensity();
          } else if (format == image::RGB) {
            *row++ = fill.r;
            *row++ = fill.g;
            *row++ = fill.b;
          } else if (format == image::BGR) {
            *row++ = fill.b;
            *row++ = fill.g;
            *row++ = fill.r;
          } else if (format == image::RGBA) {
            *row++ = fill.r;
            *row++ = fill.g;
            *row++ = fill.b;
            *row++ = fill.a;
          } else if (format == image::ARGB) {
            *row++ = fill.a;
            *row++ = fill.r;
            *row++ = fill.g;
            *row++ = fill.b;
          } else {
            FATAL("INTERNAL: image::resize: unexpected format");
          }
        }
        // clear the tail
        memset(row, 0, new_pitch_slack);
      }
    }
  } else {
    // new size is too large, copy needed
    const size_t old_pitch = pitch;
    if (bytes_per_px * new_width > pitch)
      pitch = bytes_per_px * new_width;
    if (new_height > alloc_height)
      alloc_height = new_height;

    uint8_t *old_bits = bits;
    bits = new uint8_t[pitch * alloc_height * 3];
    size_t copy_h = new_height > old_height ? old_height : new_height;
    size_t copy_w = new_width > old_width ? old_width : new_width;
    size_t y;
    for (y = 0; y < copy_h; y++) {
      uint8_t *old_row = old_bits + old_pitch * y;
      uint8_t *new_row = bits + pitch * y;
      if (new_width >= old_width) {
        // copy old pixels
        memcpy(new_row, old_row, bytes_per_px * old_width);
        // set new pixels to fill
        for (size_t x = old_width; x < new_width; x++) {
          if (format == image::I) {
            *new_row++ = fill.intensity();
          } else if (format == image::RGB) {
            *new_row++ = fill.r;
            *new_row++ = fill.g;
            *new_row++ = fill.b;
          } else if (format == image::BGR) {
            *new_row++ = fill.b;
            *new_row++ = fill.g;
            *new_row++ = fill.r;
          } else if (format == image::RGBA) {
            *new_row++ = fill.r;
            *new_row++ = fill.g;
            *new_row++ = fill.b;
            *new_row++ = fill.a;
          } else if (format == image::ARGB) {
            *new_row++ = fill.a;
            *new_row++ = fill.r;
            *new_row++ = fill.g;
            *new_row++ = fill.b;
          } else {
            FATAL("image::resize: unexpected format");
          }
        }
      } else { // new_width < old_width
               // copy old
        memcpy(new_row, old_row, 3 * new_width);
        // no new fill
      }
      // zero slack
      memset(new_row + 3 * new_width, 0, pitch - bytes_per_px * new_width);
    } // for copy_h

      // handle potential new height that needs filling
    for (; y < old_height; y++) {
      uint8_t *new_row = bits + bytes_per_px * y;
      for (size_t x = 0; x < new_width; x++) {
        if (format == image::I) {
          *new_row++ = fill.intensity();
        } else if (format == image::RGB) {
          *new_row++ = fill.r;
          *new_row++ = fill.g;
          *new_row++ = fill.b;
        } else if (format == image::BGR) {
          *new_row++ = fill.b;
          *new_row++ = fill.g;
          *new_row++ = fill.r;
        } else if (format == image::RGBA) {
          *new_row++ = fill.r;
          *new_row++ = fill.g;
          *new_row++ = fill.b;
          *new_row++ = fill.a;
        } else if (format == image::ARGB) {
          *new_row++ = fill.a;
          *new_row++ = fill.r;
          *new_row++ = fill.g;
          *new_row++ = fill.b;
        } else {
          FATAL("image::resize: unexpected format");
        }
      }
      // zero slack
      memset(new_row + 3 * new_width, 0, pitch - bytes_per_px * new_width);
    }
    // zero the rest of the allocation height
    memset(
      bits + pitch * new_height,
      0,
      (alloc_height - new_height) * pitch);

    delete[] old_bits;
  }
}
