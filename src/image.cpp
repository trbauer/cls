#include "image.hpp"
#include "fatal.hpp"
#include "system.hpp"
#ifdef USE_LODE_PNG
// up from src and in deps
#include "../deps/lodepng/lodepng.h"
#endif

#ifdef _WIN32
#include <Windows.h>
// #include <Wincodec.h>
#endif
#include <cmath>
#include <cstdlib>
#include <fstream>
#include <iomanip>
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
  , format(data_format::INVALID)
{
}
image::image(size_t w, size_t h, size_t p, enum data_format f)
  : pitch(p)
  , bits(new uint8_t[pitch * h])
  , width(w)
  , height(h)
  , alloc_height(h)
  , format(f)
{
  if (p < bytes_per_pixel(f) * w)
    cls::fatal("image::image: pitch is too small for width*bytes-per-pixel");
  memset(bits, 0, pitch*alloc_height);
}
image::image(size_t w, size_t h, enum data_format f)
  : image(w, h, bytes_per_pixel(f) * w, f) { }
image::~image() {release();}

size_t image::bytes_per_pixel(enum data_format f) {
  switch (f) {
  case image::I:
    return 1;
  case image::RGB:
  case image::BGR:
    return 3;
  case image::RGBA:
  case image::ARGB:
  case image::BGRA:
    return 4;
  default: cls::fatal("invalid image format: ", f);
  }
}

void image::assign(
  const void *imgbits,
  size_t w,
  size_t h,
  enum data_format f,
  size_t p,
  size_t ah)
{
  const size_t bpp = bytes_per_pixel(f);
  if (ah < h) {
    cls::fatal("image::assign: invalid allocation height");
  } else if (p < w * bpp) {
    cls::fatal("image::assign: invalid pitch");
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

image image::convert(enum data_format to) const {
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
      } else if (format == image::ARGB) {
        a = *ibits++;
        r = *ibits++;
        g = *ibits++;
        b = *ibits++;
      } else if (format == image::BGRA) {
        b = *ibits++;
        g = *ibits++;
        r = *ibits++;
        a = *ibits++;
      } else {
        cls::fatal("image::convert: invalid format");
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
      } else if (copy.format == image::BGRA) {
        *obits++ = b;
        *obits++ = g;
        *obits++ = r;
        *obits++ = a;
      } else {
        cls::fatal("image::convert: invalid format");
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


// PPM format http://netpbm.sourceforge.net/doc/ppm.html
//
// EXAMPLE:
// P6
// 640 480
// 255
// [.... binary data RGB,RGB,RGB...]
// [or RRGGBBRRGGBB] if max value >= 256
// EXAMPLE: (not supported yet)
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
#define FAIL(X) \
  do { \
    if (fatal_if_error) { \
      cls::fatal("image::load_ppm: could not open file"); \
    } else { \
      return nullptr; \
    } \
  } while (0)

  std::ifstream ifs(file, std::ifstream::binary);
  if (!ifs.is_open())
    cls::fatal("image::load_ppm: could not open file");

  std::string ln;
  auto getLine = [&]() {
    bool failed = !std::getline(ifs, ln);
    if (failed)
      return false;
    if (!ln.empty() && ln[ln.size() - 1] == '\r') {
      ln = ln.substr(0, ln.size() - 1);
    }
    return true;
  };

  while (ifs.peek() == '#')
    getLine();
  if (!getLine()) {
    cls::fatal("image::load_ppm: error parsing file");
  }
  bool is_p6 = ln == "P6";
  bool is_p3 = ln == "P3";
  if (!is_p6 && !is_p3) {
    cls::fatal("image::load_ppm: error parsing file");
  }

  while (ifs.peek() == '#')
    getLine();

  int width, height, max_value;
  ifs >> width;
  ifs >> height;
  ifs >> max_value;
  size_t off0 = (size_t)ifs.tellg();
  while (ifs.peek() == '\n' || ifs.peek() == '\r') {
    ifs.get();
  }
  if (!ifs)
    cls::fatal("image::load_ppm: error parsing image header");
  if (max_value <= 0 || width <= 0 || height <= 0)
    cls::fatal("image::load_ppm: invalid image dimensions");
  if (max_value > 255)
    cls::fatal("image::load_ppm: only single byte-per-channel images supported");
  // size_t data_start = (size_t)ifs.tellg();
  image *i = new image(width, height, image::RGB);
  if (is_p6) { // binary
    ifs.read((char *)i->bits, 3*width*height);
  } else { // text
    for (int h = 0; h < height; h++) {
      for (int w = 0; w < width; w++) {
        int r = -1, g = -1, b = -1;
        ifs >> r;
        if (!ifs || r < 0 || r > max_value)
          cls::fatal("image::load_ppm: error reading pixel");
        ifs >> g;
        if (!ifs || g < 0 || g > max_value)
          cls::fatal("image::load_ppm: error reading pixel");
        ifs >> b;
        if (!ifs || b < 0 || b > max_value)
          cls::fatal("image::load_ppm: error reading pixel");
        i->bits[h*i->pitch + 3*w + 0] = (uint8_t)r;
        i->bits[h*i->pitch + 3*w + 1] = (uint8_t)g;
        i->bits[h*i->pitch + 3*w + 2] = (uint8_t)b;
      }
    }
    while (isspace(ifs.peek()))
      ifs.get();
  }
  // size_t data_end = (size_t)ifs.tellg();
  if (ifs.get() != EOF) { // can't use ifs.eof()
    cls::fatal("image::load_ppm: expected end of file");
  }
  return i;
}

void image::save_ppm(const char *file, bool use_binary)
{
  if (format != image::RGB) {
    image i = image::convert(image::RGB);
    i.save_ppm(file,use_binary);
    return;
  }
  if (use_binary) {
    std::ofstream ofs(file,std::ostream::binary);
    ofs << "P6\n";
    ofs << width << "  " << height << "\n";
    ofs << "255\n";
    for (size_t h = 0; h < height; h++)
      ofs.write(
        (const char *)(bits + h*pitch),
        image::bytes_per_pixel(format)*width);
  } else {
    std::ofstream ofs(file);
    ofs << "P3\n";
    ofs << width << "  " << height << "\n";
    ofs << "255\n";
    for (size_t h = 0; h < height; h++) {
      for (size_t w = 0; w < width; w++) {
        if (w > 0)
          ofs << "  ";
        ofs << std::setw(3) << std::setfill(' ') << std::dec <<
          (int)bits[h*pitch + 3*w + 0];
        ofs << " ";
        ofs << std::setw(3) << std::setfill(' ') << std::dec <<
          (int)bits[h*pitch + 3*w + 1];
        ofs << " ";
        ofs << std::setw(3) << std::setfill(' ') << std::dec <<
          (int)bits[h*pitch + 3*w + 2];
      } // for w
      ofs << "\n";
    } // for h
  }
}


#ifdef IMAGE_HPP_SUPPORTS_BMP
// TODO: Enable cross platform by replacing names below.
//       Some testing should be done (hence, why I don't do it now).
//
// for cross platform
struct bitmap_info_header {
  uint32_t   biSize;
  long       biWidth;
  long       biHeight;
  uint16_t   biPlanes;
  uint16_t   biBitCount;
  uint32_t   biCompression;
  uint32_t   biSizeImage;
  long       biXPelsPerMeter;
  long       biYPelsPerMeter;
  uint32_t   biClrUsed;
  uint32_t   biClrImportant;
};
// static_assert(sizeof(bitmap_info_header) == sizeof(BITMAPINFOHEADER));
#pragma pack(push,2)
struct bitmap_file_header {
  uint16_t   bfType;
  uint32_t   bfSize;
  uint16_t   bfReserved1;
  uint16_t   bfReserved2;
  uint32_t   bfOffBits;
};
#pragma pack(pop)
// static_assert(sizeof(bitmap_file_header) == sizeof(BITMAPFILEHEADER));
// static_assert(offsetof(bitmap_file_header,bfSize) == offsetof(BITMAPFILEHEADER,bfSize));
struct bitmap_info {
  bitmap_info_header    bmiHeader;
  struct {
    uint8_t    rgbBlue;
    uint8_t    rgbGreen;
    uint8_t    rgbRed;
    uint8_t    rgbReserved;
  } bmiColors[1];
};
// static_assert(sizeof(bitmap_info) == sizeof(BITMAPINFO));

static void write_bmp_info_header(
  const image &img, BITMAPINFOHEADER &bih)
{
  const size_t row_pitch = sys::align_round_up<size_t>(3 * img.width, 4);
  bih.biSize = sizeof(bih);
  bih.biWidth = (long)img.width;
  bih.biHeight = (long)img.height;
  bih.biPlanes = 1;
  bih.biBitCount = 24; // BGR
  bih.biCompression = 0;
  bih.biSizeImage = (unsigned long)(row_pitch * img.height);
  bih.biXPelsPerMeter = bih.biYPelsPerMeter = 0;
  bih.biClrUsed = 0;
  bih.biClrImportant = 0;
}

// creates and passes the out_size back
static void *create_bmp_image_data(const image &img, size_t &out_size)
{
  const size_t row_pitch = sys::align_round_up<size_t>(3 * img.width, 4);
  static const char zeros[4] = { 0, 0, 0, 0 };
  const uint8_t *img_bits = img.bits;
  const size_t pitch_slack =
    img.pitch - image::bytes_per_pixel(img.format) * img.width;
  out_size = img.height * row_pitch; // (3 * img.width + out_row_slack);
  uint8_t *obuf_base = new uint8_t[out_size];
  // bitmaps reverse the row-order
  for (size_t y = 1; y <= img.height; y++) {
    uint8_t *orow = obuf_base + row_pitch * (img.height - y);
    for (size_t x = 0; x < img.width; x++) {
      uint8_t r, g, b;
      // we force this version here
      if (img.format == image::BGR) {
        b = *img_bits++;
        g = *img_bits++;
        r = *img_bits++;
      } else {
        cls::fatal("create_bmp_image_data: invalid format");
      }
#if 0
      // Got rid of this because I kept adding formats and needed a uniform
      // approach.  From now on, the caller just converts to BGR for us
      //
      // E.g. BGRA won't work below.
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
        cls::fatal("create_bmp_image_data: invalid format");
      }
#endif
      // TODO: support intensity images?
      // copy out in BGR
      *orow++ = b;
      *orow++ = g;
      *orow++ = r;
    }
    // set the row pitch slack to 0
    for (size_t i = 0; i < row_pitch - 3 * img.width; i++)
      *orow++ = 0;

    // skip over image pitch slack
    img_bits += pitch_slack;
  }
  return obuf_base;
}


void image::save_bmp(const char *file_name) const
{
  if (format != data_format::BGR) {
    convert(data_format::BGR).save_bmp(file_name);
    return;
  }
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
    cls::fatal("image::save_bmp: could not open file");
  }
  file.write((const char *)&bfh, sizeof(bfh));
  // no color table: using BITMAPINFOHEADER
  file.write((const char *)&bih, sizeof(BITMAPINFOHEADER));
  size_t img_data_bytes;
  void *img_data = create_bmp_image_data(*this, img_data_bytes);
  file.write((const char *)img_data, img_data_bytes);
  delete[] ((uint8_t*)img_data);
}

image *image::load_bmp(const char *file_name, bool fatal_if_error)
{
  std::ifstream file(file_name, std::ios::binary);
  if (!file.is_open()) {
    if (fatal_if_error)
      cls::fatal(file_name, ": failed to open");
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
      cls::fatal(file_name, ": failed to read");
    return nullptr;
  }
  BITMAPFILEHEADER& bfh = (BITMAPFILEHEADER&)buffer[0];
  if (bfh.bfType != 0x4D42) {
    if (fatal_if_error)
      cls::fatal("%s: not a bitmap file", file_name);
    return nullptr;
  }
  BITMAPINFO& bi = (BITMAPINFO&)buffer[sizeof(BITMAPFILEHEADER)];
  BITMAPINFOHEADER& bih = bi.bmiHeader;
  if (bih.biCompression != BI_RGB) {
    if (fatal_if_error)
      cls::fatal(
          file_name, ": unsupported format (bih.biCompression != BI_RGB)");
    return nullptr;
  }
  if (bih.biClrUsed != 0) {
    if (fatal_if_error)
      cls::fatal(file_name, ": unsupported format (biClrUsed != 0)");
    return nullptr;
  }
  if (bih.biClrImportant != 0) {
    if (fatal_if_error)
      cls::fatal(file_name, ": unsupported format (biClrImportant != 0)");
    return nullptr;
  }

  const size_t W = bih.biWidth;
  const size_t H = bih.biHeight;
  const size_t row_pitch = sys::align_round_up<size_t>(3 * W, 4);
  const uint8_t* bitmap_bits =
    (const uint8_t *)&buffer[bfh.bfOffBits];

  image *img = new image(W, H, image::RGB);
  size_t pitch_slack = img->pitch - 3 * img->width;
  uint8_t *imgbits = img->bits;
  // Bitmaps put rows in reverse order.  Hence, row[0] is really row[h - 1].
  // We simply read the rows out backwards to fix this.
  for (size_t y = 1; y <= H; y++) {
    const uint8_t *row_bits = bitmap_bits + (H - y) * row_pitch;
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

#ifdef USE_LODE_PNG
image *image::load_png(const char *file, bool fatal_if_error)
{
  std::vector<unsigned char> bits; //the raw pixels
  unsigned width = 0, height = 0;
  auto err = lodepng::decode(bits, width, height, file);
  if (err) {
    auto err_str = lodepng_error_text(err);
    if (fatal_if_error)
      cls::fatal(file, ": failed to decode PNG: ", err_str);
    else
      return nullptr;
  }
  // the pixels are now in the vector "image", 4 bytes per pixel RGBARGBA....
  image *i = new image(width, height, data_format::RGBA);
  if (!bits.empty())
    memcpy(i->bits, bits.data(), 4*width*height);
  return i;
}
void image::save_png(const char *file) const
{
  if (format != data_format::RGBA || pitch != 4*width) {
    convert(data_format::RGBA).save_png(file);
    return;
  }
  auto err =
    lodepng_encode32_file(file, bits, (unsigned)width, (unsigned)height);
  if (err) {
    cls::fatal(file, ": failed to write PNG (", lodepng_error_text(err), ")");
  }
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
            cls::fatal("INTERNAL: image::resize: unexpected format");
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
            cls::fatal("image::resize: unexpected format");
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
          cls::fatal("image::resize: unexpected format");
        }
      }
      // zero the slack
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
