#include "half.hpp"

// #define USE_FP16C
// #define TEST_FP16

#if defined(USE_FP16C) || defined(TEST_FP16)
// #include <emmintrin.h>
#include <immintrin.h>
#endif // defined(USE_FP16C) || defined(TEST_FP16)


constexpr static inline uint32_t float_to_bits(float f) {
  union{float f; uint32_t i;} u{f};
  return u.i;
}

constexpr static inline float float_from_bits(uint32_t f) {
  union{uint32_t i; float f;} u{f};
  return u.f;
}

static const int F32_BITS = 32;
static const int F32_MNT_BITS = 23;
static const int F32_EXP_BITS = F32_BITS - 1 - F32_MNT_BITS; // 23
static const int F32_BIAS = (1 << (F32_EXP_BITS - 1)) - 1; // 127
static const uint32_t F32_SIGN_BIT  = 1 << (F32_BITS - 1); // 0x80000000
static const uint32_t F32_EXP_MASK =
  ((1 << F32_EXP_BITS) - 1) << F32_MNT_BITS;
static const uint32_t F32_MANT_MASK = (1 << F32_MNT_BITS) - 1; // 0x007FFFFF
static const uint32_t F32_QNAN_BIT = (F32_MANT_MASK + 1) >> 1; // 0x00400000

static const int F16_BITS = 16;
static const int F16_MNT_BITS = 10;
static const int F16_EXP_BITS = F16_BITS - F16_MNT_BITS - 1;
static const int F16_BIAS = (1 << (F16_EXP_BITS - 1)) - 1; // 15
static const uint16_t F16_SIGN_BIT = 1 << (F16_BITS - 1); // 0x8000
static const uint16_t F16_EXP_MASK =
  ((1 << F16_EXP_BITS) - 1) << F16_MNT_BITS; // 0x7C00
static const uint16_t F16_MANT_MASK = (1 << F16_MNT_BITS) - 1; // 0x03FF
static const uint16_t F16_QNAN_BIT = (F16_MANT_MASK + 1) >> 1; // 0x0200

static const int F32_F16_BIAS_DIFF = F32_BIAS - F16_BIAS;

static const int F32_F16_MNT_DIFF = 23 - 10;


#if defined(USE_FP16C) || defined(TEST_FP16)
static float avx_f16_to_f32(uint16_t u16, bool set_qnan)
{
  float f = _mm_cvtss_f32(_mm_cvtph_ps(_mm_cvtsi32_si128(u16)));
  if (!set_qnan && half::is_snan(u16)) {
    // the AVX op will set the qnan bit, but we've been asked to preserve the
    // signaling state of the float so the next operation can potentially
    // signal.
    uint32_t u32 = float_to_bits(f) & ~F32_QNAN_BIT;
    if ((u32 & F32_MANT_MASK) == 0) {
      u32++;
    }
    f = float_from_bits(u32);
  }
  return f;
}
static uint16_t avx_f32_to_f16(float f, bool set_qnan)
{
  // should be on all 3rd generation or newer Intel CPUs (HSW/SNB)
  // _MM_FROUND_NO_EXC
  const auto old_csr = _mm_getcsr();
  _mm_setcsr(old_csr | _MM_FROUND_NO_EXC);
  uint16_t w16 = (uint16_t)_mm_cvtsi128_si32(
    _mm_cvtps_ph(_mm_set_ss(f), _MM_FROUND_TO_NEAREST_INT));
  _mm_setcsr(old_csr);

  if (!set_qnan && half::is_snan(f)) {
    // see comment in avx_f16_to_f32
    w16 &= ~F16_QNAN_BIT;
    if ((w16 & F16_MANT_MASK) == 0) {
      w16++;
    }
  }
  return w16;
}
#endif // defined(USE_FP16C) || defined(TEST_FP16)


static float half_bits_to_float_impl(uint16_t u16, bool set_qnan)
{
#ifdef USE_FP16C
  return avx_f16_to_f32(u16, set_qnan);
#else // !USE_FP16C
  uint16_t u16_u = u16 & 0x7FFF;
  uint32_t s32 = ((uint32_t)u16 & F16_SIGN_BIT) << 16;
  uint32_t m16 = u16 & F16_MANT_MASK;
  if (u16_u > F16_EXP_MASK) {
    // preserve qNaN bit disposition
    // initially match whatever the fp16 qNaN bit was
    uint32_t m32 =
      (u16 & F16_QNAN_BIT) << F32_F16_MNT_DIFF;
    if (set_qnan) {
      m32 |= F32_QNAN_BIT; // ensure it's set irrespective of input
    }
    if (m32 == 0) {
      m32 = 1; // ensure still NaN
    }
    return float_from_bits(s32 | F32_EXP_MASK | m32);
  }
  uint32_t e16 = (u16 & F16_EXP_MASK) >> F16_MNT_BITS;
  uint32_t e32, m32;
  if (u16_u == F16_EXP_MASK) {
    // +-infinity
    e32 = F32_EXP_MASK >> F32_MNT_BITS;
    m32 = 0;
  } else if (e16 != 0 && e16 < 0x1F) {
    //  normal number
    e32 = e16 + F32_F16_BIAS_DIFF; // bias difference; // 0x70
    m32 = m16 << F32_F16_MNT_DIFF; // (23 - 10);
  } else if (e16 == 0 && m16 != 0) {
    // denorm/subnorm number (e16 == 0) => renormalize it
    // shift the mantissa left until the hidden one gets set
    for (e32 = F32_F16_BIAS_DIFF + 1;
      (m16 & (F16_MANT_MASK + 1)) == 0;
      m16 <<= 1, e32--)
      ;
    m32 = (m16 << F32_F16_MNT_DIFF) & F32_MANT_MASK;
  } else { // if (e16 == 0) // +/- 0.0
    e32 = 0;
    m32 = 0;
  }
  return float_from_bits(s32 | (e32 << F32_MNT_BITS) | m32);
#endif // !USE_FP16C
}

float half_bits_to_float(uint16_t u16)
{
  return half_bits_to_float_impl(u16, false);
}
float half_bits_to_float_preserving_snan(uint16_t u16)
{
  return half_bits_to_float_impl(u16, true);
}

uint16_t float_to_half_bits_impl(float f, bool set_qnan)
{
#ifdef USE_FP16C
  return avx_f32_to_f16(f, set_qnan);
#else // !USE_FP16C
  const uint32_t w32 = float_to_bits(f);
  const uint32_t w32_u = w32 & 0x7FFFFFFF;
  const uint32_t sign = w32 & 0x80000000;
  const uint16_t sign16 = (uint16_t)(sign >> 16);

  if (w32_u > F32_EXP_MASK) { // NaN
    uint16_t m16 = 0;
    m16 |= (F32_QNAN_BIT & w32_u) >> // preserve qnan bit
      (F32_MNT_BITS - F16_MNT_BITS);
    m16 |= (F16_MANT_MASK >> 1) & w32_u; // and bottom 9b
                                         //
                                         // s eeeeeeee qmmmmmmmmmmmmmmmmmmmmm
                                         //            |            |||||||||
                                         //            |            vvvvvvvvv
                                         //            +---------->qmmmmmmmmm
    if (set_qnan) {
      m16 |= F16_QNAN_BIT;
    }
    if (m16 == 0x0) {
      // if the nonzero payload is in the high bits and and gets
      // dropped and the signal bit is non-zero, then m16 is 0;
      // to maintain it as a nan we must set at least one bit
      m16 = 0x1;
    }
    return sign16 | F16_EXP_MASK | m16;
  } else if (w32_u == F32_EXP_MASK) { // +/-Infinity
    return sign16 | F16_EXP_MASK;
  } else {
    // norm/denorm
    static const uint32_t LOWEST_OVERFLOW = // 0x47800000
      (uint32_t)(F32_BIAS + F16_BIAS + 1) << F32_MNT_BITS;
    static const uint32_t LOWEST_NORM = // 0x38800000
      (uint32_t)(F32_BIAS - F16_BIAS + 1) << F32_MNT_BITS;
    static const uint32_t LOWEST_DENORM = // 0x33000000
      (uint32_t)(F32_BIAS - F16_BIAS - F16_MNT_BITS) << F32_MNT_BITS;
    auto round = [] (uint32_t v, uint32_t g, uint32_t s) {
      return v + (g & (s | v));
    };
    if (w32_u >= LOWEST_OVERFLOW) { // overflows to infinity
      return sign16 | F16_EXP_MASK;
    } else if (w32_u >= LOWEST_NORM) { // fits as normalized half
      uint32_t v =
        (((w32_u >> F32_MNT_BITS) - (F32_F16_BIAS_DIFF)) << F16_MNT_BITS) |
          ((w32_u >> F32_F16_MNT_DIFF) & F16_MANT_MASK);
      uint32_t g = (w32_u >> (F32_MNT_BITS - F16_MNT_BITS - 1)) & 0x1;
      uint32_t s = (w32_u & 0x0FFF) ? 1 : 0;
      return (uint16_t)round(sign16 | v, g, s);
    } else if (w32_u >= LOWEST_DENORM) { // fits as normalized half
      uint32_t i = (F32_BIAS - 1 - 1) - (w32_u >> F32_MNT_BITS);
      uint32_t w32_u2 = (w32_u & F32_MANT_MASK) | (F32_MANT_MASK + 1);
      uint32_t v = sign16 | (w32_u2 >> (i + 1));
      uint32_t g = (w32_u2 >> i) & 1;
      uint32_t s = (w32_u2 & ((1 << i) - 1)) ? 1 : 0;
      return (uint16_t)round(v, g, s);
    } else {
      // underflow to +-0
      return sign16;
    }
  }
#endif // !USE_FP16C
}
uint16_t  float_to_half_bits(float x) {
  return float_to_half_bits_impl(x, true);
}
uint16_t  float_to_half_bits_preserving_snan(float x) {
  return float_to_half_bits_impl(x, false);
}

bool half::is_nan(uint16_t x)
{
  return (x & F16_EXP_MASK) == F16_EXP_MASK &&
    (F16_MANT_MASK & x) != 0;
}
bool half::is_qnan(uint16_t x)
{
  return is_nan(x) && ((x & F16_QNAN_BIT) != 0);
}
bool half::is_snan(uint16_t x)
{
  return is_nan(x) && !is_qnan(x);
}
bool half::is_nan(float f)
{
  uint32_t x = float_to_bits(f);
  return (x & F32_EXP_MASK) == F32_EXP_MASK &&
    (F32_MANT_MASK & x) != 0;
}
bool half::is_qnan(float f)
{
  uint32_t x = float_to_bits(f);
  return half::is_nan(f) && ((x & F32_QNAN_BIT) != 0);
}
bool half::is_snan(float f)
{
  return half::is_nan(f) && !half::is_qnan(f);
}



#ifdef TEST_FP16
#include <cstdlib>
#include <iostream>
#include <iomanip>
#include <sstream>
#include <string>
#include <immintrin.h>


static bool nan_eq(float a, float b) {
  return (a == b) ||
    (half::is_qnan(a) && half::is_qnan(b)) ||
    (half::is_snan(a) && half::is_snan(b));
}

template <typename T>
static std::string fmt_hex_val(T val, int cols = 2*sizeof(T))
{
  std::stringstream ss;
  ss << "0x" << std::hex << std::uppercase << std::setfill('0') <<
    std::setw(cols) << val;
  return ss.str();
}
static std::string fmt_float(float val) {
  std::stringstream ss;
  if (half::is_qnan(val)) {
    ss << "qnan";
  } else if (half::is_snan(val)) {
    ss << "snan";
  } else {
    ss << val; // handles inf
  }
  return ss.str();
}
static void test_f16_to_f32(bool set_qnan)
{
  std::cout << "+----------------------------------------------------------+\n";
  std::cout << "test_f16_to_f32(" <<
    (set_qnan ? "set_qnan" : "preserve_snan") << ")\n";
  uint16_t u16 = 0;
  do {
    auto ref_32 = avx_f16_to_f32(u16, set_qnan);
    auto sut_32 = half_bits_to_float_impl(u16, set_qnan);
    if (!nan_eq(sut_32, ref_32)) {
      std::cout << fmt_hex_val(u16) << ": produces wrong output\n";
      auto fmt = [&](const char *what, float val) {
        std::cout << what << ": " << fmt_hex_val(float_to_bits(val)) << "  " <<
          fmt_float(val) << "\n";
      };
      fmt("SUT", sut_32);
      fmt("REF", ref_32);
      exit(1);
    }
  } while (u16++ != 0xFFFF);
}


static bool nan_eq(uint16_t a, uint16_t b) {
  return (a == b) ||
    (half::is_qnan(a) && half::is_qnan(b)) ||
    (half::is_snan(a) && half::is_snan(b));
}
static std::string fmt_half(uint16_t val) {
  std::stringstream ss;
  if (half::is_qnan(val)) {
    ss << "qnan";
  } else if (half::is_snan(val)) {
    ss << "snan";
  } else {
    ss << half_bits_to_float_impl(val, false);
  }
  return ss.str();
}
static void test_f32_to_f16(bool set_qnan, uint32_t starting_u32 = 0)
{
  std::cout << "+----------------------------------------------------------+\n";
  std::cout << "test_f32_to_f16(" <<
    (set_qnan ? "set_qnan" : "preserve_snan") << ")\n";
  uint32_t u32 = starting_u32;
  std::cout << "0123456789ABCDEF\n";
  do {
    auto ref_16 = avx_f32_to_f16(float_from_bits(u32), set_qnan);
    auto sut_16 = float_to_half_bits_impl(float_from_bits(u32), set_qnan);
    if (!nan_eq(sut_16, ref_16)) {
      std::cout << fmt_hex_val(u32) << ": produces wrong output\n";
      auto fmt = [&](const char *what, uint16_t val) {
        std::cout << what << ": " << fmt_hex_val(val) << "  " <<
          fmt_half(val) << "\n";
      };
      fmt("SUT", sut_16);
      fmt("REF", ref_16);

      exit(1);
    }
    if ((u32 & 0x0FFFFFFF) == 0) {
      std::cout << '.';
      std::cout.flush();
    }
  } while (u32++ != 0xFFFFFFFF);
  std::cout << '\n';
}

void test_fp16_conversion()
{
  // test_f32_to_f16(true, 0x7F800001);

  test_f16_to_f32(false);
  test_f16_to_f32(true);
  test_f32_to_f16(false);
  test_f32_to_f16(true);
  std::cout << "SUCCESS!";
  exit(0);
}
#endif


