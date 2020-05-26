#include "half.hpp"

constexpr static inline uint32_t float_to_bits(float f) {
  union{float f; uint32_t i;} u{f};
  return u.i;
}

constexpr static inline float float_from_bits(uint32_t f) {
  union{uint32_t i; float f;} u{f};
  return u.f;
}

static const uint32_t F32_SIGN_BIT  = 0x80000000;
static const uint32_t F32_EXP_MASK  = 0x7F800000;
static const uint32_t F32_MANT_MASK = 0x007FFFFF;
static const uint32_t F32_QNAN_BIT  = 0x00400000;
static const int F32_MANTISSA_BITS = 23;
static const uint16_t F16_SIGN_BIT  = 0x8000;
static const uint16_t F16_EXP_MASK  = 0x7C00;
static const uint16_t F16_MANT_MASK = 0x03FF;
static const uint16_t F16_QNAN_BIT  = 0x0200;
static const int F16_MANTISSA_BITS = 10;


float half_bits_to_float(uint16_t h)
{
  uint16_t u16 = h;
  const int MANTISSA_DIFFERENCE = // 23 - 10
     F32_MANTISSA_BITS - F16_MANTISSA_BITS;
  const int F32_F16_BIAS_DIFFERENCE = 127 - 15;

  uint32_t s32 = ((uint32_t)u16 & F16_SIGN_BIT) << 16;
  uint32_t e16 = (u16 & F16_EXP_MASK) >> F16_MANTISSA_BITS;
  uint32_t m16 = u16 & F16_MANT_MASK;

  uint32_t m32 = 0, e32 = 0;
  if (e16 != 0 && e16 < (F16_EXP_MASK >> F16_MANTISSA_BITS)) { // e16 < 0x1F
    //  normal number
    e32 = e16 + F32_F16_BIAS_DIFFERENCE;
    m32 = m16 << MANTISSA_DIFFERENCE;
  } else if (e16 == 0 && m16 != 0) {
    // denorm/subnorm number (e16 == 0)
    // shift the mantissa left until the hidden one gets set
    for (e32 = (F32_F16_BIAS_DIFFERENCE + 1);
        (m16 & (F16_MANT_MASK + 1)) == 0;
        m16 <<= 1, e32--)
        ;
    m32 = (m16 << MANTISSA_DIFFERENCE) & F32_MANT_MASK;
  } else if (e16 == 0) { // +/- 0.0
    e32 = 0;
    m32 = 0;
  } else {
    e32 = F32_EXP_MASK >> F32_MANTISSA_BITS;
    if (m16 == 0) { // Infinity
      m32 = 0;
    } else { // NaN:  m16 != 0 && e16 == 0x1F
      m32 = (u16 & F16_QNAN_BIT) << MANTISSA_DIFFERENCE; // preserve sNaN bit
      m32 |= (F16_MANT_MASK >> 1) & m16;
      if (m32 == 0) {
          m32 = 1; // ensure still NaN
      }
    }
  }

  return float_from_bits(s32 | (e32 << F32_MANTISSA_BITS) | m32);
}



uint16_t float_to_half_bits(float f)
{
  uint32_t f32 = float_to_bits(f);

  uint32_t m32 = F32_MANT_MASK & f32;
  uint32_t e32 = (F32_EXP_MASK & f32) >> F32_MANTISSA_BITS;

  uint32_t m16 = 0;
  uint32_t e16 = 0;

  if (e32 == (F32_EXP_MASK >> F32_MANTISSA_BITS)) {
    // NaN or Infinity
    e16 = F16_EXP_MASK;
    m16 = (F16_MANT_MASK >> 1) & f32;
    if (m32 != 0) {
      // preserve the bottom 9 bits of the NaN payload and
      // shift the signaling bit (high bit) down as bit 10
      m16 |= (F32_QNAN_BIT & f32) >>
          (F32_MANTISSA_BITS - F16_MANTISSA_BITS);
      // s eeeeeeee mmmmmmmmmmmmmmmmmmmmmm
      //            |            |||||||||
      //            |            vvvvvvvvv
      //            +---------->mmmmmmmmmm
      if (m16 == 0) {
          // if the nonzero payload is in the high bits and and gets
          // dropped and the signal bit is non-zero, then m16 is 0,
          // to maintain it as a qnan, we must set at least one bit
          m16 = 0x1;
      }
    }
  } else if (e32 > (127 - 15) + 0x1E) { // e16 overflows 5 bits after bias fix
    // Too large for f16 => infinity
    e16 = F16_EXP_MASK;
    m16 = 0;
  } else if (e32 <= (127 - 15) && e32 >= 0x66) {
    // Denorm/subnorm float
    //
    // Normal floats are:
    //   (1 + sum{m[i]^(23-i)*2^(-i)}) * 2^(e - bias)
    //   (each mantissa bit is a fractional power of 2)
    // Denorms are:
    //   (0 + ...)
    // This is a zero exponent, but non-zero mantissa
    //
    // set leading bit past leading mantissa bit (low exponent bit)
    // (hidden one)
    m32 |= (F32_QNAN_BIT << 1);
    // repeatedly increment the f32 exponent and divide the denorm
    // mantissa until the exponent reachs a non-zero value
    for (; e32 <= 127 - 15; m32 >>= 1, e32++)
        ;
    e16 = 0;
    m16 = m32 >> (F32_MANTISSA_BITS - F16_MANTISSA_BITS);
  } else if (e32 < 0x66) {
    // Too small: rounds to +/-0.0
    e16 = 0;
    m16 = 0;
  } else {
    // Normalized float
    e16 = (e32 - (127 - 15)) << F16_MANTISSA_BITS;
    m16 = m32 >> (F32_MANTISSA_BITS - F16_MANTISSA_BITS);
    // TODO: rounding voodoo?
  }

  uint32_t s16 = (f32 >> 16) & F16_SIGN_BIT;
  uint16_t h{(uint16_t)(s16 | e16 | m16)};

  return h;
}

