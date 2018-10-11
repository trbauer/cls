#ifndef HALF_HPP

#include <cstdint>
#include <ostream>


float           half_bits_to_float(uint16_t);
uint16_t        float_to_half_bits(float);


// we don't use a typedef because we want a unique type for templates
// and whatnot
struct half {
  uint16_t bits;

  half() {}
  half(float f);
  half(double f) : half((float)f) { }
  half(int64_t i) : half((double)i) { }
  half(uint64_t i) : half((double)i) { }
  operator float() const;
  operator double() const {return (float)*this;}

  explicit operator int() const{return (int)half_bits_to_float(bits);}

  // const half &operator=(const half &rhs){bits = rhs.bits; return *this;}

  half operator+(const half &rhs) const {return half(half_bits_to_float(bits) + half_bits_to_float(rhs.bits));}
  half operator-(const half &rhs) const {return half(half_bits_to_float(bits) - half_bits_to_float(rhs.bits));}
  half operator*(const half &rhs) const {return half(half_bits_to_float(bits) * half_bits_to_float(rhs.bits));}
  half operator/(const half &rhs) const {return half(half_bits_to_float(bits) / half_bits_to_float(rhs.bits));}
  bool operator==(const half &rhs) const {return half_bits_to_float(bits) == half_bits_to_float(rhs.bits);}
  bool operator!=(const half &rhs) const {return !(*this == rhs);}
  bool operator<(const half &rhs) const {return half_bits_to_float(bits) < half_bits_to_float(rhs.bits);}
  bool operator<=(const half &rhs) const {return half_bits_to_float(bits) <= half_bits_to_float(rhs.bits);}
  bool operator>(const half &rhs) const {return !(*this <= rhs);}
  bool operator>=(const half &rhs) const {return !(*this < rhs);}
};
static_assert(sizeof(half) == sizeof(uint16_t),"wrong size for cls::half");

// TODO: make a quick test to verify these
static float           half_to_float(half h) {return half_bits_to_float(h.bits);}
static half            float_to_half(float x) {half h; h.bits = float_to_half_bits(x); return h;}


inline half::half(float f) : bits(float_to_half_bits(f)) { }
//  inline half::operator double() const {return (double)half_bits_to_float(bits);}
inline half::operator float() const {return half_bits_to_float(bits);}

static std::ostream &operator <<(std::ostream &os, const half &h) {
  // os << "0x" << std::hex << h.bits;
  os << half_to_float(h);
  return os;
}

///////////////////////////////////////////////////////////////////////////////
// Constants from http://www.open-std.org/jtc1/sc22/wg21/docs/papers/2016/p0192r0.pdf
//                http://www.open-std.org/jtc1/sc22/wg14/www/docs/n2016.pdf
// Smallest positive short float
#define SFLT_MIN              5.96046448e-08
// Smallest positive
// normalized short float
#define SFLT_NRM_MIN          6.10351562e-05
// Largest positive short float
#define SFLT_MAX              65504.0
// Smallest positive e
// for which (1.0 + e) != (1.0)
#define SFLT_EPSILON          0.00097656
// Number of digits in mantissa
// (significand + hidden leading 1)
#define SFLT_MANT_DIG         11
// Number of base 10 digits that
// can be represented without change
#define SFLT_DIG              2
// Base of the exponent
#define SFLT_RADIX            2
// Minimum negative integer such that
// HALF_RADIX raised to the power of
// one less than that integer is a
// normalized short float
#define SFLT_MIN_EXP          -13
// Maximum positive integer such that
// HALF_RADIX raised to the power of
// one less than that integer is a
// normalized short float
#define SFLT_MAX_EXP          16
// Minimum positive integer such
// that 10 raised to that power is
// a normalized short float
#define SFLT_MIN_10_EXP       -4
// Maximum positive integer such
// that 10 raised to that power is
// a normalized short float
#define SFLT_MAX_10_EXP       4


#endif
