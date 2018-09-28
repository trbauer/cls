#include "cl_headers.hpp"

#include <sstream>

std::string cls::status_to_symbol(cl_int error)
{
#define CASE(X) case X: return #X
  switch (error) {
  CASE(CL_SUCCESS);
  CASE(CL_DEVICE_NOT_FOUND);
  CASE(CL_DEVICE_NOT_AVAILABLE);
  CASE(CL_COMPILER_NOT_AVAILABLE);
  CASE(CL_MEM_OBJECT_ALLOCATION_FAILURE);
  CASE(CL_OUT_OF_RESOURCES);
  CASE(CL_OUT_OF_HOST_MEMORY);
  CASE(CL_PROFILING_INFO_NOT_AVAILABLE);
  CASE(CL_MEM_COPY_OVERLAP);
  CASE(CL_IMAGE_FORMAT_MISMATCH);
  CASE(CL_IMAGE_FORMAT_NOT_SUPPORTED);
  CASE(CL_BUILD_PROGRAM_FAILURE);
  CASE(CL_MAP_FAILURE);
  CASE(CL_MISALIGNED_SUB_BUFFER_OFFSET);
  CASE(CL_EXEC_STATUS_ERROR_FOR_EVENTS_IN_WAIT_LIST);
  CASE(CL_COMPILE_PROGRAM_FAILURE);
  CASE(CL_LINKER_NOT_AVAILABLE);
  CASE(CL_LINK_PROGRAM_FAILURE);
  CASE(CL_DEVICE_PARTITION_FAILED);
  CASE(CL_KERNEL_ARG_INFO_NOT_AVAILABLE);
  CASE(CL_INVALID_VALUE);
  CASE(CL_INVALID_DEVICE_TYPE);
  CASE(CL_INVALID_PLATFORM);
  CASE(CL_INVALID_DEVICE);
  CASE(CL_INVALID_CONTEXT);
  CASE(CL_INVALID_QUEUE_PROPERTIES);
  CASE(CL_INVALID_COMMAND_QUEUE);
  CASE(CL_INVALID_HOST_PTR);
  CASE(CL_INVALID_MEM_OBJECT);
  CASE(CL_INVALID_IMAGE_FORMAT_DESCRIPTOR);
  CASE(CL_INVALID_IMAGE_SIZE);
  CASE(CL_INVALID_SAMPLER);
  CASE(CL_INVALID_BINARY);
  CASE(CL_INVALID_BUILD_OPTIONS);
  CASE(CL_INVALID_PROGRAM);
  CASE(CL_INVALID_PROGRAM_EXECUTABLE);
  CASE(CL_INVALID_KERNEL_NAME);
  CASE(CL_INVALID_KERNEL_DEFINITION);
  CASE(CL_INVALID_KERNEL);
  CASE(CL_INVALID_ARG_INDEX);
  CASE(CL_INVALID_ARG_VALUE);
  CASE(CL_INVALID_ARG_SIZE);
  CASE(CL_INVALID_KERNEL_ARGS);
  CASE(CL_INVALID_WORK_DIMENSION);
  CASE(CL_INVALID_WORK_GROUP_SIZE);
  CASE(CL_INVALID_WORK_ITEM_SIZE);
  CASE(CL_INVALID_GLOBAL_OFFSET);
  CASE(CL_INVALID_EVENT_WAIT_LIST);
  CASE(CL_INVALID_EVENT);
  CASE(CL_INVALID_OPERATION);
  CASE(CL_INVALID_GL_OBJECT);
  CASE(CL_INVALID_BUFFER_SIZE);
  CASE(CL_INVALID_MIP_LEVEL);
  CASE(CL_INVALID_GLOBAL_WORK_SIZE);
  CASE(CL_INVALID_PROPERTY);
  CASE(CL_INVALID_IMAGE_DESCRIPTOR);
  CASE(CL_INVALID_COMPILER_OPTIONS);
  CASE(CL_INVALID_LINKER_OPTIONS);
  CASE(CL_INVALID_DEVICE_PARTITION_COUNT);
  CASE(CL_INVALID_PIPE_SIZE);
  CASE(CL_INVALID_DEVICE_QUEUE);
  CASE(CL_INVALID_SPEC_ID);
  CASE(CL_MAX_SIZE_RESTRICTION_EXCEEDED);
  default:
    std::stringstream ss;
    ss << error << "?";
    return ss.str();
  }
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


static uint32_t float_to_bits(float f) {
  union{float f; uint32_t i;} u;
  u.f = f;
  return u.i;
}

static float float_from_bits(uint32_t f) {
    union{float f; uint32_t i;} u;
    u.i = f;
    return u.f;
}

float cls::half_to_float(cl_half h)
{
  uint16_t u16 = h;
  static const int MANTISSA_DIFFERENCE = // 23 - 10
     F32_MANTISSA_BITS - F16_MANTISSA_BITS;
  const int F32_F16_BIAS_DIFFERENCE = 127 - 15;

  uint32_t s32 = ((uint32_t)u16 & F16_SIGN_BIT) << 16;
  uint32_t e16 = (u16 & F16_EXP_MASK) >> F16_MANTISSA_BITS;
  uint32_t m16 = u16 & F16_MANT_MASK;

  uint32_t m32, e32;
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





cl_half cls::float_to_half(float f)
{
  uint32_t f32 = float_to_bits(f);

  uint32_t m32 = F32_MANT_MASK & f32;
  uint32_t e32 = (F32_EXP_MASK & f32) >> F32_MANTISSA_BITS;

  uint32_t m16;
  uint32_t e16;

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
  cl_half h{(uint16_t)(s16 | e16 | m16)};

  return h;
}
