#include "cls.hpp"
#include "system.hpp"
#include "svm.hpp"
#include "text.hpp"

#include <string>
#include <sstream>
#include <utility>
#include <algorithm>
#include <iostream>
#include <fstream>
#include <iomanip>
#include <random>

#include <sstream>
#include <iostream>
// #include <filesystem>
// using namespace std::tr2::sys;
// namespace fs = std::experimental::filesystem;
#include <experimental/filesystem>
namespace fs = std::experimental::filesystem;

using namespace cls;

#if 0
init::init(loc l) : location(l), intval(0), fltval(0) { }

init::~init() {
  if (host_buf.alloc) {
    if (transfer == TRANS_SVM) {
      auto svmFree = GetSVMFree();
      if (svmFree) {
        svmFree((*context)(),host_buf.alloc);
      } else {
        WARNING("unable to find clSVMFree\n");
      }
    } else {
      _aligned_free(host_buf.alloc);
    }
  }
  if (buffer)
    delete buffer;
  if (image)
    delete image;
  if (img)
    delete img;
  for (auto *c : children)
    delete c;
}

void init::str(std::ostream &os) const
{
  switch (type) {
  case LIT_INT:
    os << intval;
    break;
  case LIT_FLT:
    os << intval;
    break;
  case LIT_VEC:
    os << '(';
    for (size_t i = 0; i < children.size(); i++) {
      if (i > 0)
        os << ',';
      children[i]->str(os);
    }
    os << ')';
    break;
  default:
    os << "[" << (int)type << "?]";
  }
}

std::string init::str() const
{
  std::stringstream ss;
  str(ss);
  return ss.str();
}


template <typename T>
static const char *TypeName();
#define TYPE_NAME_INSTANCE(T)             \
    template <>                           \
    static const char *TypeName<T>()      \
    {                                     \
        return #T;                        \
    }                                     \
    template <>                           \
    static const char *TypeName<T##2>()   \
    {                                     \
        return #T "2";                    \
    }                                     \
    template <>                           \
    static const char *TypeName<T##4>()   \
    {                                     \
        return #T "4";                    \
    }                                     \
    template <>                           \
    static const char *TypeName<T##8>()   \
    {                                     \
        return #T "8";                    \
    }                                     \
    template <>                           \
    static const char *TypeName<T##16>()  \
    {                                     \
        return #T "16";                   \
    }

TYPE_NAME_INSTANCE(cl_char);
TYPE_NAME_INSTANCE(cl_uchar);
TYPE_NAME_INSTANCE(cl_short);
TYPE_NAME_INSTANCE(cl_ushort);
TYPE_NAME_INSTANCE(cl_int);
TYPE_NAME_INSTANCE(cl_uint);
TYPE_NAME_INSTANCE(cl_long);
TYPE_NAME_INSTANCE(cl_ulong);
TYPE_NAME_INSTANCE(cl_float);
TYPE_NAME_INSTANCE(cl_double);

#define MAKE_APPEND_ARG_VEC(T, K)                                    \
    static void appendArgStr(std::stringstream &ss, const T##K &t)   \
    {                                                                \
        ss << TypeName<T>() << K << '(';                             \
        appendArgStr(ss, t.s[0]);                                    \
        for (int i = 1; i < K; i++) {                                \
            ss << ',';                                               \
            appendArgStr(ss, t.s[i]);                                \
        }                                                            \
        ss << ')';                                                   \
    }
#define MAKE_APPEND_ARG_VECS(T) \
    MAKE_APPEND_ARG_VEC(T, 2)   \
    MAKE_APPEND_ARG_VEC(T, 4)   \
    MAKE_APPEND_ARG_VEC(T, 8)   \
    MAKE_APPEND_ARG_VEC(T, 16)

#define MAKE_APPEND_ARG(T)                                                     \
    static void appendArgStr(std::stringstream &ss, const T &t) { ss << t; }   \
    MAKE_APPEND_ARG_VECS(T)

// manually expand cl_char and cl_uchar to treat select decimal output instead
// of character output (what << defaults to for char).
static void appendArgStr(std::stringstream &ss, const cl_char &t)
{
  ss << (int)t;
}
MAKE_APPEND_ARG_VECS(cl_char)
static void appendArgStr(std::stringstream &ss, const cl_uchar &t)
{
  ss << (unsigned)t;
}
MAKE_APPEND_ARG_VECS(cl_uchar)
// The rest function normally
MAKE_APPEND_ARG(cl_short)
MAKE_APPEND_ARG(cl_ushort)
MAKE_APPEND_ARG(cl_int)
MAKE_APPEND_ARG(cl_uint)
MAKE_APPEND_ARG(cl_long)
MAKE_APPEND_ARG(cl_ulong)

// MAKE_APPEND_ARG(cl_half)
MAKE_APPEND_ARG(cl_float)
MAKE_APPEND_ARG(cl_double)


// Expands convertTo for integral vector CL-types.
//
// For example:
//   ...
//   init::convertTo<cl_int2>(cl_int2 &t, ...)
//   init::convertTo<cl_int4>(cl_int4 &t, ...)
//   ...
//   init::convertTo<cl_long2>(cl_long2 &t, ...)
//   init::convertTo<cl_long4>(cl_long4 &t, ...)
//   ...
//
// The macro argument T is the type name suffix ('int' for cl_int4),
// the N argument is the vector dimension (e.g. '4' for cl_int4).
#define LIT_CONVERT_TO_INTEGRAL_VEC(T, N)                                      \
  template <>                                                                  \
  void init::convertTo<cl_##T##N>(cl_##T##N & value, const ErrorHandler &err)  \
      const {                                                                  \
    if (isZeroConst()) {                                                       \
      memset(&value, 0, sizeof(value));                                        \
    } else if (type == LIT_INT) {                                              \
      /* fanout the literal: 42 implicitly gets replicated to a vector of */   \
      /* {42, 42, 42, ...} */                                                  \
      for (size_t i = 0; i < (N); i++) {                                       \
        convertTo<cl_##T>(value.s[i], err);                                    \
      }                                                                        \
    } else if (type != LIT_VEC) {                                              \
      err("literal must be a vector");                                         \
    } else if (children.size() != (N)) {                                       \
      err("literal must have " #N " elements");                                \
    } else {                                                                   \
      for (size_t i = 0; i < (N); i++) {                                       \
        children[i]->convertTo<cl_##T>(value.s[i], err);                       \
      }                                                                        \
    }                                                                          \
  }                                                                            \
                                                                               \
  template <>                                                                  \
  void init::convertTo<cl_u##T##N>(cl_u##T##N & value,                         \
                                   const ErrorHandler &err) const {            \
    if (isZeroConst()) {                                                       \
      memset(&value, 0, sizeof(value));                                        \
    } else if (type == LIT_INT) {                                              \
      /* broadcast the literal */                                              \
      for (size_t i = 0; i < (N); i++) {                                       \
        convertTo<cl_u##T>(value.s[i], err);                                   \
      }                                                                        \
    } else if (type != LIT_VEC) {                                              \
      err("literal must be a vector");                                         \
    } else if (children.size() != (N)) {                                       \
      err("literal must have " #N " elements");                                \
    } else {                                                                   \
      for (size_t i = 0; i < (N); i++) {                                       \
        children[i]->convertTo<cl_u##T>(value.s[i], err);                      \
      }                                                                        \
    }                                                                          \
  }

// Expands convertTo for scalar instances of OpenCL integral primitive types.
// This expands all vector instances for this integral type as well.
//
// E.g. init::convert<cl_ushort>(cl_ushort &, ...)
// T is the type name suffix 'short' for cl_short.
// T_UPPER is the uppercase name ('INT' for cl_int, 'SHRT' for short) so
// we can range change via MIN and MAX constants like CL_MAX_SHRT
#define LIT_CONVERT_TO_INTEGRAL(T, T_UPPER)                                    \
  template <>                                                                  \
  void init::convertTo<cl_##T>(cl_##T & value, const ErrorHandler &err)        \
      const {                                                                  \
    if (type != LIT_INT) {                                                     \
      err("literal must be integral");                                         \
    } else if (intval < CL_##T_UPPER##_MIN || intval > CL_##T_UPPER##_MAX) {   \
      err("literal out of range for a " #T);                                   \
    }                                                                          \
    value = (cl_##T)intval;                                                    \
  }                                                                            \
                                                                               \
  template <>                                                                  \
  void init::convertTo<cl_u##T>(cl_u##T & value, const ErrorHandler &err)      \
      const {                                                                  \
    if (type != LIT_INT) {                                                     \
      err("literal must be integral");                                         \
    } else if (intval < 0 || intval > CL_U##T_UPPER##_MAX) {                   \
      err("literal out of range for a u" #T);                                  \
    }                                                                          \
    value = (cl_u##T)intval;                                                   \
  }                                                                            \
                                                                               \
  LIT_CONVERT_TO_INTEGRAL_VEC(T, 2)                                            \
  LIT_CONVERT_TO_INTEGRAL_VEC(T, 4)                                            \
  LIT_CONVERT_TO_INTEGRAL_VEC(T, 8)                                            \
  LIT_CONVERT_TO_INTEGRAL_VEC(T, 16)

// Expand the integral instances.
LIT_CONVERT_TO_INTEGRAL(char, CHAR)
LIT_CONVERT_TO_INTEGRAL(short, SHRT)
LIT_CONVERT_TO_INTEGRAL(int, INT)
LIT_CONVERT_TO_INTEGRAL(long, LONG)

// Constructs instances for a vector type.
#define LIT_CONVERT_TO_FLOATING_VEC(T, N)                                      \
  template <>                                                                  \
  void init::convertTo<cl_##T##N>(cl_##T##N & value, const ErrorHandler &err)  \
      const {                                                                  \
    if (isZeroConst()) {                                                       \
      memset(&value, 0, sizeof(value));                                        \
    } else if (type == LIT_FLT) {                                              \
      /* broadcast literal */                                                  \
      for (size_t i = 0; i < (N); i++) {                                       \
        convertTo<cl_##T>(value.s[i], err);                                    \
      }                                                                        \
    } else if (type == LIT_INT) {                                              \
      /* broadcast literal */                                                  \
      for (size_t i = 0; i < (N); i++) {                                       \
        convertTo<cl_##T>(value.s[i], err);                                    \
      }                                                                        \
    } else if (type != LIT_VEC) {                                              \
      err("literal must be a vector");                                         \
    } else if (children.size() != (N)) {                                       \
      err("literal must have " #N " elements");                                \
    } else {                                                                   \
      for (size_t i = 0; i < (N); i++) {                                       \
        children[i]->convertTo<cl_##T>(value.s[i], err);                       \
      }                                                                        \
    }                                                                          \
  }

// Constructs all the floating point instances of convertTo for
// a given floating point type.  This includes the scalar and vector
// instances both.
//
// T is the type name suffix.  E.g. 'double' for cl_double
// T_UPPER is the uppercase name used in the CL_[T_UPPER]_MAX.
//   E.g. 'DOUBLE' for for cl_double (to construct CL_DOUBLE_MAX)
#define LIT_CONVERT_TO_FLOATING(T, T_UPPER)                                    \
  template <>                                                                  \
  void init::convertTo<cl_##T>(cl_##T & value, const ErrorHandler &err)        \
      const {                                                                  \
    if (isZeroConst()) {                                                       \
      memset(&value, 0, sizeof(value));                                        \
    } else if (type == LIT_FLT) {                                              \
      value = (cl_##T)fltval;                                                  \
    } else if (type == LIT_INT) {                                              \
      value = (cl_##T)intval;                                                  \
    } else {                                                                   \
      err("literal must be floating point (or integral)");                     \
    }                                                                          \
  }                                                                            \
  LIT_CONVERT_TO_FLOATING_VEC(T, 2)                                            \
  LIT_CONVERT_TO_FLOATING_VEC(T, 4)                                            \
  LIT_CONVERT_TO_FLOATING_VEC(T, 8)                                            \
  LIT_CONVERT_TO_FLOATING_VEC(T, 16)

LIT_CONVERT_TO_FLOATING(float, FLT)
LIT_CONVERT_TO_FLOATING(double, DBL)

#define DISPATCH_INTEGRAL_TYPES_VEC_CASE(FUNC, T, N)                           \
  case N:                                                                      \
    if (a.elem_signed) {                                                       \
      cl_##T##N value;                                                         \
      i.convertTo<cl_##T##N>(value, err);                                      \
      FUNC<cl_##T##N>(value, arg_ix, extra_arg, err);                          \
    } else {                                                                   \
      cl_u##T##N value;                                                        \
      i.convertTo<cl_u##T##N>(value, err);                                     \
      FUNC<cl_u##T##N>(value, arg_ix, extra_arg, err);                         \
    }                                                                          \
    break
#define DISPATCH_INTEGRAL_TYPES(FUNC, T)                                       \
  switch (a.vec_width) {                                                       \
  case 1:                                                                      \
    if (a.elem_signed) {                                                       \
      cl_##T value;                                                            \
      i.convertTo<cl_##T>(value, err);                                         \
      FUNC<cl_##T>(value, arg_ix, extra_arg, err);                             \
    } else {                                                                   \
      cl_u##T value;                                                           \
      i.convertTo<cl_u##T>(value, err);                                        \
      FUNC<cl_u##T>(value, arg_ix, extra_arg, err);                            \
    }                                                                          \
    break;                                                                     \
  DISPATCH_INTEGRAL_TYPES_VEC_CASE(FUNC, T, 2);                                \
  DISPATCH_INTEGRAL_TYPES_VEC_CASE(FUNC, T, 4);                                \
  DISPATCH_INTEGRAL_TYPES_VEC_CASE(FUNC, T, 8);                                \
  DISPATCH_INTEGRAL_TYPES_VEC_CASE(FUNC, T, 16);                               \
  default:                                                                     \
    err("unsupported vector size on kernel arg");                              \
  }                                                                            \
  break

#define DISPATCH_FLOATING_TYPES_VEC_CASE(FUNC, T, N)                           \
  case N: {                                                                    \
    cl_##T##N value;                                                           \
    i.convertTo<cl_##T##N>(value, err);                                        \
    FUNC<cl_##T##N>(value, arg_ix, extra_arg, err);                            \
    break;                                                                     \
  }
#define DISPATCH_FLOATING_TYPES(FUNC, T)                                       \
  switch (a.vec_width) {                                                       \
  case 1: {                                                                    \
    cl_##T value;                                                              \
    i.convertTo<cl_##T>(value, err);                                           \
    FUNC<cl_##T>(value, arg_ix, extra_arg, err);                               \
    break;                                                                     \
  }                                                                            \
  DISPATCH_FLOATING_TYPES_VEC_CASE(FUNC, T, 2);                                \
  DISPATCH_FLOATING_TYPES_VEC_CASE(FUNC, T, 4);                                \
  DISPATCH_FLOATING_TYPES_VEC_CASE(FUNC, T, 8);                                \
  DISPATCH_FLOATING_TYPES_VEC_CASE(FUNC, T, 16);                               \
  default:                                                                     \
    err("unsupported vector size on kernel arg");                              \
  }                                                                            \
  break

template <typename T>
static void
setScalarArg(
  const T &value,
  cl_uint arg_ix,
  kernel &k,
  const ErrorHandler &err)
{
  try {
    std::stringstream ss;
    appendArgStr(ss,value);
    k.argStrs.push_back(ss.str());
    k.cl_kernel->setArg(arg_ix, value);
  } catch (const cl::Error &) {
    err("unable to set argument");
  }
}

static void SetScalarArg(
  const init &i,
  const arg &a,
  cl_uint arg_ix,
  kernel& extra_arg,
  const ErrorHandler &err)
{
  DEBUG("setting scalar arg %s\n", a.toSyntax().c_str());
  if (a.elem_class == INTEGRAL) {
    switch (a.elem_size) {
    case 1: // cl_u?char[n]
      DISPATCH_INTEGRAL_TYPES(setScalarArg, char);
    case 2: // cl_u?short[n]
      DISPATCH_INTEGRAL_TYPES(setScalarArg, short);
    case 4: // cl_u?int[n]
      DISPATCH_INTEGRAL_TYPES(setScalarArg, int);
    case 8: // cl_u?long[n]
      DISPATCH_INTEGRAL_TYPES(setScalarArg, long);
    default:
      err("unsupported kernel arg type vector size");
    }
  } else if (a.elem_class == FLOATING) {
    switch (a.elem_size) {
    case 4: // cl_?float[n]
      DISPATCH_FLOATING_TYPES(setScalarArg, float);
    case 8: // cl_?double[n]
      DISPATCH_FLOATING_TYPES(setScalarArg, double);
    }
  } else {
    err("unsupported kernel arg type");
  }
}

static void SetGlobalBufferArg(
  init &init,
  cl_uint arg_ix,
  kernel &k,
  const ErrorHandler &eh)
{
  std::stringstream ss;
  cl_mem ptr = (*init.buffer)();
  size_t size = init.buffer->getInfo<CL_MEM_SIZE>();
  ss << "B" << std::setfill('0') << std::setw(sizeof(void*) == 4 ? 8 : 16) << ptr;
  ss << "[";
  if (init.host_buf.mem_size % (1024*1024) == 0) {
    ss << init.host_buf.mem_size/1024/1024 << "MB";
  } else if (init.host_buf.mem_size % 1024 == 0) {
    ss << init.host_buf.mem_size/1024 << "KB";
  } else {
    ss << init.host_buf.mem_size << "B";
  }
  ss << "]";
  k.argStrs.push_back(ss.str());

  DEBUG("setting buffer arg %s\n", k.argStrs.back().c_str());
  try {
    k.cl_kernel->setArg(arg_ix, *init.buffer);
  } catch (const cl::Error &e) {
    std::stringstream ss;
    ss << "INTERNAL ERROR: " << e.what() << ": " << e.err();
    eh(ss.str().c_str());
  }
}

static void SetGlobalImageArg(
  init &init,
  cl_uint arg_ix,
  kernel &k,
  const ErrorHandler &eh)
{
  std::stringstream ss;
  cl_mem ptr = (*init.image)();
  ss << "I" << std::setfill('0') << std::setw(sizeof(void*) == 4 ? 8 : 16) << ptr;
  ss << "[";
  // size_t bs = init.img->pitch * init.img->height * 3;
  size_t size = init.image->getInfo<CL_MEM_SIZE>();
  if (size % (1024*1024) == 0) {
    ss << size/1024/1024 << "MB";
  } else if (size % 1024 == 0) {
    ss << size/1024 << "KB";
  } else {
    ss << size << "B";
  }
  ss << "]";
  k.argStrs.push_back(ss.str());

  DEBUG("setting image arg %s\n", k.argStrs.back().c_str());
  try {
    k.cl_kernel->setArg(arg_ix, *init.image);
  } catch (const cl::Error &e) {
    std::stringstream ss;
    ss << "INTERNAL ERROR: " << e.what() << ": " << e.err();
    eh(ss.str().c_str());
  }
}

template <typename T>
static void
initMem(
  const T &prototype,
  cl_uint arg_ix,
  struct hostbuf &hmem,
  const ErrorHandler &err)
{
  T *mem = (T *)hmem.mem;
  for (size_t i = 0, len = hmem.mem_size/sizeof(T); i < len; i++)
    mem[i] = prototype;
}

static void InitializeBuffer(
  const init &i,
  const arg &a,
  cl_uint arg_ix,
  struct hostbuf& extra_arg,
  const ErrorHandler &err)
{
  DEBUG("setting buffer arg %s\n", a.toSyntax().c_str());
  if (i.type == init::LIT_INT ||
    i.type == init::LIT_FLT ||
    i.type == init::LIT_VEC)
  {
    if (a.elem_class == INTEGRAL) {
      switch (a.elem_size) {
      case 1: // cl_u?char[n]
        DISPATCH_INTEGRAL_TYPES(initMem, char);
      case 2: // cl_u?short[n]
        DISPATCH_INTEGRAL_TYPES(initMem, short);
      case 4: // cl_u?int[n]
        DISPATCH_INTEGRAL_TYPES(initMem, int);
      case 8: // cl_u?long[n]
        DISPATCH_INTEGRAL_TYPES(initMem, long);
      default:
        err("unsupported kernel arg type vector size");
      }
    } else if (a.elem_class == FLOATING) {
      switch (a.elem_size) {
//      case 2: // cl_half[n]
//        DISPATCH_FLOATING_TYPES(initMem, uint16_t);
      case 4: // cl_float[n]
        DISPATCH_FLOATING_TYPES(initMem, float);
      case 8: // cl_double[n]
        DISPATCH_FLOATING_TYPES(initMem, double);
      }
    } else {
      err("unsupported kernel arg type");
    }
  } else if (
    i.type == init::LIT_SEQ ||
    i.type == init::LIT_CYC ||
    i.type == init::LIT_RND)
  {
    union {
      double  f64;
      int64_t i64;
    } val;

    std::random_device rd;
    auto seed = i.rand_seed == 0 ? rd() : i.rand_seed;
    std::mt19937 gen(seed);
    std::uniform_int_distribution<int64_t> unif_int;
    std::uniform_real_distribution<> unif_flt(0,1);

    auto next_value = [&](size_t ix) {
      if (i.type == init::LIT_SEQ) {
        val.i64 = i.seq_start + i.seq_delta * (int64_t)ix;
        if (a.elem_class == FLOATING)
          val.f64 = (double)val.i64;
      } else if (i.type == init::LIT_CYC) {
        val.i64 = i.cycle_values[ix % i.cycle_values.size()];
        if (a.elem_class == FLOATING)
          val.f64 = (double)val.i64;
      } else if (i.type == init::LIT_RND) {
        if (a.elem_class == FLOATING) {
          val.f64 = unif_flt(gen);
        } else {
          val.i64 = unif_int(gen);
        }
      } else {
        FATAL("INTERNAL ERROR: unhandled init function\n");
      }
    };

    char *ptr = (char *)extra_arg.mem;
    const char *end = ptr + extra_arg.mem_size;
    size_t ix = 0; // seq ix
    while (ptr < end) {
      for (size_t v = 0; v < a.vec_width; v++) {
        next_value(ix++);
        switch (a.elem_size) {
        case 1:
          if (a.elem_class == FLOATING) {
            FATAL("8-bit floats not supported yet\n");
          } else {
            *(uint8_t *)ptr = (uint8_t)val.i64;
          }
          break;
        case 2:
          if (a.elem_class == FLOATING) {
            FATAL("half floats not supported yet\n");
          } else {
            *(uint16_t *)ptr = (uint16_t)val.i64;
          }
          break;
        case 4:
          if (a.elem_class == FLOATING) {
            *(float *)ptr = (float)val.f64;
          } else {
            *(uint32_t *)ptr = (uint32_t)val.i64;
          }
          break;
        case 8:
          if (a.elem_class == FLOATING) {
            *(double *)ptr = (double)val.f64;
          } else {
            *(uint64_t *)ptr = (uint64_t)val.i64;
          }
          break;
        default:
          FATAL("unreachable");
        }
        ptr += a.elem_size;
      } // for
    } // while
  } else {
    FATAL("INTERNAL ERROR: unsupported init type\n");
  }
}


void init::initHostMem(arg &arg, cl_uint arg_ix, const ErrorHandler &eh)
{
  if (buffer) {
    InitializeBuffer(
      *this,
      arg,
      arg_ix,
      host_buf,
      eh);
  } else if (image) {
    // no work needed, the backing buffer is in img
  }
}

void init::readDevMem(cl::CommandQueue &cq, const ErrorHandler &eh)
{
  if (buffer) {
    switch (transfer) {
    case TRANS_MAP: {
      void *mptr = cq.enqueueMapBuffer(
        *buffer, CL_TRUE, CL_MEM_READ_ONLY, 0, host_buf.mem_size);
      if (mptr != host_buf.mem) {
        WARNING("buffer map generated new copy\n");
        memcpy(host_buf.mem,mptr,host_buf.mem_size);
      }
      cq.enqueueUnmapMemObject(*buffer, mptr);
      break;
    }
    case TRANS_COPY:
      cq.enqueueReadBuffer(*buffer, CL_TRUE, 0, host_buf.mem_size, host_buf.mem);
      break;
    case TRANS_SVM:
      // no op
      break;
    default:
      eh("unsupported transfer");
    }
  } else if (image) {
    cl::array<size_t,3> origin;
    cl::array<size_t,3> region;
    region[0] = img->width;
    region[1] = img->height;
    region[2] = 1;
    cq.enqueueReadImage(
      *image, CL_TRUE, origin, region, img->pitch, 0, img->bits);
    if (save_post) {
      img->save_bmp(fileval.c_str());
    }
  }
}

void init::writeDevMem(cl::CommandQueue &cq, const ErrorHandler &eh) {
  try {
    if (buffer) {
      switch (transfer) {
      case TRANS_MAP:
        break;
      case TRANS_COPY:
        cq.enqueueWriteBuffer(*buffer, CL_TRUE, 0, host_buf.mem_size, host_buf.mem);
        break;
      case TRANS_SVM:
        // no op
        break;
      default:
        eh("unsupported transfer");
      }
    } else if (image) {
      cl::array<size_t,3> origin;
      cl::array<size_t,3> region;
      region[0] = img->width;
      region[1] = img->height;
      region[2] = 1;
      cq.enqueueWriteImage(
        *image, CL_TRUE, origin, region, img->pitch, 0, img->bits);
    }
  } catch (const cl::Error &e) {
    std::stringstream ss;
    ss << e.what() << " " << e.err() << " (" << status_to_symbol(e.err()) << ")";
    eh(ss.str().c_str());
  }
}

void init::setKernelArg(
  ndr &c,
  kernel &k,
  arg &a,
  cl_uint arg_ix,
  cl::Context *pContext,
  const ErrorHandler &eh)
{
  context = pContext;
  if (a.type_class == BUFFER) {
    setKernelArgBuf(c, k, a, arg_ix, pContext, eh);
  } else if (a.type_class == IMAGE2D) {
    setKernelArgImg(c, k, a, arg_ix, pContext, eh);
  } else if (
    a.type_class == VECTOR ||
    a.elem_class == INTEGRAL ||
    a.elem_class == FLOATING)
  {
    SetScalarArg(*this, a, arg_ix, k, eh);
  } else {
    eh("unsupported argument class");
  }
}

void init::setKernelArgBuf(
  ndr &c,
  kernel &k,
  arg &a,
  cl_uint arg_ix,
  cl::Context *ctx,
  const ErrorHandler &eh)
{
  size_t total_elems = 0;
  if (explicit_element_count > 0) {
    // explicit dimension. E.g. "0:[256]rw"
    total_elems = explicit_element_count;
  } else {
    // implicit dimension e.g. "0:w" or (1,2,3,4):w
    auto calcTotalWorkitems = [](const cl::NDRange &nd) {
      if (nd.dimensions() == 0) { // cl::NullRange or inferred
        return (size_t)0;
      } else {
        const size_t *dptr = nd;
        size_t prod        = 1;
        for (size_t i = 0; i < nd.dimensions(); i++) {
          prod *= dptr[i];
        }
        return prod;
      }
    };
    if (a.addr_qual == CL_KERNEL_ARG_ADDRESS_GLOBAL) {
      total_elems = calcTotalWorkitems(c.global_size);
    } else if (a.addr_qual == CL_KERNEL_ARG_ADDRESS_LOCAL) {
      total_elems = calcTotalWorkitems(c.local_size);
      if (total_elems == 0) {
        eh("cannot infer initializer dimension for local memory object; "
          "program requires non-null (explicit) workgroup size for "
          "auto-sized local buffers");
      }
    } else {
      eh("cannot infer initializer dimension for address qualifier type");
    }
  }

  host_buf.mem_size = total_elems * a.elem_size * a.vec_width;
  host_buf.alloc_size = host_buf.mem_size;
  if (a.addr_qual == CL_KERNEL_ARG_ADDRESS_GLOBAL) {
    cl_mem_flags flags = 0;
    if (buffer_r && buffer_w)
      flags |= CL_MEM_READ_WRITE;
    else if (buffer_r)
      flags |= CL_MEM_READ_ONLY;
    else if (buffer_w)
      flags |= CL_MEM_WRITE_ONLY;
    else
      WARNING("buffer cl_mem_flags do not contain read or write bits!\n");

    if (transfer == init::TRANS_MAP || transfer == init::TRANS_COPY) {
      // let the driver allocate it
      // explicitly allocate it
      host_buf.alloc_size =
        sys::align_round_up<size_t>(host_buf.mem_size,4096);
      host_buf.mem = host_buf.alloc =
        _aligned_malloc(host_buf.alloc_size,4096);
      if (!host_buf.mem)
        eh("allocation of backing buffer failed");
      if (transfer == init::TRANS_MAP) {
        buffer = new cl::Buffer(
          *context,
          flags | CL_MEM_USE_HOST_PTR,
          host_buf.mem_size,
          host_buf.mem);
      } else {
        buffer = new cl::Buffer(
          *context,
          flags,
          host_buf.mem_size);
      }
    } else if (transfer == init::TRANS_SVM) {
      eh("svm allocation not supported");
      clSVMAllocType allocFunc = GetSVMAlloc();
      if (!allocFunc) {
        eh("unable to bind to clSVMAlloc(...)");
      }
      if (use_svm_fine_grained) {
        flags |= CL_MEM_SVM_FINE_GRAIN_BUFFER;
      }
      if (use_svm_atomics) {
        flags |= CL_MEM_SVM_ATOMICS;
      }
      host_buf.alloc = host_buf.mem =
        allocFunc((*context)(), flags, host_buf.alloc_size, 4096);
    } else {
      eh("unsupported transfer type");
    }
  } else if (a.addr_qual == CL_KERNEL_ARG_ADDRESS_LOCAL) {
    // k->define_next_argument_slm(marg.memlen);
    eh("local buffer not supported yet");
  } else {
    eh("only global and local buffers are supported");
  }

  if (type == LIT_FILE) {
    fs::path fp = fileval;
    auto st = fs::status(fp);
    if (!fs::exists(st)) {
      eh((fp.string() + ": file not found").c_str());
    } else if (!is_regular_file(st)) {
      eh((fp.string() + ": not a regular file").c_str());
    }
    auto bs = sys::read_file_binary(fileval);
    if (bs.size() != host_buf.mem_size) {
      WARNING("buffer initializer is wrong size for buffer %lld\n",
        (uint64_t)bs.size());
    }
    filebytes.resize(host_buf.mem_size);
    memcpy(filebytes.data(), bs.data(), bs.size());
  }

  SetGlobalBufferArg(*this, arg_ix, k, eh);
}

void init::setKernelArgImg(
  ndr &c,
  kernel &k,
  arg &a,
  cl_uint arg_ix,
  cl::Context *ctx,
  const ErrorHandler &eh)
{
  ::image *img = nullptr;
  if (buffer_r) {
    if (!sys::file_exists(fileval.c_str())) {
      eh((fileval + ": file not found").c_str());
    }
    img = image::load_bmp(fileval.c_str(), false);
    if (img == nullptr) {
      eh((fileval + ": malformed BMP").c_str());
    } else if (c.global_size.dimensions() != 2) {
      eh((fileval + ": NDRange must be 2 dimensions").c_str());
    } else if (img->width > c.global_size[0] || img->height > c.global_size[1]) {
      img->resize(c.global_size[0], c.global_size[1]);
    }
    *img = img->convert(image::RGBA);
  } else if (buffer_w) {
    img = new ::image(c.global_size[0], c.global_size[1], image::RGBA);
    if (!img)
      eh((fileval + ": image allocation failed").c_str());
    if (!fileval.empty()) {
      save_post = true;
    }
  } else {
    eh("image needs read or write flag");
  }

  cl_mem_flags flags = CL_MEM_ALLOC_HOST_PTR;
  if (buffer_r)
    flags |= CL_MEM_COPY_HOST_PTR;
  if (buffer_r && buffer_w)
    flags |= CL_MEM_READ_WRITE;
  else if (buffer_r)
    flags |= CL_MEM_READ_ONLY;
  else if (buffer_w)
    flags |= CL_MEM_WRITE_ONLY;
  else
    WARNING("image cl_mem_flags do not contain read or write bits!\n");
  cl::ImageFormat img_fmt(CL_RGBA,CL_UNORM_INT8);
  // cl::ImageFormat img_fmt(CL_RGB,CL_UNORM_SHORT_555);
  try {
    image = new cl::Image2D(
      *ctx,
      flags,
      img_fmt,
      c.global_size[0],
      c.global_size[1],
      0, // img->pitch since the driver is allocating it
      buffer_r ? img->bits : nullptr);
  } catch (const cl::Error &e) {
    std::stringstream ss;
    ss << "clCreateImage: " << e.err() <<
      " (" << status_to_symbol(e.err()) << ")";
    eh(ss.str().c_str());
  }
  this->img = img;
  SetGlobalImageArg(*this, arg_ix, k, eh);
}

void ndr::initBuffers(cl::CommandQueue &cq) {
  for (cl_uint arg_ix = 0; arg_ix < inits.size(); arg_ix++) {
    init &i = *inits[arg_ix];
    arg &a = entry->args[arg_ix];
    ErrorHandler eh = [&](const char *msg) {
      std::stringstream ss;
      ss << "(re-)initializing buffer or image argument " << msg;
      diag d(i.location, ss.str(), source);
      FATAL("%s", d.toString().c_str());
    };
    DEBUG("initializing buffer or image arg: %s\n", a.toSyntax().c_str());
    i.initHostMem(a, arg_ix, eh);
    if (i.buffer && i.display_pre) {
      std::cout << "PRE  " << a.toSyntax() << "\n";
      text::format_buffer(
        std::cout,
        i.host_buf.mem,
        i.host_buf.mem_size,
        a.elem_size * a.vec_width,
        a.vec_width,
        a.elem_class == FLOATING);
    }
    DEBUG("writing buffer/image arg: %s\n", a.toSyntax().c_str());
    i.writeDevMem(cq, eh);
  } // for args
}

void ndr::readDevMem(cl::CommandQueue &cq) {
  for (cl_uint arg_ix = 0; arg_ix < inits.size(); arg_ix++) {
    init &i = *inits[arg_ix];
    arg &a = entry->args[arg_ix];
    ErrorHandler eh = [&](const char *msg) {
      std::stringstream ss;
      ss << "(re-)initializing buffer argument " << msg;
      diag d(i.location, ss.str(), source);
      FATAL("%s",d.toString().c_str());
    };
    i.readDevMem(cq, eh);
    if (i.buffer && i.display_pst) {
      std::cout << "POST " << a.toSyntax() << "\n";
      // print the buffer out
      text::format_buffer(
        std::cout,
        i.host_buf.mem,
        i.host_buf.mem_size,
        a.elem_size * a.vec_width,
        a.vec_width,
        a.elem_class == FLOATING);
    }
    if (i.save_post) {
      if (i.img) {
        std::stringstream ss;
        ss << a.name << ".bmp";
        i.img->save_bmp(ss.str().c_str());
      } else {
        std::stringstream ss;
        ss << a.name << ".buf";
        std::ofstream os(ss.str(), std::ofstream::out | std::ofstream::binary);
        os.write((const char *)i.host_buf.mem, i.host_buf.mem_size);
      }
    }
  } // for
}

void ndr::str(std::ostream &os) const {
  auto emitDim = [&os](const cl::NDRange &ndr) {
    for (size_t i = 0; i < ndr.dimensions(); i++) {
      if (i > 0) {
        os << 'x';
      }
      os << ndr[i];
    }
  };
  // cl_kernel k = (*entry->cl_kernel)();
  // os << 'K' << (void *)k;
  os << entry->name;
  os << '<';
  emitDim(global_size);
  if (local_size.dimensions() != cl::NullRange.dimensions()) {
    os << ',';
    emitDim(local_size);
  }
  os << '>';
  os << '(';
  for (size_t i = 0; i < entry->argStrs.size(); i++) {
    if (i > 0) {
      os << ", ";
    }
    os << entry->argStrs[i];
  }
  os << ')';
  os << '\n';
}

std::string ndr::str() const
{
  std::stringstream ss;
  str(ss);
  return ss.str();
}

#endif