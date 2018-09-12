#ifndef CLS_HPP_
#define CLS_HPP_

#include "image.hpp"
#include "lexemes.hpp"

#include <exception>
#include <functional>
#include <cstdint>
#include <iostream>
#include <sstream>
#include <string>
#include <utility>
#include <vector>
#define __CL_ENABLE_EXCEPTIONS
#include <CL/cl.hpp>


namespace cls {
struct filtered_stream {
    bool            should_log;
    std::ostream   &underlying_stream;

    filtered_stream(
      bool _should_log,
      std::ostream &_underlying_stream = std::cout)
      : should_log(_should_log)
      , underlying_stream(_underlying_stream)
    {
    }

    template <typename T>
    const filtered_stream& operator<<(const T &v) const {
        if (should_log) {
            std::cout << v;
        }
        return *this;
    }
};

struct Opts {
  int                         verbosity = 0;
  std::string                 input;
  std::vector<cl::Device>     devices;
  int                         iterations = 1;
  int                         enqueues = 1;
  bool                        list_devices = false;
  bool                        wall_time = false;
  bool                        prof_time = false;

  filtered_stream always() const{return filtered_stream(true);}
  filtered_stream normal() const{return filtered_stream(verbosity >= 0);}
  filtered_stream verbose() const{return filtered_stream(verbosity >= 1);}
  filtered_stream debug() const{return filtered_stream(verbosity >= 2);}
};

struct loc {
  uint32_t line;
  uint32_t col;
  uint32_t offset; // file offset
  uint32_t extent; // length (in characters/bytes)

  loc() {}
  loc(
    uint32_t ln,
    uint32_t cl,
    uint32_t off,
    uint32_t len)
    : line(ln)
    , col(cl)
    , offset(off)
    , extent(len)
  {
  }
  bool operator==(const loc& rhs) const {
    return
      line == rhs.line &&
      col == rhs.col &&
      offset == rhs.offset &&
      extent == rhs.extent;
  }
  bool operator!=(const loc& rhs) const {
    return !(*this == rhs);
  }
  bool operator>(const loc& rhs) const {
    return offset > rhs.offset;
  }
  bool operator>=(const loc& rhs) const {
    return offset >= rhs.offset;
  }
  bool operator<(const loc& rhs) const {
    return offset < rhs.offset;
  }
  bool operator<=(const loc& rhs) const {
    return offset <= rhs.offset;
  }

  static loc INVALID;
};

typedef std::function<void(const char *)> ErrorHandler;

struct hostbuf {
  void *mem = nullptr;
  size_t mem_size = 0;
  void *alloc = nullptr;
  size_t alloc_size = 0;
};

struct arg;
struct ndr;
struct kernel;

// Respresents literal initializers for kernel arguments.
// This is implemented as a tree to support structural elements.
struct init {
  loc location;
  // the syntax we parsed to create this literal.
  // This may not be set for subnodes.
  std::string syntax;

  enum {
    LIT_INVALID = 0,
    LIT_REC,  // {c1, c2, ...} children hold c1, c2, ...
    LIT_VEC,  // (c1, c2, ...) children hold c1, c2, ...
    LIT_INT,  // "0", "0:w", "0x123:w", "-34:r"
    LIT_FLT,  // "1.2", "1.2:rw"
    LIT_SYM,  // a symbol (like "^g.x")
    LIT_FILE, // "image(foo.bmp):w"
    LIT_SEQ,  // "seq:r" or "seq(start,delta):r"
    LIT_CYC,  // "cyc(x1,x2,...)"
              // TODO: there should be a second argument to specify the image type
              // If specified as an input, this loads as a buffer or image of
              // RGB24 or an intensity image if the BMP is monochrome.
    LIT_RND,  // e.g. rand, rand(13)
  } type = LIT_INVALID;

  std::vector<struct init *> children;
  // TODO: factor value type into this (an imm_val)
  union {
    double fltval;
    int64_t intval;
  };
  std::string fileval;
  std::vector<uint8_t> filebytes; // 'foo.bmp'

  // if the type is 'ramp' these are optional arguments.
  // TODO: lower to children
  int64_t seq_start = 0, seq_delta = 1; // could be struct lit's themselves?
  // TODO: lower into children
  std::vector<int64_t> cycle_values;
  // TODO: lower to children
  int32_t rand_seed = 0;

  // For an optional explicit dimension
  //  E.g. <lit>:[1024]rw
  // Set to 0 by default.
  // The dimension is in elements
  size_t explicit_element_count = 0; // [1024*g.x]

  // buffer flags
  bool buffer_r = false; // <lit>:r (can be combined with w)
  bool buffer_w = false; // <lit>:w (can be combined with r)

  bool display_pre = false;        // <lit>:p
 // size_t display_pre_pw = false;   // <lit>:[pP]## (elements to show per line)
  bool display_pre_as_hex = false; // <lit>:px (forces hex printing)

  bool display_pst = false;        // <lit>:P
 // size_t display_pst_pw;   // ...
  bool display_pst_as_hex = false; // <lit>:Px (forces hex printing)

  bool save_post = false; // <lit>:S (save after op)

  enum {
    TRANS_INVALID,
    TRANS_MAP,  // :m -> mirror memory/ALLOC_HOST_POINTER clEnqueueMapBuffer
    TRANS_COPY, // :c -> mirror memory and clEnqueue{Write,Read}Buffer
    TRANS_SVM,  // :v -> clSVMAlloc (or :vf for fine grained)
  } transfer = TRANS_INVALID;
  bool use_svm_fine_grained = false; // <lit>:vf use CL_MEM_SVM_FINE_GRAIN_BUFFER
  bool use_svm_atomics = false;      // <lit>:va or <lit>:vfa use CL_MEM_SVM_ATOMICS

  cl::Image2D *image = nullptr;
  ::image *img = nullptr;
  cl::Buffer *buffer = nullptr;
  cl::Context *context = nullptr;
  hostbuf host_buf; // backing memory (if applicable)
  void setKernelArg(ndr &c, kernel &k, arg &a, cl_uint arg_ix, cl::Context *ctx, const ErrorHandler &eh);
  void setKernelArgBuf(ndr &c, kernel &k, arg &a, cl_uint arg_ix, cl::Context *ctx, const ErrorHandler &eh);
  void setKernelArgImg(ndr &c, kernel &k, arg &a, cl_uint arg_ix, cl::Context *ctx, const ErrorHandler &eh);
  void initHostMem(arg &arg, cl_uint arg_ix, const ErrorHandler &eh);
  void readDevMem(cl::CommandQueue &cq, const ErrorHandler &eh);
  void writeDevMem(cl::CommandQueue &cq, const ErrorHandler &eh);

  init(loc l);
  ~init();

  void str(std::ostream &os) const;
  std::string str() const;

  bool canVectorize() const {
    return (type == LIT_FLT || type == LIT_INT) && children.size() == 0;
  }

  bool isZeroConst() const { return type == LIT_INT && intval == 0; }

  // This templated method is defined below for many types through a bunch
  // of macro expansions for all OpenCL primitive types.
  //
  // The first argument 't' is the type to be assigned by this literal object.
  // If the conversion is invalid, then the errhandler is invoked with a
  // diagnostic.
  template <typename T>
  void convertTo(T &t, const ErrorHandler &errhandler) const;
}; // init



enum arg_type {
  INVALID = 0,
  OTHER,
  INTEGRAL,       // E.g. uchar, int (not vector types)
  FLOATING,       // E.g. float, double (not vector types)
  VECTOR,         // E.g. an opencl vector (e.g. uchar16 or float2)
  STRUCT,         // E.g. a user-defined structure
  BUFFER,         // E.g. global int* buf (or const int *buf)
  IMAGE2D,        // image2d_t
  IMAGE3D,        // image3d_t
  IMAGE2D_ARRAY,  // image2d_array_t
  IMAGE1D,        // image1d_t
  IMAGE1D_BUFFER, // image1d_buffer_t
  IMAGE1D_ARRAY,  // image1d_array_t
  SAMPLER,        // sampler_t
  EVENT,          // event_t
};

struct arg {
  // there's a const in front of the type
  loc                              location;

  bool                             has_const = false;

  cl_kernel_arg_address_qualifier  addr_qual = CL_KERNEL_ARG_ADDRESS_PRIVATE;
  cl_kernel_arg_access_qualifier   accs_qual = CL_KERNEL_ARG_ACCESS_NONE;
  arg_type                         type_class = INVALID; // BUFFER for buffers
  std::string                      type_name;
  bool                             elem_signed = false;
  arg_type                         elem_class = INVALID; // e.g. INTEGRAL for uchar
  std::string                      elem_name; // "uchar8" for uchar8*
  size_t                           elem_size = 0; // e.g. 2 in ushort8 or half
  size_t                           vec_width = 1; // e.g. 4 in uchar4* or uchar4, 1 for half or uchar
  std::string                      name;
  bool                             has_restrict = false;

  arg(loc l) : location(l) { }

  std::string toSyntax() const;
};

struct program;
struct kernel {
  loc                       location;
  std::string               name;
  std::vector<arg>          args;
  std::vector<std::string>  argStrs; // for debug
  cl::Kernel               *cl_kernel = nullptr;
  struct program           *program = nullptr;
  kernel(loc l, struct program *p) : location(l), program(p) { }
};
struct program {
  std::string             source;
  cl::Program            *cl_program = nullptr;
  std::vector<kernel>     kernels;
  std::string             build_opts;
  std::vector<cl::Kernel> cl_kernels; // manages kernels
};

// a kernel call (an NDRange); arguments are all pre-bound
// EXAMPLE:  matrix.cl`naive<1024x1024>(0:w,1:r,1:r)
struct ndr {
  std::string source;

  program prg;
  kernel *entry = nullptr;

  cl::NDRange global_size;
  cl::NDRange local_size;

  std::vector<init*> inits;

//  ndr(loc l) : location(l) { }
//  ~ndr() {for (auto i : inits) {delete i;}}

  void initBuffers(cl::CommandQueue &cq);
  void readDevMem(cl::CommandQueue &cq);
  void str(std::ostream &os) const;
  std::string str() const;
};

// a let expression
//   EXAMPLE: A=0:w   // defines a writable buffer A initialized to zeros
struct let {
  std::string    identifier;
  init          *definition;
};
// a sequence of statements
//   EXAMPLE: A=0:w; matrix.cl`naive<1024x1024>(A,1:r,1:r)
struct expr;
struct seq {
  std::vector<expr*>  exprs;
};

struct expr {
  loc location;

  enum class kind{NDR, LET, SEQ};
  kind kind;

  union {
    ndr  *e_ndr;
    let  *e_let;
    seq  *e_seq;
    // diff
    // diff with tolerance
    // print
    //
    // .... others?
  };

  expr(loc _loc, ndr *_ndr) : location(_loc) {
    e_ndr = _ndr;
    kind = kind::NDR;
  }
};

// diagnostic
struct diag : std::exception {
  loc           location;
  std::string   message;
  std::string   input;
  diag(loc l, const std::string &m, const std::string &inp)
    : location(l), message(m), input(inp) { }

  std::string toString() const;
};

// can throw Diag
ndr *ParseCLS(
  cl::Context &ctx,
  const cl::Device &dev,
  const std::string &inp);

std::string ErrorToString(cl_int error);
} // namespace cls
#endif // CLS_HPP_