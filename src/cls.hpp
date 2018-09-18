#ifndef CLS_HPP
#define CLS_HPP

#include "image.hpp"
#include "fatal.hpp"
#include "parser/lexemes.hpp"
#include "cls_opts.hpp"
#include "cl_headers.hpp"

#include <cstdint>
#include <exception>
#include <functional>
#include <iostream>
#include <sstream>
#include <string>
#include <utility>
#include <vector>

// TODO: delete
using ErrorHandler = std::function<void(const char *)>;

namespace cls
{
  struct hostbuf {
    void *mem = nullptr;
    size_t mem_size = 0;
    void *alloc = nullptr;
    size_t alloc_size = 0;
  };

#if 0
  struct arg;
  struct ndr;
  struct kernel;

  // Respresents literal initializers for kernel arguments.
  // This is implemented as a tree to support structural literals.
  // e.g. {1,2,4,5}
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
    std::string fileval; // e.g. 'foo.bmp'
    std::vector<uint8_t> filebytes; // e.g contents of 'foo.bmp'

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

    // can this value be promoted to a vector type.
    // e.g. 22 can expand to a float 4
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
#endif

  std::string status_to_symbol(cl_int error);
} // namespace cls

#endif // CLS_HPP_