#ifndef PARSER_KARGS
#define PARSER_KARGS

#include "../cl_headers.hpp"
#include "../fatal.hpp"
#include "../opts.hpp"

#include <string>
#include <ostream>
#include <vector>
#include <variant>

namespace cls
{
  struct program_source {
//    using binary = std::vector<unsigned char>;

//    bool is_source;

//    program_source() : is_source(false) { }
//    program_source(std::string s, std::string opts)
//      : is_source(true), source(s), build_opts(opts) { }
//    program_source(binary b, std::string opts)
//      : is_source(false), source(b), build_opts(opts) { }
//    program_source() { }
  //  program_source(std::string s, std::string opts)
  //    : source(s), build_opts(opts) { }
  //  program_source(binary b, std::string opts)
  //    : source(b), build_opts(opts) { }
//    std::variant<std::string,binary>  source;
    bool                              is_binary = false;
    std::string                       path;
    std::string                       build_opts;
  };

  namespace k
  {
    ///////////////////////////////////////////////////////////////////////////
    // float, int, or unsigned
    struct type_num {
      enum {
        UNSIGNED = 0,
        SIGNED,
        FLOATING
      } flavor;
    };
    ///////////////////////////////////////////////////////////////////////////
    // enum foo{BAR,BAZ,QUX}
    struct type_enum {
      std::vector<std::tuple<std::string,int>> symbols;
    };
    ///////////////////////////////////////////////////////////////////////////
    // struct foo {int x,float y,struct{int z,short w}bar};
    struct type;
    struct type_struct {
      int                   aligned = 0; // __attribute__ ((aligned (8))); (technically this can work on other types too, but we don't support it)
      bool                  is_packed = false; // __attribute__ ((packed))
      std::vector<type*>    elements;
    };
    ///////////////////////////////////////////////////////////////////////////
    // e.g. image2d_t
    struct type_builtin {
      // https://www.khronos.org/registry/OpenCL/sdk/2.0/docs/man/xhtml/otherDataTypes.html
      enum {
        IMAGE2D,        // image2d_t
        IMAGE3D,        // image3d_t
        IMAGE2D_ARRAY,  // image2d_array_t
        IMAGE2D_DEPTH,  // image2d_depth_t
        IMAGE2D_ARRAY_DEPTH, // image2d_array_depth_t
        IMAGE1D,        // image1d_t
        IMAGE1D_BUFFER, // image1d_buffer_t
        IMAGE1D_ARRAY,  // image1d_array_t
        SAMPLER,        // sampler_t
        QUEUE,          // queue_t
        NDRANGE,        // ndrange_t
        CLK_EVENT,      // clk_event_t
        RESERVE_ID,     // reserve_id_t
        EVENT,          // event_t
        CL_MEM_FENCE_FLAGS, // cl_mem_fence_flags
        IMAGE2D_MSAA,    // image2d_msaa_t
        IMAGE2D_ARRAY_MSAA, // image2d_array_msaa_t
        IMAGE2D_DEPTH_MSAA, // image2d_msaa_depth_t
        IMAGE2D_ARRAY_MSAA_DEPTH, // image2d_array_msaa_depth_t
      } kind;
    };
    struct type_ptr {
      enum {
        EMPTY_ATTRS = 0,
        CONST = (1 << 0),
        VOLATILE = (1 << 1),
        RESTRICT = (1 << 2)
      } attrs = EMPTY_ATTRS;
      type *element_type;
    };

    struct type {
      std::variant<type_num,type_struct,type_builtin,type_ptr>   var;
      std::string                                                name;
    };

    // global int *foo, ...
    struct arg {
      cl_kernel_arg_address_qualifier  addr_qual = CL_KERNEL_ARG_ADDRESS_PRIVATE;
      cl_kernel_arg_access_qualifier   accs_qual = CL_KERNEL_ARG_ACCESS_NONE;
      std::string                      name;
      const type                      *type;
    };

    // a kernel definition from the source
    struct kernel {
      // kernel  void foo(...)
      //              ^^^
      std::string         name;

      // https://www.khronos.org/registry/OpenCL/sdk/2.1/docs/man/xhtml/functionQualifiers.html
      // e.g. __attribute__((reqd_work_group_size(X, Y, Z)))
      // std::vector<std::string> attrs; // reqd_work_group_size(X,Y,Z)  [remove spaces]
      size_t              req_word_group_size[3];

      // kernel void foo(...)
      //                 ^^^
      std::vector<arg>    args;
    };

    // struct foo {...};
    // kernel void bar(...)
    // enum baz {...};
    // kernel void qux(...)
    struct prog {
      std::vector<kernel> kernel;
      std::vector<type>   types;
    };


    /////////////////////////////////////////////////////////////////////////////
    // API

    cls::k::prog parseProgramInfo(
      const cls::fatal_handler *fh, cls::loc at,
      const cls::program_source &src);

    // https://www.khronos.org/registry/OpenCL/sdk/2.1/docs/man/xhtml/functionQualifiers.html
    // __attribute__((vec_type_hint(<type>)))
    // __attribute__((work_group_size_hint(X, Y, Z)))
    // __attribute__((reqd_work_group_size(X, Y, Z)))
    // __attribute__((nosvm))

  } // namespace cls::k
} // namespace cls::



#endif