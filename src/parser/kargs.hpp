#ifndef PARSER_KARGS
#define PARSER_KARGS

#include "../cl_headers.hpp"
#include "../cls_opts.hpp"
#include "../fatal.hpp"

#include <array>
#include <initializer_list>
#include <ostream>
#include <string>
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
      enum kind {
        UNSIGNED = 0,
        SIGNED,
        FLOATING
      } kind;
      int size_in_bytes;
      const char *name;

      constexpr type_num(
        const char *_name, enum kind _kind, int _size_in_bytes)
        : name(_name), kind(_kind), size_in_bytes(_size_in_bytes) { }
    };
    ///////////////////////////////////////////////////////////////////////////
    // enum foo{BAR,BAZ,QUX}
    struct type_enum {
      std::vector<std::tuple<std::string,int>> symbols;
    };
    ///////////////////////////////////////////////////////////////////////////
    // other built-in types
    // e.g. image2d_t
    struct type_builtin {
      // https://www.khronos.org/registry/OpenCL/sdk/2.0/docs/man/xhtml/otherDataTypes.html
      enum kind {
        IMAGE1D,        // image1d_t
        IMAGE1D_BUFFER, // image1d_buffer_t
        IMAGE1D_ARRAY,  // image1d_array_t
        IMAGE2D,        // image2d_t
        IMAGE2D_ARRAY,  // image2d_array_t
        IMAGE2D_DEPTH,  // image2d_depth_t
        IMAGE2D_ARRAY_DEPTH, // image2d_array_depth_t
        IMAGE2D_MSAA,    // image2d_msaa_t
        IMAGE2D_ARRAY_MSAA, // image2d_array_msaa_t
        IMAGE2D_DEPTH_MSAA, // image2d_msaa_depth_t
        IMAGE2D_ARRAY_MSAA_DEPTH, // image2d_array_msaa_depth_t
        IMAGE3D,        // image3d_t
        SAMPLER,        // sampler_t
        QUEUE,          // queue_t
        NDRANGE,        // ndrange_t
        CLK_EVENT,      // clk_event_t
        RESERVE_ID,     // reserve_id_t
        EVENT,          // event_t
        CL_MEM_FENCE_FLAGS, // cl_mem_fence_flags
      } kind;
      constexpr type_builtin(enum kind _kind) : kind(_kind) { }
    };
    ///////////////////////////////////////////////////////////////////////////
    // struct foo {int x,float y,struct{int z,short w}bar};
    struct type;
    struct type_struct {
      const char                 *name;
      bool                        packed = false; // __attribute__ ((packed))
      size_t                      alignment = 0; // e.g. __attribute__ ((aligned (8)));

      const type                 *elements_memory[16];
      const type                **elements = elements_memory;
      size_t                      elements_length = 0;
      constexpr type_struct(
        const char *_name,
        bool _packed,
        size_t _alignment,
        std::initializer_list<const type *> ts)
        : name(_name), packed(_packed), alignment(_alignment), elements_memory{0}
      {
        if (ts.size() > sizeof(elements_memory)/sizeof(elements_memory[0]))
          throw "too many elements for constexpr";
        elements_length = 0;
        for (const type *t : ts)
          elements[elements_length++] = t;
      }
      // for built-in CL vector types (e.g. float4)
      constexpr type_struct(
        const char *_name,
        size_t elem_size, // element size
        const type *type, // underlying type
        int len) // vector length
        : name(_name)
        , packed(true)
        , alignment(elem_size*(size_t)len)
        , elements_memory{0}
        , elements_length(len)
      {
        for (int i = 0; i < elements_length; i++)
          elements[i] = type;
      }

      type_struct() : elements_memory{0} { }
    };
    struct type_union {
      const type                 *elements_memory[2];

      int                         aligned = 0; // __attribute__ ((aligned (8))); (technically this can work on other types too, but we don't support it)
      bool                        is_packed = false; // __attribute__ ((packed))
      const type                **elements = elements_memory;
      size_t                      elements_length = 0;

      constexpr type_union(std::initializer_list<const type *> ts)
        : elements_memory{0}
      {
        if (ts.size() > sizeof(elements_memory)/sizeof(elements_memory[0]))
          throw "too many elements for constexpr";
        elements_length = 0;
        for (const type *t : ts)
          elements[elements_length++] = t;
      }
      type_union() : elements_memory{0} { }
    };

    struct type_ptr {
      enum {
        EMPTY_ATTRS = 0,
        CONST       = (1 << 0),
        RESTRICT    = (1 << 1),
        VOLATILE    = (1 << 2),
      } attrs = EMPTY_ATTRS;
      const type *element_type;
      constexpr type_ptr(const type *t) : element_type(t) { }
    };

    struct type {
//      std::variant<type_num,type_builtin,type_struct,type_enum,type_ptr> var;
      std::variant<type_num,type_builtin,type_struct>   var;
//      const char *                                               name;
      // std::string                                                name;

      constexpr type(const type_num &t) : var(t) { }
      constexpr type(const type_struct &t) : var(t) { }

      // template <typename T>
      // type(std::string _name, T t) : name(_name) {var = t;}
//      constexpr type(const char *_name, type_num t) : name(_name), var(t) { }
//      constexpr type(std::string _name, type_num t) : name(_name), var(t) { }
    };

    // global int *foo, ...
    struct arg_info {
      bool                             is_const = false;
      bool                             is_volatile = false;
      bool                             is_restrict = false;
      cl_kernel_arg_address_qualifier  addr_qual = CL_KERNEL_ARG_ADDRESS_PRIVATE;
      cl_kernel_arg_access_qualifier   accs_qual = CL_KERNEL_ARG_ACCESS_NONE;
      std::string                      name;
      const type                      *type = nullptr;
    };

    // a kernel definition from the source
    struct kernel_info {
      // kernel  void foo(...)
      //              ^^^
      std::string            name;

      // https://www.khronos.org/registry/OpenCL/sdk/2.1/docs/man/xhtml/functionQualifiers.html
      // e.g. __attribute__((reqd_work_group_size(X, Y, Z)))
      // std::vector<std::string> attrs; // reqd_work_group_size(X,Y,Z)  [remove spaces]
      size_t                 req_word_group_size[3];

      // kernel void foo(...)
      //                 ^^^
      std::vector<arg_info>  args;
    };

    // struct foo {...};
    // kernel void bar(...)
    // enum baz {...};
    // kernel void qux(...)
    struct program_info {
      std::vector<kernel_info>    kernels;
      std::vector<type>           types;
    };

    /////////////////////////////////////////////////////////////////////////////
    // API

    cls::k::program_info parseProgramInfo(
      const cls::Opts &os,
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