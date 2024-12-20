#ifndef PARSER_KARGS
#define PARSER_KARGS

#include "../cl_lib.hpp"
#include "../cls_opts.hpp"
#include "../fatal.hpp"
#include "../ir/types.hpp"
#include "../text.hpp"

#include <algorithm>
#include <ostream>
#include <string>
#include <vector>

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
    enum {INVALID, SOURCE, SPIRV, BINARY} kind = INVALID;
    std::string                       path;
    std::string                       build_opts;
  };

  namespace k
  {
    ///////////////////////////////////////////////////////////////////////////
    // Program, Kernels, and Argument Info Structures
    ///////////////////////////////////////////////////////////////////////////

    // global int *foo, ...
    struct arg_info {
      cl_kernel_arg_address_qualifier  addr_qual = 0;
      cl_kernel_arg_access_qualifier   accs_qual = CL_KERNEL_ARG_ACCESS_NONE;
      cl_kernel_arg_type_qualifier     type_qual = CL_KERNEL_ARG_TYPE_NONE;
      std::string                      name;
      const type                      *arg_type;

      std::string type_syntax() const;
    };

    // a kernel definition from the source
    struct kernel_info {
      // kernel  void foo(...)
      //              ^^^
      std::string            name;

      // https://www.khronos.org/registry/OpenCL/sdk/2.1/docs/man/xhtml/functionQualifiers.html
      // e.g. __attribute__((reqd_work_group_size(X, Y, Z)))
      size_t                 reqd_word_group_size[3];

      // kernel void foo(...)
      //                 ^^^
      std::vector<arg_info>  args;
    };

    // This keeps track of kernels and custom types etc...
    //
    // struct foo {...};
    // kernel void bar(...)
    // enum baz {...};
    // kernel void qux(...)
    //
    struct program_info {
      std::vector<kernel_info>     kernels;

      std::vector<const type*>     types;

      // binaries and SPIRV define a pointer size; we can sanity check
      // the binary with CL_DEVICE_ADDRESS_BITS; for program sources this
      // will just be derived from CL_DEVICE_ADDRESS_BITS (no check needed)
      size_t                       pointer_size = 0;

      // default constructible, not copyable
      program_info() { }
      program_info(const program_info &) = delete;
      program_info& operator=(program_info const&) = delete;
      ~program_info();

      const type &pointer_to(const type &t, size_t ptr_size);
    };

    /////////////////////////////////////////////////////////////////////////////
    // API

    program_info *parse_program_info(
      const cls::opts &os,
      cls::diagnostics &ds, cls::loc at,
      const cls::program_source &src,
      cl_device_id dev_id);

    program_info *parse_program_info_from_api(
      const cls::opts &os,
      cls::diagnostics &ds, cls::loc at,
      cl_program program,
      cl_device_id dev_id,
      const cl_lib &cl);

    // https://www.khronos.org/registry/OpenCL/sdk/2.1/docs/man/xhtml/functionQualifiers.html
    // __attribute__((vec_type_hint(<type>)))
    // __attribute__((work_group_size_hint(X, Y, Z)))
    // __attribute__((reqd_work_group_size(X, Y, Z)))
    // __attribute__((nosvm))
  } // namespace cls::k
} // namespace cls::

#endif