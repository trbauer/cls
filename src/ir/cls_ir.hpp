#ifndef CLS_IR_HPP
#define CLS_IR_HPP

#include "../cl_headers.hpp"
#include "../cls_opts.hpp"
#include "../fatal.hpp"
#include "../text.hpp"

#include <ostream>
#include <map>
#include <sstream>
#include <string>
#include <vector>

namespace cls
{
  struct format_opts {
    using color_span = typename text::spans::ansi_span<std::string>;

    enum {
      NONE = 0,
      USE_COLOR = (1 << 0), // syntax highlighting
      NO_SEMI_STATEMENT_LISTS = (1 << 1), // never use S1; S2; S3 short hand
    } opts = NONE;

    format_opts() { }

    // coloring primitives (automatically checks if USE_COLOR is enabled)
    color_span error(std::string s) const;
    color_span keyword(std::string s) const;
    color_span let_var(std::string s) const;
    color_span str_lit(std::string s) const;
  };

  /////////////////////////////////////////////////////////////////////////////
  // specification of some syntactic form
  struct spec {
    enum spec_kind {
      INVALID_SPEC = 0
    , STATEMENT_LIST_SPEC
    // statement forms
    , STATEMENT_SPEC // e.g. dispatch_spec or let_spec
    // partial forms
    , INIT_SPEC
    , DEVICE_SPEC
    , PROGRAM_SPEC
    , KERNEL_SPEC
    } kind;
    loc defined_at;
    spec(loc _loc, spec_kind _kind) : defined_at(_loc), kind(_kind) { }
    void            str(std::ostream &os, format_opts fopts) const;
    std::string     str() const;

    // a friendly name for debugging
    std::string     name() const;
  };

  // buffer/image/scalar initializer
  struct init_spec : spec {
    enum init_spec_kind {
      IS_INVALID = 0,
      ////////////////////////////////////
      // atomic initializers
      //
      IS_INT,  // "0", "0:w", "0x123:w", "-34:r"
      IS_FLT,  // "1.2", "1.2:rw"
      IS_REC,  // {c1, c2, ...} children hold c1, c2, ... (includes vectors)
      IS_SYM,  // a symbol (like "^g.x" or a reference to a let binding)
      IS_BEX, IS_UEX, // an expression (binary or unary)
      // initialize via a file
      IS_FILE, // "image(foo.bmp):w"
      // TODO: there should be a second argument to specify the image type
      // If specified as an input, this loads as a buffer or image of
      // RGB24 or an intensity image if the BMP is monochrome.
      // generators
      IS_RND,   // e.g. random, random<13>(0.0,1.0)
      IS_SEQ,  // "seq:r" or "seq(start,delta):r"
      // IS_CYC,  // "cyc(x1,x2,...)"
      ////////////////////////////////////
      // buffer initializers
      //
      IS_MEM,
    } kind = IS_INVALID;
    init_spec(loc loc, init_spec::init_spec_kind k)
      : spec(loc, INIT_SPEC), kind(k) { }
    void str(std::ostream &os, format_opts fopts) const;
  };

  /////////////////////////////////////////////////////////////////////////////
  // atomic initializers (syntactically atomic) like simple integers etc...
  struct init_spec_atom : init_spec {
    init_spec_atom(loc loc, init_spec_kind k) : init_spec(loc, k) { }
  };
  // a variable reference (e.g. a buffer)
  // or a built-in like ^g.x
  struct init_spec_symbol : init_spec_atom {
    std::string identifier;
    init_spec_symbol(loc loc, std::string _identifier) :
      init_spec_atom(loc, IS_SYM), identifier(_identifier) { }
    void str(std::ostream &os, format_opts fopts) const {fopts.let_var(identifier);}
  };
  // e.g. "0x114", "-124", or "3.141"
  struct init_spec_int : init_spec_atom {
    int64_t value;
    init_spec_int(loc loc, int64_t _value = 0)
      : init_spec_atom(loc, IS_INT), value(_value) { }
    void str(std::ostream &os, format_opts fopts) const {os << value;}
  };
  struct init_spec_float : init_spec_atom {
    double value = 0.0;
    init_spec_float(loc loc, double _value)
      : init_spec_atom(loc, IS_FLT), value(_value) { }
    void str(std::ostream &os, format_opts fopts) const {os << value;}
  };
  // e.g. {1,2,4,8}
  struct init_spec_record : init_spec_atom {
    std::vector<init_spec_atom *> children;
    init_spec_record(loc loc) : init_spec_atom(loc, IS_REC) { }
    void str(std::ostream &os, format_opts fopts) const {
      os << "{";
      bool first = true;
      for (const auto *c : children) {
        if (first)
          first = false;
        else
          os << ", ";
        c->str(os,fopts);
      }
      os << "}";
    }
  };
  struct init_spec_bin_expr : init_spec_atom {
    init_spec_atom *el = nullptr, *er = nullptr;
    enum bin_op {
      EB_INVALID = 0
    , E_POW
    , E_OR, E_XOR, E_AND
    , E_LSH, E_RSH
    , E_ADD, E_SUB
    , E_MUL, E_DIV, E_MOD
    } e_kind;
    init_spec_bin_expr(loc loc) : init_spec_atom(loc, IS_BEX) { }
    init_spec_bin_expr(loc loc, bin_op k, init_spec_atom *l, init_spec_atom *r)
      : init_spec_atom(loc, IS_BEX), el(l), er(r), e_kind(k) { }
    void str(std::ostream &os, format_opts fopts) const;
  };
  struct init_spec_unr_expr : init_spec_atom {
    init_spec_atom *e = nullptr;
    enum unr_op {
      EU_INVALID = 0
      // operators
    , E_NEG, E_COMPL
      // functions
    , E_ABS, E_SQT, E_SIN, E_COS, E_TAN
    } e_kind;
    init_spec_unr_expr(loc loc) : init_spec_atom(loc, IS_UEX) { }
    init_spec_unr_expr(loc loc, unr_op k, init_spec_atom *_e)
      : init_spec_atom(loc, IS_UEX), e(_e), e_kind(k) { }
    void str(std::ostream &os, format_opts fopts) const;
  };

  // 'foo.bmp'
  struct init_spec_file : init_spec_atom {
    std::string path;
    init_spec_file(loc loc, std::string _path)
      : init_spec_atom(loc, IS_FILE), path(_path) { }
    void str(std::ostream &os, format_opts fopts) const {
      os << fopts.str_lit("'" + path + "'");
    }
  };
  // struct init_spec_atom_function : init_spec_atom {
  //   std::vector<init_spec_atom> arguments;
  //   init_spec_atom_function(loc loc, enum init_spec_kind k)
  //     : init_spec_atom(loc, k) { }
  //  };

  // random               auto seed and full range for type
  // random<13>           seed 13 and full range for the type
  // random[0.0,1.0]      auto seed and from 0.0 to 1.0 (inclusive)
  // random<13>[0.0,1.0]  combination
  struct init_spec_rng_generator : init_spec_atom {
    int64_t seed = 0;
    init_spec_atom *e_lo = nullptr, *e_hi = nullptr;

    init_spec_rng_generator(loc loc, int64_t _seed = 0)
      : init_spec_atom(loc, IS_RND), seed(_seed) { }
    void str(std::ostream &os, format_opts fopts) const;
  };
  struct init_spec_seq_generator : init_spec_atom {
    init_spec_seq_generator(loc loc) : init_spec_atom(loc, IS_SEQ) { }
    void str(std::ostream &os, format_opts fopts) const {
      os << "seq(TODO)";
    }
  };

  /////////////////////////////////////////////////////////////////////////////
  // memory initializers (buffers and images)
  //
  // e.g. "0:w", "file.bmp:rw", or "binary.buf:r"
  struct init_spec_memory : init_spec {
    init_spec_atom    *root = nullptr; // 0x44

    init_spec_atom *dimension = nullptr; // optional dimension expression: e.g. [g.x]

    enum access {
      INIT_SPEC_MEM_NONE = 0,
      INIT_SPEC_MEM_READ  = (1 << 0), // :r
      INIT_SPEC_MEM_WRITE = (1 << 1), // :w
    } access_properties = INIT_SPEC_MEM_NONE;

    enum transfer {
      TX_INVALID = 0,
      TX_DEFAULT, // probably mapped or copied, but up to the implementation
      TX_MAP,  // :m -> mirror memory/ALLOC_HOST_POINTER clEnqueueMapBuffer
      TX_COPY, // :c -> mirror memory and clEnqueue{Write,Read}Buffer
      TX_SVM_COARSE, // :sc -> clSVMAlloc (or :sf for fine grained)
      TX_SVM_FINE,  // :sf -> clSVMAlloc
    } transfer_properties = TX_DEFAULT;

    bool use_svm_fine_grained = false; // <lit>:vf use CL_MEM_SVM_FINE_GRAIN_BUFFER
    bool use_svm_atomics = false;      // <lit>:va or <lit>:vfa use CL_MEM_SVM_ATOMICS

    init_spec_memory(loc loc) : init_spec(loc,IS_MEM) { }

    void str(std::ostream &os, format_opts fopts) const;
  };

  struct statement_spec : spec {
    enum statement_type {
      INVALID_STATEMENT = 0,
      DISPATCH,  // #1`foo.cl`kernel<...>(...)
      LET,       // let B = 0:w
      BARRIER,   // barrier
      DIFF,      // diff
      SAVE,      // save('foo.bin',buffer)
      PRINT,     // print(buffer)
    } kind = INVALID_STATEMENT;

    statement_spec(loc loc, enum statement_type k)
      : spec(loc,spec::STATEMENT_SPEC), kind(k) { }
    void str(std::ostream &os, format_opts fopts) const;
  };

  // S1; S2; S3
  struct statement_list_spec : spec {
    int                                 indent = 0;
    std::vector<statement_spec *>       statements;
    statement_list_spec(loc loc) : spec(loc,spec::STATEMENT_LIST_SPEC) { }
    void str(std::ostream &os, format_opts fopts) const;
  };

  // #1 or #GTX
  struct device_spec : spec {
    enum kind {
      INVALID_DEVICE,
      BY_DEFAULT,
      BY_INDEX,
      BY_NAME
    } kind = kind::BY_DEFAULT;
    int           by_index_value = -1;
    std::string   by_name_value;

    device_spec(loc loc) : spec(loc,spec::DEVICE_SPEC) { }

    void setSource(std::string name);
    void setSource(int index);

    void str(std::ostream &os, format_opts fopts) const;
  };

  // fills in for anything that can be referenced or defined immediately
  template <typename T>
  struct refable {
    loc              defined_at;
    std::string      identifier;
    T                value = nullptr;

    refable() : defined_at(0,0,0,0) { }
    refable(loc l, std::string ident, T _value = nullptr) // symbolic ref
      : defined_at(l), identifier(ident), value(_value) { }
    refable(T _value) // immediate ref
      : defined_at(0,0,0,0), value(_value) { }
    bool is_resolved() const {return value != nullptr;}
          T &operator->()       {return *value;}
    const T &operator->() const {return *value;}
             operator T()       {return value;}
             operator T() const {return value;}
    void str(std::ostream &os, format_opts fopts) const {
      if (identifier.empty())
        if (value != nullptr)
          value->str(os,fopts);
        else
          os << fopts.error("<nullptr>");
      else
        os << fopts.let_var(identifier);
    }
  };
  struct program_spec : spec {
    device_spec     device;
    std::string     path;
    std::string     build_options;

    program_spec(loc loc) : spec(loc,spec::PROGRAM_SPEC), device(loc) { }
    void str(std::ostream &os, format_opts fopts) const;
  };
  struct kernel_spec : spec {
    refable<program_spec*>    program;
    std::string               name;

    kernel_spec(program_spec *ps)
      : spec(ps->defined_at,spec::KERNEL_SPEC), program(ps) { }
    void str(std::ostream &os, format_opts fopts) const;
  };
  struct dim {
    loc                       defined_at;
    std::vector<size_t>       dims; // empty means use nullptr or reqd wg sz

    size_t total_size() const {
      size_t p = 1; for (size_t d : dims) {p *= d;}; return p;
    }
    void str(std::ostream &os, format_opts fopts) const;
  };
  struct dispatch_spec : statement_spec {
    refable<kernel_spec*>                               kernel;
    dim                                                 global_size;
    dim                                                 local_size;
    std::vector<refable<init_spec*>>                    arguments;
    std::vector<std::tuple<std::string,init_spec*>>     where_bindings;

    dispatch_spec(loc loc) : statement_spec(loc, statement_spec::DISPATCH) { }
    void str(std::ostream &os,format_opts fopts) const;
  };

  // let X = ...
  struct let_spec : statement_spec {
    using param_refs = std::vector<refable<init_spec*>*>;
    using param_map = std::map<std::string,param_refs>;

    std::string                                         identifier;
    // can be an:
    //  - partial object: init_spec, kernel_spec, program_spec, ...
    //  - statement form: dispatch_spec, statement_list_spec, barrier_spec, ....
    spec                                               *value;
    std::vector<std::string>                            params;

    param_map                                           param_uses;

    let_spec(loc loc,std::string ident, spec *_value = nullptr)
      : statement_spec(loc, statement_spec::LET)
      , identifier(ident)
      , value(value) { }
    void str(std::ostream &os, format_opts fopts) const;
  };
  // barrier
  struct barrier_spec : statement_spec {
    barrier_spec(loc loc) : statement_spec(loc, statement_spec::BARRIER) { }
    void str(std::ostream &os,format_opts fopts) const {os << "barrier";}
  };
  // diff(REF,SUT)
  struct diff_spec : statement_spec {
    init_spec *ref;
    init_spec_symbol *sut;
    diff_spec(loc loc, init_spec *_ref, init_spec_symbol *_sut)
      : statement_spec(loc, statement_spec::DIFF), ref(_ref), sut(_sut) { }
    void str(std::ostream &os,format_opts fopts) const {
      os << "diff(";
      ref->str(os,fopts); os << ","; sut->str(os,fopts);
      os << ")";
    }
  };
  // print(X)
  struct print_spec : statement_spec {
    init_spec_symbol *arg;
    print_spec(loc loc, init_spec_symbol *a)
      : statement_spec(loc, statement_spec::PRINT), arg(a) { }
    void str(std::ostream &os,format_opts fopts) const {
      os << "print("; arg->str(os,fopts); os << ")";
    }
  };
  // save('foo.bmp',X)
  struct save_spec : statement_spec {
    // init_spec_symbol *file; // foo.bpm
    std::string file;      // 'foo.bmp'
    init_spec_symbol *arg; // X
    save_spec(loc loc, std::string _file, init_spec_symbol *_arg)
      : statement_spec(loc, statement_spec::SAVE), file(_file), arg(_arg) { }
    void str(std::ostream &os,format_opts fopts) const {
      os << "save(" <<
        fopts.str_lit("'" + file + "'") << ", ";
        arg->str(os,fopts); os << ")";
    }
  };


  struct script {
    const std::string                 &source;

    std::map<std::string,let_spec*>    let_bindings;
    statement_list_spec                statement_list;

    script(const std::string &_source)
      : source(_source), statement_list(loc(1,1,0,0)) { }

    // script() { }
    // script(const script &) = delete;
    // script& operator=(const script &) = delete;

    void str(std::ostream &os,format_opts fopts) const {
      statement_list.str(os,fopts);
    }
  };

} // namespace

#endif