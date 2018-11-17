#ifndef CLS_IR_HPP
#define CLS_IR_HPP

#include "../cl_headers.hpp"
#include "../cls_opts.hpp"
#include "../half.hpp"
#include "../ir/types.hpp"
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

  struct val {
    bool is_f;
    bool is_u;
    union {
      int64_t  s64;
      uint64_t u64;
      double   f64;
    };
    val() : val((int64_t)0) {}
    val(int8_t _s) : val((int64_t)_s) {}
    val(int16_t _s) : val((int64_t)_s) {}
    val(int32_t _s) : val((int64_t)_s) {}
    val(int64_t _s64) : s64(_s64), is_f(false), is_u(false) {}
    val(uint8_t _u) : val((uint64_t)_u) {}
    val(uint16_t _u) : val((uint64_t)_u) {}
    val(uint32_t _u) : val((uint64_t)_u) {}
    val(uint64_t _u64) : u64(_u64), is_f(false), is_u(true) {}
    val(double _f64) : f64(_f64), is_f(true), is_u(false) {}
    val(float _f32) : val((double)_f32) {}

    val &operator=(uint64_t _val) {*this = val(_val); return *this;}
    val &operator=(uint32_t _val) {*this = (uint64_t)_val; return *this;}
    val &operator=(uint16_t _val) {*this = (uint64_t)_val; return *this;}
    val &operator=(uint8_t  _val) {*this = (uint64_t)_val; return *this;}
    val &operator=(int64_t  _val) {*this = val(_val); return *this;}
    val &operator=(int32_t  _val) {*this = (int64_t)_val; return *this;}
    val &operator=(int16_t  _val) {*this = (int64_t)_val; return *this;}
    val &operator=(int8_t   _val) {*this = (int64_t)_val; return *this;}
    val &operator=(half     _val) {*this = (double)_val; return *this;}
    val &operator=(float    _val) {*this = (double)_val; return *this;}
    val &operator=(double   _val) {*this = val(_val); return *this;}

    bool is_float() const {return is_f;}
    bool is_int() const {return !is_f;}
    bool is_signed() const {return !is_f && !is_u;}
    bool is_unsigned() const {return is_u;}

    template <typename T>
    T as() const
    {
      if (std::is_floating_point<T>()) {
        // * -> {half,float,double}
        if (is_float())
          return (T)f64; // double -> ...
        else if (is_signed())
          return (T)s64; // int64_t -> ...
        else // if (is_unsigned())
          return (T)u64; // uint64_t -> ...
      } else {
        // * -> uint{8,16,32,64}_t
        if (is_float())
          return (T)f64; // double-> ...
        return std::is_signed<T>() ?
          (T)s64 : (T)u64; // {u,}int64-> ...
      }
    }
  }; // val

  /////////////////////////////////////////////////////////////////////////////
  // fills in for anything that can be referenced or defined immediately
  // the type T must be a (spec type) with a spec::defined_at loc
  template <typename T>
  struct refable {
    loc              defined_at;
    std::string      identifier;
    T               *value = nullptr;

    refable() : defined_at(0,0,0,0) { }
    refable(loc at, std::string _identifier, T *_value = nullptr) // symbolic ref
      : defined_at(at), identifier(_identifier), value(_value) { }
    refable(T *_value) // immediate ref
      : defined_at(_value->defined_at), value(_value) { }
    bool is_resolved() const {return value != nullptr;}
    const T &operator->() const {return *value;}
          T &operator->()       {return *value;}
             operator const T*() const {return value;}
             operator T*()       {return value;}
    //         operator const T&() const {return *value;}
    //         operator T&()       {return *value;}
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
    } skind;

    loc defined_at;

    spec(loc at, spec_kind _kind) : defined_at(at), skind(_kind) { }

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
      IS_SZO,  // sizeof(...)
      IS_REC,  // {c1, c2, ...} children hold c1, c2, ... (includes vectors)
      IS_BIV,  // built-in variable
      IS_SYM,  // a symbol (a reference to a let binding)
      IS_BEX, IS_UEX, // an expression (binary or unary)
      ////////////////////////////////////
      // Non-atoms
      IS_FIL, // e.g. "file<bin>('foo.dat'):w"
      IS_IMG, // e.g. "image<rgb>('foo.ppm'):r"
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
    } skind = IS_INVALID;

    init_spec(loc at, init_spec::init_spec_kind k)
      : spec(at, INIT_SPEC), skind(k) { }

    void str(std::ostream &os, format_opts fopts) const;

    bool is_atom() const {
      switch (skind) {
      case IS_INT:
      case IS_FLT:
      case IS_SZO:
      case IS_REC:
      case IS_BIV:
      case IS_BEX:
      case IS_UEX:
        return true;
      default: return false;
      }
    }
  };

  /////////////////////////////////////////////////////////////////////////////
  // atomic initializers (syntactically atomic) like simple integers etc...
  struct init_spec_atom : init_spec {
    init_spec_atom(loc loc, init_spec_kind k) : init_spec(loc, k) { }
  };

  /////////////////////////////////////////////////////////////////////////////
  // memory initializers (buffers and images)
  //
  // e.g. "0:w", "file.bmp:rw", or "binary.buf:r"
  struct init_spec_mem : init_spec {
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

    // either :p# or :P#
    bool print_pre = false, print_post = false;
    // e.g. :p16 means 16 elements per row
    int print_pre_elems_per_row = 0, print_post_elems_per_row = 0;
    // :S save after (edits extension)
    bool save_post = false;

    init_spec_mem(loc loc) : init_spec(loc,IS_MEM) { }

    void str(std::ostream &os, format_opts fopts) const;
  };

  // e.g. "0x114", "-124"
  struct init_spec_int : init_spec_atom {
    int64_t value;
    init_spec_int(loc at, int64_t _value = 0)
      : init_spec_atom(at, IS_INT), value(_value) { }
    void str(std::ostream &os, format_opts fopts) const {os << value;}
  };

  // e.g. "3.141"
  struct init_spec_float : init_spec_atom {
    double value = 0.0;
    init_spec_float(loc at, double _value)
      : init_spec_atom(at, IS_FLT), value(_value) { }
    void str(std::ostream &os, format_opts fopts) const {os << value;}
  };

  // e.g. {1,2,4,8}
  struct init_spec_record : init_spec_atom {
    std::vector<init_spec_atom *> children;
    init_spec_record(loc at) : init_spec_atom(at, IS_REC) { }
    void str(std::ostream &os, format_opts fopts) const;
  };

  // a variable reference (e.g. to a let)
  struct init_spec_symbol : init_spec_atom {
    std::string identifier;
    init_spec_symbol(loc at, std::string _identifier) :
      init_spec_atom(at, IS_SYM), identifier(_identifier) { }
    void str(std::ostream &os, format_opts fopts) const {fopts.let_var(identifier);}
  };

  // sizeof(int2) or sizeof(BUF)
  struct init_spec_sizeof : init_spec_atom {
    std::string               type_name;
    refable<init_spec_mem>    mem_object;
    init_spec_sizeof(loc at, std::string _type_name)
      : init_spec_atom(at, IS_SZO), type_name(_type_name) { }
    init_spec_sizeof(loc at, const refable<init_spec_mem> &_mem_object)
      : init_spec_atom(at, IS_SZO), mem_object(_mem_object) { }
    void str(std::ostream &os, format_opts fopts) const;
  };

  // a built-in variable (e.g. disapatch parameter)
  struct init_spec_builtin : init_spec_atom {
    enum biv_kind {
      BIV_INVALID = 0, BIV_FIRST = 1,
      BIV_GX = BIV_FIRST,
      BIV_GY,
      BIV_GZ,
      BIV_LX,
      BIV_LY,
      BIV_LZ,
      BIV_LAST = BIV_LZ
    } skind = BIV_INVALID;
    init_spec_builtin(loc loc, biv_kind _kind) :
      init_spec_atom(loc, IS_BIV), skind(_kind) { }
    void str(std::ostream &os, format_opts fopts) const;
    static const char *syntax_for(biv_kind skind);
  };

  struct init_spec_bex : init_spec_atom {
    struct op_spec {
      const char              *symbol;
      int                      precedence; // precedence == 0 implies a function symbol
      enum {N,R,L}             assoc = N;
      // std::function<val(fatal_handler *,const val&,const val&)> apply;
      val (*apply)(fatal_handler *,const loc&,const val&,const val&);
    };
    const op_spec &e_op;
    const init_spec_atom *e_l, *e_r;

    init_spec_bex(const op_spec &_os, init_spec_atom *l, init_spec_atom *r)
      : init_spec_atom(l->defined_at, IS_BEX), e_op(_os), e_l(l), e_r(r)
    {
      defined_at.extend_past(r->defined_at);
    }
    void str(std::ostream &os, format_opts fopts) const;

    static const op_spec *lookup_op(const char *symbol);
  };

  struct init_spec_uex : init_spec_atom {
    struct op_spec {
      const char     *symbol;
      int             precedence;
      val (*apply)(fatal_handler *,const loc&,const val&);
    };
    const op_spec &e_op;
    init_spec_atom *e;
    init_spec_uex(loc loc, const op_spec &_op, init_spec_atom *_e)
      : init_spec_atom(loc, IS_UEX), e(_e), e_op(_op)
    {
      defined_at.extend_past(_e->defined_at);
    }
    void str(std::ostream &os, format_opts fopts) const;

    static const op_spec *lookup_op(const char *symbol);
  };

  // file<bin>('foo.bin')
  struct init_spec_file : init_spec_atom {
    std::string path;
    enum file_flavor {INVALID=0,BIN,TXT,TXT_COL} flavor;
    int col = 0; // only for TXT_COL
    std::string sep; // only for TXT_COL

    init_spec_file(
      loc loc,
      std::string _path,
      file_flavor flv,
      int _col,
      std::string _sep)
      : init_spec_atom(loc, IS_FIL)
      , path(_path)
      , flavor(flv)
      , col(_col)
      , sep(_sep) { }

    void str(std::ostream &os, format_opts fopts) const;
  };

  // image<CHORD,CHDAT,DIMS>(...);
  //
  // DIM = WIDTH (ROW_PITCH)? x HEIGHT (SLICE_PITCH?) x DEPTH
  //
  // e.g. image<rgba,f32,1024x768>('file.ppm')
  // e.g. image<rgba,f32, 1024(1200*4*4) x 768>('file.bmp')
  //                          ^^^^^^^^^^ pitches are in bytes
  // e.g. image<rgba,f32, 1024(4*4*1200) x 768>('file.bmp')
  //                          ^^^^^^^^^^ pitches are in bytes
  // e.g. image<rgb,f16, 1024x768(3*2*1024*800)>('file.bmp')
  //                             ^^^^^^^^^^^^^^ pitches are in bytes
  // e.g. image<rgb,u8>('file.ppm') // image dimensions are inferred from file
  //
  // e.g. image<rgb,u8,1024x760>    // empty image
  struct init_spec_image : init_spec_atom {
    std::string         path;
    enum channel_order {INVALID_CO=0,R,RG,RGB,RGBA} ch_order;
    enum channel_type {INVALID_CT=0,U8,F32,F16} ch_data_type;
    init_spec_atom     *width;
    init_spec_atom     *row_pitch;
    init_spec_atom     *height;
    init_spec_atom     *slice_pitch;
    init_spec_atom     *depth;
    init_spec_image(
      loc loc,
      std::string _path,
      channel_order ord,
      channel_type ty,
      init_spec_atom *_width,
      init_spec_atom *_row_pitch,
      init_spec_atom *_height,
      init_spec_atom *_slice_pitch,
      init_spec_atom *_depth)
      : init_spec_atom(loc, IS_IMG)
      , path(_path)
      , ch_order(ord)
      , ch_data_type(ty)
      , width(_width)
      , row_pitch(_row_pitch)
      , height(_height)
      , slice_pitch(_slice_pitch)
      , depth(_depth)
    { }
    void str(std::ostream &os, format_opts fopts) const;
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
  struct init_spec_rng : init_spec_atom {
    int64_t seed = 0;
    bool has_seed = false;
    init_spec_atom *e_lo = nullptr, *e_hi = nullptr;

    init_spec_rng(loc loc) : init_spec_atom(loc, IS_RND) { }
    void set_seed(int64_t _seed) {seed = _seed; has_seed = true;}
    void str(std::ostream &os, format_opts fopts) const;
  };
  // seq       => 0,1,...
  // seq(4)    => 4,5,6...
  // seq(4,2)  => 4,6,8,...
  struct init_spec_seq : init_spec_atom {
    const init_spec_atom *base;
    const init_spec_atom *delta;
    init_spec_seq(
      loc loc, const init_spec_atom *_base, const init_spec_atom *_delta)
      : init_spec_atom(loc, IS_SEQ), base(_base), delta(_delta) { }
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
    } skind = INVALID_STATEMENT;

    statement_spec(loc loc, enum statement_type k)
      : spec(loc,spec::STATEMENT_SPEC), skind(k) { }
    void str(std::ostream &os, format_opts fopts) const;
  };

  // S1; S2; S3
  struct statement_list_spec : spec {
    int                                 indent = 0;
    std::vector<statement_spec *>       statements;
    statement_list_spec(loc loc) : spec(loc,spec::STATEMENT_LIST_SPEC) { }
    void str(std::ostream &os, format_opts fopts) const;
  };

  // #1, #GTX, #1#FOO, #1#BAR
  struct device_spec : spec {
    enum skind {
      INVALID_DEVICE,
      BY_DEFAULT,
      BY_INDEX,
      BY_NAME
    } skind = skind::BY_DEFAULT;
    int           by_index_value = -1;
    std::string   by_name_value;

    std::string   instance; // :BAR

    device_spec(loc loc) : spec(loc,spec::DEVICE_SPEC) { }

    void setSource(std::string name);
    void setSource(int index);

    void str(std::ostream &os, format_opts fopts) const;
  };

  struct program_spec : spec {
    device_spec     device;
    std::string     path;
    std::string     build_options;

    program_spec(loc loc) : spec(loc,spec::PROGRAM_SPEC), device(loc) { }
    void str(std::ostream &os, format_opts fopts) const;
  };
  struct kernel_spec : spec {
    refable<program_spec>     program;
    std::string               name;

    kernel_spec(program_spec *ps)
      : spec(ps->defined_at,spec::KERNEL_SPEC), program(ps) { }
    void str(std::ostream &os, format_opts fopts) const;
  };
  struct ndr {
    size_t num_dims;
    size_t dims[3];

    ndr();
    ndr(size_t x);
    ndr(size_t x, size_t y);
    ndr(size_t x, size_t y, size_t z);
    // ndr(const ndr &) = default;

    const size_t *get() const {return dims;}

    size_t rank() const {return num_dims;}
    size_t product() const;

    std::string str() const;
    void        str(std::ostream &os) const;
  };
  struct dispatch_spec : statement_spec {
    refable<kernel_spec>                                kernel;
    std::vector<init_spec_atom*>                        global_size;
    std::vector<init_spec_atom*>                        local_size;
    std::vector<refable<init_spec>>                     arguments;
    std::vector<std::tuple<std::string,init_spec*>>     where_bindings;

    dispatch_spec(loc loc) : statement_spec(loc, statement_spec::DISPATCH) { }
    void str(std::ostream &os,format_opts fopts) const;
  };

  // let X = ...
  struct let_spec : statement_spec {
    using param_refs = std::vector<refable<init_spec>*>;
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
  // diff(REF,SUT)       => exits the program non-zero if there is a difference
  // diff<TYPE>(REF,SUT) => exits the program non-zero if there is a difference
  struct diff_spec : statement_spec {
    refable<init_spec>            ref;
    refable<init_spec_mem>        sut;
    const type                   *element_type;

    // double max_diff;
    // int max_diff_ulps;

    diff_spec(
      loc loc,
      refable<init_spec> &_ref,
      refable<init_spec_mem> &_sut,
      const type *__element_type = nullptr)
      : statement_spec(loc, statement_spec::DIFF)
      , ref(_ref)
      , sut(_sut)
      , element_type(__element_type) { }
    void str(std::ostream &os,format_opts fopts) const;
  };
  // print(X)
  // print<TYPE>(X)
  // print<INT>(X)
  // print<TYPE,INT>(X)
  struct print_spec : statement_spec {
    refable<init_spec_mem>        arg;
    const type                   *element_type;
    int                           elements_per_row;
    print_spec(
      loc at,
      refable<init_spec_mem> &_arg,
      const type *_elem_type,
      int _elems_per_row)
      : statement_spec(at, statement_spec::PRINT)
      , arg(_arg)
      , element_type(_elem_type)
      , elements_per_row(_elems_per_row)
    { }
    void str(std::ostream &os,format_opts fopts) const;
  };
  // save('foo.bmp',X)
  struct save_spec : statement_spec {
    // init_spec_symbol *file; // foo.bpm
    std::string              file; // 'foo.bmp'
    refable<init_spec_mem>   arg;  // X
    save_spec(loc loc, std::string _file, const refable<init_spec_mem> &_arg)
      : statement_spec(loc, statement_spec::SAVE), file(_file), arg(_arg) { }
    void str(std::ostream &os,format_opts fopts) const {
      os << "save(" <<
        fopts.str_lit("'" + file + "'") << ", ";
        arg.str(os,fopts); os << ")";
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