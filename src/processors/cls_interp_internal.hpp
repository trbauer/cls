#include "cls_interp.hpp"
#include "../cl_headers.hpp"
#include "../half.hpp"
// #include "../list_map.hpp"
#include "../parser/kargs.hpp"
#include "../stats.hpp"

#include <fstream>
#include <functional>
#include <random>


using namespace cls;
using namespace cls::k;


// Device state object.  This contains information for an device_spec
// mapping and allows us to use a shared context etc... in multi kernel
// scripts.
//
// E.g.  #1`Prog.cl`Kernel1<...>(A,...)
//       ...
//       #1`Prog.cl`Kernel2<...>(...,A)
//
// Device #1 context is shared between calls.
// (Which is necessary or we can't use the same buffer.)
//
struct device_object {
  const device_spec *spec; // first definition of this device; SPECIFY: others?
  cl::Device         device;
  std::string        callback_prefix; // e.g. "[1.2: Intel HD]: "
  size_t             pointer_size; // in bytes

  device_object(const device_spec *_spec, cl::Device dev)
    : spec(_spec), device(dev)
  {
    std::stringstream ss;
    ss << "[" << spec->defined_at.line << "." <<
      spec->defined_at.column << "]: ";
    callback_prefix = ss.str();
    pointer_size = dev.getInfo<CL_DEVICE_ADDRESS_BITS>()/8;
  }
  // device_object(const device_object &) = delete;
  // device_object &operator=(const device_object &) = delete;
  ~device_object() {std::cerr << "destructing!\n";}

  cl::Context *context = nullptr;
  cl::CommandQueue *queue = nullptr;

  void contextNotify(
    const char *errinfo,
    const void *private_info, size_t cb,
    void *user_data)
  {
    if (errinfo) {
      std::cout << text::prefix_lines(callback_prefix, errinfo);
    }
  }
};
struct program_object {
  const program_spec     *spec;
  device_object          *device;
  cl_program              program = nullptr;
  cls::k::program_info    program_info;
  program_object(const program_spec *_spec, device_object *_device)
    : spec(_spec), device(_device) { }
};
struct kernel_object {
  const kernel_spec     *spec;
  program_object        *program;
  cl::Kernel            *kernel = nullptr;
  cls::k::kernel_info   *kernel_info = nullptr;
  kernel_object(const kernel_spec *_spec, program_object *_program)
    : spec(_spec), program(_program) { }
};

struct dispatch_command;

struct surface_object {
  enum skind {
    SO_INVALID = 0,
    SO_BUFFER,
    SO_IMAGE
  } skind;

  const init_spec_mem      *spec;
  size_t                    size_in_bytes;
  cl_mem                    memobj = nullptr;
  int                       memobj_index;
  cl_command_queue          queue = nullptr;

  // only non-zero if it's an image
  cl_image_format           image_format{0};
  cl_image_desc             image_desc{0};

  // e.g. if it's used in a diff command only diff(seq(1,2):w,...)
  bool                      dummy_object = false;

  // - command used in (same command can appear multiple times if reused)
  // - arg index
  using use = std::tuple<dispatch_command *,cl_uint,const arg_info &>;
  std::vector<use>          dispatch_uses;

  sampler                   init_times;

  surface_object(
    const init_spec_mem *_spec,
    enum surface_object::skind _kind,
    size_t _size_in_bytes,
    cl_mem _mem,
    int _memobj_index,
    cl_command_queue _queue)
    : skind(_kind)
    , spec(_spec)
    , size_in_bytes(_size_in_bytes)
    , memobj(_mem)
    , memobj_index(_memobj_index)
    , queue(_queue)
  { }
  std::string str() const {
    std::stringstream ss;
    ss << "0x" << std::hex << std::uppercase << memobj <<
      "  (" << std::dec << size_in_bytes << " B)";
    return ss.str();
  }
};


// Interpreter dispatch object
//
// All dispatch_spec's are converted into these objects.
// I.e. each one maps onto an NDRange enqueue
struct dispatch_command {
  const dispatch_spec                             *spec;
  device_object                                   *dobj;
  kernel_object                                   *kernel;

  // we replicate this because ds can be null and reqd size may be set
  ndr                                              global_size, local_size;

  using surface_use = std::tuple<surface_object *,const type&,const arg_info &,const loc &>;
  std::vector<surface_use>                         surfaces;

  std::vector<std::string>                         evaluated_args;

  sampler                                          wall_times;
  sampler                                          prof_times;

  dispatch_command(const dispatch_spec *_spec, kernel_object *_kernel)
    : spec(_spec), kernel(_kernel)
    , dobj(_kernel->program->device)
  {
  }

  size_t pointer_size() const {return kernel->program->device->pointer_size;}

  std::string str() const {
    std::stringstream ss;
    ss << "#" <<
      kernel->program->device->device.getInfo<CL_DEVICE_NAME>().c_str();
    ss << "`";
    cl_program p = kernel->program->program;
    ss << p << "`" << (*kernel->kernel)();
    ss << "<";
    if (spec) {
      ss << global_size.str();
      if (local_size.rank() > 0) {
        ss << ",";
        ss << local_size.str();
      }
    } else {
      ss << "<nullptr>";
    }
    ss << ">";
    ss << "(";
    size_t next_so = 0;
    for (size_t i = 0; i < evaluated_args.size(); i++) {
      if (i > 0)
        ss << ", ";
      ss << evaluated_args[i];
    }
    ss << ")";
    return ss.str();
  }
};

template <typename K,typename V>
struct mapped_objects {
  using map_iterator = typename std::map<K,V*>::iterator;
  using list_iterator = typename std::vector<V*>::iterator;
  using list_const_iterator = typename std::vector<V*>::const_iterator;

  std::vector<V*> list;
  std::map<K,V*> map;

  map_iterator find(K k) {return map.find(k);}
  map_iterator find_end() {return map.end();}
  list_iterator begin() {return list.begin();}
  list_iterator end() {return list.end();}
  list_const_iterator begin() const {return list.begin();}
  list_const_iterator end() const {return list.end();}
  size_t size() const {return list.size();}

  template <typename...As>
  V& emplace_back(K k, As...as) {
    V *v = new V(as...);
    list.push_back(v);
    map[k] = v;
    return *v;
  }
  V& get(K k) {
    // TODO: use find and blow up if object is missing
    return *map[k];
  }
};

using buffer_reader = std::function<void(const void *)>;
using image_reader = std::function<void(size_t, size_t,const void *)>;
using buffer_writer = std::function<void(void *)>;

// e.g. CL_COMMAND(spec->defined_at,clEnqueueNDRange,...)
struct cl_fatal_handler : fatal_handler {
  cl_fatal_handler(const std::string &src) : fatal_handler(src) { }

#define CL_SYM_STR(X) #X
#define CL_COMMAND(LOC,CL_FUNCTION,...) \
  do { \
    cl_int _err = CL_FUNCTION(__VA_ARGS__); \
    if (_err != CL_SUCCESS) { \
      fatalAt(LOC, CL_SYM_STR(CL_FUNCTION), \
        " (",cls::status_to_symbol(_err),")"); \
    } \
  } while(0)
#define CL_COMMAND_CREATE(ASSIGN_TO,LOC,CL_FUNCTION,...) \
  do { \
    cl_int _err = 0; \
    ASSIGN_TO = CL_FUNCTION(__VA_ARGS__,&_err); \
    if (_err != CL_SUCCESS) { \
      fatalAt(LOC, CL_SYM_STR(CL_FUNCTION), \
        " (",cls::status_to_symbol(_err),")"); \
    } \
  } while(0)

  void withBufferMapRead(
    const loc &at,
    const surface_object *so,
    buffer_reader apply);
  void withImageMapRead(
    const loc &at,
    const surface_object *so,
    image_reader apply);

  void withBufferMapWrite(
    const loc &at,
    surface_object *so,
    buffer_writer apply);
}; // cl_fatal_handler


struct interp_fatal_handler : cl_fatal_handler {
  const opts &os;

  interp_fatal_handler(const opts &_os, const std::string &src)
    : cl_fatal_handler(src), os(_os) { }

  template <typename...Ts>
  void debug(loc loc, Ts... ts) {
    if (os.verbosity >= 2) {
      formatMessageWithContext(
        std::cout, loc, &text::ANSI_GREEN, input(), ts...);
    }
  }
  template <typename...Ts>
  void debug(const spec *spec, Ts... ts) {
    debug(spec->defined_at,ts...);
  }
};

// uniform diff
// e.g. diff<int>(44,S);
struct diffu_command {
  const diff_spec           *spec;
  surface_object            *so;
  const type                *element_type;
  diffu_command(const diff_spec *_spec, surface_object *_so, const type *et)
    : spec(_spec), so(_so), element_type(et)
  {
  }
};

// surface diff
//  e.g. diff<int>(REF,SUT);
//  e.g. diff(seq(1,2):w, SUT);
struct diffs_command {
  const diff_spec           *spec;
  surface_object            *so_ref;
  surface_object            *so_sut; // SUT means "subject under test"
  const type                *element_type;
  diffs_command(
    const diff_spec *_spec,
    surface_object *_so_ref,
    surface_object *_so_sut,
    const type *et)
    : spec(_spec), so_ref(_so_ref), so_sut(_so_sut), element_type(et)
  {
  }
};

// print<int2,4>(SURF); // prints 4 x int2's per line
struct print_command {
  const print_spec           *spec;
  surface_object             *so;
  const type                 *element_type;
  print_command(const print_spec *_spec, surface_object *_so, const type *et)
    : spec(_spec), so(_so), element_type(et)
  {
  }
};

struct save_command {
  const save_spec            *spec;
  surface_object             *so;
  save_command(const save_spec *_spec, surface_object *_so)
    : spec(_spec), so(_so)
  {
  }
};

struct script_instruction {
  enum {DISPATCH,DIFFS,DIFFU,PRINT,SAVE} skind;
  union {
    dispatch_command *dsc;
    diffs_command    *dfsc;
    diffu_command    *dfuc;
    print_command    *prc;
    save_command     *svc;
  };
  script_instruction(dispatch_command *_dc) : dsc(_dc), skind(DISPATCH) { }
  script_instruction(diffs_command *_dc) : dfsc(_dc), skind(DIFFS) { }
  script_instruction(diffu_command *_dc) : dfuc(_dc), skind(DIFFU) { }
  script_instruction(print_command *_prc) : prc(_prc), skind(PRINT) { }
  script_instruction(save_command *_svc) : svc(_svc), skind(SAVE) { }
};


// A buffer that we can write values to for kernel arguments.
// It is also used as the host side backing store to setup buffers.
struct arg_buffer : fatal_handler {
  loc                    at;
  size_t                 capacity;
  uint8_t               *base, *curr;
  bool                   owns_memory;

  arg_buffer(const fatal_handler *_fh, loc _at, size_t _len)
    : fatal_handler(_fh->input()), at(_at), capacity(_len)
  {
    curr = base = new uint8_t[capacity];
    owns_memory = true;
    memset(curr, 0, capacity);
  }
  // e.g. writing to a mapped buffer
  arg_buffer(const fatal_handler *_fh, loc _at, void *ptr, size_t cap)
    : fatal_handler(_fh->input())
    , at(_at)
    , base((uint8_t *)ptr)
    , capacity(cap)
  {
    curr = base;
    owns_memory = false;
  }
  ~arg_buffer() {
    if (owns_memory)
      delete base;
  }
  arg_buffer(const arg_buffer &) = delete;
  arg_buffer &operator==(const arg_buffer &) = delete;

  const uint8_t *ptr() const {return base;}
  size_t         num_left() const {return capacity - size();}
  size_t         size() const {return (curr - base);}
  uint8_t       *take_ownership() {
    uint8_t *t = base;
    base = nullptr;
    owns_memory = false;
    return t;}

  template <typename T>
  void write(const T &t) {write(&t,sizeof(t));}
  void write(const void *src, size_t len) {
    if (len > num_left()) {
      internalAt(at,"INTERNAL ERROR: buffer overflow");
    }
    memcpy(curr, src, len);
    curr += len;
  }
  void fill_with_zeros() {
    memset(curr, 0, num_left());
    curr += num_left();
  }

  template <typename T>
  T    read() {
    T t;
    read(&t,sizeof(t));
    return t;
  }
  void read(void *val, size_t len) {
    if (len > num_left()) {
      internalAt(at,"INTERNAL ERROR: buffer underflow");
    }
    memcpy(val, curr, len);
    curr += len;
  }
}; // arg_buffer


struct evaluator : interp_fatal_handler {
  struct context {
    size_t     sizeof_pointer; // in bytes
    const ndr &global_size;
    const ndr &local_size;
    context(
      size_t sizeof_ptr,
      const ndr &gs, const ndr &ls,
      std::stringstream *dss = nullptr)
      : sizeof_pointer(sizeof_ptr)
      , global_size(gs), local_size(ls)
      , debug_stream(dss) { }
    context() : context(0, ndr(),ndr()) { }
    context(const dispatch_command &dc, std::stringstream *dss = nullptr)
      : context(dc.pointer_size(), dc.global_size, dc.local_size, dss)
    {
    }

    std::stringstream *debug_stream;

    template <typename T> void evaluated(const T &t) {
      if (debug_stream)
        *debug_stream << t;
    }
  };

  std::random_device                                          rd;
  std::mt19937                                                gen;

  struct compiled_script_impl                                *csi;

  evaluator(compiled_script_impl *_csi);

  /////////////////////////////////////////////////////////////////////////////
  // primitive value evaluation
  val eval(context &ec, const init_spec_atom *e);
  val evalI(context &ec, const init_spec_atom *e);

  template <typename T>
  val evalToI(context &ec, const init_spec_atom *e) {
    val v = eval(ec, e);
    if (v.is_float())
      fatalAt(e->defined_at,"cannot implicitly convert float to int");
    if (v.is_signed()) {
      if ((T)v.s64 < std::numeric_limits<T>::min() ||
        (T)v.s64 > std::numeric_limits<T>::max())
      {
        fatalAt(e->defined_at,"value out of range");
      }
      v = (T)v.s64;
    } else { // v.is_unsigned()
      if ((T)v.u64 > std::numeric_limits<T>::max()) {
        fatalAt(e->defined_at,"value out of range");
      }
      v = (T)v.u64;
    }
    return v;
  }
  template <typename T>
  val evalToF(context &ec, const init_spec_atom *e) {
    val v = eval(ec, e);
    //
    // implicit conversion from int to float is allowed
    if (v.is_signed())
      v = (double)v.s64;
    else if (v.is_unsigned())
      v = (double)v.u64;
    //
    // we enforce bounds checking, but not precision
    // SPECIFY: do we care about min? we could RTZ
    auto checkBounds =
      [&](double low, double hi, double min) {
        if (std::isnan(v.f64) || std::isinf(v.f64))
          return; // unordered values are fine
        if (v.f64 < low || v.f64 > hi) {
          fatalAt(e->defined_at,"value out of range");
        } else if (v.f64 != 0.0 && std::abs(v.f64) < min) {
          fatalAt(e->defined_at,"value out of range");
        }
      };
    if (sizeof(T) == 8) {
      checkBounds(
        std::numeric_limits<double>::lowest(),
        std::numeric_limits<double>::max(),
        std::numeric_limits<double>::min());
    } else if (sizeof(T) == 4) {
      checkBounds(
        std::numeric_limits<float>::lowest(),
        std::numeric_limits<float>::max(),
        std::numeric_limits<float>::min());
    } else { // half
      checkBounds(
        -SFLT_MAX,
        SFLT_MAX,
        SFLT_MIN);
    }
    return v;
  }

  template <typename T>
  val evalTo(context &ec,const init_spec_atom *e) {
    // we have to be careful about evaluating in this order
    // std::is_floating_point fails on half
    if (std::is_integral<T>()) {
      return evalToI<T>(ec,e);
    } else { // std::is_floating_point<T>()
      return evalToF<T>(ec,e);
    }
  }
  val evalF(context &ec,const init_spec_atom *e);
  val evalToF(context &ec,const init_spec_atom *e);

  /////////////////////////////////////////////////////////////////////////////
  // value generators for init_spec*
  void setKernelArgImmediate(
    cl_uint arg_index,
    dispatch_command &dc,
    std::stringstream &ss,
    const refable<init_spec> &ris,
    const arg_info &ai); // top-level
  void setKernelArgBuffer(
    cl_uint arg_index,
    dispatch_command &dc,
    std::stringstream &ss, // debug string for arg
    const loc &arg_loc,
    const refable<init_spec> &ris,
    const arg_info &ai);
  void setKernelArgImage(
    cl_uint arg_index,
    dispatch_command &dc,
    std::stringstream &ss, // debug string for arg
    const loc &arg_loc,
    const refable<init_spec> &ris,
    const arg_info &ai);
  void setKernelArgSLM(
    cl_uint arg_index,
    dispatch_command &dc,
    std::stringstream &ss, // debug string for arg
    const refable<init_spec> &ris,
    const arg_info &ai);

  void evalInto(
    context &ec,
    const loc &arg_loc,
    const init_spec_atom *e,
    arg_buffer &ab,
    const type &t);
  void evalInto(
    context &ec,
    const loc &arg_loc,
    const init_spec_atom *e,
    arg_buffer &ab,
    const type_num &tn);
  template <typename T>
  void evalIntoT(
    context &ec,
    const loc &arg_loc,
    const init_spec_atom *e,
    arg_buffer &ab);
  void evalInto(
    context &ec,
    const loc &arg_loc,
    const init_spec_atom *e,
    arg_buffer &ab,
    const type_struct &ts);
  void evalInto(
    context &ec,
    const loc &arg_loc,
    const init_spec_atom *e,
    arg_buffer &ab,
    const type_ptr &tp); // for SVM?
}; // evaluator

using device_key = std::tuple<cl_device_id,std::string>;
struct compiled_script_impl : interp_fatal_handler {
  const script                                           &s;
  struct evaluator                                       *e;

  mapped_objects<const dispatch_spec*,dispatch_command>   dispatches;
  // mapped_objects<const device_spec*,device_object>        devices;
  mapped_objects<device_key,device_object>                devices;
  mapped_objects<const program_spec*,program_object>      programs;
  mapped_objects<const kernel_spec*,kernel_object>        kernels;

  mapped_objects<const init_spec_mem*,surface_object>     surfaces;

  std::vector<script_instruction>                         instructions;

  compiled_script_impl(const opts &_os,const script &_s);

  surface_object *define_surface(
    const init_spec_mem *_spec,
    enum surface_object::skind _kind,
    size_t _size_in_bytes,
    cl_mem _mem,
    cl_command_queue _queue);

  void init_surfaces();
  void init_surface(
    surface_object &so,
    evaluator::context &ec,
    const type *elem_type,
    void *host_ptr);
  void execute(dispatch_command &dc);
  void execute(diffu_command &dfc, const void *buf);
  void execute(diffs_command &dfc, const void *buf_ref, const void *buf_sut);
  void execute(print_command &prc, const void *buf);
  void execute(save_command &svc, const void *buf);
}; // compiled_script_impl
