#include "cls_interp.hpp"
#include "../cl_headers.hpp"
#include "../parser/kargs.hpp"
#include "../stats.hpp"
#include "../list_map.hpp"

#if __has_include(<filesystem>)
#include <filesystem>
namespace fs = std::filesystem;
#elif __has_include(<experimental/filesystem>)
#include <experimental/filesystem>
namespace fs = std::experimental::filesystem;
#else
#error "#include <filesystem> not found"
#endif
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
  cl::Program            *program = nullptr;
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

struct generator_state {
  std::uniform_int_distribution<int64_t>                s_dist;
  std::uniform_int_distribution<uint64_t>               u_dist;
  std::uniform_real_distribution<double>                f_dist;
  std::mt19937                                          gen;

  std::fstream                                          file;
  size_t                                                file_size = 0;

  // generator_state(std::random_device &_rd) : gen(_rd) { }
  generator_state(int64_t seed) {gen.seed((unsigned)seed);}
  generator_state(std::string _file)
    : file(_file,std::ios_base::in|std::ios_base::binary) { }
};

struct surface_object {
  enum kind {
    SO_INVALID = 0,
    SO_BUFFER,
    SO_IMAGE
  } kind;

  const init_spec_memory   *spec;
  size_t                    size_in_bytes;
  cl_mem                    memobj = nullptr;

  surface_object(
    const init_spec_memory *_spec,
    enum surface_object::kind _kind,
    size_t _size_in_bytes,
    cl_mem _mem)
    : kind(_kind), spec(_spec), size_in_bytes(_size_in_bytes), memobj(_mem) { }
};


// Interpreter dispatch object
//
// All dispatch_spec's are converted into these objects.
// I.e. each one maps onto an NDRange enqueue
struct dispatch_command {
  const dispatch_spec                             *ds;
  device_object                                   *dobj;
  kernel_object                                   *kernel;
  using surf_arg =
    std::tuple<
      loc,
      cls::k::type_ptr,
      surface_object*>;
  std::vector<surf_arg>                            inits; // the surfaces we have to re-initialize

  sampler                                          wall_times;
  sampler                                          prof_times;

  // this is unique to the dispatch
  cl::NDRange local_size = cl::NullRange;
  cl::NDRange global_size;

  dispatch_command(const dispatch_spec *_ds, kernel_object *_kernel)
    : ds(_ds), kernel(_kernel), dobj(_kernel->program->device) { }
  size_t pointer_size() const {
    return kernel->program->device->pointer_size;
  }

  std::string str() const {
    std::stringstream ss;
    ss << "#" <<
      kernel->program->device->device.getInfo<CL_DEVICE_NAME>().c_str();
    ss << "`";
    cl_program p = (*kernel->program->program)();
    ss << p << "`" << (*kernel->kernel)();
    ss << "<";
    ss << fmtNDRange(global_size);
    ss << ",";
    ss << fmtNDRange(local_size);
    ss << ">";
    ss << "(";
    size_t next_so = 0;
    for (size_t i = 0; i < ds->arguments.size(); i++) {
      if (i > 0)
        ss << ", ";
      const init_spec *is = ds->arguments[i];
      if (is->kind == init_spec::IS_MEM) {
        const surface_object *so = std::get<2>(inits[next_so++]);
        ss << so->memobj << "[" << so->size_in_bytes << "]";
      } else {
        is->str(ss,format_opts());
      }
    }
    ss << ")";
    return ss.str();
  }
};

template <typename K,typename V>
struct mapped_objects {
  using map_iterator = typename std::map<K,V*>::iterator;
  using list_iterator = typename std::vector<V>::iterator;
  using list_const_iterator = typename std::vector<V>::const_iterator;

  std::vector<V> list;
  std::map<K,V*> map;

  map_iterator find(K k) {return map.find(k);}
  map_iterator find_end() {return map.end();}
  list_iterator begin() {return list.begin();}
  list_iterator end() {return list.end();}
  list_const_iterator begin() const {return list.begin();}
  list_const_iterator end() const {return list.end();}

  template <typename...As>
  V& emplace_back(K k, As...as) {
    list.emplace_back(as...);
    V &obj = list.back();
    map[k] = &obj;
    return obj;
  }
  V& get(K k) {
    // TODO: use find and blow up if object is missing
    return *map[k];
  }
};

using buffer_reader = std::function<void(const void *)>;
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
#define CL_COMMAND_CREATE(ASSIGN,LOC,CL_FUNCTION,...) \
  do { \
    cl_int _err = 0; \
    ASSIGN = CL_FUNCTION(__VA_ARGS__,&_err); \
    if (_err != CL_SUCCESS) { \
      fatalAt(LOC, CL_SYM_STR(CL_FUNCTION), \
        " (",cls::status_to_symbol(_err),")"); \
    } \
  } while(0)

  void withBufferMapRead(
    loc at,
    cl_command_queue cq,
    const surface_object *so,
    buffer_reader apply);
  void withBufferMapWrite(
    loc at,
    cl_command_queue cq,
    surface_object *so,
    buffer_writer apply);
}; // cl_fatal_handler

struct compiled_script_impl : cl_fatal_handler {
  const opts                                             &os;
  const script                                           &s;
  struct evaluator                                       *e;

  mapped_objects<const dispatch_spec*,dispatch_command>   dispatches;
  mapped_objects<const device_spec*,device_object>        devices;
  mapped_objects<const program_spec*,program_object>      programs;
  mapped_objects<const kernel_spec*,kernel_object>        kernels;

  mapped_objects<const init_spec_memory*,surface_object>  surfaces;

  compiled_script_impl(const opts &_os,const script &_s);

  void init_surfaces(dispatch_command &dc);
  void execute(dispatch_command &dc);
};

// A buffer that we can write values to for kernel arguments.
// It is also used as the host side backing store to setup buffers.
struct arg_buffer : fatal_handler {
  loc                    arg_loc;
  size_t                 capacity;
  uint8_t               *base, *curr;
  bool                   owns_memory;

  arg_buffer(const fatal_handler *_fh, loc _arg_loc, size_t _len)
    : fatal_handler(_fh->input()), arg_loc(_arg_loc), capacity(_len)
  {
    curr = base = new uint8_t[capacity];
    owns_memory = true;
    memset(curr, 0, capacity);
  }
  // e.g. writing to a mapped buffer
  arg_buffer(const fatal_handler *_fh, loc _arg_loc, void *ptr, size_t cap)
    : fatal_handler(_fh->input()), arg_loc(_arg_loc), base((uint8_t *)ptr), capacity(cap)
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
      fatalAt(arg_loc,"INTERNAL ERROR: buffer overflow");
    }
    memcpy(curr, src, len);
    curr += len;
  }
}; // arg_buffer


struct evaluator : cl_fatal_handler {
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
    val &operator=(float    _val) {*this = (double)_val; return *this;}
    val &operator=(double   _val) {*this = val(_val); return *this;}

    bool is_float() const {return is_f;}
    bool is_int() const {return !is_f;}
    bool is_signed() const {return !is_f && !is_u;}
    bool is_unsigned() const {return is_u;}
  };

  std::random_device                                          rd;
  std::mt19937                                                gen;
  std::unordered_map<const init_spec_rng *,generator_state>   file_gens;
  std::unordered_map<const init_spec_file *,generator_state>  rng_gens;

  const opts                                                 &os;
  compiled_script_impl                                       *csi;

  loc eval_loc;

  evaluator(compiled_script_impl *_csi)
    : cl_fatal_handler(csi->input()), os(csi->os), csi(_csi) { }

  generator_state &get_state(
    dispatch_command &dc,
    const init_spec_rng *isr,
    const type_num &tn);
  generator_state &get_state(
    dispatch_command &dc,
    const init_spec_file *isf);

  /////////////////////////////////////////////////////////////////////////////
  // primitive value evaluation
  val eval(dispatch_command &dc,const init_spec_atom *e);
  val evalI(dispatch_command &dc,const init_spec_atom *e);
  template <typename T>
  val evalTo(dispatch_command &dc,const init_spec_atom *e) {
    val v = evalI(dc, e);
    if (v.is_signed()) {
      if ((T)v.s64 < std::numeric_limits<T>::min() ||
        (T)v.s64 > std::numeric_limits<T>::max())
      {
        fatalAt(e->defined_at,"value out of range");
      }
      v = (T)v.s64;
    } else if (v.is_unsigned()) {
      if ((T)v.u64 > std::numeric_limits<T>::max()) {
        fatalAt(e->defined_at,"value out of range");
      }
      v = (T)v.u64;
    }
    return v;
  }
  val evalF(dispatch_command &dc,const init_spec_atom *e);
  val evalToF(dispatch_command &dc,const init_spec_atom *e);

  /////////////////////////////////////////////////////////////////////////////
  // value generators for init_spec*
  void genArg(
    dispatch_command &dc,
    arg_buffer &ab,
    const refable<init_spec *> &ris,
    const arg_info &ai); // top-level
  void genArgSurface(
    dispatch_command &dc,
    arg_buffer &ab,
    const loc &arg_loc,
    const  init_spec_memory *ism,
    const arg_info &ai);
  void genArg(
    arg_buffer &ab,
    generator_state *r,
    const init_spec *is,
    const type &t);
  void genArg(
    arg_buffer &ab,
    generator_state *r,
    const init_spec *is,
    const type_num &tn);
  void genArg(
    arg_buffer &ab,
    generator_state *r,
    const init_spec *is,
    const type_struct &ts);
  void genArg(
    arg_buffer &ab,
    generator_state *r,
    const init_spec *is,
    const type_ptr &tp); // for SVM?
};

