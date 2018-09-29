#include "cls_interp.hpp"
#include "../cl_headers.hpp"
#include "../parser/kargs.hpp"
#include "../stats.hpp"
#include "../list_map.hpp"



using namespace cls;


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

  ~device_object() {
    std::cerr << "destructing!\n";
  }
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

struct surface_object {
  enum kind {
    SO_INVALID = 0,
    SO_BUFFER,
    SO_IMAGE
  } kind;

  const init_spec_memory   *spec;
  size_t                    size_in_bytes;
  cl_mem                    memobj = nullptr;

  const void               *initializer = nullptr;

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
  const dispatch_spec *ds;
  kernel_object *kernel;

  dispatch_command(const dispatch_spec *_ds, kernel_object *_kernel)
    : ds(_ds), kernel(_kernel) { }
  size_t pointer_size() const {
    return kernel->program->device->pointer_size;
  }

  sampler times;

  // this is unique to the dispatch
  cl::NDRange local_size = cl::NullRange;
  cl::NDRange global_size;



  size_t global_items() const {
    size_t prod = 1;
    for (size_t i = 0; i < global_size.dimensions(); i++) {
      prod *= global_size.get()[i];
    }
    return prod;
  }
  size_t local_items() const {
    if (local_size.dimensions() == 0)
      return 0;
    size_t prod = 1;
    for (size_t i = 0; i < local_size.dimensions(); i++) {
      prod *= local_size.get()[i];
    }
    return prod;
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


struct compiled_script_impl {
  const script                                           &s;

  mapped_objects<const dispatch_spec*,dispatch_command>   dispatches;
  mapped_objects<const device_spec*,device_object>        devices;
  mapped_objects<const program_spec*,program_object>      programs;
  mapped_objects<const kernel_spec*,kernel_object>        kernels;

  mapped_objects<const init_spec_memory*,surface_object>  surfaces;

  // std::map<const device_spec*,device_object*>      devices;
  // std::vector<device_object>                      device_object_list;
  // std::map<const program_spec*,program_object*>    programs;
  // std::vector<program_object>                     program_object_list;
  // std::map<const kernel_spec*,kernel_object*>      kernels;
  // std::vector<kernel_object>                      kernel_object_list;

  /// OL
  // std::vector<interp_buffer_define>              buffer_defines;
  // std::vector<interp_image2d_define>             image2d_defines;

  compiled_script_impl(const script &_s) : s(_s) { }
};




/*
template <typename T>
struct interp_memory_define {
  init_spec_memory                   *spec;
  T                                       *memory;
  // std::vector<dispatch_command*>          read_by, written_by;
  interp_memory_define(init_spec_memory *_spec, T *_memory)
    : spec(_spec), memory(_memory) { }
};
using interp_buffer_define = interp_memory_define<cl::Buffer>;
using interp_image2d_define = interp_memory_define<cl::Image2D>;
// TODO: other image types
*/
