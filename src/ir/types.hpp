#ifndef IR_TYPES_HPP
#define IR_TYPES_HPP

#include "../macros.hpp"
#include "../text.hpp"

#include <cstdint>
#include <initializer_list>
#include <ostream>
#include <sstream>
#include <variant>

namespace cls
{
  // forward decl for types below
  struct type;

  ///////////////////////////////////////////////////////////////////////////
  // int foo[16];
  struct type_array {
    const type    *element_type;
    size_t         length;
    //
    type_array(const type *t, size_t l)
      : element_type(t), length(l)
    {
    }
    constexpr type_array(type *t, size_t l)
      : element_type(t), length(l)
    {
    }
    size_t size() const;
    std::string syntax() const;
    bool operator==(const type_array &t) const;
    bool operator!=(const type_array &t) const { return !(*this == t); }
  };

  ///////////////////////////////////////////////////////////////////////////
  // enum foo{BAR,BAZ,QUX}
  struct type_enum {
    // std::vector<std::tuple<std::string,int>> symbols;
    // type_num *represenative_type = INT;
    const char *name;
    constexpr type_enum(
      const char *_name,
      std::initializer_list<std::tuple<std::string,int>> _symbols)
      : name(_name)
    {
    }

    constexpr size_t size(size_t) const {return 4;}
    std::string syntax() const {return name;}
    bool operator==(const type_enum &t) const {return text::streq(name,t.name);}
    bool operator!=(const type_enum &t) const {return !(*this == t);}
  };
  ///////////////////////////////////////////////////////////////////////////
  // other built-in types that we allow special recognition and
  // potentially handle specially
  //
  // e.g. image2d_t
  struct type_builtin {
    // https://www.khronos.org/registry/OpenCL/sdk/2.0/docs/man/xhtml/otherDataTypes.html
    enum bi_kind {
      IMAGE1D,                  // image1d_t
      IMAGE1D_ARRAY,            // image1d_array_t
      IMAGE1D_BUFFER,           // image1d_buffer_t
      IMAGE2D,                  // image2d_t
      IMAGE2D_ARRAY,            // image2d_array_t
      IMAGE2D_DEPTH,            // image2d_depth_t
      IMAGE2D_ARRAY_DEPTH,      // image2d_array_depth_t
      IMAGE2D_MSAA,             // image2d_msaa_t
      IMAGE2D_ARRAY_MSAA,       // image2d_array_msaa_t
      IMAGE2D_MSAA_DEPTH,       // image2d_msaa_depth_t
      IMAGE2D_ARRAY_MSAA_DEPTH, // image2d_array_msaa_depth_t
      IMAGE3D,                  // image3d_t
      SAMPLER,                  // sampler_t
      QUEUE,                    // queue_t
      NDRANGE,                  // ndrange_t
      CLK_EVENT,                // clk_event_t
      RESERVE_ID,               // reserve_id_t
      EVENT,                    // event_t
      CL_MEM_FENCE_FLAGS,       // cl_mem_fence_flags
    } skind;

    size_t pointer_size;

    // TODO: read up on image details
    // https://www.khronos.org/registry/OpenCL/sdk/2.0/docs/man/xhtml/read_imagef2d.html
    // cl_khr_mipmap_image https://www.khronos.org/registry/OpenCL/sdk/2.0/docs/man/xhtml/cl_khr_mipmap_image.html

    constexpr type_builtin(const type_builtin &tb) = default;
    constexpr type_builtin(bi_kind _kind, size_t ptr_size)
      : skind(_kind), pointer_size(ptr_size) { }

    bool is_surface() const {
      switch (skind) {
      case type_builtin::IMAGE1D:
      case type_builtin::IMAGE1D_ARRAY:
      case type_builtin::IMAGE1D_BUFFER:
      case type_builtin::IMAGE2D:
      case type_builtin::IMAGE2D_ARRAY:
      case type_builtin::IMAGE2D_ARRAY_DEPTH:
      case type_builtin::IMAGE2D_ARRAY_MSAA:
      case type_builtin::IMAGE2D_ARRAY_MSAA_DEPTH:
      case type_builtin::IMAGE3D:
        return true;
      }
      return false;
    }

    size_t size() const {return pointer_size;}
    std::string syntax() const {
      switch (skind) {
      case IMAGE1D:                   return "image1d_t";
      case IMAGE1D_ARRAY:             return "image1d_array_t";
      case IMAGE1D_BUFFER:            return "image1d_buffer_t";
      case IMAGE2D:                   return "image2d_t";
      case IMAGE2D_ARRAY:             return "image2d_array_t";
      case IMAGE2D_DEPTH:             return "image2d_depth_t";
      case IMAGE2D_ARRAY_DEPTH:       return "image2d_array_depth_t";
      case IMAGE2D_MSAA:              return "image2d_msaa_t";
      case IMAGE2D_ARRAY_MSAA:        return "image2d_array_msaa_t";
      case IMAGE2D_MSAA_DEPTH:        return "image2d_msaa_depth_t";
      case IMAGE2D_ARRAY_MSAA_DEPTH:  return "image2d_array_msaa_depth_t";
      case IMAGE3D:                   return "image3d_t";
      case SAMPLER:                   return "sampler_t";
      case QUEUE:                     return "queue_t";
      case NDRANGE:                   return "ndrange_t";
      case CLK_EVENT:                 return "clk_event_t";
      case RESERVE_ID:                return "reserve_id_t";
      case EVENT:                     return "event_t";
      // could be an enum
      case CL_MEM_FENCE_FLAGS:        return "cl_mem_fence_flags";
      default:                        return "???";
      }
    }
    bool operator==(const type_builtin &t) const {return skind == t.skind;}
    bool operator!=(const type_builtin &t) const {return !(*this == t);}
  };

  ///////////////////////////////////////////////////////////////////////////
  // float, int, or unsigned
  struct type_num {
    enum skind {
      UNSIGNED = 0,
      SIGNED,
      FLOATING
    } skind;
    size_t size_in_bytes;
    const char *name;
    constexpr type_num(
      const char *_name, enum skind _kind, size_t _size_in_bytes)
      : name(_name), skind(_kind), size_in_bytes(_size_in_bytes) { }
    constexpr size_t     size() const { return size_in_bytes; }
    std::string          syntax() const { return name; }
    bool operator==(const type_num &t) const { return text::streq(name, t.name); }
    bool operator!=(const type_num &t) const { return !(*this == t); }

    /*
    template <typename RET,typename FUNC,typename...Ts>
    RET reify(FUNC f,Ts...ts) const {
      switch (skind) {
      case UNSIGNED:
        switch (size) {
        case 1: return f<uint8_t>(ts...);
        case 2: return f<uint16_t>(ts...);
        case 4: return f<uint32_t>(ts...);
        case 8: return f<uint64_t>(ts...);
        }
        break;
      case SIGNED:
        switch (size) {
        case 1: return f<int8_t>(ts...);
        case 2: return f<int16_t>(ts...);
        case 4: return f<int32_t>(ts...);
        case 8: return f<int64_t>(ts...);
        }
        break;
      case FLOATING:
        switch (size) {
        case 2: return f<half>(ts...);
        case 4: return f<float>(ts...);
        case 8: return f<double>(ts...);
        }
      default: throw "INTERNAL ERROR: corrupt type_num";
      }
    } // reify
    */

  }; // type_num

  ///////////////////////////////////////////////////////////////////////////
  struct type_ptr {
    enum {
      EMPTY_ATTRS = 0,
      CONST = (1 << 0),
      RESTRICT = (1 << 1),
      VOLATILE = (1 << 2),
    } attrs = EMPTY_ATTRS;
    const type *element_type;
    size_t pointer_size = 0;

    constexpr type_ptr(const type_ptr &t)
      : element_type(t.element_type), pointer_size(t.pointer_size) { }
    constexpr type_ptr(const type *t, size_t ptr_size)
      : element_type(t), pointer_size(ptr_size) { }

    constexpr size_t size() const { return pointer_size; }
    std::string syntax() const;
    bool operator==(const type_ptr &t) const;
    bool operator!=(const type_ptr &t) const { return !(*this == t); }
  };

  ///////////////////////////////////////////////////////////////////////////
  // Product types.
  //
  // e.g. struct foo {int x,float y,struct{int z,short w}bar};
  struct type_struct {
    const char                 *name;

    bool                        packed = false; // __attribute__ ((packed))
    size_t                      alignment = 0; // e.g. __attribute__ ((aligned (8)));

    const type                 *elements_memory[16];
    const type                **elements = elements_memory;
    size_t                      elements_length = 0;

/*
    constexpr type_struct(const type_struct &ts) = default;
      : name(ts.name)
      , packed(ts.packed)
      , alignment(ts.alignment)
      , elements_length(ts.elements_length)
    {
      memcpy(elements_memory, ts.elements_memory, sizeof(elements_memory));
    }
    */
    // user-defined struct
    type_struct(
      const char *_name,
      bool _packed,
      size_t _alignment,
      const std::vector<const type *> &ts)
      : packed(_packed), alignment(_alignment)
    {
      char *name_buf = new char[strlen(_name+1)];
      memcpy(name_buf, _name, strlen(_name));
      name_buf[strlen(_name)] = 0;
      name = name_buf;
      //
      elements_length = ts.size();
      elements = (const type **)(new type *[ts.size()]);
      memcpy(elements, &ts[0], sizeof(ts[0])*ts.size());
      //
      elements_length = 0;
      for (const type *t : ts)
        elements[elements_length++] = t;
    }
    //
    bool needs_cleanup() const {return elements != elements_memory;}
    void cleanup() const {
      delete[] name;
      delete[] elements;
    }
    /*
    ~type_struct() {
      if () {
        delete[] elements;
        delete[] name;
      }
    }
    */
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
      size_t len) // vector length
      : name(_name)
      , packed(true)
      , alignment(elem_size*(size_t)len)
      , elements_memory{0}
      , elements_length(len)
    {
      for (size_t i = 0; i < elements_length; i++)
        elements[i] = type;
    }
    type_struct() : elements_memory{0} { }

    size_t size() const;
    std::string syntax() const {return name;}

    bool operator==(const type_struct &t) const {return text::streq(name,t.name);}
    bool operator!=(const type_struct &t) const {return !(*this == t);}
  };

  struct type_union {
    const char                 *name;
    // __attribute__ ((aligned (8)));
    // (technically this can work on other types too, but we don't support it)
    int                         aligned = 0;
    // __attribute__ ((packed))
    bool                        is_packed = false;
    //
    const type                 *elements_memory[2];
    const type                **elements = elements_memory;
    size_t                      elements_length = 0;

    constexpr type_union(
      const char *_name, std::initializer_list<const type *> ts)
      : name(_name), elements_memory{0}
    {
      if (ts.size() > sizeof(elements_memory)/sizeof(elements_memory[0]))
        throw "too many elements for constexpr";
      elements_length = 0;
      for (const type *t : ts)
        elements[elements_length++] = t;
    }
    // type_union() : elements_memory{0} { }
    std::string syntax() const {return name;}
    size_t size() const;
    bool operator==(const type_union &t) const {return text::streq(name,t.name);}
    bool operator!=(const type_union &t) const {return !(*this == t);}
  };

  // e.g. int4, float4
  struct type_vector {
    const type_num &element_type;
    size_t          length;

    constexpr type_vector(
      const type_num   &_element_type,
      size_t            _vector_length)
      : element_type(_element_type)
      , length(_vector_length)
    {
    }

    size_t size() const;
    std::string syntax() const;
    bool operator==(const type_vector &t) const;
    bool operator!=(const type_vector &t) const { return !(*this == t); }
  };

  ///////////////////////////////////////////////////////////////////////////
  // The void type
  struct type_void {
    constexpr size_t    size() const { return 0; }
    std::string         syntax() const { return "void"; }
    bool operator==(const type_void &) const { return true; }
    bool operator!=(const type_void &t) const { return !(*this == t); }
  };
  static constexpr type_void TV_VOID;

  ///////////////////////////////////////////////////////////////////////////
  // Union type
  struct type {
    std::variant<
        type_array
      , type_num
    //  , type_enum
      , type_builtin
      , type_struct
      , type_union
      , type_ptr
      , type_vector
      , type_void>   var;

    constexpr type() : var(TV_VOID) { }
//    constexpr type(const type &t) : var(t.var) { }
//    constexpr type(const type &) = default;
              type(type_array t) : var(t) { }
    constexpr type(type_builtin t) : var(t) { }
    constexpr type(type_num t) : var(t) { }
    constexpr type(type_struct t) : var(t) { }
    constexpr type(type_union t) : var(t) { }
    constexpr type(type_ptr t) : var(t) { }
    constexpr type(type_vector t) : var(t) { }

    constexpr size_t size() const {
      if (is<type_array>()) {
        return as<type_array>().size();
      } else if (is<type_num>()) {
        return as<type_num>().size();
      } else if (is<type_builtin>()) {
        return as<type_builtin>().size();
      } else if (is<type_struct>()) {
        return as<type_struct>().size();
      } else if (is<type_union>()) {
        return as<type_union>().size();
      } else if (is<type_ptr>()) {
        return as<type_ptr>().size();
      } else if (is<type_vector>()) {
        return as<type_vector>().size();
      } else if (is<type_void>()) {
        return as<type_void>().size();
      } else {
        throw "unreachable";
      }
    }

    std::string syntax() const;

    // template <typename T>
    // operator const T&() const {return std::get<T>(var);}

    template <typename T>
    constexpr bool is() const noexcept {return std::holds_alternative<T>(var);}

    template <typename T>
    constexpr const T &as() const {return std::get<T>(var);}
  }; // types


  static inline bool operator==(const type &t1,const type &t2) {
    return t1.var == t2.var;
  }
  static inline bool operator!=(const type &t1,const type &t2) {
    return !(t1 == t2);
  }

  /////////////////////////////////////////////////////////////////////////////
  inline std::string type_array::syntax() const {
    std::stringstream ss;
    ss << element_type->syntax() << "[" << length << "]";
    return ss.str();
  }
  inline size_t type_array::size() const {
    return element_type->size() * length;
  }
  inline bool type_array::operator==(const type_array &t) const {
    return element_type == t.element_type && length == t.length;
  }
  /////////////////////////////////////////////////////////////////////////////
  inline bool type_ptr::operator==(const type_ptr &t) const {
    return *element_type == *t.element_type;
  }
  inline std::string type_ptr::syntax() const {
    std::stringstream ss;
    ss << element_type->syntax();
    ss << " *";
    return ss.str();
  }
  /////////////////////////////////////////////////////////////////////////////
  inline size_t type_struct::size() const {
    size_t sum = 0;
    for (size_t i = 0; i < elements_length; i++)
      sum += elements[i]->size();
    return sum;
    // TODO: align up, obey packing and etc...
  }
  /////////////////////////////////////////////////////////////////////////////
  inline size_t type_union::size() const {
    size_t max = 0;
    for (size_t i = 0; i < elements_length; i++)
      max = std::max<size_t>(max,elements[i]->size());
    // TODO: align up?
    return max;
  }
  /////////////////////////////////////////////////////////////////////////////
  inline std::string type_vector::syntax() const {
    std::stringstream ss;
    ss << element_type.syntax() << length;
    return ss.str();
  }
  inline size_t type_vector::size() const {
    return element_type.size() * length;
  }
  inline bool type_vector::operator==(const type_vector &t) const {
    return element_type == t.element_type && length == t.length;
  }
  /////////////////////////////////////////////////////////////////////////////

  const type &VOID();

  // generate: constexpr const type& INT(); etc...
 #define MAKE_TYPE_ACCESSORS(IDENT)\
  const type &IDENT();\
  const type &CAT(IDENT,2)();\
  const type &CAT(IDENT,3)();\
  const type &CAT(IDENT,4)();\
  const type &CAT(IDENT,8)();\
  const type &CAT(IDENT,16)()
  //
  MAKE_TYPE_ACCESSORS(HALF);
  MAKE_TYPE_ACCESSORS(FLOAT);
  MAKE_TYPE_ACCESSORS(DOUBLE);
  //
  MAKE_TYPE_ACCESSORS(CHAR);
  MAKE_TYPE_ACCESSORS(SHORT);
  MAKE_TYPE_ACCESSORS(INT);
  MAKE_TYPE_ACCESSORS(LONG);
  MAKE_TYPE_ACCESSORS(UCHAR);
  MAKE_TYPE_ACCESSORS(USHORT);
  MAKE_TYPE_ACCESSORS(UINT);
  MAKE_TYPE_ACCESSORS(ULONG);

  const type *lookupBuiltinType(std::string name, size_t bytes_per_addr);

  // works for buffer or scalar
  void format(
    std::ostream &os,
    const void *memory,
    size_t buffer_length_in_bytes,
    const type &t);
  void formatBuffer(
    std::ostream &os,
    const void *buffer,
    size_t buffer_length_in_bytes,
    const type &elem_type,
    int elems_per_row);
  void formatBufferElement(
    std::ostream &os,
    const type &t,
    const void *ptr);

} // cls::


#endif