#include "types.hpp"
#include "../half.hpp"
#include "../system.hpp"

using namespace cls;



#define DEFINE_PRIM_TYPE_VEC(N,IDENT,SYMBOL,CATEGORY,SIZE) \
  constexpr type_struct TS_ ## IDENT ## N{SYMBOL #N,SIZE,&(T_ ## IDENT),N}; \
  constexpr type T_ ## IDENT ## N{TS_ ## IDENT ## N}; \
  const type & cls:: IDENT ## N () {return T_ ## IDENT ## N;}

#define DEFINE_PRIM_TYPE(IDENT,SYMBOL,CATEGORY,SIZE) \
  constexpr type_num TN_ ## IDENT{type_num{SYMBOL,type_num::CATEGORY,SIZE}}; \
  constexpr type T_ ## IDENT{TN_ ## IDENT}; \
  const type & cls:: IDENT () {return T_ ## IDENT;} \
  DEFINE_PRIM_TYPE_VEC(2,IDENT,SYMBOL,CATEGORY,SIZE) \
  DEFINE_PRIM_TYPE_VEC(3,IDENT,SYMBOL,CATEGORY,SIZE) \
  DEFINE_PRIM_TYPE_VEC(4,IDENT,SYMBOL,CATEGORY,SIZE) \
  DEFINE_PRIM_TYPE_VEC(8,IDENT,SYMBOL,CATEGORY,SIZE) \
  DEFINE_PRIM_TYPE_VEC(16,IDENT,SYMBOL,CATEGORY,SIZE)

DEFINE_PRIM_TYPE(HALF,   "half",   FLOATING, 2);
DEFINE_PRIM_TYPE(FLOAT,  "float",  FLOATING, 4);
DEFINE_PRIM_TYPE(DOUBLE, "double", FLOATING, 8);

DEFINE_PRIM_TYPE( CHAR,  "char",     SIGNED, 1);
DEFINE_PRIM_TYPE(UCHAR,  "uchar",  UNSIGNED, 1);
DEFINE_PRIM_TYPE( SHORT, "short",    SIGNED, 2);
DEFINE_PRIM_TYPE(USHORT, "ushort", UNSIGNED, 2);
DEFINE_PRIM_TYPE( INT,   "int",      SIGNED, 4);
DEFINE_PRIM_TYPE(UINT,   "uint",   UNSIGNED, 4);
DEFINE_PRIM_TYPE( LONG,  "long",     SIGNED, 8);
DEFINE_PRIM_TYPE(ULONG,  "ulong",  UNSIGNED, 8);

constexpr type_builtin TBI_IMAGE1D_32b(type_builtin::IMAGE1D,4);
const type T_IMAGE1D_32b(TBI_IMAGE1D_32b);
constexpr type_builtin TBI_IMAGE1D_64b(type_builtin::IMAGE1D,8);
const type T_IMAGE1D_64b(TBI_IMAGE1D_64b);
//
constexpr type_builtin TBI_IMAGE2D_32b(type_builtin::IMAGE2D,4);
const type T_IMAGE2D_32b(TBI_IMAGE2D_32b);
constexpr type_builtin TBI_IMAGE2D_64b(type_builtin::IMAGE2D,8);
const type T_IMAGE2D_64b(TBI_IMAGE2D_64b);
//
constexpr type_builtin TBI_IMAGE3D_32b(type_builtin::IMAGE3D,4);
const type T_IMAGE3D_32b(TBI_IMAGE3D_32b);
constexpr type_builtin TBI_IMAGE3D_64b(type_builtin::IMAGE3D,8);
const type T_IMAGE3D_64b(TBI_IMAGE3D_64b);
//
constexpr type_builtin TBI_SAMPLER_32b(type_builtin::SAMPLER,4);
const type T_SAMPLER_32b(TBI_SAMPLER_32b);
constexpr type_builtin TBI_SAMPLER_64b(type_builtin::SAMPLER,8);
const type T_SAMPLER_64b(TBI_SAMPLER_64b);

// TODO: others ...

/*
DEFINE_PRIM_TYPE(CHAR,"char",SIGNED,1);
constexpr static type_struct ST_CHAR2{"char2",sizeof(char),&CHAR,2};
constexpr static type CHAR2{ST_CHAR2};
DEFINE_PRIM_TYPE(INT,"int",SIGNED,4);

template <class T, class... Types>
constexpr bool holds(const std::variant<Types...>& v) noexcept {
  return std::holds_alternative<>(v);
}
*/

std::string type::syntax() const {
  if (is<type_void>()) {
    return as<type_void>().syntax();
  } else if (is<type_num>()) {
    return as<type_num>().syntax();
  } else if (is<type_builtin>()) {
    return as<type_builtin>().syntax();
  } else if (is<type_struct>()) {
    return as<type_struct>().syntax();
  } else if (is<type_union>()) {
    return as<type_union>().syntax();
  } else if (is<type_ptr>()) {
    return as<type_ptr>().syntax();
  } else {
    throw "unreachable";
  }
}
// constexpr static type FLOAT{type_num{"float",type_num::FLOATING,4}};
// constexpr static type_struct ST_FLOAT2{"float2",sizeof(float),&FLOAT,2};
// constexpr static type FLOAT2{ST_FLOAT2};
// ...

// constexpr static type CHAR{type_num("char",type_num::SIGNED,1)};
// constexpr static type_struct TS2 = type_struct{&CHAR};
// constexpr static type CHAR2{TS2};
const type *cls::lookupBuiltinType(std::string name, size_t bytes_per_addr)
{
  // normalize primitive names (e.g. unsigned int -> uint)
  if (name == "signed") {
    name = "int";
  } else if (name == "unsigned") {
    name = "uint";
  } else {
    auto pos = name.find(' ');
    if (pos != std::string::npos) {
      auto tk1 = name.substr(0,pos);
      auto tk2 = name.substr(pos);
      if (tk2 == "char" || tk2 == "short" || tk2 == "int" || tk2 == "long") {
        if (tk1 == "signed") {
          name = tk2;
        } else if (tk1 == "unsigned") {
          name = "u" + tk2; //
        }
      }
    }
  }
  //
#define MATCH_PRIM_TYPE(ID,SYMBOL) \
  do { \
    if (name == (SYMBOL)) return &(ID); \
    if (name == (SYMBOL "2")) return &(CAT(ID,2)); \
    if (name == (SYMBOL "3")) return &(CAT(ID,3)); \
    if (name == (SYMBOL "4")) return &(CAT(ID,4)); \
    if (name == (SYMBOL "8")) return &(CAT(ID,8)); \
    if (name == (SYMBOL "16")) return &(CAT(ID,16)); \
  } while (0)
  //
  MATCH_PRIM_TYPE(T_HALF,"half");
  MATCH_PRIM_TYPE(T_FLOAT,"float");
  MATCH_PRIM_TYPE(T_DOUBLE,"double");
  //
  MATCH_PRIM_TYPE(T_CHAR,"char");
  MATCH_PRIM_TYPE(T_UCHAR,"uchar");
  MATCH_PRIM_TYPE(T_SHORT,"short");
  MATCH_PRIM_TYPE(T_USHORT,"ushort");
  MATCH_PRIM_TYPE(T_INT,"int");
  MATCH_PRIM_TYPE(T_UINT,"uint");
  MATCH_PRIM_TYPE(T_LONG,"long");
  MATCH_PRIM_TYPE(T_ULONG,"ulong");
  //
  // extra types that alias to something else
  if (name == "size_t" || name == "uintptr_t")
    return bytes_per_addr == 4 ? &UINT() : &ULONG();
  else if (name == "intptr_t" || name == "ptrdiff_t")
    return bytes_per_addr == 4 ? &INT() : &LONG();
  //
  // image and other built-in types
#define BUILTIN_CASE(LEXEME,ID) \
  if (name == (LEXEME)) \
    return bytes_per_addr == 4 ? &T_ ## ID ## _32b : &T_ ## ID ## _64b

  BUILTIN_CASE("image1d_t",IMAGE1D);
  BUILTIN_CASE("image2d_t",IMAGE2D);
  BUILTIN_CASE("image3d_t",IMAGE3D);
  BUILTIN_CASE("sampler_t",SAMPLER);
#undef BUILTIN_CASE

  return nullptr;
}


void cls::format(
  std::ostream &os,
  const void *memory,
  size_t memory_size,
  const type &memory_type)
{
  if (memory_type.is<type_ptr>()) {
    formatBuffer(
      os,
      memory,
      memory_size,
      *memory_type.as<type_ptr>().element_type,
      0);
  } else {
    formatBufferElement(os, memory_type, memory);
  }
}

void cls::formatBuffer(
  std::ostream &os,
  const void *buffer,
  size_t buffer_length_in_bytes,
  const type &elem_type,
  int elems_per_row)
{
  const size_t max_cols = sys::get_terminal_width();
  bool is_os_tty = sys::is_tty(os);
  const uint8_t *base = (const uint8_t *)buffer;
  const uint8_t *curr = base;

  size_t curr_col = 0;
  size_t elems_on_row = 0;

  auto startNewLine = [&] {
    os << std::setw(5) << std::setfill('0') << std::hex << (curr - base);
    os << ":";
    curr_col = 6;
    elems_on_row = 0;
  };

  auto fmtElem = [&] () {
    std::stringstream ss;
    ss << "  ";
    formatBufferElement(ss, elem_type, curr);
    if (elems_per_row > 0 && elems_on_row == elems_per_row ||
      is_os_tty && curr_col + ss.tellp() >= max_cols)
    {
      os << "\n";
      startNewLine();
    }

    os << ss.str();
    curr += elem_type.size();

    curr_col += (size_t)ss.tellp();
    elems_on_row++;
  };
  if (buffer_length_in_bytes > 0)
    startNewLine();
  while (curr < base + buffer_length_in_bytes)
    fmtElem();
}

template <typename T>
static T read_unaligned(const void *buf)
{
  T val;
  memcpy(&val,buf,sizeof(val));
  return val;
}

void cls::formatBufferElement(
  std::ostream &os,
  const type &t,
  const void *ptr)
{
  if (t.is<type_num>()) {
    const type_num &tn = t.as<type_num>();
    switch (tn.skind) {
    case type_num::FLOATING:
      switch (tn.size()) {
      case 2:
        os << std::setw(8) << std::fixed << std::setprecision(3) <<
          (float)(read_unaligned<half>(ptr));
        break;
      case 4:
        // std::numeric_limits<float>::digits10()
        os << std::setw(10) << std::fixed << std::setprecision(5) <<
          read_unaligned<float>(ptr);
        break;
      case 8:
        os << std::setw(12) << std::fixed << std::setprecision(8) <<
          read_unaligned<double>(ptr);
        break;
      }
      break;
    case type_num::SIGNED:
      os << std::dec;
      switch (tn.size()) {
      case 1:
        os << std::setw(4) << (int)read_unaligned<int8_t>(ptr);
        break;
      case 2:
        os << std::setw(6) << read_unaligned<int16_t>(ptr);
        break;
      case 4:
        os << std::setw(12) << read_unaligned<int32_t>(ptr);
        break;
      case 8:
        os << std::setw(22) << read_unaligned<int64_t>(ptr);
        break;
      }
      break;
    case type_num::UNSIGNED:
      os << std::hex << std::uppercase;
      switch (tn.size()) {
      case 1:
        os << "0x" << std::setw(2) << std::setfill('0') <<
          (unsigned)read_unaligned<uint8_t>(ptr);
        break;
      case 2:
        os << "0x" << std::setw(4) << std::setfill('0') <<
          read_unaligned<uint16_t>(ptr);
        break;
      case 4:
        os << "0x" << std::setw(8) << std::setfill('0') <<
          read_unaligned<uint32_t>(ptr);
        break;
      case 8:
        os << "0x" << std::setw(16) << std::setfill('0') <<
          read_unaligned<uint64_t>(ptr);
        break;
      }
      break;
    }
  // } else if (t.holds<type_enum>()) {
    // type_enum te = t.as<type_enum>();
    // ss << std::setw(12) << read_unaligned<int32_t>(ptr);
  } else if (t.is<type_builtin>()) {
    os << std::hex << std::uppercase;
    const type_builtin &tbi = t.as<type_builtin>();
    if (std::get<type_ptr>(t.var).size() == 4) {
      os << "0x" << std::setw(8) << std::setfill('0') <<
        read_unaligned<uint32_t>(ptr);
    } else {
      os << "0x" << std::setw(16) << std::setfill('0') <<
        read_unaligned<uint64_t>(ptr);
    }
  } else if (t.is<type_struct>()) {
    const type_struct &ts = t.as<type_struct>();
    os << "{";
    const uint8_t *struct_ptr = (const uint8_t *)ptr;
    for (size_t i = 0; i < ts.elements_length; i++) {
      if (i > 0)
        os << ",";
      formatBufferElement(os, *ts.elements[i], struct_ptr);
      struct_ptr += ts.elements[i]->size();
    }
    os << "}";
  } else if (t.is<type_union>()) {
    const type_union &tu = t.as<type_union>();
    os << "{";
    for (size_t i = 0; i < tu.elements_length; i++) {
      if (i > 0)
        os << "#";
      formatBufferElement(os, *tu.elements[i], ptr);
    }
    os << "}";
  } else if (t.is<type_ptr>()) {
    os << std::hex << std::uppercase;
    if (t.as<type_ptr>().size() == 4) {
      os << "0x" << std::setw(8) << std::setfill('0') <<
        read_unaligned<uint32_t>(ptr);
    } else {
      os << "0x" << std::setw(16) << std::setfill('0') <<
        read_unaligned<uint64_t>(ptr);
    }
  } else {
    os << "formatBufferElement<" << t.syntax() << ">?";
  }
}
