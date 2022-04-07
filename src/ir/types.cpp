#include "types.hpp"
#include "../half.hpp"
#include "../system.hpp"

#include <cmath>

using namespace cls;



#define DEFINE_PRIM_TYPE_VEC(N,IDENT,ELEM_TYPE) \
  constexpr type_vector TV_ ## IDENT ## N{ELEM_TYPE,N}; \
  constexpr type T_ ## IDENT ## N{TV_ ## IDENT ## N}; \
  const type & cls:: IDENT ## N () {return T_ ## IDENT ## N;}

#define DEFINE_PRIM_TYPE(IDENT,SYMBOL,CATEGORY,SIZE) \
  constexpr type_num TN_ ## IDENT{SYMBOL,type_num::CATEGORY,SIZE}; \
  constexpr type T_ ## IDENT{TN_ ## IDENT}; \
  const type & cls:: IDENT () {return T_ ## IDENT;} \
  DEFINE_PRIM_TYPE_VEC( 2,IDENT,TN_ ## IDENT) \
  DEFINE_PRIM_TYPE_VEC( 3,IDENT,TN_ ## IDENT) \
  DEFINE_PRIM_TYPE_VEC( 4,IDENT,TN_ ## IDENT) \
  DEFINE_PRIM_TYPE_VEC( 8,IDENT,TN_ ## IDENT) \
  DEFINE_PRIM_TYPE_VEC(16,IDENT,TN_ ## IDENT)

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

#define MKBUILTIN_TYPES(KEY) \
  constexpr type_builtin TBI_ ## KEY ## _32b(type_builtin:: KEY, 4); \
  constexpr type T_ ## KEY ## _32b(TBI_ ## KEY ## _32b); \
  constexpr type_builtin TBI_ ## KEY ## _64b(type_builtin:: KEY, 8); \
  constexpr type T_ ## KEY ## _64b(TBI_ ## KEY ## _64b); \

MKBUILTIN_TYPES(IMAGE1D)
MKBUILTIN_TYPES(IMAGE1D_ARRAY);
MKBUILTIN_TYPES(IMAGE1D_BUFFER);
//
MKBUILTIN_TYPES(IMAGE2D);
MKBUILTIN_TYPES(IMAGE2D_ARRAY);
MKBUILTIN_TYPES(IMAGE2D_ARRAY_DEPTH);
MKBUILTIN_TYPES(IMAGE2D_ARRAY_MSAA);
MKBUILTIN_TYPES(IMAGE2D_ARRAY_MSAA_DEPTH);
MKBUILTIN_TYPES(IMAGE2D_DEPTH);
MKBUILTIN_TYPES(IMAGE2D_MSAA);
MKBUILTIN_TYPES(IMAGE2D_MSAA_DEPTH);
//
MKBUILTIN_TYPES(IMAGE3D);
//
MKBUILTIN_TYPES(SAMPLER);
MKBUILTIN_TYPES(QUEUE);
MKBUILTIN_TYPES(NDRANGE);
MKBUILTIN_TYPES(CLK_EVENT);
MKBUILTIN_TYPES(RESERVE_ID);
MKBUILTIN_TYPES(EVENT);
MKBUILTIN_TYPES(CL_MEM_FENCE_FLAGS);

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
  if (is<type_array>()) {
    return as<type_array>().syntax();
  } else if (is<type_builtin>()) {
    return as<type_builtin>().syntax();
  } else if (is<type_num>()) {
    return as<type_num>().syntax();
  } else if (is<type_struct>()) {
    return as<type_struct>().syntax();
  } else if (is<type_ptr>()) {
    return as<type_ptr>().syntax();
  } else if (is<type_union>()) {
    return as<type_union>().syntax();
  } else if (is<type_vector>()) {
    return as<type_vector>().syntax();
  } else if (is<type_void>()) {
    return as<type_void>().syntax();
  } else {
    throw "unreachable";
  }
}

static type T_VOID {};

const type &cls::VOID() {
  return T_VOID;
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
  if (name == "void") {
    return &cls::VOID();
  } else if (name == "signed") {
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

  BUILTIN_CASE("image1d_t", IMAGE1D);
  BUILTIN_CASE("image1d_array_t", IMAGE1D_ARRAY);
  BUILTIN_CASE("image1d_buffer_t", IMAGE1D_BUFFER);
  BUILTIN_CASE("image2d_t", IMAGE2D);
  BUILTIN_CASE("image2d_array_t", IMAGE2D_ARRAY);
  BUILTIN_CASE("image2d_depth_t", IMAGE2D_ARRAY_DEPTH);
  BUILTIN_CASE("image2d_msaa_t", IMAGE2D_ARRAY_MSAA);
  BUILTIN_CASE("image2d_array_msaa_depth_t", IMAGE2D_ARRAY_MSAA_DEPTH);
  BUILTIN_CASE("image2d_depth_t", IMAGE2D_DEPTH);
  BUILTIN_CASE("image2d_msaa_t", IMAGE2D_MSAA);
  BUILTIN_CASE("image2d_msaa_depth_t", IMAGE2D_MSAA_DEPTH);
  BUILTIN_CASE("image3d_t",IMAGE3D);
  BUILTIN_CASE("sampler_t", SAMPLER);
  BUILTIN_CASE("queue_t", QUEUE);
  BUILTIN_CASE("clk_event_t", CLK_EVENT);
  BUILTIN_CASE("reserve_id_t", RESERVE_ID);
  BUILTIN_CASE("event_t", EVENT);
  BUILTIN_CASE("l_mem_fence_flags", CL_MEM_FENCE_FLAGS);
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

  // Each new line starts with the buffer's byte address there
  //   00F0: ....
  // Compute the number of digits for that column based on the buffer's
  // overall size so that it's fixed width throughout the listing.
  const int hex_digits_for_addrs =
    std::max<int>(4,
      (int)std::ceil(std::log(buffer_length_in_bytes + 1)/std::log(16)));
  auto startNewLine = [&] {
    auto addr = text::fmt_hex(curr - base, hex_digits_for_addrs);
    os << addr << ":";
    curr_col = addr.size() + 1;
    elems_on_row = 0;
  };

  auto fmtElem = [&] () {
    std::stringstream ss;
    ss << "  ";
    formatBufferElement(ss, elem_type, curr);
    if (elems_per_row > 0 && elems_on_row == elems_per_row ||
      (elems_per_row <= 0) && is_os_tty && curr_col + ss.tellp() >= max_cols)
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
  memcpy(&val, buf, sizeof(val));
  return val;
}

static const int FLTPREC = 5;
static const int DBLPREC = 8;

static bool parsesBackIdentically(float x)
{
  std::stringstream ss;
  ss << std::fixed << std::setprecision(FLTPREC) << x;
  try {
    float y = std::stof(ss.str());
    return x == y;
  } catch (...) {
  }
  return false;
}
static bool parsesBackIdentically(double x)
{
  std::stringstream ss;
  ss << std::fixed << std::setprecision(DBLPREC) << x;
  try {
    double y = std::stod(ss.str());
    return x == y;
  } catch (...) {
  }
  return false;
}

static void emitFloatBits(std::ostream &os, uint64_t bits, int e, int m)
{
  os << "... (";
  if ((1ull << (e + m)) & bits) {
    os << '1';
  } else {
    os << '0';
  }
  os << '`';
  for (int i = e + m - 1; i >= m; --i) {
    if ((1ull << i) & bits) {
      os << '1';
    } else {
      os << '0';
    }
  }
  os << '`';
  for (int i = m - 1; i >= 0; --i) {
    if ((1ull << i) & bits) {
      os << '1';
    } else {
      os << '0';
    }
  }
  os << ')';
}

void cls::formatBufferElementExt(
  std::ostream &os,
  const type &t,
  const void *ptr)
{
  if (t.is<type_num>()) {
    const type_num &tn = t.as<type_num>();
    bool emitted_exact = false;
    uint64_t bits = 0;
    int e = 0, m = 0;
    switch (tn.skind) {
    case type_num::FLOATING:
      switch (tn.size()) {
      case 2:
      case 4:
      {
        float x;
        if (tn.size() == 2) {
          x = (float)read_unaligned<half>(ptr);
          bits = read_unaligned<uint16_t>(ptr);
          e = 5; m = 8;
        } else {
          x = (float)read_unaligned<float>(ptr);
          bits = read_unaligned<uint32_t>(ptr);
          e = 8; m = 23;
        }
        emitted_exact = parsesBackIdentically(x);
        os << std::setw(8) << std::fixed << std::setprecision(FLTPREC) << x;
        break;
      }
      case 8:
        os << std::setw(12) << std::fixed << std::setprecision(DBLPREC) <<
          read_unaligned<double>(ptr);
        emitted_exact = parsesBackIdentically(read_unaligned<double>(ptr));
        bits = read_unaligned<uint64_t>(ptr);
        e = 11; m = 52;
        break;
      }
      break;
    default:
      formatBufferElement(os, t, ptr);
    }
    if (!emitted_exact)
      emitFloatBits(os, bits, e, m);
  } else {
    formatBufferElement(os, t, ptr);
  }
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
  } else if (t.is<type_vector>()) {
    const type_vector &ts = t.as<type_vector>();
    os << "(";
    const uint8_t *struct_ptr = (const uint8_t *)ptr;
    for (size_t i = 0; i < ts.length; i++) {
      if (i > 0)
        os << ",";
      formatBufferElement(os, ts.element_type, struct_ptr);
      struct_ptr += ts.element_type.size();
    }
    os << ")";
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
