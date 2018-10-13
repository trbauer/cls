#include "types.hpp"
#include "../half.hpp"
#include "../system.hpp"

using namespace cls;



#define DEFINE_PRIM_TYPE_VEC(N,IDENT,SYMBOL,CATEGORY,SIZE) \
  constexpr type_struct TS_ ## IDENT ## N{SYMBOL #N,SIZE,&(T_ ## IDENT),N}; \
  constexpr type T_ ## IDENT ## N{TS_ ## IDENT ## N}; \
  const type_struct & cls:: IDENT ## N () {return TS_ ## IDENT ## N;}

#define DEFINE_PRIM_TYPE(IDENT,SYMBOL,CATEGORY,SIZE) \
  constexpr type_num TN_ ## IDENT{type_num{SYMBOL,type_num::CATEGORY,SIZE}}; \
  constexpr type T_ ## IDENT{TN_ ## IDENT}; \
  const type_num & cls:: IDENT () {return TN_ ## IDENT;} \
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

// constexpr static type FLOAT{type_num{"float",type_num::FLOATING,4}};
// constexpr static type_struct ST_FLOAT2{"float2",sizeof(float),&FLOAT,2};
// constexpr static type FLOAT2{ST_FLOAT2};
// ...


// constexpr static type CHAR{type_num("char",type_num::SIGNED,1)};
// constexpr static type_struct TS2 = type_struct{&CHAR};
// constexpr static type CHAR2{TS2};

const type *cls::lookupPrimtiveType(std::string name)
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

#define MATCH_PRIM_TYPE(ID,SYMBOL) \
  do { \
    if (name == (SYMBOL)) return &(ID); \
    if (name == (SYMBOL "2")) return &(CAT(ID,2)); \
    if (name == (SYMBOL "3")) return &(CAT(ID,3)); \
    if (name == (SYMBOL "4")) return &(CAT(ID,4)); \
    if (name == (SYMBOL "8")) return &(CAT(ID,8)); \
    if (name == (SYMBOL "16")) return &(CAT(ID,16)); \
  } while (0)

  MATCH_PRIM_TYPE(T_HALF,"half");
  MATCH_PRIM_TYPE(T_FLOAT,"float");
  MATCH_PRIM_TYPE(T_DOUBLE,"double");

  MATCH_PRIM_TYPE(T_CHAR,"char");
  MATCH_PRIM_TYPE(T_UINT,"uchar");
  MATCH_PRIM_TYPE(T_SHORT,"short");
  MATCH_PRIM_TYPE(T_USHORT,"ushort");
  MATCH_PRIM_TYPE(T_INT,"int");
  MATCH_PRIM_TYPE(T_UINT,"uint");
  MATCH_PRIM_TYPE(T_LONG,"long");
  MATCH_PRIM_TYPE(T_ULONG,"ulong");

  return nullptr;
}



template <typename T>
static T read_unaligned(const void *buf)
{
  T val;
  memcpy(&val,buf,sizeof(val));
  return val;
}

void formatBufferElement(
  std::ostream &os,
  const type &t,
  const void *ptr)
{
  if (std::holds_alternative<type_num>(t.var)) {
    type_num tn = std::get<type_num>(t.var);
    switch (tn.kind) {
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
        os << std::setw(4) << read_unaligned<int8_t>(ptr);
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
      os << std::hex;
      switch (tn.size()) {
      case 1:
        os << "0x" << std::setw(2) << std::setfill('0') <<
          read_unaligned<uint8_t>(ptr);
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
  // } else if (std::holds_alternative<type_enum>(t.var)) {
    // type_enum te = std::get<type_enum>(t.var);
    // ss << std::setw(12) << read_unaligned<int32_t>(ptr);
  } else if (std::holds_alternative<type_builtin>(t.var)) {
    const type_builtin &tbi = std::get<type_builtin>(t.var);
    if (std::get<type_ptr>(t.var).size() == 4) {
      os << "0x" << std::setw(8) << std::setfill('0') <<
        read_unaligned<uint32_t>(ptr);
    } else {
      os << "0x" << std::setw(16) << std::setfill('0') <<
        read_unaligned<uint64_t>(ptr);
    }
  } else if (std::holds_alternative<type_struct>(t.var)) {
    const type_struct &ts = std::get<type_struct>(t.var);
    os << "{";
    const uint8_t *struct_ptr = (const uint8_t *)ptr;
    for (size_t i = 0; i < ts.elements_length; i++) {
      if (i > 0)
        os << ",";
      formatBufferElement(os, *ts.elements[i], struct_ptr);
      struct_ptr += ts.elements[i]->size();
    }
    os << "}";
  } else if (std::holds_alternative<type_union>(t.var)) {
    const type_union &tu = std::get<type_union>(t.var);
    os << "{";
    for (size_t i = 0; i < tu.elements_length; i++) {
      if (i > 0)
        os << "|";
      formatBufferElement(os, *tu.elements[i], ptr);
    }
    os << "}";
  } else if (std::holds_alternative<type_ptr>(t.var)) {
    os << std::hex;
    if (std::get<type_ptr>(t.var).size() == 4) {
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

void cls::formatBuffer(
  std::ostream &os,
  const void *buffer,
  size_t buffer_length_in_bytes,
  const type &buffer_type)
{
  const size_t max_cols = sys::get_terminal_width();
  size_t curr_col = 1;
  const uint8_t *base = (const uint8_t *)buffer;
  const uint8_t *curr = base;

  auto startNewLine = [&] {
    os << std::setw(5) << std::setfill('0') << std::hex << (curr - base);
    os << ": ";
    curr_col = 7;
  };
  auto fmtElem = [&] (const type &t) {
    std::stringstream ss;
    formatBufferElement(ss, t, curr);
    if (curr_col + ss.tellp() > max_cols) {
      startNewLine();
    }
    os << ss.str();
    curr += t.size();
  };

  if (std::holds_alternative<type_ptr>(buffer_type.var)) {
    while (curr < base + buffer_length_in_bytes) {
      if (curr_col > max_cols) {
        os << "\n";
        startNewLine();
      }
      fmtElem(*std::get<type_ptr>(buffer_type.var).element_type);
    }
  } else {
    fmtElem(buffer_type);
  }
}