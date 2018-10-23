#include "cls_ir.hpp"
#include "../system.hpp"
#include "../text.hpp"

#include <cfenv>
#include <cmath>
#include <numeric>
#include <tuple>

using namespace cls;
using namespace text::spans;

static format_opts::color_span make_colored(
  const format_opts &fopts,
  const char *color,
  std::string s)
{
  if (fopts.opts & format_opts::USE_COLOR) {
    return format_opts::color_span(color, s);
  } else {
    return format_opts::color_span(nullptr, s);
  }
}

std::string spec::name() const
{
  switch (kind) {
  case spec::STATEMENT_LIST_SPEC:  return "statement list";
  case spec::STATEMENT_SPEC:
    switch (((const statement_spec *)this)->kind) {
    case statement_spec::DISPATCH: return "dispatch";
    case statement_spec::LET:      return "let";
    case statement_spec::BARRIER:  return "barrier";
    case statement_spec::DIFF:     return "diff";
    case statement_spec::SAVE:     return "save";
    case statement_spec::PRINT:    return "print";
    default:                       return "unknown statement";
    }
  case spec::INIT_SPEC:
    switch (((const init_spec *)this)->kind) {
    case init_spec::IS_INT: return "integral initializer";
    case init_spec::IS_FLT: return "floating-point initializer";
    case init_spec::IS_REC: return "record initializer";
    case init_spec::IS_BIV: return "built-in variable initializer";
    case init_spec::IS_SYM: return "symbol initializer";
    case init_spec::IS_BEX: return "binary expression initializer";
    case init_spec::IS_UEX: return "unary expression initializer";
    case init_spec::IS_FIL: return "file initializer";
    case init_spec::IS_RND: return "random value initializer";
    case init_spec::IS_SEQ: return "sequence initializer";
    case init_spec::IS_MEM: return "memory initializer";
    default:                return "unknown initializer";
    }
  case spec::DEVICE_SPEC:  return "device";
  case spec::PROGRAM_SPEC: return "program";
  case spec::KERNEL_SPEC:  return "kernel";
  default:                 return "unknown syntax";
  }
}

format_opts::color_span format_opts::error(std::string s) const {
  return make_colored(*this,text::ANSI_DRED.esc,s);
}
format_opts::color_span format_opts::keyword(std::string s) const {
  return make_colored(*this,text::ANSI_BLUE.esc,s);
}
format_opts::color_span format_opts::let_var(std::string s) const {
  return make_colored(*this,text::ANSI_DYELLOW.esc,s);
}
format_opts::color_span format_opts::str_lit(std::string s) const {
  return make_colored(*this,text::ANSI_DCYAN.esc,s);
}

template <typename S>
static void fmt(std::ostream &os, format_opts fopts, const S *s) {
  if (s) {
    s->str(os,fopts);
  } else {
    os << fopts.error("<nullptr>");
  }
};

void spec::str(std::ostream &os, format_opts fopts) const {
  switch (kind) {
  case spec::STATEMENT_SPEC: ((const statement_spec*)this)->str(os,fopts); break;
  case spec::STATEMENT_LIST_SPEC: ((const statement_list_spec*)this)->str(os,fopts); break;
  //
  case spec::INIT_SPEC: ((const init_spec*)this)->str(os,fopts); break;
  case spec::DEVICE_SPEC: ((const device_spec*)this)->str(os,fopts); break;
  case spec::PROGRAM_SPEC: ((const program_spec*)this)->str(os,fopts); break;
  case spec::KERNEL_SPEC: ((const kernel_spec*)this)->str(os,fopts); break;
  }
}

std::string spec::str() const
{
  std::stringstream ss;
  str(ss, format_opts());
  return ss.str();
}


void statement_list_spec::str(std::ostream &os, format_opts fopts) const {
  // We do this in two passes, first to a string stream to see if the thing
  // will fit in 80 columns with ';' delims.  If so we repeat the output to
  // the requested output stream so that they can get their TTY behavior
  // right etc...
  auto formatTo =
    [&] (std::ostream &oss, const char *delim) {
      bool first = true;
      for (auto &s : statements) {
        if (first) first = false; else oss << delim;
        s->str(oss,fopts);
      }
    };

  auto formatAsLines = [&]() {
    std::string delim = "\n";
    for (int i = 0; i < indent; i++) {
      delim += "  ";
    }
    formatTo(os, delim.c_str());
  };

  if (fopts.opts & format_opts::NO_SEMI_STATEMENT_LISTS) {
    formatAsLines();
  } else {
    std::stringstream ss;
    formatTo(ss, "; ");
    if (ss.tellp() <= 80) {
      formatTo(os, "; ");
    } else {
      formatAsLines();
    }
  }
}

void statement_spec::str(std::ostream &os, format_opts fopts) const {
  switch (kind) {
  case DISPATCH: ((const dispatch_spec*)this)->str(os,fopts); break;
  case LET:      ((const let_spec     *)this)->str(os,fopts); break;
  case BARRIER:  ((const barrier_spec *)this)->str(os,fopts); break;
  case DIFF:     ((const diff_spec    *)this)->str(os,fopts); break;
  case SAVE:     ((const save_spec    *)this)->str(os,fopts); break;
  case PRINT:    ((const print_spec   *)this)->str(os,fopts); break;
  default:       os << "statement_spec???"; break;
  }
}

void device_spec::setSource(std::string name) {
  by_name_value = name;
  kind = kind::BY_NAME;
}
void device_spec::setSource(int index) {
  by_index_value = index;
  kind = kind::BY_INDEX;
}
void device_spec::str(std::ostream &os, format_opts fopts) const {
  switch (kind) {
  case device_spec::BY_DEFAULT: return;
  case device_spec::BY_INDEX: os << "#" << by_index_value; return;
  case device_spec::BY_NAME: os << "#" << by_name_value; return;
  default: os << "device_spec??"; break;
  }
}

void program_spec::str(std::ostream &os, format_opts fopts) const {
  device.str(os, fopts);
  os << "`";
  os << path;
  if (!build_options.empty())
    os << "[" << build_options << "]";
}

void init_spec_mem::str(std::ostream &os, format_opts fopts) const {
  fmt(os, fopts, root);
  os << ":";
  if (dimension) {
    os << "["; dimension->str(os,fopts); os << "]";
  }
  if (access_properties & init_spec_mem::INIT_SPEC_MEM_READ)
    os << 'r';
  if (access_properties & init_spec_mem::INIT_SPEC_MEM_WRITE)
    os << 'w';
//  if (access_properties & init_spec_memory::INIT_SPEC_MEM_DIRECT)
//    os << 'd'; // direct access (SVM)
  // SPECIFY: do we allow for a default
  if (transfer_properties == init_spec_mem::TX_MAP)
    os << 'm';
  if (transfer_properties == init_spec_mem::TX_COPY)
    os << 'c';
  if (transfer_properties == init_spec_mem::TX_SVM_COARSE)
    os << 's';
  if (transfer_properties == init_spec_mem::TX_SVM_FINE)
    os << 's' << 'f';

}

void init_spec::str(std::ostream &os, format_opts fopts) const
{
  switch (kind) {
  case IS_SYM: fmt(os, fopts, (const init_spec_symbol        *)this); break;
  case IS_INT: fmt(os, fopts, (const init_spec_int           *)this); break;
  case IS_FLT: fmt(os, fopts, (const init_spec_float         *)this); break;
  case IS_BIV: fmt(os, fopts, (const init_spec_builtin       *)this); break;
  case IS_REC: fmt(os, fopts, (const init_spec_record        *)this); break;
  case IS_BEX: fmt(os, fopts, (const init_spec_bex           *)this); break;
  case IS_UEX: fmt(os, fopts, (const init_spec_uex           *)this); break;
  case IS_FIL: fmt(os, fopts, (const init_spec_file          *)this); break;
  case IS_RND: fmt(os, fopts, (const init_spec_rng           *)this); break;
  case IS_SEQ: fmt(os, fopts, (const init_spec_seq           *)this); break;
  case IS_MEM: fmt(os, fopts, (const init_spec_mem        *)this); break;
  default: os << "init_spec?"; break;
  }
}

void init_spec_builtin::str(std::ostream &os, format_opts fopts) const {
  os << syntax_for(kind);
}

const char *init_spec_builtin::syntax_for(biv_kind kind)
{
  switch (kind) {
  case BIV_GX:   return "g.x";
  case BIV_GY:   return "g.y";
  case BIV_GZ:   return "g.z";
  // case BIV_GXY:  return "g.xy"; // use g.x*g.y
  // case BIV_GXYZ: return "g.xyz";
  case BIV_LX:   return "l.x";
  case BIV_LY:   return "l.y";
  case BIV_LZ:   return "l.z";
  // case BIV_LXY:  return "l.xy";
  // case BIV_LXYZ: return "l.xyz";
  default:       return "?biv?";
  }
}

void init_spec_record::str(std::ostream &os, format_opts fopts) const {
  os << "{";
  bool first = true;
  for (const auto *c : children) {
    if (first)
      first = false;
    else
      os << ",";
    c->str(os,fopts);
  }
  os << "}";
}


#if 0
static
val apply(fatal_handler *,const val &vl, const val &vr)
{
  if (vl.is_float() || vr.is_float()) {
    return F1(vl.as<double>(),vr.as<double>());
  } else if (vl.is_signed() || vr.is_signed()) {
    return F2(vl.as<int64_t>(),vr.as<int64_t>());
  } else {
    return F3(vl.as<uint64_t>(),vr.as<uint64_t>());
  }
}
template <
  double (*FD)(double,double),
  int64_t (*FS)(int64_t,int64_t),
  uint64_t (*FU)(uint64_t,uint64_t)>
static
val apply(fatal_handler *,const val &vl, const val &vr)
{
  return val(0);
}

#define FSU_FUNC(ID) apply<ID<double>,ID<int64_t>,ID<uint64_t>>

template <
  double (*FD)(double,double)>
static
val applyX(fatal_handler *,const val &vl, const val &vr)
{
  return val(0);
}

template <double (*F)(double,double)>
static void apply() {
  F(1.0,2.0);
}
template <const double &(*F)(const double &,const double &)>
static void applyRef() {
  F(1.0,2.0);
}
template <double (*F)(std::initializer_list<double> il)>
static void applyInit() {
  F(1.0,2.0);
}

double minD(double,double) {return 0.0;}
void test()
{
  auto x1 = apply<minD>();
  // auto x2 = apply<std::min<double>>(); // FAILS
  applyRef<std::min<const double &>>(); // FAILS
  applyInit<std::min<double>>();
//  std::min<double>(x);
}

#endif
// I couldn't get this to work with templates :(
// I sort of got it to work, but the template deduction and function
// conversion is too complex
#define BIN_OP(F) \
  static val apply_ ## F (fatal_handler *,const loc &,const val &vl,const val &vr) {\
    if (vl.is_float() || vr.is_float()) {\
      return std:: F (vl.as<double>(),vr.as<double>());\
    } else if (vl.is_signed() || vr.is_signed()) {\
      return std:: F (vl.as<int64_t>(),vr.as<int64_t>());\
    } else {\
      return std:: F (vl.as<uint64_t>(),vr.as<uint64_t>());\
    }\
  }
#define BIN_OP_INFIX(SYM,OP) \
  static val apply_ ## SYM (fatal_handler *,const loc &,const val &vl,const val &vr) {\
    if (vl.is_float() || vr.is_float()) {\
      return (vl.as<double>() OP vr.as<double>());\
    } else if (vl.is_signed() || vr.is_signed()) {\
      return (vl.as<int64_t>() OP vr.as<int64_t>());\
    } else {\
      return (vl.as<uint64_t>() OP vr.as<uint64_t>());\
    }\
  }

#define BIN_OP_INTEGRAL(F) \
  static val apply_ ## F (fatal_handler *fh,const loc &at,const val &vl,const val &vr) {\
    if (vl.is_float() || vr.is_float()) {\
      fh->fatalAt(at,"function/operator requires integral arguments");\
      return val(0);\
    } else if (vl.is_signed() || vr.is_signed()) {\
      return std:: F (vl.as<int64_t>(),vr.as<int64_t>());\
    } else {\
      return std:: F (vl.as<uint64_t>(),vr.as<uint64_t>());\
    }\
  }
#define BIN_OP_INFIX_INTEGRAL(SYM,OP) \
  static val apply_ ## SYM (fatal_handler *fh,const loc &at,const val &vl,const val &vr) {\
    if (vl.is_float() || vr.is_float()) {\
      fh->fatalAt(at,"function/operator requires integral arguments");\
      return val(0);\
    } else if (vl.is_signed() || vr.is_signed()) {\
      return (vl.as<int64_t>() OP vr.as<int64_t>());\
    } else {\
      return (vl.as<uint64_t>() OP vr.as<uint64_t>());\
    }\
  }

static val apply_pow(fatal_handler *,const loc &,const val &vl, const val &vr)
{
  if (vl.is_float()) {
    if (vr.is_float()) {
      return std::pow(vl.as<double>(),vr.as<double>());
    } else {
      return std::pow(vl.as<double>(),vr.as<int64_t>());
    }
  } else if (vr.is_signed()) {
    return std::pow(vl.as<int64_t>(),vr.as<int64_t>());
  } else {
    return std::pow(vl.as<uint64_t>(),vr.as<uint64_t>());
  }
}
static val apply_div(fatal_handler *fh,const loc &at,const val &vl, const val &vr)
{
  if (vl.is_float() || vr.is_float()) {
    return vl.as<double>() / vr.as<double>();
  } else if (vr.s64 == 0) {
    fh->fatalAt(at,"division by 0");
  } else if (vl.is_signed() || vr.is_signed()) {
    return vl.as<int64_t>() / vr.as<int64_t>();
  } else {
    return vl.as<uint64_t>() / vr.as<uint64_t>();
  }
}
static val apply_mod(fatal_handler *fh,const loc &at,const val &vl, const val &vr)
{
  if (vl.is_float() || vr.is_float()) {
    return std::fmod(vl.as<double>(),vr.as<double>());
  } else if (vr.s64 == 0) {
    fh->fatalAt(at,"division by 0");
  } else if (vl.is_signed() || vr.is_signed()) {
    return vl.as<int64_t>() % vr.as<int64_t>();
  } else {
    return vl.as<uint64_t>() % vr.as<uint64_t>();
  }
}

BIN_OP(fmod)
//
BIN_OP(fdim)
BIN_OP(hypot)
BIN_OP(atan2)
// BIN_OP(pow) is manual
BIN_OP(min)
BIN_OP(max)
BIN_OP_INTEGRAL(gcd)
BIN_OP_INTEGRAL(lcm)
//
BIN_OP_INFIX_INTEGRAL(ior,|)
BIN_OP_INFIX_INTEGRAL(xor,^)
BIN_OP_INFIX_INTEGRAL(and,&)
BIN_OP_INFIX_INTEGRAL(lsh,<<)
BIN_OP_INFIX_INTEGRAL(rsh,>>)
//
BIN_OP_INFIX(add,+)
BIN_OP_INFIX(sub,-)
BIN_OP_INFIX(mul,*)
// BIN_OP(div) is manual (check division by 0)
// BIN_OP(mod) is manual (check division by 0)
//

const static init_spec_bex::op_spec BIN_FUNCS[] {
  {"fmod", 0, init_spec_bex::op_spec::N, apply_fmod},
  //
  {"atan2", 0, init_spec_bex::op_spec::N, apply_atan2},
  {"fdim", 0, init_spec_bex::op_spec::N, apply_fdim},
  {"hypot", 0, init_spec_bex::op_spec::N, apply_hypot},
  //
  {"pow", 0, init_spec_bex::op_spec::N, apply_pow},
  {"min", 0, init_spec_bex::op_spec::N, apply_min},
  {"max", 0, init_spec_bex::op_spec::N, apply_max},
  {"gcd", 0, init_spec_bex::op_spec::N, apply_gcd},
  {"lcm", 0, init_spec_bex::op_spec::N, apply_lcm},
  //
  //
  {"|",   5, init_spec_bex::op_spec::N, apply_ior},
  //
  {"^",   6, init_spec_bex::op_spec::N, apply_xor},
  //
  {"&",   7, init_spec_bex::op_spec::N, apply_and},
  //
  {">>",  8, init_spec_bex::op_spec::N, apply_rsh},
  {"<<",  8, init_spec_bex::op_spec::N, apply_lsh},
  //
  {"+",   9, init_spec_bex::op_spec::N, apply_add},
  {"-",   9, init_spec_bex::op_spec::N, apply_sub},
  //
  {"*",  10, init_spec_bex::op_spec::N, apply_mul},
  {"/",  10, init_spec_bex::op_spec::N, apply_div},
  {"%",  10, init_spec_bex::op_spec::N, apply_mod},
};


const init_spec_bex::op_spec *init_spec_bex::lookup_op(const char *symbol)
{
  for (const auto &bos : BIN_FUNCS) {
    if (text::streq(bos.symbol,symbol)) {
      return &bos;
    }
  }
  return nullptr;
}


void init_spec_bex::str(std::ostream &os, format_opts fopts) const
{
  if (e_op.precedence == 0) {
    os << e_op.symbol << "("; fmt(os, fopts, e_l); os <<
      ", "; fmt(os, fopts, e_r); os << ")";
  } else {
    auto fmtWithPrec =
      [&](const init_spec *e) {
        if (e->kind == init_spec::IS_BEX &&
          ((const init_spec_bex *)e)->e_op.precedence > e_op.precedence)
        {
          os << "(";
          fmt(os, fopts, e);
          os << ")";
        } else {
          fmt(os, fopts, e);
        }
      };
    fmtWithPrec(e_l);
    os << e_op.symbol;
    fmtWithPrec(e_r);
  }
}


void init_spec_uex::str(std::ostream &os, format_opts fopts) const
{
  if (e_op.precedence == 0 || e->kind == init_spec::IS_BEX) {
    os << e_op.symbol << "(";
    fmt(os, fopts, e);
    os << ")";
  } else {
    os << e_op.symbol;
    fmt(os, fopts, e);
  }
}

void init_spec_file::str(std::ostream &os, format_opts fopts) const
{
  os << "file<";
  switch (flavor) {
  case file_flavor::BIN: os << "bin"; break;
  case file_flavor::TXT: os << "text"; break;
  case file_flavor::TXT_COL:
    os << "text_col" << "," << col << "," << sep;
    break;
  }
  os << ">(\"" << path << "\")";
}


#define UNR_OP(F) \
  static val apply_ ## F (fatal_handler *,const loc &,const val &v) {\
    if (v.is_float()) {\
      return std:: F (v.as<double>());\
    } else if (v.is_signed()) {\
      return std:: F (v.as<int64_t>());\
    } else {\
      return std:: F (v.as<uint64_t>());\
    }\
  }
#define UNR_OP_FLOAT(F) \
  static val apply_ ## F (fatal_handler *,const loc &,const val &v) {\
    return std:: F (v.as<double>());\
  }

static val apply_float(fatal_handler *,const loc &,const val &v) {
  return v.as<double>();
}
static val apply_signed(fatal_handler *,const loc &,const val &v) {
  return v.as<int64_t>();
}
static val apply_unsigned(fatal_handler *,const loc &,const val &v) {
  return v.as<uint64_t>();
}
static val apply_abs(fatal_handler *,const loc &,const val &v) {
  if (v.is_float()) {
    return std::abs(v.as<double>());
  } else if (v.is_signed()) {
    return std::abs(v.as<int64_t>());
  } else {
    return v.as<uint64_t>();
  }
}

static val apply_negate(fatal_handler *,const loc &,const val &v) {
  if (v.is_float()) {
    return -v.as<double>();
  } else if (v.is_signed()) {
    return -v.as<int64_t>();
  } else {
    return v.as<uint64_t>();
  }
}
static val apply_complement(fatal_handler *fh,const loc &at,const val &v) {
  if (v.is_float()) {
    fh->fatalAt(at,"~ requires integer argument");
  } else if (v.is_signed()) {
    return ~v.as<int64_t>();
  } else {
    return ~v.as<uint64_t>();
  }
}

UNR_OP_FLOAT(fabs)
UNR_OP_FLOAT(sqrt)
UNR_OP_FLOAT(cbrt)
UNR_OP_FLOAT(exp)
UNR_OP_FLOAT(exp2)
UNR_OP_FLOAT(expm1)
UNR_OP_FLOAT(log)
UNR_OP_FLOAT(log2)
UNR_OP_FLOAT(log10)
UNR_OP_FLOAT(log1p)
UNR_OP_FLOAT(sin)
UNR_OP_FLOAT(cos)
UNR_OP_FLOAT(tan)
UNR_OP_FLOAT(asin)
UNR_OP_FLOAT(acos)
UNR_OP_FLOAT(atan)
UNR_OP_FLOAT(sinh)
UNR_OP_FLOAT(cosh)
UNR_OP_FLOAT(tanh)
UNR_OP_FLOAT(asinh)
UNR_OP_FLOAT(acosh)
UNR_OP_FLOAT(atanh)
//
UNR_OP_FLOAT(erf)
UNR_OP_FLOAT(erfc)
UNR_OP_FLOAT(tgamma)
UNR_OP_FLOAT(lgamma)
//
UNR_OP_FLOAT(ceil)
UNR_OP_FLOAT(floor)
UNR_OP_FLOAT(trunc)
UNR_OP_FLOAT(round)
static val apply_llround(fatal_handler *,const loc &,const val &v) {
  if (v.is_float()) {
    return std::llround(v.as<double>());
  } else if (v.is_signed()) {
    return v.as<int64_t>();
  } else {
    return v.as<uint64_t>();
  }
}
static val apply_nearbyint(fatal_handler *fh,const loc &at,const val &v) {
  return  std::nearbyint(v.as<double>());
}
template<int mode>
static val apply_nearbyint_by(fatal_handler *fh,const loc &at,const val &v) {
  auto old_mode = std::fegetround();
  if (old_mode < 0) {
    fh->fatalAt(at,"cannot determine old rounding mode");
  }
  double d = std::nearbyint(v.as<double>());
  if (std::fesetround(old_mode) != 0)
    fh->fatalAt(at,"cannot restore old rounding mode");
  return d;
}
#define UNR_OP_FLOAT_BOOL(F) \
  static val apply_ ## F (fatal_handler *fh,const loc &at,const val &v) { \
    if (v.is_float()) { \
      return std:: F (v.as<double>()) ? 1 : 0; \
    } else { \
      fh->fatalAt(at, "floating point input required"); \
    } \
  }

UNR_OP_FLOAT_BOOL(isfinite)
UNR_OP_FLOAT_BOOL(isinf)
UNR_OP_FLOAT_BOOL(isnan)
UNR_OP_FLOAT_BOOL(isnormal)

// https://en.cppreference.com/w/cpp/numeric/math
const static init_spec_uex::op_spec UNR_FUNCS[] {
  // type conversions
  {"float",    0, apply_float},
  {"int",      0, apply_signed},
  {"signed",   0, apply_signed},
  {"unsigned", 0, apply_unsigned},
  //
  {"fabs",     0, apply_fabs},
  {"abs",      0, apply_abs},
  //
  {"sqrt",     0, apply_sqrt},
  {"cbrt",     0, apply_cbrt},
  //
  {"exp",      0, apply_exp},
  {"exp2",     0, apply_exp2},
  {"expm1",    0, apply_expm1},
  {"log",      0, apply_log},
  {"log2",     0, apply_log2},
  {"log10",    0, apply_log10},
  {"log1p",    0, apply_log1p},
  //
  {"sin",      0, apply_sin},
  {"cos",      0, apply_cos},
  {"tan",      0, apply_tan},
  {"asin",     0, apply_asin},
  {"acos",     0, apply_acos},
  {"atan",     0, apply_atan},
  //
  {"sinh",     0, apply_sinh},
  {"cosh",     0, apply_cosh},
  {"tanh",     0, apply_tanh},
  {"asinh",    0, apply_asinh},
  {"acosh",    0, apply_acosh},
  {"atanh",    0, apply_atanh},
  //
  {"erf",      0, apply_erf},
  {"erfc",     0, apply_erfc},
  {"tgamma",   0, apply_tgamma},
  {"lgamma",   0, apply_lgamma},
  //
  {"ceil",     0, apply_ceil},
  {"floor",    0, apply_floor},
  {"trunc",    0, apply_trunc},
  {"round",    0, apply_round},
  {"lround",   0, apply_llround},
  {"llround",  0, apply_llround},
  {"llround",  0, apply_llround},

  {"nearbyint",      0, apply_nearbyint},
  {"nearbyint_rde",  0, apply_nearbyint_by<FE_TONEAREST>},
  {"nearbyint_rdd",  0, apply_nearbyint_by<FE_DOWNWARD>},
  {"nearbyint_rdu",  0, apply_nearbyint_by<FE_UPWARD>},
  {"nearbyint_rtz",  0, apply_nearbyint_by<FE_TOWARDZERO>},

  {"isfinite",  0, apply_isfinite},
  {"isinf",     0, apply_isinf},
  {"isnan",     0, apply_isnan},
  {"isnormal",  0, apply_isnormal},

  /////////////////////////////
  //
  {"-",        1, apply_negate},
  {"~",        1, apply_complement},
};

const init_spec_uex::op_spec *init_spec_uex::lookup_op(const char *symbol)
{
  for (const init_spec_uex::op_spec &op : UNR_FUNCS) {
    if (streq(symbol,op.symbol))
      return &op;
  }
  return nullptr;
}

void init_spec_rng::str(std::ostream &os, format_opts fopts) const
{
  os << "random";
  if (has_seed)
    os << "<" << seed << ">";
  if (e_hi) {
    os << "(";
    if (e_lo) {
      e_lo->str(os,fopts);
      os << ",";
    }
    e_hi->str(os,fopts);
    os << ")";
  }
}

void init_spec_seq::str(std::ostream &os, format_opts fopts) const
{
  os << "seq";
  if (base) {
    os << "(";
    base->str(os,fopts);
    if (delta) {
      os << ",";
      delta->str(os,fopts);
    }
    os << ")";
  }
}

void kernel_spec::str(std::ostream &os, format_opts fopts) const {
  program.str(os,fopts);
  os << "`" <<  name;
}

ndr::ndr() {
  dims[0] = 0; dims[1] = 0; dims[2] = 0;
  num_dims = 0;
}
ndr::ndr(size_t x) {
  dims[0] = x; dims[1] = 0; dims[2] = 0;
  num_dims = 1;
}
ndr::ndr(size_t x, size_t y) {
  dims[0] = x; dims[1] = y; dims[2] = 0;
  num_dims = 2;
}
ndr::ndr(size_t x, size_t y, size_t z)  {
  dims[0] = x; dims[1] = y; dims[2] = z;
  num_dims = 3;
}

std::string ndr::str() const {
  std::stringstream ss;
  str(ss);
  return ss.str();
}

void ndr::str(std::ostream &os) const {
  for (int i = 0; i < sizeof(dims)/sizeof(dims[0]) && dims[i] != 0; i++) {
    if (i > 0) os << "x";
    os << dims[i];
  }
}

size_t ndr::product() const {
  size_t p = 1;
  for (int i = 0; i < sizeof(dims)/sizeof(dims[0]) && dims[i] != 0; i++)
    p *= dims[i];
  return p;
}

void dispatch_spec::str(std::ostream &os, format_opts fopts) const {
  kernel.str(os,fopts);
  os << "<";
  global_size.str(os);
  if (local_size.rank() > 0) {
    os << ",";
    local_size.str(os);
  }
  os << ">";
  os << "(";
  for (size_t i = 0; i < arguments.size(); i++) {
    if (i > 0)
      os << ", ";
    arguments[i].str(os,fopts);
  }
  os << ")";
  if (!where_bindings.empty()) {
    os << " " << fopts.keyword("where") << " ";
    for (size_t i = 0; i < where_bindings.size(); i++) {
      if (i > 0)
        os << ", ";
      os << fopts.let_var(std::get<0>(where_bindings[i])) << " = ";
      std::get<1>(where_bindings[i])->str(os,fopts);
    }
  }
}

void let_spec::str(std::ostream &os, format_opts fopts) const {
  os << fopts.keyword("let") << " " << fopts.let_var(identifier);
  if (!params.empty()) {
    os << "(" << params[0];
    for (size_t i = 1; i < params.size(); i++)
      os << "," << params[i];
    os << ")";
  }
  os << " = ";
  value->str(os,fopts);
}

void diff_spec::str(std::ostream &os,format_opts fopts) const  {
  os << "diff";
  if (element_type)
    os << "<" << element_type->syntax() << ">";
  os << "("; ref.str(os,fopts); os << ","; sut.str(os,fopts); os << ")";
}

void print_spec::str(std::ostream &os,format_opts fopts) const  {
  os << "print";
  if (element_type)
    os << "<" << element_type->syntax() << ">";
  os << "("; arg.str(os,fopts); os << ")";
}
