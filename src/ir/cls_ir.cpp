#include "cls_ir.hpp"
#include "../system.hpp"
#include "../text.hpp"

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
  case spec::STATEMENT_LIST_SPEC: return "statement list";
  case spec::STATEMENT_SPEC:
    switch (((const statement_spec *)this)->kind) {
    case statement_spec::DISPATCH: return "dispatch";
    case statement_spec::LET:      return "let";
    case statement_spec::BARRIER: return "barrier";
    case statement_spec::DIFF:    return "diff";
    case statement_spec::SAVE:    return "save";
    case statement_spec::PRINT:   return "print";
    default: return "unknown statement";
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
    default: return "unknown initializer";
    }
  case spec::DEVICE_SPEC:  return "device";
  case spec::PROGRAM_SPEC: return "program";
  case spec::KERNEL_SPEC:  return "kernel";
  default: return "unknown syntax";
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
  case SAVE:
  case PRINT:
  default:
    os << "statement_spec???";
    break;
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

void init_spec_memory::str(std::ostream &os, format_opts fopts) const {
  fmt(os, fopts, root);
  os << ":";
  if (dimension) {
    os << "["; dimension->str(os,fopts); os << "]";
  }
  if (access_properties & init_spec_memory::INIT_SPEC_MEM_READ)
    os << 'r';
  if (access_properties & init_spec_memory::INIT_SPEC_MEM_WRITE)
    os << 'w';
//  if (access_properties & init_spec_memory::INIT_SPEC_MEM_DIRECT)
//    os << 'd'; // direct access (SVM)
  // SPECIFY: do we allow for a default
  if (transfer_properties == init_spec_memory::TX_MAP)
    os << 'm';
  if (transfer_properties == init_spec_memory::TX_COPY)
    os << 'c';
  if (transfer_properties == init_spec_memory::TX_SVM_COARSE)
    os << 's';
  if (transfer_properties == init_spec_memory::TX_SVM_FINE)
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
  case IS_BEX: fmt(os, fopts, (const init_spec_bin_expr      *)this); break;
  case IS_UEX: fmt(os, fopts, (const init_spec_unr_expr      *)this); break;
  case IS_FIL: fmt(os, fopts, (const init_spec_file          *)this); break;
  case IS_RND: fmt(os, fopts, (const init_spec_rng_generator *)this); break;
  case IS_SEQ: fmt(os, fopts, (const init_spec_seq_generator *)this); break;
  case IS_MEM: fmt(os, fopts, (const init_spec_memory        *)this); break;
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
  case BIV_GXY:  return "g.xy";
  case BIV_GXYZ: return "g.xyz";
  case BIV_LX:   return "l.x";
  case BIV_LY:   return "l.y";
  case BIV_LZ:   return "l.z";
  case BIV_LXY:  return "l.xy";
  case BIV_LXYZ: return "l.xyz";
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
      os << ", ";
    c->str(os,fopts);
  }
  os << "}";
}

void init_spec_bin_expr::str(std::ostream &os, format_opts fopts) const
{
  auto fmtFunc = [&] (const char *name) {
    os << name << "("; fmt(os, fopts, el); os <<
      ", "; fmt(os, fopts, er); os << ")";
  };
  switch (e_kind) {
  // function syntax
  case init_spec_bin_expr::E_POW: fmtFunc("pow"); break;
  case init_spec_bin_expr::E_MIN: fmtFunc("min"); break;
  case init_spec_bin_expr::E_MAX: fmtFunc("max"); break;
  case init_spec_bin_expr::E_GCD: fmtFunc("gcd"); break;
  case init_spec_bin_expr::E_LCM: fmtFunc("lcm"); break;
  default:
   // infix operators
    fmt(os, fopts, el);
    switch (e_kind) {
    case init_spec_bin_expr::E_OR:   os << "|"; break;
    case init_spec_bin_expr::E_XOR:  os << "^"; break;
    case init_spec_bin_expr::E_AND:  os << "&"; break;
    case init_spec_bin_expr::E_LSH:  os << "<<"; break;
    case init_spec_bin_expr::E_RSH:  os << ">>"; break;
    case init_spec_bin_expr::E_ADD:  os << "+"; break;
    case init_spec_bin_expr::E_SUB:  os << "-"; break;
    case init_spec_bin_expr::E_MUL:  os << "*"; break;
    case init_spec_bin_expr::E_DIV:  os << "/"; break;
    case init_spec_bin_expr::E_MOD:  os << "%"; break;
    default: os << "?binop?"; break;
    }
    fmt(os, fopts, er);
  }
}

void init_spec_unr_expr::str(std::ostream &os, format_opts fopts) const
{
  auto unaryFunction = [&] (const char *sym) {
    os << sym << "("; fmt(os, fopts, e); os << ")";
  };
  switch (e_kind) {
  case init_spec_unr_expr::E_NEG: os << "-"; fmt(os, fopts, e); break;
  case init_spec_unr_expr::E_COMPL: os << "~"; fmt(os, fopts, e); break;
  case init_spec_unr_expr::E_ABS: unaryFunction("abs"); break;

  case init_spec_unr_expr::E_SQT: unaryFunction("sqt"); break;

  case init_spec_unr_expr::E_EXP: unaryFunction("exp"); break;
  case init_spec_unr_expr::E_LOG: unaryFunction("log"); break;
  case init_spec_unr_expr::E_LOG2: unaryFunction("log2"); break;
  case init_spec_unr_expr::E_LOG10: unaryFunction("log10"); break;

  case init_spec_unr_expr::E_SIN: unaryFunction("sin"); break;
  case init_spec_unr_expr::E_COS: unaryFunction("cos"); break;
  case init_spec_unr_expr::E_TAN: unaryFunction("tan"); break;
  case init_spec_unr_expr::E_ATAN: unaryFunction("atan"); break;

  default: os << "?unop?"; fmt(os, fopts, e); break;
  }
}

void init_spec_rng_generator::str(std::ostream &os, format_opts fopts) const
{
  os << "random";
  if (seed != 0) {
    os << "<" << seed << ">";
  }
  if (e_lo) {
    os << "[";
    e_lo->str(os,fopts);
    if (e_hi) {
      os << ", ";
      e_hi->str(os,fopts);
    }
    os << "]";
  }
}

void kernel_spec::str(std::ostream &os, format_opts fopts) const {
  program.str(os,fopts);
  os << "`" <<  name;
}

void dim::str(std::ostream &os, format_opts fopts) const {
  bool first = true;
  for (size_t d : dims) {
    if (first) first = false; else os << "x";
    os << d;
  }
}

void dispatch_spec::str(std::ostream &os, format_opts fopts) const {
  kernel.str(os,fopts);
  os << "<";
  global_size.str(os,fopts);
  if (!local_size.dims.empty()) {
    os << ",";
    local_size.str(os,fopts);
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


