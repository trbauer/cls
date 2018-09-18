#include "cls_ir.hpp"
#include "../system.hpp"



void cls::statement_spec::str(std::ostream &os) const {
  switch (kind) {
  case DISPATCH: ((const dispatch_spec*)this)->str(os); break;
  case LET:      ((const let_spec*)this)->str(os);      break;
  case BARRIER:  ((const barrier_spec*)this)->str(os);  break;
  case SAVE:
  case PRINT:
  default:
    os << "cls::statement_spec???";
    break;
  }
}

void cls::init_spec_memory::str(std::ostream &os) const {
  if (root)
    root->str(os);
  else
    os << "<nullptr>";
  os << ":";
  if (dimension) {
    os << "["; dimension->str(os); os << "]";
  }
  if (access_properties & cls::init_spec_memory::INIT_SPEC_MEM_READ)
    os << 'r';
  if (access_properties & cls::init_spec_memory::INIT_SPEC_MEM_WRITE)
    os << 'w';
//  if (access_properties & cls::init_spec_memory::INIT_SPEC_MEM_DIRECT)
//    os << 'd'; // direct access (SVM)
  if (transfer_properties == cls::init_spec_memory::TX_MAP)
    os << 'm';
  if (transfer_properties == cls::init_spec_memory::TX_COPY)
    os << 'c';
  if (transfer_properties == cls::init_spec_memory::TX_SVM_COARSE)
    os << 's';
  if (transfer_properties == cls::init_spec_memory::TX_SVM_FINE)
    os << 's' << 'f';

}


void cls::init_spec::str(std::ostream &os) const
{
  switch (kind) {
  case IS_SYM: ((const cls::init_spec_symbol *)this)->str(os); break;
  case IS_INT:((const cls::init_spec_int *)this)->str(os); break;
  case IS_FLT: ((const cls::init_spec_float *)this)->str(os); break;
  case IS_REC: ((const cls::init_spec_record *)this)->str(os); break;
  case IS_BEX: ((const cls::init_spec_bin_expr *)this)->str(os); break;
  case IS_UEX: ((const cls::init_spec_unr_expr *)this)->str(os); break;
  case IS_FILE: ((const cls::init_spec_file *)this)->str(os); break;
  case IS_RND: ((const cls::init_spec_rng_generator *)this)->str(os); break;
  case IS_SEQ: ((const cls::init_spec_seq_generator *)this)->str(os); break;
  case IS_MEM: ((const cls::init_spec_memory *)this)->str(os); break;
  default: os << "cls::init_spec?"; break;
  }
}

static void emitExpr(std::ostream &os, cls::init_spec_atom *e) {
  if (e) {
    e->str(os);
  } else {
    os << "<nullptr>";
  }
};

void cls::init_spec_bin_expr::str(std::ostream &os) const
{
  switch (e_kind) {
  case init_spec_bin_expr::E_POW:
  // function syntax
    os << "pow("; emitExpr(os, el); os << ", "; emitExpr(os, er); os << ")";
  default:
  // infix operators
    emitExpr(os, el);
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
    default: os << "??"; break;
    }
    emitExpr(os, er);
  }

  if (er) {
    er->str(os);
  } else {
    os << "<nullptr>";
  }
}

void cls::init_spec_unr_expr::str(std::ostream &os) const
{
  switch (e_kind) {
  case init_spec_unr_expr::E_NEG: os << "-"; emitExpr(os, e); break;
  case init_spec_unr_expr::E_COMPL: os << "~"; emitExpr(os, e); break;
  case init_spec_unr_expr::E_ABS: os << "abs("; emitExpr(os, e); os << ")"; break;
  case init_spec_unr_expr::E_SQT: os << "sqt("; emitExpr(os, e); os << ")"; break;
  case init_spec_unr_expr::E_SIN: os << "sin("; emitExpr(os, e); os << ")"; break;
  case init_spec_unr_expr::E_COS: os << "cos("; emitExpr(os, e); os << ")"; break;
  case init_spec_unr_expr::E_TAN: os << "tan("; emitExpr(os, e); os << ")"; break;
  default:
    os << "?"; emitExpr(os, e); break;
  }
}

void cls::dim_spec::str(std::ostream &os) const {
  bool first = true;
  for (size_t d : dims) {
    if (first) first = false; else os << "x";
    os << d;
  }
}

void cls::dispatch_spec::str(std::ostream &os) const {
  device.str(os);
  os << '`';
  program.str(os);
  os << '`';
  kernel.str(os);
  os << "<";
  global_size.str(os);
  if (!local_size.dims.empty()) {
    os << ",";
    local_size.str(os);
  }
  os << ">";
  os << "(";
  for (size_t i = 0; i < arguments.size(); i++) {
    if (i > 0)
      os << ", ";
    arguments[i]->str(os);
  }
  os << ")";
}
