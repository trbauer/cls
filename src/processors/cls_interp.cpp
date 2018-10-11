#include "cls_interp.hpp"
#include "cls_interp_internal.hpp"
#include "../devices.hpp"
#include "../system.hpp"
#include "../text.hpp"

#include <cmath>
#include <functional>
#include <map>
#include <numeric>
#include <tuple>
#include <type_traits>


using namespace cls;

compiled_script_impl::compiled_script_impl(const opts &_os,const script &_s)
  : interp_fatal_handler(_os,_s.source)
  , s(_s)
  , e(new evaluator(this)) { }

evaluator::val evaluator::eval(
  context &ec,
  const init_spec_atom *e)
{
  // TODO: merge with evalInto<type_num>
  switch (e->kind) {
  case init_spec::IS_INT:
    return val(((const init_spec_int *)e)->value);
  case init_spec::IS_FLT:
    return val(((const init_spec_float *)e)->value);
  case init_spec::IS_BEX: {
    const init_spec_bin_expr *be = ((const init_spec_bin_expr *)e);
    switch (be->e_function) {
    case init_spec_bin_expr::E_OR:
    case init_spec_bin_expr::E_XOR:
    case init_spec_bin_expr::E_AND:
    case init_spec_bin_expr::E_LSH:
    case init_spec_bin_expr::E_RSH: {
      val
        vl = evalI(ec, be->el),
        vr = evalI(ec, be->er);
      switch (be->e_function) {
      case init_spec_bin_expr::E_OR:   vl.s64 |= vr.s64; break;
      case init_spec_bin_expr::E_XOR:  vl.s64 ^= vr.s64; break;
      case init_spec_bin_expr::E_AND:  vl.s64 &= vr.s64; break;
      case init_spec_bin_expr::E_LSH:
        if (vr.s64 > 63)
          warningAt(be->er->defined_at,"shift amount too large");
        vl.s64 <<= vr.s64;
        break;
      case init_spec_bin_expr::E_RSH:
        if (vr.s64 > 63)
          warningAt(be->er->defined_at,"shift amount too large");
        vl.s64 >>= vr.s64; break;
      }
      return vl;
    } // end bitwise ops
    case init_spec_bin_expr::E_POW:
    case init_spec_bin_expr::E_MAX:
    case init_spec_bin_expr::E_MIN:
    case init_spec_bin_expr::E_GCD:
    case init_spec_bin_expr::E_LCM:
    case init_spec_bin_expr::E_ADD:
    case init_spec_bin_expr::E_SUB:
    case init_spec_bin_expr::E_MUL:
    case init_spec_bin_expr::E_DIV:
    case init_spec_bin_expr::E_MOD: {
      val vl = eval(ec, be->el);
      val vr = eval(ec, be->er);
      if (vl.is_float() || vr.is_float()) {
        // compute as floating point
        if (!vl.is_float())
          vl.f64 = (double)vl.s64;
        if (!vr.is_float())
          vr.f64 = (double)vr.s64;
        switch (be->e_function) {
        case init_spec_bin_expr::E_POW: vl.f64 = std::pow(vl.f64,vr.f64); break;
        case init_spec_bin_expr::E_MAX: vl.f64 = std::max(vl.f64,vr.f64); break;
        case init_spec_bin_expr::E_MIN: vl.f64 = std::min(vl.f64,vr.f64); break;
        case init_spec_bin_expr::E_LCM:
        case init_spec_bin_expr::E_GCD:
          fatalAt(e->defined_at,"function requires integer arguments");
        case init_spec_bin_expr::E_ADD: vl.f64 += vr.f64; break;
        case init_spec_bin_expr::E_SUB: vl.f64 -= vr.f64; break;
        case init_spec_bin_expr::E_MUL: vl.f64 *= vr.f64; break;
        case init_spec_bin_expr::E_DIV: vl.f64 /= vr.f64; break;
        case init_spec_bin_expr::E_MOD: vl.f64 = std::fmod(vl.f64,vr.f64); break;
        default: fatalAt(e->defined_at,"unsupported binary expression");
        }
      } else { // compute as integer
        switch (be->e_function) {
        case init_spec_bin_expr::E_POW:
          fatalAt(e->defined_at,"function requires floating-point arguments"); break;
        case init_spec_bin_expr::E_MAX: vl.s64 = std::max(vl.s64,vr.s64); break;
        case init_spec_bin_expr::E_MIN: vl.s64 = std::min(vl.s64,vr.s64); break;
        case init_spec_bin_expr::E_GCD: vl.s64 = std::gcd(vl.s64,vr.s64); break;
        case init_spec_bin_expr::E_LCM: vl.s64 = std::lcm(vl.s64,vr.s64); break;

        case init_spec_bin_expr::E_ADD: vl.s64 += vr.s64; break;
        case init_spec_bin_expr::E_SUB: vl.s64 -= vr.s64; break;
        case init_spec_bin_expr::E_MUL: vl.s64 *= vr.s64; break;
        case init_spec_bin_expr::E_DIV:
        case init_spec_bin_expr::E_MOD:
          if (vl.s64 == 0)
            fatalAt(e->defined_at,"division by zero");
          if (be->kind == init_spec_bin_expr::E_DIV)
            vl.s64 /= vr.s64;
          else
            vl.s64 %= vl.s64;
          break;
        default: fatalAt(e->defined_at,"unsupported binary expression");
        }
      } // else integer
      return vl;
    } // end arithmetic/transcendental functions
    default: fatalAt(e->defined_at,"unsupported binary expression");
    } // end binary expression switch
  } // binary expression
  case init_spec::IS_UEX: {
    const init_spec_unr_expr *ue = ((const init_spec_unr_expr *)e);
    val v = eval(ec,ue->e);
    switch (ue->e_function) {
    case init_spec_unr_expr::E_NEG:
      if (v.is_float())
        v.f64 = -v.f64;
      else
        v.s64 = -v.s64;
      break;
    case init_spec_unr_expr::E_COMPL:
      if (v.is_float())
        fatalAt(e->defined_at,"complement of floating point value");
      else
        v.s64 = ~v.s64;
      break;
    case init_spec_unr_expr::E_ABS:
      if (v.is_float())
        v.f64 = std::abs(v.f64);
      else
        v.s64 = std::abs(v.s64);
      break;
    case init_spec_unr_expr::E_SQT:
    case init_spec_unr_expr::E_EXP:
    case init_spec_unr_expr::E_LOG:
    case init_spec_unr_expr::E_LOG2:
    case init_spec_unr_expr::E_LOG10:
    case init_spec_unr_expr::E_SIN:
    case init_spec_unr_expr::E_COS:
    case init_spec_unr_expr::E_TAN:
    case init_spec_unr_expr::E_ATAN:
      if (!v.is_float())
        fatalAt(e->defined_at,"floating-point argument required");
      switch (ue->kind) {
      case init_spec_unr_expr::E_SQT:   v.f64 = std::sqrt(v.f64);  break;
      case init_spec_unr_expr::E_EXP:   v.f64 = std::exp(v.f64);   break;
      case init_spec_unr_expr::E_LOG:   v.f64 = std::log(v.f64);   break;
      case init_spec_unr_expr::E_LOG2:  v.f64 = std::log2(v.f64);  break;
      case init_spec_unr_expr::E_LOG10: v.f64 = std::log10(v.f64); break;
      case init_spec_unr_expr::E_SIN:   v.f64 = std::sin(v.f64);   break;
      case init_spec_unr_expr::E_COS:   v.f64 = std::cos(v.f64);   break;
      case init_spec_unr_expr::E_TAN:   v.f64 = std::tan(v.f64);   break;
      case init_spec_unr_expr::E_ATAN:  v.f64 = std::atan(v.f64);  break;
      }
      break;
    default:
      fatalAt(e->defined_at,"unsupported unary expression for type");
    }
    break;
  } // end case IS_UEX:
  case init_spec::IS_BIV: {
    auto computeDim =
      [&](const cl::NDRange &ndr, int dim_ix, int dim_len) {
        if (dim_ix + dim_len > ndr.dimensions())
          fatalAt(
            e->defined_at,
            &ndr == &ec.global_size ? "global" : "local",
            " dimension size is out of bounds for this dispatch");
        size_t prod = 1;
        for (int ix = dim_ix; ix < dim_ix + dim_len; ix++)
          prod *= ndr.get()[ix];
        return val(prod);
      };
    switch (((const init_spec_builtin *)e)->kind) {
    case init_spec_builtin::BIV_GX:   return computeDim(ec.global_size, 0, 1);
    case init_spec_builtin::BIV_GY:   return computeDim(ec.global_size, 1, 1);
    case init_spec_builtin::BIV_GZ:   return computeDim(ec.global_size, 2, 1);
    case init_spec_builtin::BIV_GXY:  return computeDim(ec.local_size,  0, 2);
    case init_spec_builtin::BIV_GXYZ: return computeDim(ec.local_size,  0, 3);
    case init_spec_builtin::BIV_LX:   return computeDim(ec.local_size,  0, 1);
    case init_spec_builtin::BIV_LY:   return computeDim(ec.local_size,  1, 1);
    case init_spec_builtin::BIV_LZ:   return computeDim(ec.local_size,  2, 1);
    case init_spec_builtin::BIV_LXY:  return computeDim(ec.local_size,  0, 2);
    case init_spec_builtin::BIV_LXYZ: return computeDim(ec.local_size,  0, 3);
    default: fatalAt(e->defined_at,"unsupported built-in variable");
    }
    break;
  }
  default: fatalAt(e->defined_at,"unsupported expression for primitive"); break;
  }
  return val((uint64_t)0); // unreachable
}

evaluator::val evaluator::evalI(context &ec,const init_spec_atom *e)
{
  val v = eval(ec, e);
  if (!v.is_int())
    fatalAt(e->defined_at,"argument must be integral");
  return v;
}
evaluator::val evaluator::evalF(context &ec,const init_spec_atom *e)
{
  val v = eval(ec, e);
  if (!v.is_float())
    fatalAt(e->defined_at,"argument must be floating point");
  return v;
}

evaluator::val evaluator::evalToF(context &ec,const init_spec_atom *e)
{
  val v = eval(ec, e);
  if (v.is_signed()) {
    v = (double)v.s64;
  } else if (v.is_unsigned()) {
    v = (double)v.u64;
  }
  return v;
}


void evaluator::setKernelArgImmediate(
  cl_uint arg_index,
  dispatch_command &dc,
  std::stringstream &ss,
  const refable<init_spec *> &ris,
  const arg_info &ai)
{
  // non-surface
  context ec(dc.global_size,dc.local_size, &ss);
  const init_spec *is = ris;

  arg_buffer ab(this, ris.defined_at, ai.type.size());

  evalInto(ec, is->defined_at, (const init_spec_atom *)is, ab, ai.type);

  if (ab.num_left() != 0) {
    fatalAt(
      ris.defined_at,
      "INTERNAL ERROR: failed to set full argument");
  }

  CL_COMMAND(
    ris.defined_at, // use the arg actual location, not the let
    clSetKernelArg,
      (*dc.kernel->kernel)(),
      arg_index,
      ab.size(),
      (const void *)ab.ptr());

  if (os.verbosity >= 2) {
    debug(ris.defined_at, "setting immediate argument for ",
      ai.type.syntax()," ",ai.name," to");
    if (std::holds_alternative<type_ptr>(ai.type.var)) {
      // fake a pointer to a pointer so the first dereference
      // in formatBuffer is ignored
      size_t ptr_size = std::get<type_ptr>(ai.type.var).size();
      formatBuffer(std::cout,ab.base,ab.capacity,
        type_ptr(&ai.type,ptr_size));
    } else {
      formatBuffer(std::cout,ab.base,ab.capacity,ai.type);
    }
    std::cout << "\n";
  }
}

static size_t computeBufferSize(
  evaluator *e,
  dispatch_command &dc,
  const type &elem_type,
  const init_spec_memory *ism)
{
  if (ism->dimension) {
    return (size_t)e->evalTo<size_t>(
      evaluator::context(
        dc.global_size,
        dc.local_size), ism->dimension).u64;
  } else {
    size_t prod = 1;
    for (size_t i = 0; i < dc.global_size.dimensions(); i++) {
      prod *= dc.global_size.get()[i];
    }
    return prod*elem_type.size();
  }
}


void evaluator::setKernelArgMemobj(
  cl_uint arg_index,
  dispatch_command &dc,
  std::stringstream &ss,
  const loc &arg_loc,
  const refable<init_spec *> &ris,
  const arg_info &ai)
{
  if (((const init_spec *)ris)->kind != init_spec::IS_MEM) {
    fatalAt(ris.defined_at,"expected surface initializer");
  }
  const init_spec_memory *ism =
    (const init_spec_memory *)(const init_spec *)ris;
  if (!std::holds_alternative<type_ptr>(ai.type.var)) {
    fatalAt(ism->defined_at, "buffer/image requires pointer type");
  }
  const type &elem_type = *std::get<type_ptr>(ai.type.var).element_type;
  size_t buffer_size = computeBufferSize(this,dc,elem_type,ism);

  surface_object *so = nullptr;
  auto itr = csi->surfaces.find(ism);
  if (itr != csi->surfaces.find_end()) {
    // ensure we fit with this object
    so = itr->second;
    if (so->size_in_bytes != buffer_size) {
      fatalAt(
        ism->defined_at,
        "buffer/image size differs from uses "
        "(see line ",so->spec->defined_at.line,")");
    }
  } else {
    // creating a new surface
    bool is_r =
      (ism->access_properties & init_spec_memory::INIT_SPEC_MEM_READ);
    bool is_w =
      (ism->access_properties & init_spec_memory::INIT_SPEC_MEM_WRITE);
    cl_mem_flags cl_mfs = 0;
    if (is_r && is_w) {
      cl_mfs |= CL_MEM_READ_WRITE;
    } else if (is_r) {
      cl_mfs |= CL_MEM_READ_ONLY;
    } else if (is_w) {
      cl_mfs |= CL_MEM_WRITE_ONLY;
    }

    cl_mem memobj = nullptr;
    cl_context context = (*dc.kernel->program->device->context)();
    CL_COMMAND_CREATE(memobj, arg_loc,
      clCreateBuffer,
        context,
        cl_mfs,
        buffer_size,
        nullptr);
    so = &csi->surfaces.emplace_back(ism,
      ism,
      surface_object::SO_BUFFER,
      buffer_size,
      memobj,
      (int)csi->surfaces.size());
  }
  ss << "MEM[" << so->memobj_index << "] (" << so->size_in_bytes << " B)";

  dc.inits.emplace_back(
    arg_loc,
    std::get<type_ptr>(ai.type.var),
    so);

  CL_COMMAND(
    arg_loc,
    clSetKernelArg,
      (*dc.kernel->kernel)(),
      arg_index,
      sizeof(cl_mem),
      (const void *)&so->memobj);
}

void evaluator::setKernelArgSLM(
  cl_uint arg_index,
  dispatch_command &dc,
  std::stringstream &ss, // debug string for arg
  const refable<init_spec *> &ris,
  const arg_info &ai)
{
  const init_spec *is = ris;
  // Special treatment of local * arguments
  // e.g. kernel void foo(..., local int2 *buffer)
  // the user must tell us how many bytes they neeed for buffer
  //  foo<1024,16>(...,16*8); // 16*sizeof(int2)
  //
  //  SPECIFY: do we allow the alternative?
  //     foo<1024,16>(...,0:rw); // assume 1 int2 per work item
  if (!std::holds_alternative<type_ptr>(ai.type.var)) {
    fatalAt(
      ris.defined_at,
      "kernel argument in local address space must be pointer type");
  } else if (!is->is_atom()) {
    fatalAt(
      ris.defined_at,
      "local pointer requires size in bytes");
  } // SPECIFY: see above  (use tp.element_type->size() * wg-size)
  const type_ptr &tp = std::get<type_ptr>(ai.type.var);
  evaluator::context ec(dc.global_size,dc.local_size);
  auto v = csi->e->evalTo<size_t>(ec,(const init_spec_atom *)is);
  size_t local_bytes = (size_t)v.u64;
  CL_COMMAND(
    ris.defined_at, // use the arg actual location, not the let
    clSetKernelArg,
      (*dc.kernel->kernel)(),
      arg_index,
      local_bytes,
      nullptr);
  ss << "SLM[" << local_bytes << " B]";
}

void evaluator::evalInto(
  context &ec,
  const loc &arg_loc,
  const init_spec_atom *is,
  arg_buffer &ab,
  const type &t)
{
  if (std::holds_alternative<type_num>(t.var)) {
    evalInto(ec, arg_loc, is, ab, std::get<type_num>(t.var));
  } else if (std::holds_alternative<type_struct>(t.var)) {
    evalInto(ec, arg_loc, is, ab, std::get<type_struct>(t.var));
  } else if (std::holds_alternative<type_ptr>(t.var)) {
    evalInto(ec, arg_loc, is, ab, std::get<type_ptr>(t.var));
  } else {
    fatalAt(is->defined_at,"unsupported argument type");
  }
}

void evaluator::evalInto(
  context &ec,
  const loc &arg_loc,
  const init_spec_atom *is,
  arg_buffer &ab,
  const type_num &tn)
{
  // is could be 4, 4*g.x, or other stuff
  switch (tn.kind) {
  case type_num::SIGNED:
    switch (tn.size_in_bytes) {
    case 1: evalIntoT<int8_t>(ec,arg_loc,is,ab); break;
    case 2: evalIntoT<int16_t>(ec,arg_loc,is,ab); break;
    case 4: evalIntoT<int32_t>(ec,arg_loc,is,ab); break;
    case 8: evalIntoT<int64_t>(ec,arg_loc,is,ab); break;
    default: fatalAt(arg_loc,"INTERNAL ERROR: unreachable");
    }
    break;
  case type_num::UNSIGNED:
    switch (tn.size_in_bytes) {
    case 1: evalIntoT<uint8_t>(ec,arg_loc,is,ab); break;
    case 2: evalIntoT<uint16_t>(ec,arg_loc,is,ab); break;
    case 4: evalIntoT<uint32_t>(ec,arg_loc,is,ab); break;
    case 8: evalIntoT<uint64_t>(ec,arg_loc,is,ab); break;
    default: fatalAt(arg_loc,"INTERNAL ERROR: unreachable");
    }
    break;
  case type_num::FLOATING:
    switch (tn.size_in_bytes) {
    case 2: evalIntoT<half>(ec,arg_loc,is,ab); break;
    case 4: evalIntoT<float>(ec,arg_loc,is,ab); break;
    case 8: evalIntoT<double>(ec,arg_loc,is,ab); break;
    default: fatalAt(arg_loc,"INTERNAL ERROR: unreachable");
    }
    break;
  default:
    fatalAt(arg_loc,"INTERNAL ERROR: unreachable");
  }
}

template <typename T>
void evaluator::evalIntoT(
  context &ec,
  const loc &arg_loc,
  const init_spec_atom *is,
  arg_buffer &ab)
{
  switch (is->kind) {
  case init_spec::IS_INT:
  case init_spec::IS_FLT:
  case init_spec::IS_BEX:
  case init_spec::IS_UEX:
  case init_spec::IS_BIV: {
    val v = evalTo<T>(ec, is);
    ab.write<T>(v.as<T>());
    ec.evaluated(v.as<T>());
    break;
  }
  default:
    fatalAt(arg_loc,"INTERNAL ERROR: unsupported expression for primitive");
  }
}


void evaluator::evalInto(
  context &ec,
  const loc &arg_loc,
  const init_spec_atom *is,
  arg_buffer &ab,
  const type_struct &ts)
{
  if (is->kind == init_spec::IS_REC) {
    const init_spec_record *isr = (const init_spec_record *)is;
    if (isr->children.size() != ts.elements_length) {
      fatalAt(arg_loc, "structure initializer has wrong number of elements");
    }
    ec.evaluated("{");
    for (size_t i = 0; i < ts.elements_length; i++) {
      if (i > 0)
        ec.evaluated(",");
      evalInto(ec, arg_loc, isr->children[i], ab, *ts.elements[i]);
    }
    ec.evaluated("}");
  } else {
    // TODO: we could support things like broadcast, random etc...
    fatalAt(arg_loc,"structure argument requires structure initializer");
  }
}

void evaluator::evalInto(
  context &ec,
  const loc &arg_loc,
  const init_spec_atom *is,
  arg_buffer &ab,
  const type_ptr &tp)
{
  fatalAt(is->defined_at,"type_ptr not implemented as primitive yet");
}
