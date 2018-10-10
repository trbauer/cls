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
  : cl_fatal_handler(_s.source)
  , os(_os)
  , s(_s)
  , e(new evaluator(this)) { }


/*
====
#0`prog1.cl`kernel<...>(...)
let X=#0`prog2.cl`kernel2
#2`prog1.cl
#GTX`prog1.cl...
barrier()
#0`prog1.cl...
print(...)
======
== construct contexts and command queues for each unique device in the system
#0`prog1.cl`kernel<...>(...)
 ^  device_state(0)
let X=#0`prog2.cl`kernel2
      ^  device_state(0)
#2`prog1.cl
#GTX`prog1.cl...
barrier()
#0`prog1.cl...
print(...)
=========
 */
evaluator::val evaluator::eval(
  dispatch_command &dc,
  const init_spec_atom *e)
{
  switch (e->kind) {
  case init_spec::IS_INT:
    return val(((const init_spec_int *)e)->value);
  case init_spec::IS_FLT:
    return val(((const init_spec_float *)e)->value);
  case init_spec::IS_BEX: {
    const init_spec_bin_expr *be = ((const init_spec_bin_expr *)e);
    switch (be->kind) {
    case init_spec_bin_expr::E_OR:
    case init_spec_bin_expr::E_XOR:
    case init_spec_bin_expr::E_AND:
    case init_spec_bin_expr::E_LSH:
    case init_spec_bin_expr::E_RSH: {
      val
        vl = evalI(dc, be->el),
        vr = evalI(dc, be->er);
      switch (be->kind) {
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
      val vl = eval(dc, be->el);
      val vr = eval(dc, be->er);
      if (vl.is_float() || vr.is_float()) {
        if (!vl.is_float())
          vl.f64 = (double)vl.s64;
        if (!vr.is_float())
          vr.f64 = (double)vr.s64;
        switch (be->kind) {
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
      } else { // integer
        switch (be->kind) {
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
    val v = eval(dc,ue->e);
    switch (ue->kind) {
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
  }
  case init_spec::IS_BIV: {
    auto computeDim =
      [&](const cl::NDRange &ndr, int dim_ix, int dim_len) {
        if (dim_ix + dim_len > ndr.dimensions())
          fatalAt(
            e->defined_at,
            &ndr == &dc.global_size ? "global" : "local",
            " dimension size is out of bounds for this dispatch");
        size_t prod = 1;
        for (int ix = dim_ix; ix < dim_ix + dim_len; ix++)
          prod *= ndr.get()[ix];
        return val(prod);
      };
    switch (((const init_spec_builtin *)e)->kind) {
    case init_spec_builtin::BIV_GX:   return computeDim(dc.global_size, 0, 1);
    case init_spec_builtin::BIV_GY:   return computeDim(dc.global_size, 1, 1);
    case init_spec_builtin::BIV_GZ:   return computeDim(dc.global_size, 2, 1);
    case init_spec_builtin::BIV_GXY:  return computeDim(dc.local_size,  0, 2);
    case init_spec_builtin::BIV_GXYZ: return computeDim(dc.local_size,  0, 3);
    case init_spec_builtin::BIV_LX:   return computeDim(dc.local_size,  0, 1);
    case init_spec_builtin::BIV_LY:   return computeDim(dc.local_size,  1, 1);
    case init_spec_builtin::BIV_LZ:   return computeDim(dc.local_size,  2, 1);
    case init_spec_builtin::BIV_LXY:  return computeDim(dc.local_size,  0, 2);
    case init_spec_builtin::BIV_LXYZ: return computeDim(dc.local_size,  0, 3);
    default: fatalAt(e->defined_at,"unsupported expression");
    }
    break;
  }
  case init_spec::IS_FIL: {
//    if () {
      fatalAt(e->defined_at,"unsupported expression");
//    }
    break;
  }
  case init_spec::IS_RND: {
//    get_state(dc,(const init_spec_rng *)e, ...);
  }
  default: fatalAt(e->defined_at,"unsupported expression"); break;
  }
  return val((uint64_t)0); // unreachable
}

evaluator::val evaluator::evalI(dispatch_command &dc,const init_spec_atom *e)
{
  val v = eval(dc, e);
  if (!v.is_int())
    fatalAt(e->defined_at,"argument must be integral");
  return v;
}
evaluator::val evaluator::evalF(dispatch_command &dc,const init_spec_atom *e)
{
  val v = eval(dc, e);
  if (!v.is_float())
    fatalAt(e->defined_at,"argument must be floating point");
  return v;
}

evaluator::val evaluator::evalToF(dispatch_command &dc,const init_spec_atom *e)
{
  val v = eval(dc, e);
  if (v.is_signed()) {
    v = (double)v.s64;
  } else if (v.is_unsigned()) {
    v = (double)v.u64;
  }
  return v;
}


void evaluator::genArg(
  dispatch_command &dc,
  arg_buffer &ab,
  const refable<init_spec *> &ris,
  const arg_info &ai)
{
  const init_spec *is = ris;
  if (std::holds_alternative<type_ptr>(ai.type.var)) {
    // surface
    if (is->kind != init_spec::IS_MEM) {
      fatalAt(is->defined_at, "pointer argument requires memory initializer");
      // TODO: unless it's local*, then it must be integral
    }
    genArgSurface(dc, ab, is->defined_at, (const init_spec_memory *)is, ai);
  } else {
    // non-surface
    genArg(ab, nullptr, is, ai.type);
  }
}

static size_t computeBufferSize(
  evaluator *e,
  dispatch_command &dc,
  const type *elem_type,
  const init_spec_memory *ism)
{
  if (ism->dimension) {
    return e->evalI(dc,ism->dimension).u64;
  } else {
    size_t prod = 1;
    for (size_t i = 0; i < dc.global_size.dimensions(); i++) {
      prod *= dc.global_size.get()[i];
    }
    return prod*elem_type->size();
  }
}

template <typename T>
static void setGeneratorStateS(
  evaluator *e,
  dispatch_command &dc,
  generator_state &gs,
  const type_num &tn,
  const init_spec_rng *isr)
{
  evaluator::val v_lo = (T)0;
  evaluator::val v_hi = std::numeric_limits<T>::max();
  if (isr->e_hi) {
    v_hi = e->evalI(dc, isr->e_hi);
    if (isr->e_lo) {
      v_lo = e->evalI(dc, isr->e_lo);
    }
  }
  gs.s_dist = std::uniform_int_distribution<int64_t>(v_lo.s64, v_hi.s64);
}
template <typename T>
static void setGeneratorStateU(
  evaluator *e,
  dispatch_command &dc,
  generator_state &gs,
  const type_num &tn,
  const init_spec_rng *isr)
{
  evaluator::val v_lo = (T)0;
  evaluator::val v_hi = std::numeric_limits<T>::max();
  if (isr->e_lo) {
    v_lo = e->evalTo<T>(dc, isr->e_lo);
  }
  if (isr->e_hi) {
    v_hi = e->evalTo<T>(dc, isr->e_hi);
  }
  gs.u_dist = std::uniform_int_distribution<uint64_t>(v_lo.u64, v_hi.u64);
}
template <typename T>
static void setGeneratorStateF(
  evaluator *e,
  dispatch_command &dc,
  generator_state &gs,
  const type_num &tn,
  const init_spec_rng *isr)
{
  evaluator::val v_lo = 0.0;
  evaluator::val v_hi = 1.0;
  if (isr->e_hi) {
    v_hi = e->evalToF(dc, isr->e_hi);
    if (isr->e_lo) {
      v_lo = e->evalToF(dc, isr->e_lo);
    }
  }
  gs.f_dist = std::uniform_real_distribution<double>(v_lo.f64, v_hi.f64);
}

generator_state &evaluator::get_state(
  dispatch_command &dc,
  const init_spec_rng *isr,
  const type_num &tn)
{
  auto itr = file_gens.find(isr);
  if (itr != file_gens.end())
    return itr->second;
  auto x = file_gens.emplace(isr,isr->seed);
  generator_state &gs = x.first->second;

  switch (tn.kind) {
  case type_num::SIGNED:
    switch (tn.size_in_bytes) {
    case 1: setGeneratorStateS<int8_t >(this, dc, gs, tn, isr); break;
    case 2: setGeneratorStateS<int16_t>(this, dc, gs, tn, isr); break;
    case 4: setGeneratorStateS<int32_t>(this, dc, gs, tn, isr); break;
    case 8: setGeneratorStateS<int64_t>(this, dc, gs, tn, isr); break;
    }
    break;
  case type_num::UNSIGNED:
    switch (tn.size_in_bytes) {
    case 1: setGeneratorStateU<uint8_t >(this, dc, gs, tn, isr); break;
    case 2: setGeneratorStateU<uint16_t>(this, dc, gs, tn, isr); break;
    case 4: setGeneratorStateU<uint32_t>(this, dc, gs, tn, isr); break;
    case 8: setGeneratorStateU<uint64_t>(this, dc, gs, tn, isr); break;
    }
    break;
  case type_num::FLOATING:
    switch (tn.size_in_bytes) {
    case 2: setGeneratorStateF<float>(this, dc, gs, tn, isr); break; // use float for half
    case 4: setGeneratorStateF<float>(this, dc, gs, tn, isr); break;
    case 8: setGeneratorStateF<double>(this, dc, gs, tn, isr); break;
    }
  }
  return gs;
}
generator_state &evaluator::get_state(
  dispatch_command &dc,
  const init_spec_file *isf)
{
  auto itr = rng_gens.find(isf);
  if (itr != rng_gens.end())
    return itr->second;
  auto x = rng_gens.emplace(isf,isf->path);
  generator_state &gs = x.first->second;
  if (!gs.file.good()) {
    fatalAt(isf->defined_at,"unable to open file");
  }

  gs.file.seekg(0, gs.file.end);
  gs.file_size = gs.file.tellg();

  return gs;
}

void evaluator::genArgSurface(
  dispatch_command &dc,
  arg_buffer &ab,
  const loc &arg_loc,
  const init_spec_memory *ism,
  const arg_info &ai)
{
  const type *elem_type = std::get<type_ptr>(ai.type.var).element_type;
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
      memobj);
  }

  dc.inits.emplace_back(
    arg_loc,
    std::get<type_ptr>(ai.type.var),
    so);

  if (std::holds_alternative<type_num>(ai.type.var)) {
    const type_num &tn = std::get<type_num>(ai.type.var);
    switch (ism->root->kind) {
    case init_spec::IS_FIL: {
      (void)get_state(dc, (const init_spec_file *)ism->root);
      break;
    }
    case init_spec::IS_RND: {
      const init_spec_rng *isr =
        (const init_spec_rng *)ism->root;
      (void)get_state(dc, isr, tn);
      break;
    }
    case init_spec::IS_SEQ:
      fatalAt(ism->root->defined_at,"INTERNAL ERROR: not IS_SEQ not implemented");
      break;
    }
  } else {
    // TODO: support structures
    fatalAt(ism->root->defined_at,"argument type must be numeric for random");
  }
}


void evaluator::genArg(
  arg_buffer &ab,
  generator_state *g,
  const init_spec *is,
  const type &t)
{
  if (std::holds_alternative<type_num>(t.var)) {
    genArg(ab, g, is, std::get<type_num>(t.var));
  } else if (std::holds_alternative<type_struct>(t.var)) {
    genArg(ab, g, is, std::get<type_struct>(t.var));
  } else if (std::holds_alternative<type_ptr>(t.var)) {
    genArg(ab, g, is, std::get<type_ptr>(t.var));
  } else {
    fatalAt(is->defined_at,"unsupported argument type");
  }
}

template <typename T>
static void genArgInt(
  fatal_handler *fh,
  arg_buffer &ab,
  const loc &arg_loc,
  int64_t val64)
{
  if (std::is_unsigned<T>()) {
    if ((uint64_t)val64 > (uint64_t)std::numeric_limits<T>::max()) {
      fh->fatalAt(arg_loc,"argument too large (overflows) for formal type");
    }
  } else if (std::is_signed<T>()) {
    if (val64 > (int64_t)std::numeric_limits<T>::max()) {
      fh->fatalAt(arg_loc,"argument too large (overflows) for formal type");
    } else if (val64 < (int64_t)std::numeric_limits<T>::lowest()) {
      fh->fatalAt(arg_loc,"argument too large (overflows) for formal type");
    }
  } else {
      fh->fatalAt(arg_loc,"INTERNAL ERROR: unreachable");
  }
  ab.write<T>((T)val64);
}

void evaluator::genArg(
  arg_buffer &ab,
  generator_state *g,
  const init_spec *is,
  const type_num &tn)
{
  switch (tn.kind) {
  case type_num::UNSIGNED:
  case type_num::SIGNED:
    if (is->kind == init_spec::IS_INT) {
      int64_t val = ((const init_spec_int *)is)->value;
      if (tn.kind == type_num::UNSIGNED) {
        switch (tn.size_in_bytes) {
        case 1: genArgInt<uint8_t>(this,ab,is->defined_at,val); break;
        case 2: genArgInt<uint16_t>(this,ab,is->defined_at,val); break;
        case 4: genArgInt<uint32_t>(this,ab,is->defined_at,val); break;
        case 8: genArgInt<uint64_t>(this,ab,is->defined_at,val); break;
        default: fatalAt(is->defined_at,"INTERNAL ERROR: unexpected primitive size");
        }
      } else { // type_num::SIGNED
        switch (tn.size_in_bytes) {
        case 1: genArgInt<int8_t>(this,ab,is->defined_at,val); break;
        case 2: genArgInt<int16_t>(this,ab,is->defined_at,val); break;
        case 4: genArgInt<int32_t>(this,ab,is->defined_at,val); break;
        case 8: genArgInt<int64_t>(this,ab,is->defined_at,val); break;
        default: fatalAt(is->defined_at,"INTERNAL ERROR: unexpected primitive size");
        }
      }
    } else if (is->kind != init_spec::IS_RND) {
      const init_spec_rng *isr = (const init_spec_rng *)is;


    } else {
      fatalAt(is->defined_at,"kernel argument requires integer parameter");
    }
    break;
  case type_num::FLOATING: {
    double f64 = 0.0;
    if (is->kind != init_spec::IS_FLT) {
      f64 = ((const init_spec_float *)is)->value;
    } else if (is->kind != init_spec::IS_INT) {
      f64 = (double)((const init_spec_int *)is)->value;
    } else {
      fatalAt(is->defined_at,"kernel argument requires floating-point parameter");
    }
    switch (tn.size_in_bytes) {
    case 2: {
      if (f64 > 65504.0)
        fatalAt(is->defined_at,"kernel argument too large for half type");
      else if (f64 < -65504.0)
        fatalAt(is->defined_at,"kernel argument too low for half type");
      else if (std::abs(f64) < 5.96046e-8)
        fatalAt(is->defined_at,"kernel argument too small for half type");
      ab.write(float_to_half((float)f64));
    }
    case 4: {
      // if (f64 != (float)f64) { FAILS on repeated values
      //  fatalAt(is->defined_at,"precision loss in parameter");
      // }
      if (f64 > std::numeric_limits<float>::max())
        fatalAt(is->defined_at,"kernel argument too large for half type");
      else if (f64 < std::numeric_limits<float>::lowest())
        fatalAt(is->defined_at,"kernel argument too low for half type");
      else if (std::abs(f64) < std::numeric_limits<float>::min())
        fatalAt(is->defined_at,"kernel argument too small for half type");
      ab.write((float)f64);
      break;
    }
    case 8:
      ab.write(f64);
      break;
    default:
      fatalAt(is->defined_at,"INTERNAL ERROR: unexpected primitive size");
    }
  } // case type_num::FLOATING
  default:
    fatalAt(is->defined_at,"INTERNAL ERROR: unexpected primitive kind");
  } // switch
}

void evaluator::genArg(
  arg_buffer &ab,
  generator_state *g,
  const init_spec *is,
  const type_struct &ts)
{
  if (is->kind == init_spec::IS_REC) {
    const init_spec_record *isr = (const init_spec_record *)is;
    if (isr->children.size() != ts.elements_length) {
      fatalAt(is->defined_at,
        "structure initializer has wrong number of elements");
    }
    for (size_t i = 0; i < ts.elements_length; i++) {
      genArg(ab, g, isr->children[i], *ts.elements[i]);
    }
  } else {
    // TODO: we could support things like broadcast, random etc...
    fatalAt(is->defined_at,
      "structure argument requires structure initializer");
  }
}

void evaluator::genArg(
  arg_buffer &ab,
  generator_state *g,
  const init_spec *is,
  const type_ptr &tp)
{
  fatalAt(is->defined_at,"type_ptr not implemented as primitive yet");
}




times compiled_script::get_wall_times() const
{
  times ts;
  const compiled_script_impl *csi = (const compiled_script_impl *)impl;
  for (const dispatch_command &dc : csi->dispatches)
    ts.emplace_back(dc.ds,dc.wall_times);
  return ts;
}

times compiled_script::get_prof_times() const
{
  times ts;
  const compiled_script_impl *csi = (const compiled_script_impl *)impl;
  for (const dispatch_command &dc : csi->dispatches)
    ts.emplace_back(dc.ds,dc.prof_times);
  return ts;
}
