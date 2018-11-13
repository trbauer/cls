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

surface_object *compiled_script_impl::define_surface(
  const init_spec_mem *_spec,
  enum surface_object::skind _kind,
  size_t _size_in_bytes,
  cl_mem _mem,
  cl_command_queue _queue)
{
  return &surfaces.emplace_back(
    _spec,
    _spec,
    _kind,
    _size_in_bytes,
    _mem,
    (int)surfaces.size(),
    _queue);
}

evaluator::evaluator(compiled_script_impl *_csi)
  : interp_fatal_handler(_csi->os,_csi->input()), csi(_csi)
{
}

val evaluator::eval(
  context &ec,
  const init_spec_atom *e)
{
  // TODO: merge with evalInto<type_num>
  switch (e->skind) {
  case init_spec::IS_INT:
    return val(((const init_spec_int *)e)->value);
  case init_spec::IS_FLT:
    return val(((const init_spec_float *)e)->value);
  case init_spec::IS_SZO: {
    const init_spec_sizeof *isz = (const init_spec_sizeof *)e;
    if (isz->type_name.empty()) {
      auto itr = csi->surfaces.find(isz->mem_object);
      if (itr == csi->surfaces.find_end()) {
        internalAt(isz->mem_object.defined_at,"undefined memory object");
      }
      const surface_object *so = itr->second;
      return val(so->size_in_bytes);
    } else {
      const type *t = lookupPrimtiveType(isz->type_name);
      return val(t->size());
    }
  }
  case init_spec::IS_BEX: {
    const init_spec_bex *be = ((const init_spec_bex *)e);
    val vl = eval(ec, be->e_l),
        vr = eval(ec, be->e_r);
    return be->e_op.apply(this,be->defined_at,vl,vr);
  } // binary expression
  case init_spec::IS_UEX: {
    const init_spec_uex *ue = ((const init_spec_uex *)e);
    val v = eval(ec,ue->e);
    return ue->e_op.apply(this, ue->defined_at, v);
  } // end case IS_UEX:
  case init_spec::IS_BIV: {
    auto computeDim =
      [&] (const ndr &ndr, size_t dim_ix) {
        if (dim_ix >= ndr.rank())
          fatalAt(e->defined_at,
            &ndr == &ec.global_size ? "global" : "local",
            " dimension size is out of bounds for this dispatch");
        return ndr.get()[dim_ix];
      };
    switch (((const init_spec_builtin *)e)->skind) {
    case init_spec_builtin::BIV_GX:   return computeDim(ec.global_size, 0);
    case init_spec_builtin::BIV_GY:   return computeDim(ec.global_size, 1);
    case init_spec_builtin::BIV_GZ:   return computeDim(ec.global_size, 2);
    case init_spec_builtin::BIV_LX:   return computeDim(ec.local_size,  0);
    case init_spec_builtin::BIV_LY:   return computeDim(ec.local_size,  1);
    case init_spec_builtin::BIV_LZ:   return computeDim(ec.local_size,  2);
    default: fatalAt(e->defined_at,"unsupported built-in variable");
    }
    break;
  }
  default: fatalAt(e->defined_at,"unsupported expression for primitive"); break;
  }
  return val((uint64_t)0); // unreachable
}

val evaluator::evalI(context &ec,const init_spec_atom *e)
{
  val v = eval(ec, e);
  if (!v.is_int())
    fatalAt(e->defined_at,"argument must be integral");
  return v;
}
val evaluator::evalF(context &ec,const init_spec_atom *e)
{
  val v = eval(ec, e);
  if (!v.is_float())
    fatalAt(e->defined_at,"argument must be floating point");
  return v;
}
val evaluator::evalToF(context &ec,const init_spec_atom *e)
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
  const refable<init_spec> &ris,
  const arg_info &ai)
{
  if (os.verbosity >= 2) {
    debug(ris.defined_at, "setting immediate argument for ",
      ai.type.syntax()," ",ai.name);
  }

    // non-surface
  context ec(dc.global_size,dc.local_size, &ss);
  const init_spec *is = ris;

  arg_buffer ab(this, ris.defined_at, ai.type.size());

  evalInto(ec, is->defined_at, (const init_spec_atom *)is, ab, ai.type);

  if (ab.num_left() != 0) {
    internalAt(ris.defined_at, "failed to set full argument");
  }

  CL_COMMAND(
    ris.defined_at, // use the arg actual location, not the let
    clSetKernelArg,
      (*dc.kernel->kernel)(),
      arg_index,
      ab.size(),
      (const void *)ab.ptr());

  if (os.verbosity >= 2) {
    std::cout << " ==> ARG " << ai.type.syntax() << " "  << ai.name << " = ";
    format(std::cout, ab.base, ab.capacity, ai.type);
    std::cout << "\n";
  }
}

static size_t computeBufferSize(
  evaluator *e,
  dispatch_command &dc,
  const type &elem_type,
  const init_spec_mem *ism)
{
  if (ism->dimension) {
    return (size_t)e->evalTo<size_t>(
      evaluator::context(dc.global_size, dc.local_size), ism->dimension).u64;
  } else {
    return dc.global_size.product()*elem_type.size();
  }
}

void evaluator::setKernelArgMemobj(
  cl_uint arg_index,
  dispatch_command &dc,
  std::stringstream &ss,
  const loc &at,
  const refable<init_spec> &ris,
  const arg_info &ai)
{
  if (os.verbosity >= 2) {
    debug(ris.defined_at, "setting memory object argument for ",
      ai.type.syntax()," ",ai.name);
  }

  if (((const init_spec *)ris)->skind != init_spec::IS_MEM) {
    fatalAt(ris.defined_at, "expected surface initializer");
  }
  const init_spec_mem *ism =
    (const init_spec_mem *)(const init_spec *)ris;
  if (!ai.type.is<type_ptr>()) {
    fatalAt(ism->defined_at, "buffer/image requires pointer type");
  }
  const type &elem_type = *ai.type.as<type_ptr>().element_type;
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
      (ism->access_properties & init_spec_mem::INIT_SPEC_MEM_READ);
    bool is_w =
      (ism->access_properties & init_spec_mem::INIT_SPEC_MEM_WRITE);
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
    CL_COMMAND_CREATE(memobj, at,
      clCreateBuffer,
        context,
        cl_mfs,
        buffer_size,
        nullptr);
    so = csi->define_surface(
      ism,
      surface_object::SO_BUFFER,
      buffer_size,
      memobj,
      (*dc.dobj->queue)());
  }
  ss << "MEM[" << so->memobj_index << "] (" << so->size_in_bytes << " B)";
  //
  dc.surfaces.emplace_back(so, *ai.type.as<type_ptr>().element_type, ai, at);
  //
  so->dispatch_uses.emplace_back(&dc, arg_index, ai);
  //
  CL_COMMAND(at,
    clSetKernelArg,
      (*dc.kernel->kernel)(),
      arg_index,
      sizeof(cl_mem),
      (const void *)&so->memobj);
  if (os.verbosity >= 2) {
    std::cout << " ==> ARG " << ai.type.syntax() << " "  << ai.name << " = " << so->str() << "\n";
  }
}

void evaluator::setKernelArgSLM(
  cl_uint arg_index,
  dispatch_command &dc,
  std::stringstream &ss, // debug string for arg
  const refable<init_spec> &ris,
  const arg_info &ai)
{
  if (os.verbosity >= 2) {
    debug(ris.defined_at, "setting SLM size for ",
      ai.type.syntax()," ",ai.name);
  }

  const init_spec *is = ris;
  // Special treatment of local * arguments
  // e.g. kernel void foo(..., local int2 *buffer)
  // the user must tell us how many bytes they neeed for buffer
  //  foo<1024,16>(...,16*8); // 16*sizeof(int2)
  //
  //  SPECIFY: do we allow the alternative?
  //     foo<1024,16>(...,0:rw); // assume 1 int2 per work item
  if (!ai.type.is<type_ptr>()) {
    fatalAt(
      ris.defined_at,
      "kernel argument in local address space must be pointer type");
  } else if (!is->is_atom()) {
    fatalAt(
      ris.defined_at,
      "local pointer requires size in bytes");
  } // SPECIFY: see above  (use tp.element_type->size() * wg-size)
  const type_ptr &tp = ai.type.as<type_ptr>();
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
  if (os.verbosity >= 2) {
    std::cout << " ==> ARG local " << ai.type.syntax() << " " << ai.name << " = " << local_bytes << " B\n";
  }
}

void evaluator::evalInto(
  context &ec,
  const loc &at,
  const init_spec_atom *is,
  arg_buffer &ab,
  const type &t)
{
  if (t.is<type_num>()) {
    evalInto(ec, at, is, ab, t.as<type_num>());
  } else if (t.is<type_struct>()) {
    evalInto(ec, at, is, ab, t.as<type_struct>());
  } else if (t.is<type_ptr>()) {
    evalInto(ec, at, is, ab, t.as<type_ptr>());
  } else {
    fatalAt(is->defined_at,"unsupported argument type");
  }
}

void evaluator::evalInto(
  context &ec,
  const loc &at,
  const init_spec_atom *is,
  arg_buffer &ab,
  const type_num &tn)
{
  // is could be 4, 4*g.x, or other stuff
  switch (tn.skind) {
  case type_num::SIGNED:
    switch (tn.size_in_bytes) {
    case 1: evalIntoT<int8_t>(ec,at,is,ab); break;
    case 2: evalIntoT<int16_t>(ec,at,is,ab); break;
    case 4: evalIntoT<int32_t>(ec,at,is,ab); break;
    case 8: evalIntoT<int64_t>(ec,at,is,ab); break;
    default: internalAt(at,"unreachable");
    }
    break;
  case type_num::UNSIGNED:
    switch (tn.size_in_bytes) {
    case 1: evalIntoT<uint8_t>(ec,at,is,ab); break;
    case 2: evalIntoT<uint16_t>(ec,at,is,ab); break;
    case 4: evalIntoT<uint32_t>(ec,at,is,ab); break;
    case 8: evalIntoT<uint64_t>(ec,at,is,ab); break;
    default: internalAt(at,"unreachable");
    }
    break;
  case type_num::FLOATING:
    switch (tn.size_in_bytes) {
    case 2: evalIntoT<half>(ec,at,is,ab); break;
    case 4: evalIntoT<float>(ec,at,is,ab); break;
    case 8: evalIntoT<double>(ec,at,is,ab); break;
    default: internalAt(at,"unreachable");
    }
    break;
  default: internalAt(at,"unreachable");
  }
}

template <typename T>
void evaluator::evalIntoT(
  context &ec,
  const loc &at,
  const init_spec_atom *is,
  arg_buffer &ab)
{
  switch (is->skind) {
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
  case init_spec::IS_SYM: fatalAt(at,"unbound symbol");
  case init_spec::IS_REC: fatalAt(at,"vector initializer passed to scalar");
  case init_spec::IS_FIL:
  case init_spec::IS_RND:
  default: internalAt(at,"unreachable");
  }
}

void evaluator::evalInto(
  context &ec,
  const loc &at,
  const init_spec_atom *is,
  arg_buffer &ab,
  const type_struct &ts)
{
  if (is->skind == init_spec::IS_REC) {
    const init_spec_record *isr = (const init_spec_record *)is;
    if (isr->children.size() != ts.elements_length) {
      fatalAt(at, "structure initializer has wrong number of elements");
    }
    ec.evaluated("{");
    for (size_t i = 0; i < ts.elements_length; i++) {
      if (i > 0)
        ec.evaluated(",");
      evalInto(ec, at, isr->children[i], ab, *ts.elements[i]);
    }
    ec.evaluated("}");
  } else {
    // TODO: we could support things like broadcast, random etc...
    fatalAt(at,"structure argument requires structure initializer");
  }
}

void evaluator::evalInto(
  context &ec,
  const loc &at,
  const init_spec_atom *is,
  arg_buffer &ab,
  const type_ptr &tp)
{
  fatalAt(is->defined_at,"type_ptr not implemented as primitive yet");
}
