#include "cls_interp.hpp"
#include "cls_interp_internal.hpp"
#include "../devices.hpp"
#include "../image.hpp"
#include "../system.hpp"
#include "../text.hpp"

#include <cmath>
#include <functional>
#include <map>
#include <numeric>
#include <tuple>
#include <type_traits>


using namespace cls;

compiled_script_impl::compiled_script_impl(
  diagnostics &_ds, const opts &_os, const script &_s)
  : cl_interface(_ds, _s.source)
  , os(_os)
  , s(_s)
  , e(new evaluator(this)) { }

compiled_script_impl::~compiled_script_impl()
{
  // deletes OpenCL driver resources
  for (surface_object *so : surfaces) {
    CL_COMMAND(so->init->defined_at,
      clReleaseMemObject,
        so->memobj);
  }
  for (kernel_object *ko : kernels) {
    CL_COMMAND(ko->spec->defined_at,
      clReleaseKernel,
        ko->kernel);
  }
  for (program_object *po : programs) {
    CL_COMMAND(po->spec->defined_at,
      clReleaseProgram,
        po->program);
  }
  for (device_object *dobj : devices) {
    CL_COMMAND(dobj->spec->defined_at,
      clReleaseCommandQueue,
        dobj->queue);
    CL_COMMAND(dobj->spec->defined_at,
      clReleaseContext,
        dobj->context);

    delete dobj->cl;
    dobj->cl = nullptr;

    if (dobj->md) {
      delete dobj->md;
      dobj->md = nullptr;
    }
  }

  for (const auto &s : samplers) {
    CL_COMMAND(std::get<0>(s),
      clReleaseSampler,
        std::get<1>(s));
  }

  delete e;

  surfaces.clear();
  kernels.clear();
  programs.clear();
  devices.clear();
  dispatches.clear();
  samplers.clear();
}
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
  : cl_interface(
    _csi->get_diagnostics(),
    _csi->get_diagnostics().input())
  , csi(_csi)
{
}

val evaluator::eval(
  context &ec,
  const init_spec_atom *e)
{
  // TODO: merge with eval_into<type_num>
  switch (e->skind) {
  case init_spec::IS_INT:
    return val(((const init_spec_int *)e)->value);
  case init_spec::IS_FLT:
    return val(((const init_spec_float *)e)->value);
  case init_spec::IS_SZO: {
    const init_spec_sizeof *isz = (const init_spec_sizeof *)e;
    if (isz->type_name.empty()) {
      // sizeof( EXPR )
      auto itr = csi->surfaces.find(isz->mem_object);
      if (itr == csi->surfaces.find_end())
        internal_at(isz->mem_object.defined_at,"undefined memory object");
      const surface_object *so = itr->second;
      return val(so->size_in_bytes);
    } else {
      // sizeof( TYPE )
      if (ec.sizeof_pointer == 0) {
        // We don't know the size of a pointer: sizeof(int*) or
        // sizeof(image2d_t).  If you hit this case consider examining
        // where the evaluator::context is constructed and passing the
        // pointer size there.
        internal_at(e->defined_at,
          "use a type name in context where size of pointer is unknown");
      }
      const type *t = lookup_builtin_type(isz->type_name, 0);
      if (t == nullptr)
        fatal_at(e->defined_at, "unrecognized type name");
      return val(t->size());
    }
  }
  case init_spec::IS_BEX: {
    const init_spec_bex *be = ((const init_spec_bex *)e);
    val vl = eval(ec, be->e_l),
        vr = eval(ec, be->e_r);
    return be->e_op.apply(get_diagnostics(), be->defined_at, vl, vr);
  } // binary expression
  case init_spec::IS_UEX: {
    const init_spec_uex *ue = ((const init_spec_uex *)e);
    val v = eval(ec, ue->e);
    return ue->e_op.apply(get_diagnostics(), ue->defined_at, v);
  } // end case IS_UEX:
  case init_spec::IS_BIV: {
    auto compute_dim =
      [&] (const ndr &ndr, size_t dim_ix) {
        if (dim_ix >= ndr.rank())
          fatal_at(e->defined_at,
            &ndr == &ec.global_size ? "global" : "local",
            " dimension size is out of bounds for this dispatch");
        return ndr.get()[dim_ix];
      };
    switch (((const init_spec_builtin *)e)->skind) {
    case init_spec_builtin::BIV_GX: return compute_dim(ec.global_size, 0);
    case init_spec_builtin::BIV_GY: return compute_dim(ec.global_size, 1);
    case init_spec_builtin::BIV_GZ: return compute_dim(ec.global_size, 2);
    case init_spec_builtin::BIV_LX: return compute_dim(ec.local_size,  0);
    case init_spec_builtin::BIV_LY: return compute_dim(ec.local_size,  1);
    case init_spec_builtin::BIV_LZ: return compute_dim(ec.local_size,  2);
    default: fatal_at(e->defined_at, "unsupported built-in variable");
    }
    break;
  }
  case init_spec::IS_SYM: {
    if (csi) {
      init_spec_symbol *iss = (init_spec_symbol *)e;
      std::map<std::string,cls::let_spec*> m = csi->s.let_bindings;
      auto itr = m.find(iss->identifier);
      if (itr != m.end()) {
        let_spec *ls = itr->second;
        if (ls->value->skind == spec::INIT_SPEC) {
          init_spec *is = (init_spec *)ls->value;
          if (is->is_atom())
            return eval(ec, (init_spec_atom *)is);
        }
      }
      internal_at(e->defined_at,
        __FILE__, ":", __LINE__,
        ": unable to bind init_spec::IS_SYM with symbol ", iss->identifier);
    } else {
      internal_at(e->defined_at,
        __FILE__, ":", __LINE__, ": no compiled script attached to evaluator");
    }
  }
  default:
    internal_at(e->defined_at,
      __FILE__, ":", __LINE__, ": unsupported primitive expression");
  }
  return val((uint64_t)0); // unreachable
}

val evaluator::eval_i(context &ec, const init_spec_atom *e)
{
  val v = eval(ec, e);
  if (!v.is_integral())
    fatal_at(e->defined_at, "argument must be integral");
  return v;
}
val evaluator::eval_f(context &ec, const init_spec_atom *e)
{
  val v = eval(ec, e);
  if (!v.is_floating())
    fatal_at(e->defined_at, "argument must be floating point");
  return v;
}
val evaluator::eval_to_f(context &ec, const init_spec_atom *e)
{
  val v = eval(ec, e);
  if (v.is_signed()) {
    v = (double)v.s64;
  } else if (v.is_unsigned()) {
    v = (double)v.u64;
  }
  return v;
}


void evaluator::set_kernel_arg_immediate(
  cl_uint arg_index,
  dispatch_command &dc,
  std::stringstream &ss,
  const refable<init_spec> &ris,
  const arg_info &ai)
{
  debug_at(ris.defined_at, "setting immediate argument for ",
    ai.arg_type->syntax()," ",ai.name);

    // non-surface
  context ec(dc, &ss);
  const init_spec *is = ris;

  arg_buffer ab(get_diagnostics(), ris.defined_at, ai.arg_type->size());

  eval_into(ec, is->defined_at, (const init_spec_atom *)is, ab, *ai.arg_type);

  if (ab.num_left() != 0) {
    internal_at(ris.defined_at, "failed to set full argument");
  }

  CL_COMMAND(
    ris.defined_at, // use the arg actual location, not the let
    clSetKernelArg,
      dc.kernel->kernel,
      arg_index,
      ab.size(),
      (const void *)ab.ptr());

  if (is_debug()) {
    std::cout << " ==> ARG " << ai.arg_type->syntax() << " "  << ai.name << " = ";
    format(std::cout, ab.base, ab.capacity, *ai.arg_type);
    std::cout << "\n";
  }
}

static cl_mem_flags init_cl_mem_flags(const init_spec_mem *ism)
{
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
  return cl_mfs;
}

void evaluator::set_kernel_arg_buffer(
  cl_uint arg_index,
  dispatch_command &dc,
  std::stringstream &ss,
  const loc &at,
  const refable<init_spec> &ris,
  const arg_info &ai)
{
  debug_at(ris.defined_at,
    "setting memory object argument for ",
    ai.arg_type->syntax(), " ", ai.name);

  if (((const init_spec *)ris)->skind != init_spec::IS_MEM) {
    fatal_at(ris.defined_at, "expected surface initializer");
  }
  const init_spec_mem *ism =
    (const init_spec_mem *)(const init_spec *)ris;
  if (!ai.arg_type->is<type_ptr>()) {
    fatal_at(ism->defined_at, "buffer/image requires pointer type");
  }
  const type &elem_type = *ai.arg_type->as<type_ptr>().element_type;
  size_t buffer_size = 0;
  if (ism->dimension) {
    evaluator::context ec(dc);
    buffer_size = (size_t)eval_to<size_t>(ec, ism->dimension).u64;
  } else {
    buffer_size = dc.global_size.product()*elem_type.size();
  }

  surface_object *so = nullptr;
  auto itr = csi->surfaces.find(ism);
  if (itr != csi->surfaces.find_end()) {
    // ensure we fit with this object
    so = itr->second;
    if (so->size_in_bytes != buffer_size) {
      fatal_at(
        ism->defined_at,
        "buffer/image size differs from uses "
        "(see line ", so->init->defined_at.line, ")");
    }
  } else {
    // creating a new surface (buffer)
    cl_mem_flags cl_mfs = init_cl_mem_flags(ism);

    cl_mem memobj = nullptr;
    cl_context context = dc.kernel->program->device->context;
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
      dc.dobj->queue);
  }
  ss << "MEM[" << so->memobj_index << "] (" << so->size_in_bytes << " B)";
  //
  dc.surfaces.emplace_back(so, *ai.arg_type->as<type_ptr>().element_type, ai, at);
  //
  so->dispatch_uses.emplace_back(&dc, arg_index, ai);
  //
  CL_COMMAND(at,
    clSetKernelArg,
      dc.kernel->kernel,
      arg_index,
      sizeof(cl_mem),
      (const void *)&so->memobj);
  debug_at(at,
    " ==> ARG ", ai.arg_type->syntax(), " ", ai.name, " = ", so->str());
}


  /*
  switch (isi->ch_order) {
  case init_spec_image::I:    img_fmt.image_channel_order = CL_INTENSITY; break;
  case init_spec_image::L:    img_fmt.image_channel_order = CL_LUMINANCE; break;
  case init_spec_image::D:    img_fmt.image_channel_order = CL_DEPTH; break;
  case init_spec_image::R:    img_fmt.image_channel_order = CL_R; break;
  case init_spec_image::Rx:   img_fmt.image_channel_order = CL_Rx; break;
  case init_spec_image::RG:   img_fmt.image_channel_order = CL_RG; break;
  case init_spec_image::RGx:  img_fmt.image_channel_order = CL_RGx; break;
  case init_spec_image::RGB:  img_fmt.image_channel_order = CL_RGB; break;
  case init_spec_image::RGBx: img_fmt.image_channel_order = CL_RGBx; break;
  case init_spec_image::RGBA: img_fmt.image_channel_order = CL_RGBA; break;
  case init_spec_image::ARGB: img_fmt.image_channel_order = CL_ARGB; break;
  case init_spec_image::BGRA: img_fmt.image_channel_order = CL_BGRA; break;
  case init_spec_image::sRGB: img_fmt.image_channel_order = CL_sRGB; break;
  case init_spec_image::sRGBx: img_fmt.image_channel_order = CL_sRGBx; break;
  case init_spec_image::sRGBA: img_fmt.image_channel_order = CL_sRGBA; break;
  case init_spec_image::sBGRA: img_fmt.image_channel_order = CL_sBGRA; break;
  default: fatal_at(ism->defined_at, "invalid channel order");
  }
*/

static void channel_info(
  evaluator *e,
  loc at,
  init_spec_image::channel_order ch_order,
  cl_channel_order *cl_ch_order,
  int *num_channels,
  image::data_format *native_format)
{
  switch (ch_order) {
  case init_spec_image::I:
    if (cl_ch_order)
      *cl_ch_order = CL_INTENSITY;
    if (num_channels)
      *num_channels = 1;
    if (native_format)
      *native_format = image::data_format::I;
    break;
  case init_spec_image::L:
    if (cl_ch_order)
      *cl_ch_order = CL_LUMINANCE;
    if (num_channels)
      *num_channels = 1;
    if (native_format)
      *native_format = image::data_format::I;
    break;
  case init_spec_image::D:
    if (cl_ch_order)
      *cl_ch_order = CL_DEPTH;
    if (num_channels)
      *num_channels = 1;
    if (native_format)
      *native_format = image::data_format::I;
    break;

  case init_spec_image::R:
    if (cl_ch_order)
      *cl_ch_order = CL_R;
    if (num_channels)
      *num_channels = 1;
    if (native_format)
      *native_format = image::data_format::I;
    break;

  case init_spec_image::Rx:
    if (cl_ch_order)
      *cl_ch_order = CL_Rx;
    if (num_channels)
      *num_channels = 1;
    if (native_format)
      *native_format = image::data_format::I;
    break;

  case init_spec_image::RG:
    if (cl_ch_order)
      *cl_ch_order = CL_RG;
    if (num_channels)
      *num_channels = 2;
    if (native_format)
      *native_format = image::data_format::INVALID;
    break;
  case init_spec_image::RGx:
    if (cl_ch_order)
      *cl_ch_order = CL_RGx;
    if (num_channels)
      *num_channels = 2;
    if (native_format)
      *native_format = image::data_format::INVALID;
    break;
  case init_spec_image::RGB:
    if (cl_ch_order)
      *cl_ch_order = CL_RGB;
    if (num_channels)
      *num_channels = 3;
    if (native_format)
      *native_format = image::data_format::RGB;
    break;

  case init_spec_image::RGBx:
    if (cl_ch_order)
      *cl_ch_order = CL_RGBx;
    if (num_channels)
      *num_channels = 3;
    if (native_format)
      *native_format = image::data_format::RGB;
    break;
  case init_spec_image::sRGB:
    if (cl_ch_order)
      *cl_ch_order = CL_sRGB;
    if (num_channels)
      *num_channels = 3;
    if (native_format)
      *native_format = image::data_format::RGB;
    break;
  case init_spec_image::sRGBx:
    if (cl_ch_order)
      *cl_ch_order = CL_sRGBx;
    if (num_channels)
      *num_channels = 3;
    if (native_format)
      *native_format = image::data_format::RGB;
    break;
  case init_spec_image::RGBA:
    if (cl_ch_order)
      *cl_ch_order = CL_RGBA;
    if (num_channels)
      *num_channels = 4;
    if (native_format)
      *native_format = image::data_format::RGBA;
    break;
  case init_spec_image::ARGB:
    if (cl_ch_order)
      *cl_ch_order = CL_ARGB;
    if (num_channels)
      *num_channels = 4;
    if (native_format)
      *native_format = image::data_format::ARGB;
    break;
  case init_spec_image::BGRA:
    if (cl_ch_order)
      *cl_ch_order = CL_BGRA;
    if (num_channels)
      *num_channels = 4;
    if (native_format)
      *native_format = image::data_format::INVALID;
    break;
  case init_spec_image::sRGBA:
    if (cl_ch_order)
      *cl_ch_order = CL_sRGBA;
    if (num_channels)
      *num_channels = 4;
    if (native_format)
      *native_format = image::data_format::RGBA;
    break;
  case init_spec_image::sBGRA:
    if (cl_ch_order)
      *cl_ch_order = CL_sBGRA;
    if (num_channels)
      *num_channels = 4;
    if (native_format)
      *native_format = image::data_format::INVALID;
    break;
  default: e->internal_at(at, "invalid channel order");
  }
}

size_t cls::channels_per_pixel(cl_channel_order co)
{
  switch (co) {
  case CL_A:
  case CL_R:
  case CL_Rx:
  case CL_INTENSITY:
  case CL_LUMINANCE:
  case CL_DEPTH:
    return 1;
  case CL_RG:
  case CL_RGx:
  case CL_RA:
    return 2;
  case CL_sRGB:
  case CL_RGB:
  case CL_RGBx:
  case CL_UNORM_SHORT_565:
  case CL_UNORM_SHORT_555:
  case CL_UNORM_INT_101010:
    return 3;
  case CL_RGBA:
  case CL_BGRA:
  case CL_ARGB:
  case CL_sRGBA:
  case CL_sBGRA:
  case CL_UNORM_INT_101010_2:
    return 4;
  default:
    return 0;
  }
}
static size_t channels_per_pixel(evaluator *e, loc at, cl_channel_order co)
{
  size_t cpp = channels_per_pixel(co);
  if (cpp == 0) {
    e->internal_at(at, "unsupported channel order");
  }
  return cpp;
}

size_t cls::bytes_per_channel(cl_channel_type ct)
{
  switch (ct) {
  case CL_SNORM_INT8:
  case CL_UNORM_INT8:
  case CL_SIGNED_INT8:
  case CL_UNSIGNED_INT8:
    return 1;
  case CL_SNORM_INT16:
  case CL_UNORM_INT16:
  case CL_SIGNED_INT16:
  case CL_UNSIGNED_INT16:
  case CL_HALF_FLOAT:
    return 2;
  case CL_UNORM_INT24:
    return 3;
  case CL_FLOAT:
  case CL_SIGNED_INT32:
  case CL_UNSIGNED_INT32:
    return 4;
  default:
    return 0;
  }
}
static size_t bytes_per_channel(evaluator *e, loc at, cl_channel_type ct)
{
  size_t bpc = bytes_per_channel(ct);
  if (bpc == 0) {
    e->fatal_at(at, "unsupported channel data type");
  }
  return bpc;

}

#if 0

// e.g. 0:w
//
// we disable this for now
//
// it's not that much work for them to give us an explicit image size
// and enabling this means funky semantics such as global size being
// the image size
static void populate_image_info_from_const(
  evaluator *e, loc at,
  cl_image_format &img_fmt,
  cl_image_desc &img_desc)
{
    // an image defined as zeros enables us to choose all the image attributes
    // e.g. "0:w"
    img_fmt.image_channel_order = CL_RGBA;
    img_fmt.image_channel_data_type = CL_UNORM_INT8;
    switch (tbi.skind) {
    case type_builtin::IMAGE1D:
      img_desc.image_type = CL_MEM_OBJECT_IMAGE1D;
      if (dc.global_size.rank() != 1) {
        fatal_at(ism->defined_at, "image1d_t's require a 1-dimension NDRange");
      }
      img_desc.image_width = dc.global_size.product();
      break;
    case type_builtin::IMAGE2D:
      img_desc.image_type = CL_MEM_OBJECT_IMAGE2D;
      if (dc.global_size.rank() != 2) {
        fatal_at(ism->defined_at, "image2d_t's require a 2-dimension NDRange");
      }
      img_desc.image_width = dc.global_size.get()[0];
      img_desc.image_height = dc.global_size.get()[1];
      break;
    case type_builtin::IMAGE3D:
      img_desc.image_type = CL_MEM_OBJECT_IMAGE3D;
      if (dc.global_size.rank() != 3) {
        fatal_at(ism->defined_at, "image3d_t's require a 3-dimension NDRange");
      }
      img_desc.image_width = dc.global_size.get()[0];
      img_desc.image_height = dc.global_size.get()[1];
      img_desc.image_depth = dc.global_size.get()[2];
      break;
    case type_builtin::IMAGE1D_ARRAY:
    case type_builtin::IMAGE2D_ARRAY:
      fatal_at(ism->defined_at,
        "image arrays cannot be default-initialized");
    default:
      fatal_at(ism->defined_at,
        "unsupported image kernel argument type");
    }
}

#endif


// suppose "foo.bmp" is a 1024x768 image
// CASES:                           image size      initial data
// image<rgb,u8>             ->      ERROR needs dimension (could infer global size, but too bad)
// image<rgb,u8,640x480>     ->      640x480
// image<rgb,u8>("foo.bmp")  ->      1024x768
// image<rgb,u8,640x480>("foo.bmp")
// image<rgb,u8>("foo.dat")  ->      ERROR raw data needs dimension
// image<rgb,u8,640x480>("foo.dat")
void evaluator::set_kernel_arg_image(
  cl_uint arg_index,
  dispatch_command &dc,
  std::stringstream &ss,
  const loc &at,
  const refable<init_spec> &ris,
  const arg_info &ai)
{
  debug_at(ris.defined_at, "setting memory object argument for ",
    ai.arg_type->syntax()," ",ai.name);

  if (((const init_spec *)ris)->skind != init_spec::IS_MEM) {
    fatal_at(ris.defined_at, "expected image surface initializer");
  }
  const init_spec_mem *ism =
    (const init_spec_mem *)(const init_spec *)ris;
  if (!ai.arg_type->is<type_builtin>()) {
    fatal_at(ism->defined_at, "image requires image type (e.g. image2d_t)");
  }
  const type_builtin &tbi = ai.arg_type->as<type_builtin>();

  cl_image_format img_fmt{0};
  cl_image_desc img_desc{0};
  if (ism->root->skind != init_spec::IS_IMG) {
    fatal_at(ism->defined_at, "image requires image initializer");
  }

  const init_spec_image *isi = (const init_spec_image*)ism->root;

  int chs_per_px;
  image::data_format native_format;
  channel_info(this,
    isi->defined_at,isi->ch_order,
    &img_fmt.image_channel_order,
    &chs_per_px,
    &native_format);

  // TODO: merge with with bytes_per_channel (channelDataTypeInfo)
  switch (isi->ch_data_type) {
  case init_spec_image::U8:
    img_fmt.image_channel_data_type = CL_UNSIGNED_INT8;
    break;
  case init_spec_image::U16:
    img_fmt.image_channel_data_type = CL_UNSIGNED_INT16;
    break;
  case init_spec_image::U32:
    img_fmt.image_channel_data_type = CL_UNSIGNED_INT32;
    break;
  case init_spec_image::S8:
    img_fmt.image_channel_data_type = CL_SIGNED_INT8;
    break;
  case init_spec_image::S16:
    img_fmt.image_channel_data_type = CL_SIGNED_INT16;
    break;
  case init_spec_image::S32:
    img_fmt.image_channel_data_type = CL_SIGNED_INT32;
    break;
  //
  case init_spec_image::SN8:
    img_fmt.image_channel_data_type = CL_SNORM_INT8;
    break;
  case init_spec_image::SN16:
    img_fmt.image_channel_data_type = CL_SNORM_INT16;
    break;
  case init_spec_image::UN8:
    img_fmt.image_channel_data_type = CL_UNORM_INT8;
    break;
  case init_spec_image::UN16:
    img_fmt.image_channel_data_type = CL_UNORM_INT16;
    break;
  case init_spec_image::UN565:
    img_fmt.image_channel_data_type = CL_UNORM_SHORT_565;
    break;
  case init_spec_image::UN555:
    img_fmt.image_channel_data_type = CL_UNORM_SHORT_555;
    break;
  case init_spec_image::UN101010:
    img_fmt.image_channel_data_type = CL_UNORM_INT_101010;
    break;
  case init_spec_image::UN101010_2:
    img_fmt.image_channel_data_type = CL_UNORM_INT_101010_2;
    break;
  //
  case init_spec_image::F16:
    img_fmt.image_channel_data_type = CL_HALF_FLOAT;
    break;
  case init_spec_image::F32:
    img_fmt.image_channel_data_type = CL_FLOAT;
    break;
  default: fatal_at(ism->defined_at, "invalid channel data type");
  }
  size_t bytes_per_chan =
    bytes_per_channel(this, ism->defined_at, img_fmt.image_channel_data_type);

  // evaluate any dimensions that are given
  context ctx(dc);
  size_t img_width = 0;
  if (isi->width)
    img_width = (size_t)eval_to<size_t>(ctx,isi->width).u64;
  size_t img_row_pitch = 0;
  if (isi->row_pitch)
    img_row_pitch = (size_t)eval_to<size_t>(ctx,isi->row_pitch).u64;
  size_t img_height = 0;
  if (isi->height)
    img_height = (size_t)eval_to<size_t>(ctx,isi->height).u64;
  size_t img_slice_pitch = 0;
  if (isi->slice_pitch)
    img_slice_pitch = (size_t)eval_to<size_t>(ctx,isi->slice_pitch).u64;
  size_t img_depth = 0;
  if (isi->depth)
    img_depth = (size_t)eval_to<size_t>(ctx,isi->depth).u64;

  void *image_arg_data = nullptr;
  if (!isi->path.empty()) {
    if (!sys::file_exists(isi->path)) {
        fatal_at(ism->defined_at, "file not found");
    }
    auto ext = sys::take_extension(isi->path);
    if (ext == ".dat") { // raw
      if (img_width == 0)
        fatal_at(ism->defined_at, "raw image data requires explicit dimensions");
      // this doesn't take into account pitch or anything...
      size_t total_pixels = img_width;
      if (img_height)
        total_pixels *= img_height;
      if (img_depth)
        total_pixels *= img_depth;

      auto binary = sys::read_file_binary(isi->path);
      if (total_pixels * chs_per_px * bytes_per_chan != binary.size()) {
        fatal_at(ism->defined_at,
          "raw image file is wrong size for given image dimensions");
      }
      image_arg_data = malloc(binary.size());
      memcpy(image_arg_data, binary.data(), binary.size());
    } else {
      image *img = nullptr;
      if (ext == ".ppm" || ext == ".pp3" || ext == ".pp6") {
        img = image::load_ppm(isi->path.c_str(), false);
#ifdef IMAGE_HPP_SUPPORTS_PNG
      } else if (ext == ".png") {
        img = image::load_png(isi->path.c_str(), false);
#endif
#ifdef IMAGE_HPP_SUPPORTS_BMP
      } else if (ext == ".bmp") {
        img = image::load_bmp(isi->path.c_str(), false);
#endif
      } else {
        std::stringstream msg;
        msg << "invalid file type for image;  the following are supported: ";
        msg << ".dat (raw bits), .ppm";
#ifdef IMAGE_HPP_SUPPORTS_PNG
        msg << ", .png";
#endif
#ifdef IMAGE_HPP_SUPPORTS_BMP
        msg << ", .bmp";
#endif
        fatal_at(ism->defined_at,msg.str());
      }

      if (!img)
        fatal_at(isi->defined_at, "failed to load image");

      if (img_width == 0)
        img_width = img->width;
      else if (img->width != img_width)
        fatal_at(ism->defined_at, "image width mismatches actual image");

      if (img_height == 0)
        img_height = img->height;
      else if (img->height != img_height)
        fatal_at(ism->defined_at, "image height mismatches actual image");

      if (img_depth > 0 && img_depth != 1)
        fatal_at(ism->defined_at, "image depth must be 0 or 1");

      size_t bpp = 0;
      image icvt;
      switch (isi->ch_order) {
      case init_spec_image::I:
      case init_spec_image::L:
      case init_spec_image::D:
      case init_spec_image::R:
      // case init_spec_image::Rx: is this like RG or R?
        bpp = image::bytes_per_pixel(image::I);
        icvt = img->convert(image::I);
        break;
      case init_spec_image::RGB:
      case init_spec_image::sRGB:
      // case init_spec_image::sRGBx:
      // case init_spec_image::RGBx:
        bpp = image::bytes_per_pixel(image::RGB);
        icvt = img->convert(image::RGB);
        break;
      case init_spec_image::RGBA:
      case init_spec_image::sRGBA:
        bpp = image::bytes_per_pixel(image::RGBA);
        icvt = img->convert(image::RGBA);
        break;
      case init_spec_image::ARGB:
        bpp = image::bytes_per_pixel(image::ARGB);
        icvt = img->convert(image::ARGB);
        break;
      case init_spec_image::BGRA:
        bpp = image::bytes_per_pixel(image::BGRA);
        icvt = img->convert(image::BGRA);
        break;
      case init_spec_image::RG:
      case init_spec_image::RGx:
      default:
        fatal_at(ism->defined_at,"unsupported channel order for loaded images");
      }
      const size_t size_bytes = icvt.width*icvt.height*bpp;
      image_arg_data = malloc(size_bytes);
      memcpy(image_arg_data, icvt.bits, size_bytes);
      delete img;
    } // not .dat (some image format)
  } // has init file

  switch (tbi.skind) {
  case type_builtin::IMAGE1D:
    img_desc.image_type = CL_MEM_OBJECT_IMAGE1D;
    img_desc.image_width = img_width;
    if (img_width == 0)
      fatal_at(ism->defined_at, "image1d_t must have width argument");
    if (isi->height || isi->depth)
      fatal_at(ism->defined_at,
        "image1d_t's must not have width or depth arguments");
    break;
  case type_builtin::IMAGE2D:
    img_desc.image_type = CL_MEM_OBJECT_IMAGE2D;
    if (img_width == 0 || img_height == 0)
      fatal_at(ism->defined_at,
        "image2d_t must have width and height arguments");
     else if (isi->depth)
      fatal_at(ism->defined_at, "image2d_t may not have a depth argument");
    img_desc.image_width = img_width;
    img_desc.image_height = img_height;
    break;
  case type_builtin::IMAGE3D:
    img_desc.image_type = CL_MEM_OBJECT_IMAGE3D;
    if (img_width == 0 || img_height == 0 || img_depth == 0)
      fatal_at(ism->defined_at,
        "image3d_t must have width, height, and depth arguments");
    img_desc.image_width = img_width;
    img_desc.image_height = img_height;
    img_desc.image_depth = img_depth;
    break;
  case type_builtin::IMAGE1D_ARRAY:
    img_desc.image_type = CL_MEM_OBJECT_IMAGE1D_ARRAY;
    if (img_width == 0 || img_height == 0)
      fatal_at(ism->defined_at,
        "image1d_array_t must have width and count arguments");
     else if (isi->depth)
      fatal_at(ism->defined_at,
        "image1d_array_t may not have a depth argument");
    img_desc.image_width = img_width;
    img_desc.image_height = img_height;
    fatal_at(ism->defined_at, "image arrays not supported yet");
    break;
  case type_builtin::IMAGE2D_ARRAY:
    img_desc.image_type = CL_MEM_OBJECT_IMAGE2D_ARRAY;
    if (img_width == 0 || img_height == 0 || img_depth == 0)
      fatal_at(ism->defined_at,
        "image2d_array_t must have width, height, and count arguments");
    img_desc.image_width = img_width;
    img_desc.image_height = img_height;
    img_desc.image_depth = img_depth;
    fatal_at(ism->defined_at, "image arrays not supported yet");
    break;
  default:
    fatal_at(ism->defined_at, "unsupported image kernel argument type");
  }

  size_t bytes_per_pixel = chs_per_px * bytes_per_chan;

  size_t image_row_packed = img_width*bytes_per_pixel;
  size_t image_row = image_row_packed;
  if (img_row_pitch > 0) {
    if (img_row_pitch < image_row)
      fatal_at(ism->defined_at, "image row pitch is too small");
    image_row = img_row_pitch;
  }
  size_t image_slice_packed = image_row*std::max(img_height,(size_t)1);
  size_t image_slice = image_slice_packed;
  if (img_slice_pitch > 0) {
    if (img_slice_pitch < image_slice)
      fatal_at(ism->defined_at, "image slice pitch is too small");
    image_slice = img_slice_pitch;
  }
  size_t image_size_bytes = image_slice*std::max(img_depth,(size_t)1);
  if ((img_row_pitch > 0 || img_slice_pitch > 0) && image_arg_data) {
    // we have to rescale the initialization data to support the
    // row and slice pitch
    uint8_t *dst = (uint8_t*)calloc(1,image_size_bytes);
    const uint8_t *src = (const uint8_t *)image_arg_data;
    if (img_depth != 0) {
      for (size_t d = 0, slices = d < img_depth == 0 ? 1 : img_depth;
        d < slices;
        d++)
      {
        for (size_t h = 0; h < img_height; h++) {
          memcpy(
            dst + d*image_slice        + h*image_row,
            src + d*image_slice_packed + h*image_row_packed,
            image_row);
        }
      }
    }
    free(image_arg_data);
    image_arg_data = dst;
  }

  if (ism->dimension) {
    evaluator::context ec(dc);
    size_t explicit_image_size =
      (size_t)eval_to<size_t>(ec, ism->dimension).u64;
    if (explicit_image_size < image_size_bytes)
      fatal_at(ism->defined_at, "image size is smaller than minimal size");
  }

  surface_object *so = nullptr;
  auto itr = csi->surfaces.find(ism);
  if (itr != csi->surfaces.find_end()) {
    // surface already exists, ensure it fits our min size
    so = itr->second;
    if (so->size_in_bytes < image_size_bytes)
      fatal_at(ism->defined_at,
        "allocated image size is smaller than min size needed in this use");
  } else {
    // first use of this image
    cl_mem_flags cl_mfs = init_cl_mem_flags(ism);
    cl_mem memobj = nullptr;
    CL_COMMAND_CREATE(memobj, at,
      clCreateImage,
        dc.kernel->program->device->context,
        cl_mfs,
        &img_fmt,
        &img_desc,
        nullptr);
    so = csi->define_surface(
      ism,
      surface_object::SO_IMAGE,
      image_size_bytes,
      memobj,
      dc.dobj->queue);
    so->image_format = img_fmt;
    so->image_desc = img_desc;
    so->image_init_bytes = image_arg_data;
  }
  //
  if (!ai.arg_type->as<type_builtin>().is_surface()) {
    fatal_at(
        ism->defined_at,
        "argument ", ai.name, " mismatches image initializer "
        "(e.g. kernel arg should image2d_t");
  }
  dc.surfaces.emplace_back(so, *ai.arg_type, ai, at);
  //
  so->dispatch_uses.emplace_back(&dc, arg_index, ai);
  //
  CL_COMMAND(at,
    clSetKernelArg,
      dc.kernel->kernel,
      arg_index,
      sizeof(cl_mem),
      (const void *)&so->memobj);
  //
  ss << "IMG<";
  ss << img_desc.image_width;
  if (img_desc.image_row_pitch)
    ss << " (" << img_desc.image_row_pitch << " B)";
  if (img_desc.image_height) {
    ss << " x ";
    ss << img_desc.image_height;
    if (img_desc.image_slice_pitch)
      ss << " (" << img_desc.image_row_pitch << " B)";
    if (img_desc.image_depth) {
      ss << " x ";
      ss << img_desc.image_depth;
    }
  }
  ss << ",";
  switch (img_fmt.image_channel_order) {
  case CL_A:          ss << "CL_A"; break;
  case CL_R:          ss << "CL_R"; break;
  case CL_INTENSITY:  ss << "CL_INTENSITY"; break;
  case CL_LUMINANCE:  ss << "CL_LUMINANCE"; break;
  case CL_RG:         ss << "CL_RG"; break;
  case CL_RA:         ss << "CL_RA"; break;
  case CL_RGB:        ss << "CL_RGB"; break;
  case CL_RGBA:       ss << "CL_RGBA"; break;
  case CL_BGRA:       ss << "CL_BGRA"; break;
  case CL_ARGB:       ss << "CL_ARGB"; break;
  default:            ss << "?"; break;
  }
  ss << ",";
  switch (img_fmt.image_channel_data_type) {
  case CL_SNORM_INT8:       ss << "CL_SNORM_INT8"; break;
  case CL_UNORM_INT8:       ss << "CL_UNORM_INT8"; break;
  case CL_SIGNED_INT8:      ss << "CL_SIGNED_INT8"; break;
  case CL_UNSIGNED_INT8:    ss << "CL_UNSIGNED_INT8"; break;
  case CL_SNORM_INT16:      ss << "CL_SNORM_INT16"; break;
  case CL_UNORM_INT16:      ss << "CL_UNORM_INT16"; break;
  case CL_SIGNED_INT16:     ss << "CL_SIGNED_INT16"; break;
  case CL_UNSIGNED_INT16:   ss << "CL_UNSIGNED_INT16"; break;
  case CL_HALF_FLOAT:       ss << "CL_HALF_FLOAT"; break;
  case CL_FLOAT:            ss << "CL_FLOAT"; break;
  case CL_SIGNED_INT32:     ss << "CL_SIGNED_INT32"; break;
  case CL_UNSIGNED_INT32:   ss << "CL_UNSIGNED_INT32"; break;
  default:                  ss << "?"; break;
  }
  ss << ">";
  ss << "[" << so->memobj_index << "] (" << so->size_in_bytes << " B)";
}
void evaluator::set_kernel_arg_sampler(
  cl_uint arg_index,
  dispatch_command &dc,
  std::stringstream &ss,
  const refable<init_spec> &ris,
  const arg_info &ai)
{
  debug_at(ris.defined_at, "setting sampler for ",
    ai.arg_type->syntax()," ",ai.name);
  // expect to find something like
  // CLK_NORMALIZED_COORDS_FALSE|CLK_ADDRESS_CLAMP_TO_EDGE|CLK_FILTER_NEAREST
  // (we demand that ordering)
  // init_spec_bex (symbol "|")
  //   - init_spec_sym
  //   - init_spec_bex (symbol "|")
  //      - init_spec_sym
  //      - init_spec_sym
  if (ris.value->skind != init_spec::IS_SMP) {
    fatal_at(ris.defined_at,
      "invalid sampler_t kernel argument (c.f. ");
  }
  const init_spec_sampler &iss =
    *(const init_spec_sampler *)ris.value;
  cl_addressing_mode am = CL_ADDRESS_CLAMP;
  cl_filter_mode fm = CL_FILTER_NEAREST;
  switch (iss.addr_mode) {
  case init_spec_sampler::AM_NONE:       am = CL_ADDRESS_NONE; break;
  case init_spec_sampler::AM_CLAMP_EDGE: am = CL_ADDRESS_CLAMP_TO_EDGE; break;
  case init_spec_sampler::AM_CLAMP:      am = CL_ADDRESS_CLAMP; break;
  case init_spec_sampler::AM_REPEAT:     am = CL_ADDRESS_REPEAT; break;
  case init_spec_sampler::AM_MIRRORED_REPEAT:
    am = CL_ADDRESS_MIRRORED_REPEAT;
    break;
  default:
    internal_at(ris.defined_at, "invalid address mode for sampler");
  }
  switch (iss.filter) {
  case init_spec_sampler::FM_NEAREST: fm = CL_FILTER_NEAREST; break;
  case init_spec_sampler::FM_LINEAR:  fm = CL_FILTER_LINEAR;  break;
  default:
    internal_at(ris.defined_at, "invalid address mode for sampler");
  }
  cl_bool ndc = (iss.normalized ? CL_TRUE : CL_FALSE);

  cl_context context = dc.dobj->context;
  cl_sampler sampler;
  CL_COMMAND_CREATE(
    sampler, ris.defined_at,
    clCreateSampler,
      context,
      ndc,
      am,
      fm);
  csi->samplers.emplace_back(ris.defined_at, sampler);
  CL_COMMAND(
    ris.defined_at,
    clSetKernelArg,
      dc.kernel->kernel,
      arg_index,
      sizeof(cl_sampler),
      &sampler);
}

void evaluator::set_kernel_arg_slm(
  cl_uint arg_index,
  dispatch_command &dc,
  std::stringstream &ss, // debug string for arg
  const refable<init_spec> &ris,
  const arg_info &ai)
{
  debug_at(ris.defined_at, "setting SLM size for ",
    ai.arg_type->syntax()," ",ai.name);

  const init_spec *is = ris;
  // Special treatment of local * arguments
  // e.g. kernel void foo(..., local int2 *buffer)
  // the user must tell us how many bytes they neeed for buffer
  //  foo<1024,16>(...,16*8); // 16*sizeof(int2)
  //
  //  SPECIFY: do we allow the alternative?
  //     foo<1024,16>(...,0:rw); // assume 1 int2 per work item
  if (!ai.arg_type->is<type_ptr>()) {
    fatal_at(
      ris.defined_at,
      "kernel argument in local address space must be pointer type");
  } else if (!is->is_atom()) {
    fatal_at(
      ris.defined_at,
      "local pointer requires size in bytes");
  } // SPECIFY: see above  (use tp.element_type->size() * wg-size)
  const type_ptr &tp = ai.arg_type->as<type_ptr>();
  evaluator::context ec(dc);
  auto v = csi->e->eval_to<size_t>(ec,(const init_spec_atom *)is);
  size_t local_bytes = (size_t)v.u64;
  CL_COMMAND(
    ris.defined_at, // use the arg actual location, not the let
    clSetKernelArg,
      dc.kernel->kernel,
      arg_index,
      local_bytes,
      nullptr);
  ss << "SLM[" << local_bytes << " B]";
  if (is_debug()) {
    std::cout << " ==> ARG local " << ai.arg_type->syntax() << " " <<
      ai.name << " = " << local_bytes << " B\n";
  }
}

void evaluator::eval_into(
  context &ec,
  const loc &at,
  const init_spec_atom *is,
  arg_buffer &ab,
  const type &t)
{
  if (t.is<type_num>()) {
    eval_into(ec, at, is, ab, t.as<type_num>());
  } else if (t.is<type_ptr>()) {
    eval_into(ec, at, is, ab, t.as<type_ptr>());
  } else if (t.is<type_struct>()) {
    eval_into(ec, at, is, ab, t.as<type_struct>());
  } else if (t.is<type_vector>()) {
    eval_into(ec, at, is, ab, t.as<type_vector>());
  } else {
    fatal_at(is->defined_at,"unsupported argument type");
  }
}

void evaluator::eval_into(
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
    case 1: eval_into_t<int8_t>(ec, at, is, ab); break;
    case 2: eval_into_t<int16_t>(ec, at, is, ab); break;
    case 4: eval_into_t<int32_t>(ec, at, is, ab); break;
    case 8: eval_into_t<int64_t>(ec, at, is, ab); break;
    default: internal_at(at, __FILE__, ":", __LINE__, ": unreachable");
    }
    break;
  case type_num::UNSIGNED:
    switch (tn.size_in_bytes) {
    case 1: eval_into_t<uint8_t>(ec, at, is, ab); break;
    case 2: eval_into_t<uint16_t>(ec, at, is, ab); break;
    case 4: eval_into_t<uint32_t>(ec, at, is, ab); break;
    case 8: eval_into_t<uint64_t>(ec, at, is, ab); break;
    default: internal_at(at, __FILE__, ":", __LINE__, ": unreachable");
    }
    break;
  case type_num::FLOATING:
    switch (tn.size_in_bytes) {
    case 2: eval_into_t<half>(ec, at, is, ab); break;
    case 4: eval_into_t<float>(ec, at, is, ab); break;
    case 8: eval_into_t<double>(ec, at, is, ab); break;
    default: internal_at(at, __FILE__, ":", __LINE__, ": unreachable");
    }
    break;
  default: internal_at(at, __FILE__, ":", __LINE__, ": unreachable");
  }
}

template <typename T>
void evaluator::eval_into_t(
  context &ec,
  const loc &at,
  const init_spec_atom *is,
  arg_buffer &ab)
{
  switch (is->skind) {
  case init_spec::IS_INT:
  case init_spec::IS_FLT:
  case init_spec::IS_SZO:
  case init_spec::IS_BEX:
  case init_spec::IS_UEX:
  case init_spec::IS_BIV: {
    val v = eval_to<T>(ec, is);
    ab.write<T>(v.as<T>());
    ec.evaluated(v.as<T>());
    break;
  }
  case init_spec::IS_SYM: fatal_at(at, "unbound symbol");
  case init_spec::IS_REC: fatal_at(at, "record initializer passed to scalar");
  case init_spec::IS_VEC: fatal_at(at, "vector initializer passed to scalar");
  case init_spec::IS_MEM: fatal_at(at, "surface initializer passed to scalar");
  case init_spec::IS_FIL:
  case init_spec::IS_RND:
  default: internal_at(at, __FILE__, ":", __LINE__, ": unreachable");
  }
}

void evaluator::eval_into(
  context &ec,
  const loc &at,
  const init_spec_atom *is,
  arg_buffer &ab,
  const type_struct &ts)
{
  if (is->skind == init_spec::IS_REC) {
    const init_spec_record *isr = (const init_spec_record *)is;
    if (isr->children.size() != ts.elements_length) {
      fatal_at(at, "structure initializer has wrong number of elements");
    }
    ec.evaluated(ts.name);
    ec.evaluated("{");
    for (size_t i = 0; i < ts.elements_length; i++) {
      if (i > 0)
        ec.evaluated(",");
      eval_into(ec, at, isr->children[i], ab, *ts.elements[i]);
    }
    ec.evaluated("}");
  } else {
    // TODO: we could support things like broadcast, random etc...
    fatal_at(at, "struct argument requires structure initializer");
  }
}

void evaluator::eval_into(
  context &ec,
  const loc &at,
  const init_spec_atom *is,
  arg_buffer &ab,
  const type_vector &tv)
{
  if (is->skind == init_spec::IS_VEC) {
    const init_spec_vector *isv = (const init_spec_vector *)is;
    if (isv->children.size() != tv.length) {
      fatal_at(at, "vector initializer has wrong number of elements");
    }
    std::stringstream ss;
    ss << "(" << tv.element_type.syntax() << ")(";
    ec.evaluated(ss.str());
    for (size_t i = 0; i < tv.length; i++) {
      if (i > 0)
        ec.evaluated(",");
      eval_into(ec, at, isv->children[i], ab, tv.element_type);
    }
    ec.evaluated(")");
  } else {
    // TODO: we could support things like broadcast, random etc...
    fatal_at(at, "vector argument requires vector initializer");
  }
}


void evaluator::eval_into(
  context &ec,
  const loc &at,
  const init_spec_atom *is,
  arg_buffer &ab,
  const type_ptr &tp)
{
  fatal_at(is->defined_at,"type_ptr not implemented as primitive yet");
}
