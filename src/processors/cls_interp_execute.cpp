#include "../half.hpp"
#include "../image.hpp"
#include "../system.hpp"
#include "../text.hpp"
#include "cls_interp_internal.hpp"

#include <chrono>
#include <cmath>
#include <fstream>


void compiled_script::destroy()
{
  delete (compiled_script_impl *)impl;
  impl = nullptr;
}

disp_times compiled_script::get_wall_times() const
{

  disp_times ts;
  const compiled_script_impl *csi = (const compiled_script_impl *)impl;
  for (const dispatch_command *dc : csi->dispatches)
    ts.emplace_back(dc->spec, dc->wall_times);
  return ts;
}

disp_times compiled_script::get_prof_times() const
{
  disp_times ts;
  const compiled_script_impl *csi = (const compiled_script_impl *)impl;
  for (const dispatch_command *dc : csi->dispatches)
    ts.emplace_back(dc->spec, dc->prof_times);
  return ts;
}

init_times compiled_script::get_init_times() const
{
  init_times ts;
  const compiled_script_impl *csi = (const compiled_script_impl *)impl;
  for (const surface_object *so : csi->surfaces)
    ts.emplace_back(so->init, so->init_times);
  return ts;
}
mdapi_ctrs compiled_script::get_mdapi_ctrs() const
{
  mdapi_ctrs cs;
  const compiled_script_impl *csi = (const compiled_script_impl *)impl;
  for (const dispatch_command *dc : csi->dispatches) {
    cs.emplace_back(dc->spec, dc->mdapi_ctrs);
  }
  return cs;
}


///////////////////////////////////////////////////////////////////////////////
// Constant initialization
template <typename T>
static void fill_buffer_with_const_loop(
  evaluator *e,
  evaluator::context &ec,
  arg_buffer &ab,
  const init_spec_atom *is)
{
  auto val = e->eval_to<T>(ec, is);
  size_t total_elems = ab.capacity / sizeof(T);
  for (size_t i = 0; i < total_elems; i++) {
    // THIS IS EVIL!!!!  MSVC 2017 allowed it GCC does not.
    // zapcc 5.0 finally clued me into it
    // https://www.jdoodle.com/online-compiler-c++
    //
    // T t = val.as<T>();
    // https://en.cppreference.com/w/cpp/language/dependent_name
    T t = val.template as<T>();
    //        ^^^^^^^^^ NUTs!
    ab.write<T>(t);
  }
}

static void fill_buffer_with_const(
  evaluator *e,
  evaluator::context &ec,
  arg_buffer &ab,
  const loc &arg_loc,
  const type_num &tn,
  const init_spec_atom *is)
{
  switch (tn.skind) {
  case type_num::SIGNED:
    switch (tn.size_in_bytes) {
    case 1: fill_buffer_with_const_loop<int8_t>(e,ec,ab,is); break;
    case 2: fill_buffer_with_const_loop<int16_t>(e,ec,ab,is); break;
    case 4: fill_buffer_with_const_loop<int32_t>(e,ec,ab,is); break;
    case 8: fill_buffer_with_const_loop<int64_t>(e,ec,ab,is); break;
    }
    break;
  case type_num::UNSIGNED:
    switch (tn.size_in_bytes) {
    case 1: fill_buffer_with_const_loop<uint8_t>(e,ec,ab,is); break;
    case 2: fill_buffer_with_const_loop<uint16_t>(e,ec,ab,is); break;
    case 4: fill_buffer_with_const_loop<uint32_t>(e,ec,ab,is); break;
    case 8: fill_buffer_with_const_loop<uint64_t>(e,ec,ab,is); break;
    }
    break;
  case type_num::FLOATING:
    switch (tn.size_in_bytes) {
    case 2: fill_buffer_with_const_loop<half>(e,ec,ab,is); break;
    case 4: fill_buffer_with_const_loop<float>(e,ec,ab,is); break;
    case 8: fill_buffer_with_const_loop<double>(e,ec,ab,is); break;
    }
    break;
  }
}

///////////////////////////////////////////////////////////////////////////////
// Random number generation
template <typename T, typename R = T>
static void fill_buffer_rng_loop_int(
  evaluator *e,
  evaluator::context &ec,
  arg_buffer &ab,
  const init_spec_rng *isr,
  std::mt19937 &g)
{
  val v_lo = std::numeric_limits<T>::min();
  val v_hi = std::numeric_limits<T>::max();
  if (isr->e_lo) {
    v_lo = e->eval_to<T>(ec, isr->e_lo);
  }
  if (isr->e_hi) {
    v_hi = e->eval_to<T>(ec, isr->e_hi);
  }
  if (v_lo.as<T>() > v_hi.as<T>()) {
    e->fatal_at(isr->defined_at, "low bound > high bound");
  }
  std::uniform_int_distribution<R> dist(v_lo.as<T>(),v_hi.as<T>());
  size_t total_elems = ab.capacity / sizeof(T);
  for (size_t i = 0; i < total_elems; i++) {
    ab.write<T>((T)dist(g));
  }
}

template <typename T,typename R = T> // r is random type
static void fill_buffer_rng_loop_flt(
  evaluator *e,
  evaluator::context &ec,
  arg_buffer &ab,
  const init_spec_rng *isr,
  std::mt19937 &g)
{
  val v_lo = 0.0;
  val v_hi = 1.0;
  if (isr->e_hi) {
    v_hi = e->eval_to_f(ec, isr->e_hi);
    if (isr->e_lo) {
      v_lo = e->eval_to_f(ec, isr->e_lo);
    }
  }
  if (v_lo.as<T>() > v_hi.as<T>()) {
    e->fatal_at(isr->defined_at,"low bound > high bound");
  }
  std::uniform_real_distribution<R> dist((R)v_lo.f64, (R)v_hi.f64);
  size_t total_elems = ab.capacity / sizeof(T);
  for (size_t i = 0; i < total_elems; i++) {
    ab.write<T>((T)dist(g));
  }
}

static void fill_buffer_rng(
  compiled_script_impl &csi,
  evaluator::context &ec,
  arg_buffer &ab,
  const init_spec_rng *isr,
  const type &t,
  const loc &at)
{
  std::mt19937 gen(csi.e->rd());
  if (isr->has_seed) {
    gen.seed((unsigned)isr->seed);
  } else {
    gen.seed(static_cast<unsigned>(
      std::chrono::high_resolution_clock::now().
        time_since_epoch().count()));
  }

  if (t.is<type_num>()) {
    const type_num &tn = t.as<type_num>();
    switch (tn.skind) {
    case type_num::SIGNED: {
      switch (tn.size_in_bytes) {
      case 1: fill_buffer_rng_loop_int<int8_t,int16_t>(csi.e,ec,ab,isr,gen); break;
      case 2: fill_buffer_rng_loop_int<int16_t>(csi.e,ec,ab,isr,gen); break;
      case 4: fill_buffer_rng_loop_int<int32_t>(csi.e,ec,ab,isr,gen); break;
      case 8: fill_buffer_rng_loop_int<int64_t>(csi.e,ec,ab,isr,gen); break;
      }
      break;
    }
    case type_num::UNSIGNED:
      switch (tn.size_in_bytes) {
      case 1: fill_buffer_rng_loop_int<uint8_t,uint16_t>(csi.e,ec,ab,isr,gen); break;
      case 2: fill_buffer_rng_loop_int<uint16_t>(csi.e,ec,ab,isr,gen); break;
      case 4: fill_buffer_rng_loop_int<uint32_t>(csi.e,ec,ab,isr,gen); break;
      case 8: fill_buffer_rng_loop_int<uint64_t>(csi.e,ec,ab,isr,gen); break;
      }
      break;
    case type_num::FLOATING:
      switch (tn.size_in_bytes) {
      case 2: fill_buffer_rng_loop_flt<half,float>(csi.e,ec,ab,isr,gen); break;
      case 4: fill_buffer_rng_loop_flt<float>(csi.e,ec,ab,isr,gen); break;
      case 8: fill_buffer_rng_loop_flt<double>(csi.e,ec,ab,isr,gen); break;
      }
      break;
    }
  } else if (t.is<type_vector>()) {
    const type_vector &tv = t.as<type_vector>();
    fill_buffer_rng(csi, ec, ab, isr, tv.element_type, at);
  } else {
    csi.fatal_at(at,"unsupported type for random generator");
  }
}

///////////////////////////////////////////////////////////////////////////////
// Sequence
template <typename T, typename R = T>
static void fill_buffer_seq_loop(
  evaluator *e,
  evaluator::context &ec,
  arg_buffer &ab,
  const init_spec_seq *iss)
{
  val v_base((R)0);
  if (iss->base)
    v_base = e->eval_to<R>(ec, iss->base);

  val v_delta((R)1);
  if (iss->delta)
    v_delta = e->eval_to<R>(ec, iss->delta);

  R curr = v_base.as<R>(), delta = v_delta.as<R>();
  size_t total_elems = ab.capacity / sizeof(T);
  for (size_t i = 0; i < total_elems; i++) {
    ab.write<T>((R)curr);
    curr += delta;
  }
}

static void fill_buffer_seq(
  compiled_script_impl &csi,
  evaluator::context &ec,
  arg_buffer &ab,
  const init_spec_seq *iss,
  const type &t,
  const loc &at)
{
 if (t.is<type_num>()) {
    const type_num &tn = t.as<type_num>();
    switch (tn.skind) {
    case type_num::SIGNED: {
      switch (tn.size_in_bytes) {
      case 1: fill_buffer_seq_loop<int8_t> (csi.e, ec, ab, iss); break;
      case 2: fill_buffer_seq_loop<int16_t>(csi.e, ec, ab, iss); break;
      case 4: fill_buffer_seq_loop<int32_t>(csi.e, ec, ab, iss); break;
      case 8: fill_buffer_seq_loop<int64_t>(csi.e, ec, ab, iss); break;
      }
      break;
    }
    case type_num::UNSIGNED:
      switch (tn.size_in_bytes) {
      case 1: fill_buffer_seq_loop<uint8_t> (csi.e, ec, ab, iss); break;
      case 2: fill_buffer_seq_loop<uint16_t>(csi.e, ec, ab, iss); break;
      case 4: fill_buffer_seq_loop<uint32_t>(csi.e, ec, ab, iss); break;
      case 8: fill_buffer_seq_loop<uint64_t>(csi.e, ec, ab, iss); break;
      }
      break;
    case type_num::FLOATING:
      switch (tn.size_in_bytes) {
      case 2: fill_buffer_seq_loop<half,float>(csi.e, ec, ab, iss); break;
      case 4: fill_buffer_seq_loop<float> (csi.e, ec, ab, iss); break;
      case 8: fill_buffer_seq_loop<double>(csi.e, ec, ab, iss); break;
      }
      break;
    }
  } else if (t.is<type_vector>()) {
    const type_vector &tv = t.as<type_vector>();
    fill_buffer_seq(csi, ec, ab, iss, tv.element_type, at);
  } else {
    csi.fatal_at(at,"unsupported type for sequence generator");
  }
}


///////////////////////////////////////////////////////////////////////////////
// Finite sequence
template <typename T>
static void fill_buffer_fseq_loop(
  evaluator *e,
  evaluator::context &ec,
  arg_buffer &ab,
  const init_spec_fseq *isc)
{
  std::vector<T> vals;
  for (const auto *arg : isc->args) {
    val v = e->eval_to<T>(ec, arg);
    vals.emplace_back(v.as<T>());
  }

  size_t total_elems = ab.capacity / sizeof(T);
  for (size_t i = 0; i < total_elems; i++) {
    ab.write<T>(vals[std::min(i, vals.size() - 1)]);
  }
}

static void fill_buffer_fseq(
  compiled_script_impl &csi,
  evaluator::context &ec,
  arg_buffer &ab,
  const init_spec_fseq *isf,
  const type &t,
  const loc &at)
{
  if (isf->args.empty()) {
    csi.internal_at(at, "fseq requires at least one argument");
    return;
  }
  if (t.is<type_num>()) {
    const type_num &tn = t.as<type_num>();
    switch (tn.skind) {
    case type_num::SIGNED: {
      switch (tn.size_in_bytes) {
      case 1: fill_buffer_fseq_loop<int8_t> (csi.e, ec, ab, isf); break;
      case 2: fill_buffer_fseq_loop<int16_t>(csi.e, ec, ab, isf); break;
      case 4: fill_buffer_fseq_loop<int32_t>(csi.e, ec, ab, isf); break;
      case 8: fill_buffer_fseq_loop<int64_t>(csi.e, ec, ab, isf); break;
      }
      break;
    }
    case type_num::UNSIGNED:
      switch (tn.size_in_bytes) {
      case 1: fill_buffer_fseq_loop<uint8_t> (csi.e, ec, ab, isf); break;
      case 2: fill_buffer_fseq_loop<uint16_t>(csi.e, ec, ab, isf); break;
      case 4: fill_buffer_fseq_loop<uint32_t>(csi.e, ec, ab, isf); break;
      case 8: fill_buffer_fseq_loop<uint64_t>(csi.e, ec, ab, isf); break;
      }
      break;
    case type_num::FLOATING:
      switch (tn.size_in_bytes) {
      case 2: fill_buffer_fseq_loop<half>(csi.e, ec, ab, isf); break;
      case 4: fill_buffer_fseq_loop<float> (csi.e, ec, ab, isf); break;
      case 8: fill_buffer_fseq_loop<double>(csi.e, ec, ab, isf); break;
      }
      break;
    }
  } else if (t.is<type_vector>()) {
    const type_vector &tv = t.as<type_vector>();
    fill_buffer_fseq(csi, ec, ab, isf, tv.element_type, at);
  } else {
    csi.fatal_at(at, "unsupported type for sequence generator");
  }
}

///////////////////////////////////////////////////////////////////////////////
// Cycle
template <typename T>
static void fill_buffer_cyc_loop(
  evaluator *e,
  evaluator::context &ec,
  arg_buffer &ab,
  const init_spec_cyc *isc)
{
  std::vector<T> vals;
  for (const auto *arg : isc->args) {
    val v = e->eval_to<T>(ec, arg);
    vals.emplace_back(v.as<T>());
  }

  size_t total_elems = ab.capacity / sizeof(T);
  for (size_t i = 0; i < total_elems; i++) {
    ab.write<T>(vals[i % vals.size()]);
  }
}

static void fill_buffer_cyc(
  compiled_script_impl &csi,
  evaluator::context &ec,
  arg_buffer &ab,
  const init_spec_cyc *isc,
  const type &t,
  const loc &at)
{
  if (isc->args.empty()) {
    csi.internal_at(at, "cyc requires at least one argument");
    return;
  }
  if (t.is<type_num>()) {
    const type_num &tn = t.as<type_num>();
    switch (tn.skind) {
    case type_num::SIGNED: {
      switch (tn.size_in_bytes) {
      case 1: fill_buffer_cyc_loop<int8_t> (csi.e, ec, ab, isc); break;
      case 2: fill_buffer_cyc_loop<int16_t>(csi.e, ec, ab, isc); break;
      case 4: fill_buffer_cyc_loop<int32_t>(csi.e, ec, ab, isc); break;
      case 8: fill_buffer_cyc_loop<int64_t>(csi.e, ec, ab, isc); break;
      }
      break;
    }
    case type_num::UNSIGNED:
      switch (tn.size_in_bytes) {
      case 1: fill_buffer_cyc_loop<uint8_t> (csi.e, ec, ab, isc); break;
      case 2: fill_buffer_cyc_loop<uint16_t>(csi.e, ec, ab, isc); break;
      case 4: fill_buffer_cyc_loop<uint32_t>(csi.e, ec, ab, isc); break;
      case 8: fill_buffer_cyc_loop<uint64_t>(csi.e, ec, ab, isc); break;
      }
      break;
    case type_num::FLOATING:
      switch (tn.size_in_bytes) {
      case 2: fill_buffer_cyc_loop<half>(csi.e, ec, ab, isc); break;
      case 4: fill_buffer_cyc_loop<float> (csi.e, ec, ab, isc); break;
      case 8: fill_buffer_cyc_loop<double>(csi.e, ec, ab, isc); break;
      }
      break;
    }
  } else if (t.is<type_vector>()) {
    const type_vector &tv = t.as<type_vector>();
    fill_buffer_cyc(csi, ec, ab, isc, tv.element_type, at);
  } else {
    csi.fatal_at(at, "unsupported type for sequence generator");
  }
}

///////////////////////////////////////////////////////////////////////////////
static void save_image(
  std::string file_name,
  loc at,
  compiled_script_impl *csi,
  const surface_object *so,
  size_t width,
  size_t height,
  size_t row_pitch,
  size_t slice_pitch,
  cl_channel_order ch_ord,
  cl_channel_type ch_type,
  const void *bits)
{
  if (so->image_desc.image_depth != 0) {
    csi->fatal_at(at, "3D images not supported for saving");
  }
  image::data_format fmt;
  switch (ch_ord) {
  case CL_A:
  case CL_R:
  // case CL_Rx:
  case CL_INTENSITY:
  case CL_LUMINANCE:
  case CL_DEPTH:
    fmt = image::I;
    break;
  // case CL_RG:
  // case CL_RGx:
  // case CL_RA:
  //
  // case CL_sRGB:
  case CL_RGB:
    fmt = image::RGB;
    break;
  // case CL_RGBx:
  // case CL_UNORM_SHORT_565:
  // case CL_UNORM_SHORT_555:
  // case CL_UNORM_INT_101010:
  case CL_RGBA:
    fmt = image::RGBA;
    break;
  case CL_BGRA:
    fmt = image::BGRA;
    break;
  case CL_ARGB:
    fmt = image::ARGB;
    break;
  // case CL_sRGBA:
  // case CL_sBGRA:
  // case CL_UNORM_INT_101010_2:
  default:
    csi->fatal_at(at, "unsupported channel order for saving");
  }

  switch (ch_type) {
  case CL_FLOAT:
  case CL_UNSIGNED_INT8:
  case CL_UNORM_INT8:
    break;
  default:
    csi->fatal_at(at, "unsupported channel type for saving");
  }
  image img(width, std::max(height, (size_t)1), fmt);
  const uint8_t *host_src = (const uint8_t *)bits;
  size_t dst_row_pitch = img.width * image::bytes_per_pixel(fmt);
  for (size_t h = 0; h < img.height; h++) {
    uint8_t *dst_row_start = img.bits + h*dst_row_pitch;
    const uint8_t *src_row_start = host_src + h*row_pitch;
    if (ch_type == CL_FLOAT) {
      // convert each pixel from float to uchar
      const float *pxs = (const float *)src_row_start;
      for (size_t i = 0; i < width * cls::channels_per_pixel(ch_ord); i++) {
        float px = std::clamp(pxs[i], 0.0f, 1.0f);
        dst_row_start[i] =
          (uint8_t)std::min<unsigned>(0xFF, (unsigned)round(255.0f * px));
      }
    } else {
      // copy entire row
      memcpy(dst_row_start, src_row_start, dst_row_pitch);
    }
  }

  auto ext = sys::take_extension(file_name);
  if (ext == ".ppm") {
    img.save_ppm(file_name.c_str(), so->size_in_bytes > 1024);
  } else if (ext == ".bmp") {
#ifdef IMAGE_HPP_SUPPORTS_BMP
    img.save_bmp(file_name.c_str());
#else
    csi->fatal_at(at, "unsupported image format (not compiled with support)");
#endif
  } else if (ext == ".png") {
#ifdef IMAGE_HPP_SUPPORTS_PNG
    img.save_png(file_name.c_str());
#else
    csi->fatal_at(at, "unsupported image format (not compiled with support)");
#endif
  } else {
    csi->warning_at(at, "unrecognized image format; falling back to .ppm");
    img.save_ppm(file_name.c_str(), so->size_in_bytes > 1024);
  }
}

static void save_buffer(
  loc at,
  compiled_script_impl *csi,
  const surface_object *so,
  const void *bits)
{
  std::stringstream ss;
  ss << "cls-surface-" << std::setfill('0') << std::setw(2) <<
    so->memobj_index << ".bits";
  std::ofstream ofs(ss.str(), std::ofstream::binary);
  ofs.write((const char *)bits, so->size_in_bytes);
}

void compiled_script_impl::execute(dispatch_command &dc)
{
  debug_at(dc.dobj->spec->defined_at, "executing dispatch");

  cl_command_queue queue = dc.dobj->queue;
  cl_kernel kernel = dc.kernel->kernel;
  loc dc_at = dc.spec->defined_at;

  auto print_surfaces = [&] (bool is_pre) {
    for (const auto &sinfo : dc.surfaces) {
      const surface_object *so = std::get<0>(sinfo);
      if (is_pre && so->init->print_pre ||
        !is_pre && (so->init->print_post || so->init->save_post))
      {
        bool is_print = is_pre && so->init->print_pre || so->init->print_post;
        bool is_image = so->skind == surface_object::SO_IMAGE;
        const type &t = std::get<1>(sinfo);
        const arg_info &ai = std::get<2>(sinfo);
        const loc &at = std::get<3>(sinfo);

        std::cout << ai.arg_type->syntax() << "  " << ai.name << " = " <<
          so->str() << "\n";

        if (is_image) {
          with_image_map_read(
            at,
            so,
            [&] (size_t row_pitch, size_t slice_pitch, const void *host_ptr)
          {
            if (is_print) {
              fatal_at(dc.spec->defined_at,
                "image printing not supported (:p or :P)");
            } else {
              std::stringstream ss;
              ss << "cls-surface-" << std::setfill('0') <<
                std::setw(2) << so->memobj_index << ".ppm";
              save_image(
                ss.str(),
                dc.spec->defined_at,
                this, so,
                so->image_desc.image_width,
                so->image_desc.image_height,
                row_pitch, slice_pitch,
                so->image_format.image_channel_order,
                so->image_format.image_channel_data_type,
                host_ptr);
            }
          });
        } else {
          with_buffer_map_read(
            at,
            so,
            [&] (const void *host_ptr) {
              if (is_print) {
                int elems_per_row = is_pre ?
                  so->init->print_pre_elems_per_row :
                  so->init->print_post_elems_per_row;
                format_buffer(
                  std::cout,
                  host_ptr,
                  so->size_in_bytes,
                  t,
                  elems_per_row);
                std::cout << "\n";
              } else { // is_save
                save_buffer(dc.spec->defined_at, this, so, host_ptr);
              }
            });
        }
      }
    }
  };

  print_surfaces(true);

  if (dc.dobj->md) {
    dc.dobj->md->activate();
  }

  auto start_execute = std::chrono::high_resolution_clock::now();

  cl_event enq_evt;
  CL_COMMAND(dc_at,
      dc.dobj->cl->clEnqueueNDRangeKernel,
        queue,
        kernel,
        (cl_uint)dc.global_size.rank(),
        nullptr, // global offset
        dc.global_size.get(),
        dc.local_size.rank() > 0 ? dc.local_size.get() : nullptr,
        0,
        nullptr,
        &enq_evt);

  CL_COMMAND(dc_at, dc.dobj->cl->clWaitForEvents, 1, &enq_evt);

  auto duration_exec =
    std::chrono::duration_cast<std::chrono::microseconds>(
      std::chrono::high_resolution_clock::now() - start_execute);
  dc.wall_times.add(duration_exec.count()/1000.0/1000.0);

  if (os.prof_time) {
    cl_ulong st;
    CL_COMMAND(dc_at,
        dc.dobj->cl->clGetEventProfilingInfo,
          enq_evt, CL_PROFILING_COMMAND_START, sizeof(st), &st, nullptr);
    cl_ulong en;
    CL_COMMAND(dc_at,
        dc.dobj->cl->clGetEventProfilingInfo,
          enq_evt, CL_PROFILING_COMMAND_END, sizeof(en), &en, nullptr);
    dc.prof_times.add((en - st)/1000.0/1000.0/1000.0);
    debug_at(dc_at,
      "CL_PROFILING_COMMAND_START: ", en, "; CL_PROFILING_COMMAND_END: ", en);
  }

  if (dc.dobj->md) {
    dc.dobj->md->deactivate();
    uint32_t rep_buf_len = dc.dobj->md->get_query_report_size();
    if (!rep_buf_len) {
      fatal_at(dc_at, "mdapi_lib get_report_size() returned 0");
    }
    char *rep_buf = new char[rep_buf_len];
    memset(rep_buf, 0, rep_buf_len);
    size_t output_size = 0;
    CL_COMMAND(dc_at,
        dc.dobj->cl->clGetEventProfilingInfo,
          enq_evt,
          CL_PROFILING_COMMAND_PERFCOUNTERS_INTEL,
          (size_t)rep_buf_len,
          rep_buf,
          &output_size);
    if (rep_buf_len != output_size) {
      fatal_at(dc_at, "mdapi_lib: get_report_size() returned wrong length");
    }

    if (!dc.dobj->md->parse_counter_buffer(dc.mdapi_ctrs, rep_buf)) {
      fatal_at(
          dc_at,
          "mdapi_lib: parsing counters failed (",
          dc.dobj->md->get_error(),
          ")");
    }

    delete[] rep_buf;
  }

  cl_int enq_evt_st = 0;
  CL_COMMAND(dc_at,
      dc.dobj->cl->clGetEventInfo,
        enq_evt,
        CL_EVENT_COMMAND_EXECUTION_STATUS,
        sizeof(enq_evt_st), &enq_evt_st, nullptr);
  if (enq_evt_st != CL_COMPLETE) {
    // this is where NVidia might return -9999
    fatal_at(dc_at, "synchronizing event status returned " ,
      enq_evt_st, " (after wait)");
  }

  CL_COMMAND(dc_at, dc.dobj->cl->clReleaseEvent, enq_evt);

  print_surfaces(false);

  CL_COMMAND(dc_at, dc.dobj->cl->clFinish, queue);
}

void compiled_script_impl::execute(
  diffs_command &dfc,
  const void *ref_host_ptr,
  const void *sut_host_ptr)
{
  debug_at(dfc.spec->defined_at, "executing surface diff");

  evaluator::context ec;
  if (dfc.so_ref->size_in_bytes == 0)
    return; // zero sized buffers always match
  const type *elem_type = dfc.element_type;
  if (elem_type == nullptr) {
    elem_type = (dfc.so_ref->size_in_bytes > 4 &&
      dfc.so_sut->size_in_bytes % 4 == 0) ?
      &UINT() : &UCHAR();
  } else if (elem_type->size() == 0 ||
    dfc.so_sut->size_in_bytes % elem_type->size() != 0)
  {
    fatal_at(
      dfc.spec->defined_at,
      "buffer size (", dfc.so_sut->size_in_bytes,
      " B) is not a multiple of diff element type ",
      elem_type->syntax(), " size (", elem_type->size(), " B)");
  }

  size_t total_elems = dfc.so_sut->size_in_bytes/elem_type->size();
  const uint8_t *ref_host_ptr8 = (const uint8_t*)ref_host_ptr;
  const uint8_t *sut_host_ptr8 = (const uint8_t*)sut_host_ptr;
  for (size_t elem_ix = 0; elem_ix < total_elems; elem_ix++) {
    execute_diff_elem(
      dfc.spec->defined_at,
      dfc.spec->max_diff,
      elem_ix,
      *elem_type,
      ref_host_ptr8 + elem_ix*elem_type->size(),
      sut_host_ptr8 + elem_ix*elem_type->size());
  }
}

void compiled_script_impl::execute(diffu_command &dfc, const void *host_ptr)
{
  debug_at(dfc.spec->defined_at, "executing uniform diff");

  evaluator::context ec;
  if (dfc.so->size_in_bytes == 0)
    return; // zero sized buffers always match
  if (!dfc.spec->ref.value->is_atom())
    fatal_at(dfc.spec->defined_at, "only atoms supported as reference argument");
  const type *elem_type = dfc.element_type;
  if (elem_type == nullptr) {
    elem_type = (dfc.so->size_in_bytes > 4 && dfc.so->size_in_bytes % 4 == 0) ?
      &UINT() : &UCHAR();
  }

  // given an explicit type we make a broadcast comparison
  arg_buffer ab_ref(get_diagnostics(), dfc.spec->defined_at, elem_type->size());
  e->eval_into(ec,
    dfc.spec->defined_at,
    (const init_spec_atom *)dfc.spec->ref.value,
    ab_ref,
    *elem_type);
  if (ab_ref.num_left() != 0) {
    fatal_at(dfc.spec->defined_at, "reference scalar value is wrong size");
  }

  if (elem_type->size() == 0 ||
    dfc.so->size_in_bytes % elem_type->size() != 0) {
    fatal_at(
      dfc.spec->defined_at,
      "buffer size (", dfc.so->size_in_bytes,
      " B) is not a multiple of diff element type ",
      elem_type->syntax(), " size (", elem_type->size() , " B)");
  }
  size_t total_elems = dfc.so->size_in_bytes/elem_type->size();
  const uint8_t *host_ptr8 = (const uint8_t*)host_ptr;
  for (size_t elem_ix = 0; elem_ix < total_elems; elem_ix++) {
    execute_diff_elem(
      dfc.spec->defined_at,
      dfc.spec->max_diff,
      elem_ix,
      *elem_type,
      ab_ref.base,
      host_ptr8 + elem_ix*elem_type->size());
  } // for elems
}

void compiled_script_impl::execute_diff_elem(
  loc defined_at,
  double max_diff,
  size_t elem_ix,
  const type &elem_type,
  const void *elem_ref,
  const void *elem_sut)
{
  // TODO: diff by type so we can enable error margins
  // diff<float,0.001>(0.0,A)
/*
  if (elem_type.is<type_struct>()) {
    const type_struct &s = elem_type.as<type_struct>();
    return s.is_uniform() && is_floating(*s.elements[0]);
  }
  return elem_type.is<type_num>() &&
    elem_type.as<type_num>().skind == type_num::FLOATING;
  */
  auto report_mismatch = [&](int vec_elem, const char *extra_message) {
    std::cerr << "mismatch on buffer element "
      << elem_ix << " (type " << elem_type.syntax() << ")\n";
    if (vec_elem >= 0)
      std::cerr << "(vector element " << vec_elem << ")\n";
    if (extra_message)
      std::cerr << extra_message << "\n";
    std::cerr << "============== vs. (SUT) ==============\n";
    format_buffer_element_ext(std::cerr, elem_type, elem_sut);
    std::cerr << "\n";
    std::cerr << "============== vs. (REF) ==============\n";
    format_buffer_element_ext(std::cerr, elem_type, elem_ref);
    std::cerr << "\n";
    if (os.no_exit_on_diff_fail) {
      warning_at(
        defined_at,
        "mismatch on element ", elem_ix, " (type ", elem_type.syntax(), ")");
    } else {
      fatal_at(
        defined_at,
        "mismatch on element ", elem_ix, " (type ", elem_type.syntax(), ")");
    }
  };

  auto diff_elem = [&](
    int vec_elem,
    const type_num &elem_type,
    const void *elem_ref,
    const void *elem_sut)
  {
    double elem_val_sut = 0.0, elem_val_ref = 0.0;
    switch (elem_type.size()) {
    case 2:
      elem_val_sut = (float)(*((const half *)elem_sut));
      elem_val_ref = (float)(*((const half *)elem_ref));
      break;
    case 4:
      elem_val_sut = *((const float *)elem_sut);
      elem_val_ref = *((const float *)elem_ref);
      break;
    case 8:
      elem_val_sut = *((const double *)elem_sut);
      elem_val_ref = *((const double *)elem_ref);
      break;
    default:
      fatal_at(defined_at, "unsupported floating point type");
    }
    //
    if (std::isnan(elem_val_sut) && !std::isnan(elem_val_ref) ||
      !std::isnan(elem_val_sut) && std::isnan(elem_val_ref))
    {
      report_mismatch(vec_elem, "one value is NaN");
    } else if (std::abs(elem_val_sut - elem_val_ref) > max_diff) {
      report_mismatch(vec_elem,
        "value difference exceeds max allowable difference");
    }
  };
  //
  auto is_floating = [&](const type &elem_type) {
    return elem_type.is<type_num>() &&
      elem_type.as<type_num>().skind == type_num::FLOATING;
  };
  //
  if (is_floating(elem_type)) {
    diff_elem(-1, elem_type.as<type_num>(), elem_ref, elem_sut);
  } else if (elem_type.is<type_vector>() &&
    is_floating(elem_type.as<type_vector>().element_type))
  {
    const type_vector &tv = elem_type.as<type_vector>();
    for (int i = 0; i < (int)tv.length; i++) {
      diff_elem(i,
        tv.element_type,
        (const uint8_t *)elem_ref + i*tv.element_type.size(),
        (const uint8_t *)elem_sut + i*tv.element_type.size());
    }
  } else if (memcmp(
    elem_ref,
    elem_sut,
    elem_type.size()))
  {
    report_mismatch(-1, nullptr);
  } // else: elements match
}

void compiled_script_impl::execute(print_command &prc, const void *host_ptr)
{
  debug_at(prc.spec->defined_at, "executing print");

  evaluator::context ec;
  if (prc.so && !prc.so->dispatch_uses.empty())
    ec.sizeof_pointer =
      std::get<0>(prc.so->dispatch_uses.front())->dobj->pointer_size;

  const type *elem_type = prc.element_type;
  if (elem_type == nullptr) {
    elem_type = (prc.so->size_in_bytes > 4 && prc.so->size_in_bytes % 4 == 0) ?
      &UINT() : &UCHAR();
  }
  std::cout << "PRINT";
  if (prc.element_type)
    std::cout << "<" << elem_type->syntax() << ">";
  std::cout << "[" << prc.so->str() << "] =>\n";
  format_buffer(
    std::cout,
    host_ptr,
    prc.so->size_in_bytes,
    *elem_type,
    prc.spec->elements_per_row);
  std::cout << "\n";
}

void compiled_script_impl::execute(saveb_command &svbc, const void *host_ptr)
{
  debug_at(svbc.spec->defined_at, "executing save");

  std::ofstream of(svbc.spec->file, std::ios::binary);
  if (!of.good()) {
    fatal_at(svbc.spec->defined_at, "failed to open file");
  }
  of.write((const char *)host_ptr, svbc.so->size_in_bytes);
  if (!of) {
    fatal_at(svbc.spec->defined_at, "failed to write file");
  }
  of.flush();
}
void compiled_script_impl::execute(savei_command &svic, const void *host_ptr)
{
  debug_at(svic.spec->defined_at, "executing save_image");

  size_t row_pitch = svic.width *
    cls::channels_per_pixel(svic.channel_order) *
    cls::bytes_per_channel(svic.channel_type);
  save_image(
    svic.spec->file, svic.spec->defined_at, this,
      svic.so,
      svic.width, svic.height,
      row_pitch,
      svic.so->image_desc.image_slice_pitch,
      svic.channel_order,
      svic.channel_type,
      host_ptr);
}

void cl_interface::with_buffer_map_read(
  const loc &at,
  const surface_object *so,
  buffer_reader apply)
{
  void *host_ptr = nullptr;
  CL_COMMAND_CREATE(host_ptr, at,
    cl_lib::DEFAULT.clEnqueueMapBuffer,
      so->queue,
      so->memobj,
      CL_BLOCKING,
      CL_MAP_READ,
      0,
      so->size_in_bytes,
      0, nullptr,
      nullptr);

  apply(host_ptr);

  CL_COMMAND(at,
      cl_lib::DEFAULT.clEnqueueUnmapMemObject,
        so->queue,
        so->memobj,
        host_ptr,
        0,
        nullptr,
        nullptr);
}

void cl_interface::with_buffer_map_write(
  const loc &at,
  surface_object *so,
  buffer_writer apply)
{
  void *host_ptr = nullptr;
  CL_COMMAND_CREATE(host_ptr, at,
      cl_lib::DEFAULT.clEnqueueMapBuffer,
        so->queue,
        so->memobj,
        CL_BLOCKING,
        CL_MAP_WRITE,
        0,
        so->size_in_bytes,
        0, nullptr,
        nullptr);

  apply(host_ptr);

  CL_COMMAND(at,
      cl_lib::DEFAULT.clEnqueueUnmapMemObject,
        so->queue,
        so->memobj,
        host_ptr,
        0,
        nullptr,
        nullptr);
}

void cl_interface::with_image_map_read(
  const loc &at,
  const surface_object *so,
  image_reader apply)
{
  void *host_ptr;
  size_t origin[3]{0};
  size_t region[3];
  region[0] = so->image_desc.image_width;
  region[1] = std::max(so->image_desc.image_height, (size_t)1);
  region[2] = std::max(so->image_desc.image_depth, (size_t)1);
  size_t row_pitch = 0, slice_pitch = 0;
  CL_COMMAND_CREATE(host_ptr, at,
      cl_lib::DEFAULT.clEnqueueMapImage,
        so->queue,
        so->memobj,
        CL_BLOCKING,
        CL_MAP_READ,
        origin,
        region,
        &row_pitch,
        &slice_pitch,
        0, nullptr, nullptr);

  apply(row_pitch, slice_pitch, host_ptr);

  CL_COMMAND(at,
      cl_lib::DEFAULT.clEnqueueUnmapMemObject,
        so->queue,
        so->memobj,
        host_ptr,
        0,
        nullptr,
        nullptr);
}

void cl_interface::with_image_map_write(
  const loc &at,
  const surface_object *so,
  image_writer apply)
{
  void *host_ptr = nullptr;
  size_t origin[3] {0};
  size_t region[3];
  region[0] = so->image_desc.image_width;
  region[1] = std::max(so->image_desc.image_height, (size_t)1);
  region[2] = std::max(so->image_desc.image_depth, (size_t)1);

  size_t row_pitch = 0, slice_pitch = 0;
  CL_COMMAND_CREATE(host_ptr, at,
      cl_lib::DEFAULT.clEnqueueMapImage,
        so->queue,
        so->memobj,
        CL_BLOCKING,
        CL_MAP_WRITE,
        origin,
        region,
        &row_pitch,
        &slice_pitch,
        0, nullptr, nullptr);

  apply(row_pitch, slice_pitch, host_ptr);

  CL_COMMAND(at,
      cl_lib::DEFAULT.clEnqueueUnmapMemObject,
        so->queue,
        so->memobj,
        host_ptr,
        0,
        nullptr,
        nullptr);
}


void compiled_script_impl::init_surfaces()
{
  for (surface_object *so : surfaces) {
    if (so->dummy_object)
      continue; // only used for a diff command

    debug_at(so->init->defined_at, "initializing surface");

    auto t_start = std::chrono::high_resolution_clock::now();

    dispatch_command *dc = nullptr;
    const type *elem_type = nullptr;
    if (!so->dispatch_uses.empty()) {
      const surface_object::use &u = so->dispatch_uses.front();
      dc = std::get<0>(u);
      const arg_info &ai = std::get<2>(u);
      if (ai.arg_type->is<type_ptr>()) {
        elem_type = ai.arg_type->as<type_ptr>().element_type;
      }
    } else if (!so->dummy_object) { // no valid uses found
      fatal_at(so->init->defined_at, "no uses of this surface found");
    }

    bool can_undef =
      so->init->root->skind == init_spec_atom::IS_UND &&
      !so->init->print_post &&
      !so->init->print_pre &&
      !so->init->save_post;
    if (can_undef) {
      debug_at(so->init->defined_at,
        "skipping initialization (no save or printing needed)");
    } else if (so->skind == surface_object::SO_BUFFER) {
      with_buffer_map_write(
        so->init->defined_at,
        so,
        [&] (void *host_ptr) {
          evaluator::context ec(*dc);
          init_surface(*so, ec, elem_type, host_ptr);
        });
    } else if (so->skind == surface_object::SO_IMAGE) {
      with_image_map_write(
        so->init->defined_at,
        so,
        [&] (size_t, size_t, void *host_ptr) {
          if (so->image_init_bytes) {
            memcpy(host_ptr, so->image_init_bytes, so->size_in_bytes);
          } else {
            memset(host_ptr, 0, so->size_in_bytes);
          }
        });
    } else {
      internal_at(so->init->defined_at, "invalid surface kind");
    }

    auto t_duration =
      std::chrono::duration_cast<std::chrono::microseconds>(
        std::chrono::high_resolution_clock::now() - t_start);
    so->init_times.add(t_duration.count()/1000.0/1000.0);
  } // for surface objects
}

void compiled_script_impl::init_surface(
  surface_object &so,
  evaluator::context &ec,
  const type *elem_type,
  void *host_ptr)
{
  arg_buffer ab(
    get_diagnostics(), so.init->defined_at, host_ptr, so.size_in_bytes);
  switch (so.init->root->skind) {
  case init_spec::IS_UND:
    // for undef we don't need to do anything to the surface
    break;
  case init_spec::IS_FIL: {
    const init_spec_file *isf = (const init_spec_file *)so.init->root;
    if (isf->flavor != init_spec_file::BIN) {
      fatal_at(isf->defined_at, "only binary files supported at the moment");
    }
    std::fstream fs(isf->path,std::ios_base::in|std::ios_base::binary);
    if (!fs.good()) {
      fatal_at(isf->defined_at, "unable to open file");
    }
    fs.seekg(0, fs.end);
    size_t file_size = (size_t)fs.tellg();
    if (file_size != so.size_in_bytes) {
      fatal_at(so.init->defined_at,
        "file size doesn't match buffer (",
        file_size,
        " != ",
        so.size_in_bytes,
        ")");
    }
    fs.seekg(0, fs.beg);
    fs.read((char *)host_ptr,file_size);
    if (!fs) {
      fatal_at(so.init->defined_at,
        "failed to read all binary input from file");
    }
    ab.curr += file_size; // fake the advance
    break;
  }
  case init_spec::IS_RND:
    if (elem_type == nullptr) {
      fatal_at(so.init->defined_at, "unable to infer element type for rng init");
    } else if (elem_type->is<type_num>()) {
      // generator_state &gs =
      //  e->get_generator_state(dc, (const init_spec_rng *)so->spec->root, tn);
      fill_buffer_rng(
        *this,
        ec,
        ab,
        (const init_spec_rng *)so.init->root,
        *elem_type,
        so.init->defined_at);
    } else if (elem_type->is<type_vector>()) {
      fill_buffer_rng(
        *this,
        ec,
        ab,
        (const init_spec_rng *)so.init->root,
        elem_type->as<type_vector>().element_type,
        so.init->defined_at);
    } else {
      fatal_at(so.init->defined_at,
        "random inits can only apply to numeric and vector element types");
    }
    break;
  case init_spec::IS_SEQ:
    if (elem_type == nullptr) {
      fatal_at(so.init->defined_at, "unable to infer element type for seq init");
    } else if (elem_type->is<type_num>()) {
      fill_buffer_seq(
        *this,
        ec,
        ab,
        (const init_spec_seq *)so.init->root,
        *elem_type,
        so.init->defined_at);
    } else {
      fatal_at(so.init->defined_at,
        "sequential inits can only apply to numeric element types");
    }
    break;
  case init_spec::IS_FSQ:
    if (elem_type == nullptr) {
      fatal_at(so.init->defined_at, "unable to infer element type for cyc init");
    } else if (elem_type->is<type_num>() || elem_type->is<type_vector>()) {
      fill_buffer_fseq(
        *this,
        ec,
        ab,
        (const init_spec_fseq *)so.init->root,
        *elem_type,
        so.init->defined_at);
    } else {
      fatal_at(so.init->defined_at,
        "fseq inits can only apply to numeric element types");
    }
    break;
  case init_spec::IS_CYC:
    if (elem_type == nullptr) {
      fatal_at(so.init->defined_at, "unable to infer element type for cyc init");
    } else if (elem_type->is<type_num>()) {
      fill_buffer_cyc(
        *this,
        ec,
        ab,
        (const init_spec_cyc *)so.init->root,
        *elem_type,
        so.init->defined_at);
    } else {
      fatal_at(so.init->defined_at,
        "cyc inits can only apply to numeric element types");
    }
    break;
  ////////////////////////////////////////
  // special handling for int since we permit 0 to broadcast
  // to structure types etc...
  case init_spec::IS_INT: {
    int64_t ival = ((const init_spec_int *)so.init->root)->value;
    if (ival == 0) {
      // special handling for 0 initializer
      // all element types accept 0
      ab.fill_with_zeros();
      break;
    }
    [[fallthrough]]; // otherwise fall through
  }
  /////////////////////////////////////////////////////////////////////////
  // Fill with a constant value.
  // this could be a literal or expression (zero is handled above);
  // but either way it needs evaluation
  default: {
    if (elem_type == nullptr) {
      fatal_at(so.init->defined_at,
        "unable to infer element type for scalar init");
    }
    size_t elem_size = elem_type->size();
    if (elem_size == 0) {
      fatal_at(so.init->defined_at,
        "cannot populate a buffer of zero byte type (e.g. void*)");
    } else if (ab.num_left() % elem_size != 0) {
      fatal_at(so.init->defined_at,
        "surface size is not a multiple of element size");
    }
    // stamp out the first element and copy it as many times as needed
    e->eval_into(ec,
      so.init->defined_at,
      (const init_spec_atom *)so.init->root,
      ab,
      *elem_type);
    if (ab.size() != elem_size) {
      internal_at(so.init->defined_at,
        "surface initializer generated wrong element size");
    }
    while (ab.num_left() > 0) {
      ab.write(ab.base, elem_size);
    }
    break;
  } // default
  } // switch

  if (so.init->root->skind != init_spec::IS_UND && ab.num_left() != 0) {
    internal_at(so.init->defined_at,
      "wrong number of elements written by surface initializer");
  }
}

void compiled_script::execute(int itr)
{
  compiled_script_impl *csi = (compiled_script_impl *)impl;
  csi->debug_at(cls::NO_LOC,
    "compiled_script::execute starting iteration ", itr);

  csi->init_surfaces();

  for (script_instruction &si : csi->instructions) {
    switch (si.skind) {
    case script_instruction::DISPATCH: {
      dispatch_command *dc = si.dsc;
      // TODO: use diagnostics::verbose(...)
      csi->verbose(
        "EXECUTING  => ", dc->spec->spec::str(), "\n",
        "              ", dc->str() );
      csi->execute(*dc);
      break;
    }
    case script_instruction::DIFFU: {
      diffu_command *dfuc = (diffu_command *)si.dfuc;
      csi->verbose("EXECUTING  => ", dfuc->spec->spec::str());
      csi->with_buffer_map_read(
        dfuc->spec->defined_at,
        dfuc->so,
        [&] (const void *host_ptr) {csi->execute(*dfuc, host_ptr);});
      break;
    }
    case script_instruction::DIFFS: {
      diffs_command *dfsc = (diffs_command *)si.dfsc;
      csi->with_buffer_map_read(
        dfsc->spec->defined_at,
        dfsc->so_ref,
        [&] (const void *ref_host_ptr) {
          csi->with_buffer_map_read(
            dfsc->spec->defined_at,
            dfsc->so_sut,
            [&] (const void *sut_host_ptr) {
              csi->execute(*dfsc, ref_host_ptr, sut_host_ptr);
            });
        });
      break;
    }
    case script_instruction::PRINT: {
      print_command *prc = (print_command *)si.prc;
      csi->debug("EXECUTING  => ", prc->spec->spec::str());
      csi->with_buffer_map_read(
        prc->spec->defined_at,
        prc->so,
        [&] (const void *host_ptr) {csi->execute(*prc, host_ptr);});
      break;
    }
    case script_instruction::SAVEB: {
      saveb_command *svbc = (saveb_command *)si.svbc;
      if (svbc->so->skind == surface_object::SO_IMAGE) {
        csi->with_image_map_read(
          svbc->spec->defined_at,
          svbc->so,
          [&](size_t row_pitch, size_t slice_pitch, const void *host_ptr)
          {
            save_image(
              svbc->spec->file,
              svbc->spec->defined_at,
              csi,
              svbc->so,
              svbc->so->image_desc.image_width,
              svbc->so->image_desc.image_height,
              row_pitch, slice_pitch,
              svbc->so->image_format.image_channel_order,
              svbc->so->image_format.image_channel_data_type,
              host_ptr);
          });
      } else {
        csi->with_buffer_map_read(
          svbc->spec->defined_at,
          svbc->so,
          [&](const void *host_ptr) {csi->execute(*svbc, host_ptr);});
      }
      break;
    }
    case script_instruction::SAVEI: {
      savei_command *svic = (savei_command *)si.svic;
      if (svic->so->skind != surface_object::SO_BUFFER) {
        csi->fatal_at(svic->spec->defined_at,
          "save_image requires buffer argument");
      }
      csi->with_buffer_map_read(
        svic->spec->defined_at,
        svic->so,
        [&](const void *host_ptr) {csi->execute(*svic, host_ptr);});
      break;
    }
    default:
      csi->internal("unsupported instruction");
    }
  }
}