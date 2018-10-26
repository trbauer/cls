#include "cls_interp_internal.hpp"
#include "../text.hpp"

#include <chrono>
#include <fstream>


disp_times compiled_script::get_wall_times() const
{
  disp_times ts;
  const compiled_script_impl *csi = (const compiled_script_impl *)impl;
  for (const dispatch_command *dc : csi->dispatches)
    ts.emplace_back(dc->spec,dc->wall_times);
  return ts;
}

disp_times compiled_script::get_prof_times() const
{
  disp_times ts;
  const compiled_script_impl *csi = (const compiled_script_impl *)impl;
  for (const dispatch_command *dc : csi->dispatches)
    ts.emplace_back(dc->spec,dc->prof_times);
  return ts;
}

init_times compiled_script::get_init_times() const
{
  init_times ts;
  const compiled_script_impl *csi = (const compiled_script_impl *)impl;
  for (const surface_object *so : csi->surfaces)
    ts.emplace_back(so->spec,so->init_times);
  return ts;
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
  auto val = e->evalTo<T>(ec, is);
  size_t total_elems = ab.capacity / sizeof(T);
  for (size_t i = 0; i < total_elems; i++) {
    ab.write<T>(val.as<T>());
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
  val v_lo = (T)0;
  val v_hi = std::numeric_limits<T>::max();
  if (isr->e_lo) {
    v_lo = e->evalTo<T>(ec, isr->e_lo);
  }
  if (isr->e_hi) {
    v_hi = e->evalTo<T>(ec, isr->e_hi);
  }
  if (v_lo.as<T>() > v_hi.as<T>()) {
    e->fatalAt(isr->defined_at,"low bound > high bound");
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
    v_hi = e->evalToF(ec, isr->e_hi);
    if (isr->e_lo) {
      v_lo = e->evalToF(ec, isr->e_lo);
    }
  }
  if (v_lo.as<T>() > v_hi.as<T>()) {
    e->fatalAt(isr->defined_at,"low bound > high bound");
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
  } else if (t.is<type_struct>()) {
    const type_struct &ts = t.as<type_struct>();
    if (ts.is_uniform() && ts.elements[0]->is<type_num>()) {
      const type_num &tn = ts.elements[0]->as<type_num>();
      fill_buffer_rng(csi, ec, ab, isr, tn, at);
    } else {
      csi.fatalAt(at,"cannot broadcast random for this struct");
    }
  } else {
    csi.fatalAt(at,"unsupported type for random generator");
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
    v_base = e->evalTo<R>(ec, iss->base);

  val v_delta((R)1);
  if (iss->delta)
    v_delta = e->evalTo<R>(ec, iss->delta);

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
      case 1: fill_buffer_seq_loop<int8_t>(csi.e,ec,ab,iss); break;
      case 2: fill_buffer_seq_loop<int16_t>(csi.e,ec,ab,iss); break;
      case 4: fill_buffer_seq_loop<int32_t>(csi.e,ec,ab,iss); break;
      case 8: fill_buffer_seq_loop<int64_t>(csi.e,ec,ab,iss); break;
      }
      break;
    }
    case type_num::UNSIGNED:
      switch (tn.size_in_bytes) {
      case 1: fill_buffer_seq_loop<uint8_t>(csi.e,ec,ab,iss); break;
      case 2: fill_buffer_seq_loop<uint16_t>(csi.e,ec,ab,iss); break;
      case 4: fill_buffer_seq_loop<uint32_t>(csi.e,ec,ab,iss); break;
      case 8: fill_buffer_seq_loop<uint64_t>(csi.e,ec,ab,iss); break;
      }
      break;
    case type_num::FLOATING:
      switch (tn.size_in_bytes) {
      case 2: fill_buffer_seq_loop<half,float>(csi.e,ec,ab,iss); break;
      case 4: fill_buffer_seq_loop<float>(csi.e,ec,ab,iss); break;
      case 8: fill_buffer_seq_loop<double>(csi.e,ec,ab,iss); break;
      }
      break;
    }
  } else if (t.is<type_struct>()) {
    const type_struct &ts = t.as<type_struct>();
    if (ts.is_uniform() && ts.elements[0]->is<type_num>()) {
      const type_num &tn = ts.elements[0]->as<type_num>();
      fill_buffer_seq(csi, ec, ab, iss, tn, at);
    } else {
      csi.fatalAt(at,"cannot broadcast random for this struct");
    }
  } else {
    csi.fatalAt(at,"unsupported type for random generator");
  }
}

void compiled_script_impl::execute(dispatch_command &dc)
{
  cl_command_queue queue = (*dc.dobj->queue)();
  cl_kernel kernel = (*dc.kernel->kernel)();
  loc dc_at = dc.dobj->spec->defined_at;

  auto printSurfaces = [&] (bool is_pre) {
    for (const auto &sinfo : dc.surfaces) {
      const surface_object *so = std::get<0>(sinfo);
      if (is_pre && so->spec->print_pre ||
        !is_pre && so->spec->print_post)
      {
        const type &t = std::get<1>(sinfo);
        const arg_info &ai = std::get<2>(sinfo);
        const loc &at = std::get<3>(sinfo);

        std::cout << ai.type.syntax() << "  " << ai.name << " = " <<
          so->str() << "\n";

        int elems_per_row = is_pre ?
          so->spec->print_pre_elems_per_row :
          so->spec->print_post_elems_per_row;

        withBufferMapRead(
          at,
          so,
          [&] (const void *host_ptr) {
            formatBuffer(
              std::cout,
              host_ptr,
              so->size_in_bytes,
              t,
              elems_per_row);
            std::cout << "\n";
          });
      }
    }
  };

  printSurfaces(true);

  auto start_execute = std::chrono::high_resolution_clock::now();

  cl_event enq_evt;
  CL_COMMAND(dc_at,
    clEnqueueNDRangeKernel,
      queue,
      kernel,
      (cl_uint)dc.global_size.rank(),
      nullptr, // global offset
      dc.global_size.get(),
      dc.local_size.rank() > 0 ? dc.local_size.get() : nullptr,
      0,
      nullptr,
      &enq_evt);

  CL_COMMAND(dc_at,clWaitForEvents,1,&enq_evt);

  auto duration_exec =
    std::chrono::duration_cast<std::chrono::microseconds>(
      std::chrono::high_resolution_clock::now() - start_execute);
  dc.wall_times.add(duration_exec.count()/1000.0/1000.0);

  if (os.prof_time) {
    cl_ulong st;
    CL_COMMAND(dc_at,
      clGetEventProfilingInfo,
        enq_evt, CL_PROFILING_COMMAND_START, sizeof(st), &st, nullptr);
    cl_ulong en;
    CL_COMMAND(dc_at,
      clGetEventProfilingInfo,
        enq_evt, CL_PROFILING_COMMAND_END, sizeof(en), &en, nullptr);
    dc.prof_times.add((en - st)/1000.0/1000.0/1000.0);
  }

  CL_COMMAND(dc_at,
    clReleaseEvent, enq_evt);

  printSurfaces(false);

  CL_COMMAND(dc_at,
    clFinish, queue);
}

void compiled_script_impl::execute(
  diffs_command &dfc,
  const void *ref_host_ptr,
  const void *sut_host_ptr)
{
  evaluator::context ec{ndr(),ndr()};
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
    fatalAt(
      dfc.spec->defined_at,
      "buffer size is not a multiple of diff element type ",
      elem_type->syntax());
  }

  size_t total_elems = dfc.so_sut->size_in_bytes / elem_type->size();
  const uint8_t *ref_host_ptr8 = (const uint8_t*)ref_host_ptr;
  const uint8_t *sut_host_ptr8 = (const uint8_t*)sut_host_ptr;
  for (size_t elem_ix = 0; elem_ix < total_elems; elem_ix++) {
    if (memcmp(
      ref_host_ptr8 + elem_ix*elem_type->size(),
      sut_host_ptr8 + elem_ix*elem_type->size(),
      elem_type->size()))
    {
      std::cerr << "mismatch on element "
        << elem_ix << " (type " << elem_type->syntax() << ")\n";
      std::cerr << "============== vs. (SUT) ==============\n";
      formatBufferElement(
        std::cerr,
        *elem_type,
        sut_host_ptr8 + elem_ix*elem_type->size());
      std::cerr << "\n";
      std::cerr << "============== vs. (REF) ==============\n";
      formatBufferElement(
        std::cerr,
        *elem_type,
        ref_host_ptr8 + elem_ix*elem_type->size());
      std::cerr << "\n";
      fatalAt(
        dfc.spec->defined_at,
        "mismatch on element ",elem_ix," (type ",elem_type->syntax(),")");
    } // if mismatch
  }
}

void compiled_script_impl::execute(diffu_command &dfc, const void *host_ptr)
{
  evaluator::context ec{ndr(),ndr()};
  if (dfc.so->size_in_bytes == 0)
    return; // zero sized buffers always match
  if (!dfc.spec->ref.value->is_atom())
    fatalAt(dfc.spec->defined_at, "only atoms supported as reference argument");
  const type *elem_type = dfc.element_type;
  if (elem_type == nullptr) {
    elem_type = (dfc.so->size_in_bytes > 4 && dfc.so->size_in_bytes % 4 == 0) ?
      &UINT() : &UCHAR();
  }

  // given an explicit type we make a broadcast comparison
  arg_buffer ab_ref(this, dfc.spec->defined_at, elem_type->size());
  e->evalInto(ec,
    dfc.spec->defined_at,
    (const init_spec_atom *)dfc.spec->ref.value,
    ab_ref,
    *elem_type);
  if (ab_ref.num_left() != 0) {
    fatalAt(dfc.spec->defined_at, "reference scalar value is wrong size");
  }

  if (elem_type->size() == 0 ||
    dfc.so->size_in_bytes % elem_type->size() != 0) {
    fatalAt(
      dfc.spec->defined_at,
      "buffer size is not a multiple of diff element type ",
      elem_type->syntax());
  }
  size_t total_elems = dfc.so->size_in_bytes / elem_type->size();
  const uint8_t *host_ptr8 = (const uint8_t*)host_ptr;
  for (size_t elem_ix = 0; elem_ix < total_elems; elem_ix++) {
    if (memcmp(
      host_ptr8 + elem_ix*elem_type->size(),
      ab_ref.base,
      elem_type->size()))
    {
      std::cerr << "mismatch on element "
        << elem_ix << " (type " << elem_type->syntax() << ")\n";
      std::cerr << "============== vs. (SUT) ==============\n";
      formatBufferElement(
        std::cerr,
        *elem_type,
        host_ptr8 + elem_ix*elem_type->size());
      std::cerr << "\n";
      std::cerr << "============== vs. (REF) ==============\n";
      formatBufferElement(
        std::cerr,
        *elem_type,
        ab_ref.base);
      std::cerr << "\n";
      fatalAt(
        dfc.spec->defined_at,
        "mismatch on element ",elem_ix," (type ",elem_type->syntax(),")");
    } // if mismatch
  } // for elems
}

void compiled_script_impl::execute(print_command &prc, const void *host_ptr)
{
  evaluator::context ec{ndr(),ndr()};

  const type *elem_type = prc.element_type;
  if (elem_type == nullptr) {
    elem_type = (prc.so->size_in_bytes > 4 && prc.so->size_in_bytes % 4 == 0) ?
      &UINT() : &UCHAR();
  }
  std::cout << "PRINT";
  if (prc.element_type)
    std::cout << "<" << elem_type->syntax() << ">";
  std::cout << "[" << prc.so->str() << "] =>\n";
  formatBuffer(
    std::cout,
    host_ptr,
    prc.so->size_in_bytes,
    *elem_type,
    prc.spec->elements_per_row);
  std::cout << "\n";
}

void compiled_script_impl::execute(save_command &svc, const void *host_ptr)
{
  std::ofstream of(svc.spec->file,std::ios::binary);
  if (!of.good()) {
    fatalAt(svc.spec->defined_at,"failed to open file");
  }
  of.write((const char *)host_ptr, svc.so->size_in_bytes);
  if (!of) {
    fatalAt(svc.spec->defined_at,"failed to write file");
  }
  of.flush();
}

void cl_fatal_handler::withBufferMapRead(
  const loc &at,
  const surface_object *so,
  buffer_reader apply)
{
  void *host_ptr = nullptr;
  CL_COMMAND_CREATE(host_ptr, at,
    clEnqueueMapBuffer,
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
    clEnqueueUnmapMemObject,
      so->queue,
      so->memobj,
      host_ptr,
      0,
      nullptr,
      nullptr);
}

void cl_fatal_handler::withBufferMapWrite(
  const loc &at,
  surface_object *so,
  buffer_writer apply)
{
  void *host_ptr = nullptr;
  CL_COMMAND_CREATE(host_ptr, at,
    clEnqueueMapBuffer,
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
    clEnqueueUnmapMemObject,
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
      continue;

    auto t_start = std::chrono::high_resolution_clock::now();

    dispatch_command *dc = nullptr;
    const type *elem_type = nullptr;
    if (!so->dispatch_uses.empty()) {
      const surface_object::use &u = so->dispatch_uses.front();
      dc = std::get<0>(u);
      const arg_info &ai = std::get<2>(u);
      if (ai.type.is<type_ptr>()) {
        elem_type = ai.type.as<type_ptr>().element_type;
      }
    } else if (!so->dummy_object) { // no valid uses found
      fatalAt(so->spec->defined_at,"no uses of this surface found");
    }

    withBufferMapWrite(
      so->spec->defined_at,
      so,
      [&] (void *host_ptr) {
        evaluator::context ec(dc->global_size, dc->local_size);

        init_surface(*so, ec, elem_type, host_ptr);
      });

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
  arg_buffer ab(this, so.spec->defined_at, host_ptr, so.size_in_bytes);
  switch (so.spec->root->skind) {
  case init_spec::IS_FIL: {
    const init_spec_file *isf = (const init_spec_file *)so.spec->root;
    if (isf->flavor != init_spec_file::BIN) {
      fatalAt(isf->defined_at,"only binary files supported at the moment");
    }
    std::fstream fs(isf->path,std::ios_base::in|std::ios_base::binary);
    if (!fs.good()) {
      fatalAt(isf->defined_at,"unable to open file");
    }
    fs.seekg(0, fs.end);
    size_t file_size = fs.tellg();
    if (file_size != so.size_in_bytes) {
      fatalAt(so.spec->defined_at,
        "file size doesn't match buffer (",
        file_size,
        " != ",
        so.size_in_bytes,
        ")");
    }
    fs.seekg(0, fs.beg);
    fs.read((char *)host_ptr,file_size);
    if (!fs) {
      fatalAt(so.spec->defined_at,
        "failed to read all binary input from file");
    }
    ab.curr += file_size; // fake the advance
    break;
  }
  case init_spec::IS_RND: {
    if (elem_type == nullptr) {
      fatalAt(so.spec->defined_at, "unable to infer element type for rng init");
    } else if (elem_type->is<type_num>()) {
      // generator_state &gs =
      //  e->get_generator_state(dc, (const init_spec_rng *)so->spec->root, tn);
      fill_buffer_rng(
        *this,
        ec,
        ab,
        (const init_spec_rng *)so.spec->root,
        *elem_type,
        so.spec->defined_at);
    } else if (
      elem_type->is<type_struct>() &&
      elem_type->as<type_struct>().is_uniform())
    {
      fill_buffer_rng(
        *this,
        ec,
        ab,
        (const init_spec_rng *)so.spec->root,
        *elem_type->as<type_struct>().elements[0],
        so.spec->defined_at);
    } else {
      fatalAt(so.spec->defined_at,
        "random inits can only apply to numeric element types");
    }
    break;
  }
  case init_spec::IS_SEQ: {
    if (elem_type == nullptr) {
      fatalAt(so.spec->defined_at, "unable to infer element type for seq init");
    } else if (elem_type->is<type_num>()) {
      // generator_state &gs =
      //  e->get_generator_state(dc, (const init_spec_rng *)so->spec->root, tn);
      fill_buffer_seq(
        *this,
        ec,
        ab,
        (const init_spec_seq *)so.spec->root,
        *elem_type,
        so.spec->defined_at);
    } else {
      fatalAt(so.spec->defined_at,
        "sequential inits can only apply to numeric element types");
    }
    break;
  }
  ////////////////////////////////////////
  // special handling for int since we permit 0 to broadcast
  // to structure types etc...
  case init_spec::IS_INT: {
    int64_t ival = ((const init_spec_int *)so.spec->root)->value;
    if (ival == 0) {
      // special handling for 0 initializer
      // all element types accept 0
      ab.fill_with_zeros();
      break;
    }
    [[fallthrough]]; // otherwise fall through
  }
  /////////////////////////////////////////////////////////////////////////
  // Some other expression that needs evaluation
  // NOTE: this could be a literal or expression requiring evaluation
  default: {
    if (elem_type == nullptr) {
      fatalAt(so.spec->defined_at,
        "unable to infer element type for scalar init");
    }
    size_t elem_size = elem_type->size();
    if (elem_size == 0) {
      fatalAt(so.spec->defined_at,
        "cannot populate a buffer of zero byte type (e.g. void*)");
    } else if (ab.num_left() % elem_size != 0) {
      fatalAt(so.spec->defined_at,
        "surface size is not a multiple of element size");
    }
    // stamp out the first element and copy it as many times as needed
    e->evalInto(ec,
      so.spec->defined_at,
      (const init_spec_atom *)so.spec->root,
      ab,
      *elem_type);
    if (ab.size() != elem_size) {
      internalAt(so.spec->defined_at,
        "surface initializer generated wrong element size");
    }
    while (ab.num_left() > 0) {
      ab.write(ab.base,elem_size);
    }
    break;
  } // default
  } // switch
  if (ab.num_left() != 0) {
    internalAt(so.spec->defined_at,
      "wrong number of elements written by surface initializer");
  }
}

void compiled_script::execute(int)
{
  compiled_script_impl *csi = (compiled_script_impl *)impl;
  csi->os.debug() << "compiled_script::execute\n";

  csi->init_surfaces();

  for (script_instruction &si : csi->instructions) {
    switch (si.skind) {
    case script_instruction::DISPATCH: {
      dispatch_command *dc = si.dsc;
      csi->os.verbose() << "EXECUTING  => " << dc->spec->spec::str() << "\n";
      csi->os.verbose() << "              " << dc->str() << "\n";
      csi->execute(*dc);
      break;
    }
    case script_instruction::DIFFU: {
      diffu_command *dfuc = (diffu_command *)si.dfuc;
      csi->os.verbose() << "EXECUTING  => " << dfuc->spec->spec::str() << "\n";
      csi->withBufferMapRead(
        dfuc->spec->defined_at,
        dfuc->so,
        [&] (const void *host_ptr) {csi->execute(*dfuc, host_ptr);});
      break;
    }
    case script_instruction::DIFFS: {
      diffs_command *dfsc = (diffs_command *)si.dfsc;
      csi->withBufferMapRead(
        dfsc->spec->defined_at,
        dfsc->so_ref,
        [&] (const void *ref_host_ptr) {
          csi->withBufferMapRead(
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
      csi->os.verbose() << "EXECUTING  => " << prc->spec->spec::str() << "\n";
      csi->withBufferMapRead(
        prc->spec->defined_at,
        prc->so,
        [&] (const void *host_ptr) {csi->execute(*prc, host_ptr);});
      break;
    }
    case script_instruction::SAVE: {
      save_command *svc = (save_command *)si.svc;
      csi->withBufferMapRead(
        svc->spec->defined_at,
        svc->so,
        [&] (const void *host_ptr) {csi->execute(*svc, host_ptr);});
      break;
    }
    default:
      std::cerr << "UNSUPPORTED INSTRUCTION!\n";
      exit(1);
    }
  }
}