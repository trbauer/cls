#include "cls_interp_internal.hpp"
#include "../text.hpp"

#include <chrono>

void compiled_script::execute(int)
{
  compiled_script_impl *csi = (compiled_script_impl *)impl;
  csi->os.debug() << "compiled_script::execute\n";
  for (dispatch_command *dc : csi->dispatches) {
    csi->os.verbose() << "EXECUTING  => " << dc->ds->spec::str() << "\n";
    csi->os.verbose() << "              " << dc->str() << "\n";

    csi->init_surfaces(*dc);

    csi->execute(*dc);
  }
}

times compiled_script::get_wall_times() const
{
  times ts;
  const compiled_script_impl *csi = (const compiled_script_impl *)impl;
  for (const dispatch_command *dc : csi->dispatches)
    ts.emplace_back(dc->ds,dc->wall_times);
  return ts;
}

times compiled_script::get_prof_times() const
{
  times ts;
  const compiled_script_impl *csi = (const compiled_script_impl *)impl;
  for (const dispatch_command *dc : csi->dispatches)
    ts.emplace_back(dc->ds,dc->prof_times);
  return ts;
}

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
  switch (tn.kind) {
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

template <typename T>
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
  std::uniform_int_distribution<uint64_t> dist(
    v_lo.u64,
    v_hi.u64);
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
  const loc &arg_loc)
{
  std::mt19937 gen(csi.e->rd());
  if (isr->has_seed) {
    gen.seed((unsigned)isr->seed);
  }

  if (std::holds_alternative<type_num>(t.var)) {
    const type_num &tn = std::get<type_num>(t.var);
    switch (tn.kind) {
    case type_num::SIGNED: {
      switch (tn.size_in_bytes) {
      case 1: fill_buffer_rng_loop_int<int8_t>(csi.e,ec,ab,isr,gen);
      case 2: fill_buffer_rng_loop_int<int16_t>(csi.e,ec,ab,isr,gen);
      case 4: fill_buffer_rng_loop_int<int32_t>(csi.e,ec,ab,isr,gen);
      case 8: fill_buffer_rng_loop_int<int64_t>(csi.e,ec,ab,isr,gen);
      }
      break;
    }
    case type_num::UNSIGNED:
      switch (tn.size_in_bytes) {
      case 1: fill_buffer_rng_loop_int<uint8_t>(csi.e,ec,ab,isr,gen);
      case 2: fill_buffer_rng_loop_int<uint16_t>(csi.e,ec,ab,isr,gen);
      case 4: fill_buffer_rng_loop_int<uint32_t>(csi.e,ec,ab,isr,gen);
      case 8: fill_buffer_rng_loop_int<uint64_t>(csi.e,ec,ab,isr,gen);
      }
      break;
    case type_num::FLOATING:
      switch (tn.size_in_bytes) {
      case 2: fill_buffer_rng_loop_flt<half,float>(csi.e,ec,ab,isr,gen);
      case 4: fill_buffer_rng_loop_flt<float>(csi.e,ec,ab,isr,gen);
      case 8: fill_buffer_rng_loop_flt<double>(csi.e,ec,ab,isr,gen);
      }
      break;
    }
  } else if (std::holds_alternative<type_struct>(t.var)) {
    const type_struct &ts = std::get<type_struct>(t.var);
    if (ts.is_uniform() && std::holds_alternative<type_num>(ts.elements[0]->var)) {
      const type_num &tn = std::get<type_num>(ts.elements[0]->var);
      fill_buffer_rng(csi, ec, ab, isr, tn, arg_loc);
    } else {
      csi.fatalAt(arg_loc,"cannot broadcast random for this struct");
    }
  } else {
    // TODO: we could support broadcast semantics here
    csi.fatalAt(arg_loc,"unsupported type for random generator");
  }
}

void compiled_script_impl::init_surfaces(dispatch_command &dc)
{
  cl_command_queue cq = (*dc.dobj->queue)();
  cl_kernel krn = (*dc.kernel->kernel)();
  evaluator::context ec(dc.global_size,dc.local_size);

  for (const auto &p : dc.inits) {
    const loc &arg_loc = std::get<0>(p);
    const type_ptr &tp = std::get<1>(p);
    surface_object *so = std::get<2>(p);
    withBufferMapWrite(
      arg_loc,
      cq,
      so,
      [&] (void *host_ptr)
    {
      arg_buffer ab(this, arg_loc, host_ptr, so->size_in_bytes);

      switch (so->spec->root->kind) {
      case init_spec::IS_FIL: {
        const init_spec_file *isf = (const init_spec_file *)so->spec->root;
        std::fstream fs(isf->path,std::ios_base::in|std::ios_base::binary);
        if (!fs.good()) {
          fatalAt(isf->defined_at,"unable to open file");
        }
        fs.seekg(0, fs.end);
        size_t file_size = fs.tellg();
        if (file_size != so->size_in_bytes) {
          fatalAt(arg_loc,
            "file size doesn't match buffer (",
            file_size,
            " != ",
            so->size_in_bytes,
            ")");
        }
        fs.seekg(0, fs.beg);
        fs.read((char *)host_ptr,file_size);
        if (!fs) {
          fatalAt(arg_loc, "failed to read all binary input from file");
        }
        break;
      }
      case init_spec::IS_RND: {
        if (std::holds_alternative<type_num>(tp.element_type->var)) {
          const type_num &tn = std::get<type_num>(tp.element_type->var);
          // generator_state &gs =
          //  e->get_generator_state(dc, (const init_spec_rng *)so->spec->root, tn);
          fill_buffer_rng(
            *this,
            ec,
            ab,
            (const init_spec_rng *)so->spec->root,
            *tp.element_type,
            arg_loc);
        } else {
          fatalAt(arg_loc,"random inits can only apply to numeric element types");
        }
        break;
      }
      ////////////////////////////////////////
      // special handling for int since we permit 0 to broadcast
      // to structure types etc...
      case init_spec::IS_INT: {
        int64_t ival = ((const init_spec_int *)so->spec->root)->value;
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
        size_t elem_size = tp.element_type->size();
        if (elem_size == 0) {
          fatalAt(arg_loc,"cannot populate a buffer of zero byte type (e.g. void*)");
        } else if (ab.num_left() % elem_size != 0) {
          fatalAt(arg_loc,"surface size is not a multiple of element size");
        }
        // stamp out the first element and copy it as many times as needed
        e->evalInto(ec,
          arg_loc,
          (const init_spec_atom *)so->spec->root,
          ab,
          *tp.element_type);
        if (ab.size() != elem_size) {
          fatalAt(arg_loc,"buffer element generated is wrong size");
        }
        while (ab.num_left() > 0) {
          ab.write(ab.base,elem_size);
        }
        break;
      } // default
      } // switch
      if (ab.num_left() != 0) {
        fatalAt(arg_loc,"wrong number of random elements written");
      }
    });
  }
}

void compiled_script_impl::execute(dispatch_command &dc)
{
  cl_command_queue cq = (*dc.dobj->queue)();
  cl_kernel krn = (*dc.kernel->kernel)();
  loc dc_at = dc.dobj->spec->defined_at;

  for (const auto &p : dc.inits) {
    type_ptr tp = std::get<1>(p);
    const surface_object *so = std::get<2>(p);
    if (so->spec->print_pre) {
      withBufferMapRead(
        std::get<0>(p),
        cq,
        so,
        [&] (const void *host_ptr) {
          formatBuffer(std::cout,host_ptr,so->size_in_bytes,tp);
          std::cout << "\n";
        });
    }
  }

  auto start_execute = std::chrono::high_resolution_clock::now();

  cl_event enq_evt;
  CL_COMMAND(dc_at,clEnqueueNDRangeKernel,
    cq,
    krn,
    (cl_uint)dc.global_size.dimensions(),
    nullptr, // global offset
    dc.global_size.get(),
    dc.local_size.dimensions() > 0 ? dc.local_size.get() : nullptr,
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

  CL_COMMAND(dc_at,clReleaseEvent,enq_evt);

  for (const auto &p : dc.inits) {
    type_ptr tp = std::get<1>(p);
    const surface_object *so = std::get<2>(p);
    if (so->spec->print_post) {
      withBufferMapRead(
        std::get<0>(p),
        cq,
        so,
        [&] (const void *host_ptr) {
          formatBuffer(std::cout,host_ptr,so->size_in_bytes,tp);
          std::cout << "\n";
        });
    }
  }

  CL_COMMAND(dc_at,clFinish,cq);
}

void cl_fatal_handler::withBufferMapRead(
  loc at,
  cl_command_queue cq,
  const surface_object *so,
  buffer_reader apply)
{
  void *host_ptr = nullptr;
  CL_COMMAND_CREATE(host_ptr, at,
    clEnqueueMapBuffer,
      cq,
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
      cq,
      so->memobj,
      host_ptr,
      0,
      nullptr,
      nullptr);
}

void cl_fatal_handler::withBufferMapWrite(
  loc at,
  cl_command_queue cq,
  surface_object *so,
  buffer_writer apply)
{
  void *host_ptr = nullptr;
  CL_COMMAND_CREATE(host_ptr, at,
    clEnqueueMapBuffer,
      cq,
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
      cq,
      so->memobj,
      host_ptr,
      0,
      nullptr,
      nullptr);
}