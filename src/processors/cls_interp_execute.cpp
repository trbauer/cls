#include "cls_interp_internal.hpp"
#include "../text.hpp"

#include <chrono>

void compiled_script::setup(int)
{
  std::cout << "compiled_script::setup\n";
}

void compiled_script::execute(int)
{
  std::cout << "compiled_script::execute\n";
  compiled_script_impl *csi = (compiled_script_impl *)impl;
  for (dispatch_command &dc : csi->dispatches) {
    std::cout << "EXECUTING  => " << dc.ds->spec::str() << "\n";
    std::cout << "              " << dc.str() << "\n";

    csi->init_surfaces(dc);

    csi->execute(dc);
  }
}

template <typename T,typename C>
static void fill_buffer_const_loop(
  arg_buffer &ab,
  C val)
{
  if (val > (C)std::numeric_limits<T>::max() ||
    val < (C)std::numeric_limits<T>::min())
  {
    ab.fatalAt(ab.arg_loc, "value out of bounds");
  }
  size_t total_elems = ab.capacity / sizeof(T);
  for (size_t i = 0; i < total_elems; i++) {
    ab.write<T>((T)val);
  }
}

template <typename C>
static void fill_buffer_const(
  arg_buffer &ab,
  const loc &arg_loc,
  const type_num &tn,
  C val)
{
  switch (tn.kind) {
  case type_num::SIGNED:
    switch (tn.size_in_bytes) {
    case 1: fill_buffer_const_loop<int8_t>(ab,val); break;
    case 2: fill_buffer_const_loop<int16_t>(ab,val); break;
    case 4: fill_buffer_const_loop<int32_t>(ab,val); break;
    case 8: fill_buffer_const_loop<int64_t>(ab,val); break;
    }
    break;
  case type_num::UNSIGNED:
    switch (tn.size_in_bytes) {
    case 1: fill_buffer_const_loop<uint8_t>(ab,val); break;
    case 2: fill_buffer_const_loop<uint16_t>(ab,val); break;
    case 4: fill_buffer_const_loop<uint32_t>(ab,val); break;
    case 8: fill_buffer_const_loop<uint64_t>(ab,val); break;
    }
    break;
  case type_num::FLOATING:
    switch (tn.size_in_bytes) {
    case 2: {
      size_t total_elems = ab.capacity / sizeof(cl_half);
      for (size_t i = 0; i < total_elems; i++) {
        ab.write<cl_half>(float_to_half((float)val));
      }
    }
    case 4: fill_buffer_const_loop<float>(ab,val); break;
    case 8: fill_buffer_const_loop<double>(ab,val); break;
    }
    break;
  }
}


template <typename T,typename D>
static void fill_buffer_rng_loop(
  arg_buffer &ab,
  const D &dist,
  std::mt19937 &g)
{
  size_t total_elems = ab.capacity / sizeof(T);
  for (size_t i = 0; i < total_elems; i++) {
    ab.write<T>((T)dist(g));
  }
}

static void fill_buffer_rng(
  compiled_script_impl &csi,
  arg_buffer &ab,
  generator_state &gs,
  const type &t,
  const loc &arg_loc)
{
  if (std::holds_alternative<type_num>(t.var)) {
    const type_num &tn = std::get<type_num>(t.var);
    switch (tn.kind) {
    case type_num::SIGNED:
      switch (tn.size_in_bytes) {
      case 1: fill_buffer_rng_loop<int8_t>(ab,gs.s_dist,gs.gen);
      case 2: fill_buffer_rng_loop<int16_t>(ab,gs.s_dist,gs.gen);
      case 4: fill_buffer_rng_loop<int32_t>(ab,gs.s_dist,gs.gen);
      case 8: fill_buffer_rng_loop<int64_t>(ab,gs.s_dist,gs.gen);
      }
      break;
    case type_num::UNSIGNED:
      switch (tn.size_in_bytes) {
      case 1: fill_buffer_rng_loop<uint8_t>(ab,gs.u_dist,gs.gen);
      case 2: fill_buffer_rng_loop<uint16_t>(ab,gs.u_dist,gs.gen);
      case 4: fill_buffer_rng_loop<uint32_t>(ab,gs.u_dist,gs.gen);
      case 8: fill_buffer_rng_loop<uint64_t>(ab,gs.u_dist,gs.gen);
      }
      break;
    case type_num::FLOATING:
      switch (tn.size_in_bytes) {
      case 2: {
        for (size_t i = 0; i < ab.capacity / sizeof(cl_half); i++) {
          cl_half h = float_to_half((float)gs.f_dist(gs.gen));
          ab.write<cl_half>(h);
        }
        break;
      }
      case 4: fill_buffer_rng_loop<float>(ab,gs.f_dist,gs.gen);
      case 8: fill_buffer_rng_loop<double>(ab,gs.f_dist,gs.gen);
      }
      break;
    }
  } else {
    csi.fatalAt(arg_loc,"unsupported type for random generator");
  }
}

void compiled_script_impl::init_surfaces(dispatch_command &dc)
{
  cl_command_queue cq = (*dc.dobj->queue)();
  cl_kernel krn = (*dc.kernel->kernel)();

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
          generator_state &gs =
            e->get_state(dc,(const init_spec_file *)so->spec->root);
          gs.file.seekg(0, gs.file.beg);
          if (gs.file_size != so->size_in_bytes) {
            fatalAt(arg_loc,
              "file size doesn't match buffer (",
              gs.file_size,
              " != ",
              so->size_in_bytes,
              ")");
          }
          gs.file.read((char *)host_ptr,gs.file_size);
          if (!gs.file) {
            fatalAt(arg_loc, "failed to read all binary input");
          }
          break;
        }
        case init_spec::IS_RND: {
          // TODO: needs get_state to not need type arg
          if (std::holds_alternative<type_num>(tp.element_type->var)) {
            const type_num &tn = std::get<type_num>(tp.element_type->var);
            generator_state &gs =
              e->get_state(dc, (const init_spec_rng *)so->spec->root, tn);
            fill_buffer_rng(
              *this,
              ab,
              gs,
              *tp.element_type,
              arg_loc);
          } else {
            fatalAt(arg_loc,"INTERNAL ERROR: not a number");
          }
          break;
        }
        ////////////////////////////////////////
        // constant values
        case init_spec::IS_INT: {
          int64_t ival = ((const init_spec_int *)so->spec->root)->value;
          if (std::holds_alternative<type_num>(tp.element_type->var)) {
            const type_num &tn = std::get<type_num>(tp.element_type->var);
            fill_buffer_const(ab, arg_loc, tn, ival);
          } else {
            fatalAt(arg_loc,"INTERNAL ERROR: not a number");
          }
          break;
        }
        case init_spec::IS_FLT: {
          double fval = ((const init_spec_float *)so->spec->root)->value;
          if (std::holds_alternative<type_num>(tp.element_type->var)) {
            const type_num &tn = std::get<type_num>(tp.element_type->var);
            fill_buffer_const(ab, arg_loc, tn, fval);
          } else {
            fatalAt(arg_loc,"INTERNAL ERROR: not a number");
          }
          break;
        }
        default:
          // need to evaluate the value to a single type
          // run one at a time
          fatalAt(arg_loc,"INTERNAL ERROR: single element per time not implemented");
          break;
        }
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
    dc.global_size.dimensions() > 0 ? dc.global_size.get() : nullptr,
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