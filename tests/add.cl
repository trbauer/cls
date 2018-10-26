#ifdef ENABLE_FP16
#pragma OPENCL EXTENSION cl_khr_fp16 : enable
#endif
#ifdef ENABLE_FP64
#pragma OPENCL EXTENSION cl_khr_fp64 : enable
#endif

kernel void add(
  global T *val,
  T  arg)
{
  uint id = get_global_id(0);
  val[id] += arg;
}
kernel void addv(
  global T *val,
  const global T *args)
{
  uint id = get_global_id(0);
  val[id] += args[id];
}