#ifndef TYPE
#define TYPE float
#endif

kernel void add(global TYPE *dst, global TYPE *src0, global TYPE *src1)
{
  int id = get_global_id(0);
  dst[id] = src0[id] + src1[id];
}

kernel void interp(global TYPE *dst, global TYPE *src0, global TYPE *src1, TYPE t)
{
  int id = get_global_id(0);
  dst[id] = (1.0 - t)*src0[id] + t*src1[id];
}
