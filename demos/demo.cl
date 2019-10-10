kernel void addK(
  global int *dst, global int *src0, global int *src1, int k)
{
  int id = get_global_id(0);
  dst[id] = src0[id] + src1[id] + k;
}