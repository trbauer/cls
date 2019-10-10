kernel void test(global float *dst)
{
  int id = get_global_id(0);
  dst[id] = sqrt(dst[id]);
}