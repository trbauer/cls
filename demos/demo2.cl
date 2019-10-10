kernel void div(global float *dst)
{
  int id = get_global_id(0);
  dst[id] /= (float)(get_local_id(0) - 1);
}