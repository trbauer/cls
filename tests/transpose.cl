kernel void transpose(global T *dst,global T *src)
{
  uint x = get_global_id(0),
       y = get_global_id(1);
  uint src_ix = y*get_global_size(0) + x;
  uint dst_ix = x*get_global_size(0) + y;
  dst[dst_ix] = src[src_ix];
}

/*
kernel void transposeSLM_STATIC(global T *val)
{
  __local T tile[TILE][TILE];
  uint x = get_global_id(0),
       y = get_global_id(1);

  uint src = y*get_global_size(0) + y;

  uint dst = x*get_global_size(1) + x;

  val[dst] = val[src];
}
*/
