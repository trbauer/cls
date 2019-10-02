__kernel void initClean(__global float* dst, float clean)
{
  uint id = get_global_id(0);
  dst[id] = clean;
}

__kernel void initDirty(__global float* dst, float clean, float dirty)
{
  uint id = get_global_id(0);
  dst[id] = clean;
  if (id == 3)
    dst[id] = dirty;
}


