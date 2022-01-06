kernel void save_uchar4(__global uchar4 *out) {
  int gx = get_global_id(0);
  int gy = get_global_id(1);
  int gszx = get_global_size(0);
  int gszy = get_global_size(1);
  uchar px_x = (uchar)round((float)gx/(float)gszx*255.0f);
  uchar px_y = (uchar)round((float)gy/(float)gszy*255.0f);
  out[gszx*gy + gx] = (uchar4)(px_x,px_y,0x0,0xFF);
}

kernel void save_float4(__global float4 *out) {
  int gx = get_global_id(0);
  int gy = get_global_id(1);
  int gszx = get_global_size(0);
  int gszy = get_global_size(1);
  float px_x = (float)gx/(float)gszx;
  float px_y = (float)gy/(float)gszy;
  out[gszx*gy + gx] = (float4)(px_x,px_y,0.0,1.0f);
}
