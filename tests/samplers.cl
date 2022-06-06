void copy(
  read_only image2d_t imgIn,
  write_only image2d_t imgOu,
  sampler_t s)
{
  const int SKEW = 256;
  float2 inCoords = (float2)(
    (float)get_global_id(0) - (float)SKEW,
    (float)get_global_id(1) - (float)SKEW);
  int2 ouCoords = (int2)(
    (int)get_global_id(0),
    (int)get_global_id(1));
  float4 px = read_imagef(imgIn, s, inCoords);
  bool inBounds =
    ouCoords.x >= SKEW && ouCoords.x < 2*SKEW &&
    ouCoords.y >= SKEW && ouCoords.y < 2*SKEW;
  bool edgeX = get_global_id(0) == SKEW || get_global_id(0) == 2*SKEW - 1;
  bool edgeY = get_global_id(1) == SKEW || get_global_id(1) == 2*SKEW - 1;
  bool edgeStipple =
    (edgeX && (get_global_id(1) % 4) == 0) ||
    (edgeY && (get_global_id(0) % 4) == 0);
  if (inBounds && edgeStipple)
    px = (float4)(0.0f,1.0f,0.0f,1.0f);
  write_imagef(imgOu, ouCoords, px);
}
kernel void inlineClampToEdge(
  read_only image2d_t imgIn,
  write_only image2d_t imgOu)
{
  const sampler_t s =
    CLK_NORMALIZED_COORDS_FALSE |
    CLK_ADDRESS_CLAMP_TO_EDGE | // CLK_ADDRESS_REPEAT | CLK_ADDRESS_CLAMP_TO_EDGE | CLK_ADDRESS_CLAMP
    CLK_FILTER_NEAREST; // CLK_FILTER_LINEAR
  copy(imgIn, imgOu, s);
}

kernel void indirect(
  read_only image2d_t imgIn,
  write_only image2d_t imgOu,
  sampler_t s)
{
  copy(imgIn, imgOu, s);
}


//////////////////////////////////////////////////////////
void copy2(
  read_only image2d_t imgIn,
  write_only image2d_t imgOu,
  sampler_t s)
{
  float fx = 3.0f*(float)get_global_id(0)/(float)get_global_size(0) - 1.0f;
  float fy = 3.0f*(float)get_global_id(1)/(float)get_global_size(1) - 1.0f;
  float2 inCoordsNDC = (float2)(fx, fy);
  float4 px = read_imagef(imgIn, s, inCoordsNDC);

  const int SKEW = 256;
  int2 ouCoords = (int2)(
    (int)get_global_id(0),
    (int)get_global_id(1));
  bool inBounds =
    ouCoords.x >= SKEW && ouCoords.x < 2*SKEW &&
    ouCoords.y >= SKEW && ouCoords.y < 2*SKEW;
  bool edgeX = get_global_id(0) == SKEW || get_global_id(0) == 2*SKEW - 1;
  bool edgeY = get_global_id(1) == SKEW || get_global_id(1) == 2*SKEW - 1;
  bool edgeStipple =
    (edgeX && (get_global_id(1) % 4) == 0) ||
    (edgeY && (get_global_id(0) % 4) == 0);
  if (inBounds && edgeStipple)
    px = (float4)(0.0f, 1.0f, 0.0f, 1.0f);


  write_imagef(imgOu, ouCoords, px);
}
kernel void inlineRepeat(
  read_only image2d_t imgIn,
  write_only image2d_t imgOu)
{
  const sampler_t s =
    CLK_NORMALIZED_COORDS_TRUE |
    CLK_ADDRESS_REPEAT |
    CLK_FILTER_LINEAR;
  copy2(imgIn, imgOu, s);
}

kernel void inlineMirroredRepeat(
  read_only image2d_t imgIn,
  write_only image2d_t imgOu)
{
  const sampler_t s =
    CLK_NORMALIZED_COORDS_TRUE |
    CLK_ADDRESS_MIRRORED_REPEAT |
    CLK_FILTER_LINEAR;
  copy2(imgIn, imgOu, s);
}
