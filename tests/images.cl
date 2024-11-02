// e.g. % cls64 -e "tests/images.cl`flip_channels(image<rgba,un8,4x4>:Sw,image<rgba,un8>('tests/test_image.ppm'):r)<4x4>"
__constant sampler_t sampler =
      CLK_NORMALIZED_COORDS_FALSE
    | CLK_ADDRESS_CLAMP_TO_EDGE
    | CLK_FILTER_NEAREST;

__kernel void scale_image(
  __write_only image2d_t output,
  __read_only image2d_t input,
  float factor)
{
  const int2 pos = {get_global_id(0), get_global_id(1)};

  float4 px_in = read_imagef(input, sampler, pos);
  float4 px_ou = factor*px_in; // clamp(px_in,0.0f,1.0f);
  write_imagef(output, pos, px_ou);
}

__kernel void flip_channels(
  __write_only image2d_t output,
  __read_only image2d_t input)
{
  const int2 pos = {get_global_id(0), get_global_id(1)};

  float4 px_in = read_imagef(input, sampler, pos);
  float4 px_ou = (float4)(px_in.z,px_in.y,px_in.x,px_in.w);
  write_imagef(output, pos, px_ou);
}