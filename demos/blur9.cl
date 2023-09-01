#define FILTER_SIZE 9
#define FILTER_RAD (FILTER_SIZE / 2)


__constant float WEIGHTS[FILTER_SIZE] =
{
  0.008486917242407799f, 0.03807468712329865f, 0.111162662506103500f,
  0.211359992623329200f, 0.26183146238327030f, 0.211359992623329200f,
  0.111162669956684100f, 0.03807468712329865f, 0.008486918173730373f
};

__kernel void blur(
  __write_only image2d_t dstImg, __read_only image2d_t srcImg)
{
  const int pX = get_global_id(0);
  const int pY = get_global_id(1);

  const sampler_t sampler =
    CLK_NORMALIZED_COORDS_FALSE |
    CLK_ADDRESS_CLAMP_TO_EDGE |
    CLK_FILTER_NEAREST;

  float4 out = read_imagef(srcImg, sampler, (int2)(pX, pY));
  out *= WEIGHTS[FILTER_RAD];

  for(int r = 0; r < FILTER_RAD; ++r) {
    float4 c0 = read_imagef(srcImg, sampler, (int2)(pX + (r - FILTER_RAD), pY));
    float4 c1 = read_imagef(srcImg, sampler, (int2)(pX + (FILTER_RAD - r), pY));

    out += c0 * WEIGHTS[r];
    out += c1 * WEIGHTS[r];
  }

  // write_imagef(dstImg, (int2)(pY,pX), out);
  write_imagef(dstImg, (int2)(pX,pY), out);
}
