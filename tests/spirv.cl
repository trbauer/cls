/*
struct p2d {
  int x, y;
};

// TODO: structures
// TODO: inline sampler
constant sampler_t s =
  CLK_NORMALIZED_COORDS_TRUE |
  CLK_ADDRESS_REPEAT |
  CLK_FILTER_LINEAR;

__attribute__((reqd_work_group_size(16,8,2)))
kernel void dummy(
  global volatile uint2 * restrict vrglb,
  global const    uint2 *          cglb,
  constant        uint2 *          consts,
  local           uint2 *          local_dynamic,
//                  struct p2d       pt,
//  global          struct p2d      *pts,
// read_only     image1d_buffer_t img_buf,
  read_only       image2d_t        img
//  , sampler_t                        s
  )
{
  local uint2 local_static[16*8*2];
  uint gid = get_global_id(0),
       gsz = get_global_size(0),
       lid = get_local_id(0),
       lsz = get_local_size(0);

  //
  local_dynamic[lid] = vrglb[gid];
  local_static[lid] = cglb[min(gid+1,gsz)];
  //
  barrier(CLK_GLOBAL_MEM_FENCE);
  //
  uint2 val = (uint2)0;
  val += local_static[(lid+1)%lsz];
  val += local_dynamic[lid+2];
  val += consts[get_global_id(2)];
  //
  int2 coord = (int2)(get_global_id(0),get_global_id(1));
  float4 vec = read_imagef(img,s,coord);
  val += (uint2)((uint2)(vec.x + vec.y + vec.z + vec.w));

  vrglb[get_global_id(0)] = val;
}
*/

// should be in type_parsing.cl?

// kernel void dummy(constant uint2 *consts, constant int x[7])
struct s1_st{int x;float y;};
typedef struct s2_st {int x;} s2_t;
typedef struct {int x[2];s2_t y;} s3_t;

kernel void structs(
  global struct s1_st *s1,
  s2_t                 s2,
  local s3_t          *s3)
{
}

// TODO: need generic pointers (CL2.0)
kernel void buffers(
  int x, float4 y,
  const global int * restrict xx,
  volatile global int *yy,
  local int *zz,
  constant int *www)
{
}

// TODO: need read_write (CL2.0)
kernel void images(
  read_only  image2d_t        i2d
  write_only image1d_buffer_t i1db
#if CL_VERSION_1_2
  read_write image3d_t        i3d,
#endif
  /* read_only */ image1d_array_t i1da // implicitly read_only
  )
{
}

