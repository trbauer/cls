
struct point2d {
  float2 pos;
  float2 vel;
  int    reflects;
  float  life; // life left
};

// ALSO c.f. http://www.reedbeta.com/blog/quick-and-easy-gpu-random-numbers-in-d3d11/
//  for testing

// http://cas.ee.ic.ac.uk/people/dt10/research/rngs-gpu-mwc64x.html
uint MWC64X(uint2 *state)
{
    enum {A=4294883355U};
    uint x=(*state).x, c=(*state).y;  // Unpack the state
    uint res=x^c;                     // Calculate the result
    uint hi=mul_hi(x,A);              // Step the RNG
    x=x*A+c;
    c=hi+(x<c);
    *state=(uint2)(x,c);              // Pack the state back up
    return res;                       // Return the next result
}

float randomFloat(
  private uint2 *state, float min_dim, float max_dim)
{
  uint val = MWC64X(state);
  const uint MAX_UINT = (uint)-1;
  float v = (float)val/(float)MAX_UINT; // 0 to 1
  return min_dim + (max_dim - min_dim)*v; // scaled to [min..max]
}

float2 randomFloat2(
  private uint2 *state, float2 min_dims, float2 max_dims)
{
  float rx = randomFloat(state, min_dims.x, max_dims.x),
        ry = randomFloat(state, min_dims.y, max_dims.y);
  return (float2)(rx, ry);
}

// http://cas.ee.ic.ac.uk/people/dt10/research/rngs-gpu-mwc64x.html
void respawn(
  float2 min_dims, float2 max_dims,
  private struct point2d *p, private uint2 *rng_state)
{
  const float2 MIN_VELS = (float2)(-100.0f); // pixels per second
  const float2 MAX_VELS = (float2)(100.0f); // pixels per second
  //
  p->pos = randomFloat2(rng_state, min_dims, max_dims);
  p->vel = randomFloat2(rng_state, MIN_VELS, MAX_VELS);
  p->reflects = 0;
  p->life = randomFloat(rng_state, 5.0f,20.0f); // 5s to 20s
}

// let PS=0:w, GS=random<12007>
// particles`advect<NUM_PARTICLES>(PS,GS,(float2)0,(float2)(800,600),0.98,0.1)
kernel void advect(
  global struct point2d *ps,
  global uint2 *rng_states,
  float2 min_dims, float2 max_dims,
  float dampen,
  float dt)
{
  int id = get_global_id(0);
  struct point2d p = ps[id];

  p.pos += p.vel*dt;
  p.life -= dt;

  if (p.life <= 0.0f) {
    uint2 rng_state = rng_states[id]; // lift to register file
    respawn(min_dims, max_dims, &p, &rng_state);
    rng_states[id] = rng_state;
  }

  int hit = 0;
  if (p.pos.x > max_dims.x || p.pos.x < min_dims.x) {
    p.vel.x = -p.vel.x;
    hit = 1;
  }
  if (p.pos.y > max_dims.y || p.pos.y < min_dims.y) {
    p.vel.y = -p.vel.y;
    hit = 1;
  }

  p.reflects += hit;

  if (hit)
    p.vel *= dampen;

  ps[id] = p;
}

constant float4 COLORS[] =
  {
    (float4)(1.0f,0.0,0.0f,1.0f) // red
  , (float4)(1.0f,1.0,0.0f,1.0f) // yellow
  , (float4)(1.0f,0.0,1.0f,1.0f) // magenta
  , (float4)(1.0f,1.0,1.0f,1.0f) // white
  , (float4)(0.0f,1.0,0.0f,1.0f) // green
  , (float4)(0.0f,1.0,1.0f,1.0f) // cyan
  , (float4)(0.0f,0.0,1.0f,1.0f) // blue
  , (float4)(0.5f,0.5,1.0f,1.0f) // something else
  };
constant uint COLORS_LENGTH = sizeof(COLORS)/sizeof(COLORS[0]);
constant float4 BACKGROUND_COLOR = (float4)(0.3f,0.3f,0.3f,1.0f);

// particles`draw_frame<800x600>(IMG)
kernel void clear_frame(write_only image2d_t img)
{
  int2 coord = (int2)(get_global_id(0),get_global_id(1));
  write_imagef(img, coord, BACKGROUND_COLOR);
}

// particles`draw_frame<NUM_PARTICLES>(PS,IMG)
kernel void draw_frame(
   global struct point2d *ps,
   write_only image2d_t img)
{
  int id = get_global_id(0);
  struct point2d p = ps[id];
  int2 coord = (int2)(round(p.pos.x),round(p.pos.y));
  write_imagef(img, coord, COLORS[id % COLORS_LENGTH]);
}







/*
// something with a forward reference
struct st2;
struct st3 {
  struct struct2 *s2;
  int             i;
};
struct st2 {
  int f;
};
*/

// union {
//  ...
// }

// enum foo {RED=1, GREEN, BLUE};


