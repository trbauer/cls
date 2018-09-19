#ifndef TYPE
#define TYPE float
#endif
#define CAT2(X,Y) X ## Y
#define CAT(X,Y) CAT2(X,Y)
#define TYPED(SYM) CAT(TYPE, _ ## SYM)

kernel void TYPED(add)(
  global TYPE *dst,
  global TYPE *src0,
  global TYPE *src1)
{
  int id = get_global_id(0);
  dst[id] = src0[id] + src1[id];
}

// VECTOR: a*x + b
kernel void TYPED(axpb) (
  global TYPE *dst,
  TYPE a,
  global TYPE *x,
  TYPE b)
{
  int id = get_global_id(0);
  dst[id] = a*x[id] + b;
}

// dst = (1-t)*src0 + t*src1
kernel void TYPED(interpolate)(
  global TYPE *dst,
  global TYPE *src0,
  global TYPE *src1,
  TYPE t)
{
  int id = get_global_id(0);
  dst[id] = ((TYPE)1.0 - t)*src0[id] + t*src1[id];
}
