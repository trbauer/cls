// example
// -D T=int4
// -D EXPECT=(int4){1,2,4,5}
kernel void test(
  global int *error,
  T arg)
{
  if (arg != (EXPECT)) {
    error[0] = 1;
  }
}