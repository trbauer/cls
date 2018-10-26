#define T    uchar

// simple histogram of radixes
void histogram(
   const global T    *inp,         // stream of input values
   global int        *hist_global, // output buffer is hist_len elements
   local  int        *hist_local,  // hist_len elements
   int                hist_len)
{
  uint gid = get_global_id(0),
       lid = get_local_id(0);
  uint lix;

  // load our value and cooperatively clear SLM
  T value = inp[gid];

  lix = lid;
  while (lix < hist_len) {
    hist_local[lix] = 0;
    lix += get_local_size(0);
  }

  barrier(CLK_GLOBAL_MEM_FENCE);

  // compute the local histogram
  // for (int i = 0; i < sizeof(value)/sizeof(value.s); i++) {
  //  int radix = value.s[i] % hist_len;
    int radix = value % hist_len;
    atomic_inc(hist_local + radix);
  // }

  barrier(CLK_GLOBAL_MEM_FENCE);

  // cooperatively write the local histogram
  lix = lid;
  while (lix < hist_len) {
    atomic_add(hist_global + lix, hist_local[lix]);
    lix += get_local_size(0);
  }
}

kernel void histogram_d(
   const global T    *inp,
   global int        *hist_global,
   local  int        *hist_local,
   int                hist_len)
{
  histogram(inp, hist_global, hist_local, hist_len);
}


#ifndef HIST_SIZE
#define HIST_SIZE 2
#endif

kernel void histogram_s(
   const global T    *inp,
   global int        *hist_global)
{
  local int hist_local[HIST_SIZE];
  histogram(inp, hist_global, hist_local, HIST_SIZE);
}

