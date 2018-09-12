////////////////////////////////////////////
// assumes square matrices
// to fix this one would have to pass in the sizes M, K, and N
// (or at least pass in the inner dimension).


kernel void mul_naive(
    global float *C,
    global const float *A,
    global const float *B)
{
    const int M = get_global_size(0),
              K = get_global_size(0),
              N = get_global_size(1);
    int m = get_global_id(0), // row in output matrix
        n = get_global_id(1); // col in output matrix

    float dot_product = 0.0f;
    for (int k = 0; k < K; k++) {
        dot_product += A[m*K + k]*B[k*N + n];
    }
    C[m*N + n] = dot_product;
}


#ifndef TILE_K_LEN
#define TILE_K_LEN 8
#endif

__attribute__((reqd_work_group_size(TILE_K_LEN,TILE_K_LEN,1)))
kernel void static_slm(
    global float *C,
    global const float *A,
    global const float *B)
{
    const int M = get_global_size(0),
              K = get_global_size(0),
              N = get_global_size(1);

    local float TILE_A[TILE_K_LEN][TILE_K_LEN];
    local float TILE_B[TILE_K_LEN][TILE_K_LEN];

    int m = get_global_id(0),
        n = get_global_id(1);

    int t_m = get_local_id(0), // tile offset
        t_n = get_local_id(1);

    float dot_product = 0.0f;

    // slide the tiles across both matrices
    for (int k = 0; k < K; k += TILE_K_LEN) {
      // everyone loads one of the elements into the shared tile
      //
      // A.......................... K
      // .
      // .         k       k += TILE_K_LEN
      // .         v       v
      // .         ........
      // .         ........
      // .         ........
      // .         ........
      // .
      // .
      // .
      // M
      TILE_A[t_m][t_n] = A[m*K + k + t_n];
      TILE_B[t_m][t_n] = B[(k + t_m)*N + n];
      // printf(
      //   "(%d,%d)[%d,%d] loaded A[%d] and B[%d]\n",
      //   m, n, get_local_id(0), get_local_id(1),
      //   m*K + k + t_n,
      //   (k + t_m)*N + n
      //   );

      barrier(CLK_LOCAL_MEM_FENCE);

      // use everyone's result
      for (int t_k = 0; t_k < TILE_K_LEN; t_k++) {
        dot_product += TILE_A[t_m][t_k]*TILE_B[t_k][t_n];
      }

      // stall until everyone is done with the SLM tile
      barrier(CLK_LOCAL_MEM_FENCE);
    }

    C[m*N + n] = dot_product;
}