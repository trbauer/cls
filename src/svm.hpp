#include <CL/cl.h>

#ifndef CL_MEM_SVM_FINE_GRAIN_BUFFER
#define CL_MEM_SVM_FINE_GRAIN_BUFFER                (1 << 10)
#endif
#ifndef CL_MEM_SVM_ATOMICS
#define CL_MEM_SVM_ATOMICS                          (1 << 11)
#endif

typedef void *(CL_API_CALL * clSVMAllocType)(
  cl_context,cl_bitfield,size_t,cl_uint);
// cl_svm_mem_flags ~ cl_bitfield

typedef void *(CL_API_CALL * clSVMFreeType)(
  cl_context,void *);

clSVMAllocType GetSVMAlloc();
clSVMFreeType GetSVMFree();
