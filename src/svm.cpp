#include "svm.hpp"
#include "system.hpp"
#include <Windows.h>

static HMODULE oclModule;

clSVMAllocType svmAlloc;
clSVMFreeType svmFree;

static void loadFunctions()
{
  if (!svmAlloc) {
    oclModule = LoadLibraryA("OpenCL.dll");
    if (!oclModule) {
      WARNING("GetSVMAlloc: failed to load OpenCL.dll");
    }
    svmAlloc = (clSVMAllocType)GetProcAddress(oclModule, "clSVMAlloc");
    svmFree = (clSVMFreeType)GetProcAddress(oclModule, "clSVMFree");
  }
}

clSVMAllocType GetSVMAlloc() {
  loadFunctions();
  return svmAlloc;
}

clSVMFreeType GetSVMFree() {
  loadFunctions();
  return svmFree;
}
