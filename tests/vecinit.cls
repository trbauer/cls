// memset 0 initializer [{0,0},{0,0}]
tests/vecinit.cl`uint2_kernel<2>(0:rP)

// SEE: runInitConstTests
// constant init ==> [{1,2},{1,2}]
tests/vecinit.cl`uint2_kernel<2>((uint2)(1,2):rP)

// SEE: runInitFiniteSequenceTests tests this
// fseq - finite sequence smears across pairs
// [{1,2} {3,3}]
tests/vecinit.cl`uint2_kernel<2>(fseq(1,2,3):rP)

// not supported yet
//
// tests/vecinit.cl`uint2_kernel<2>(seq(1):rP)
// tests/vecinit.cl`uint2_kernel<2>(cyc(1,2,3):rP)

