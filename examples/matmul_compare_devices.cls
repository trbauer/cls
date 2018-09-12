// This program demonstrates testing two kernels within the
// same program for correctness

// Compiles and builds the programs for both devices matching
// "HD" and "Core"
P1=#HD`examples/matmul.cl[-cl-fast-relaxed-math -DTYPE=float]
P2=#Core`examples/matmul.cl[-cl-fast-relaxed-math -DTYPE=float]
// definfes the input buffer for both sizes; not the dimensions are not
// well-defined until we see the NDRanges's these are going to be used in
INP=random<12007>(0.0,1.0):r; OUP1=0:w; OUP2=0:w

// Make the calls
P1.matmul<1024x768>(OUP1,INP,INP)
barrier // prevents running overlap (so we could compare time with -tCL)
P2.matmul<1024x768>(OUP2,INP,INP)

// <1024,768>: the dimension gives the error messages some sanity
// 0.00001: the tolerance in comparisons (note: we'd throw a fit if
//          the buffer types were integral (given this parameter)
diff<1024,768>(OUP1,OUP2,0.001)

