// This program demonstrates testing two kernels within the same program on
// the same device for equality.

// Compiles and builds the program and binds it to P
P=examples/matmul.cl[-cl-fast-relaxed-math -DTYPE=float]

// Defines the input buffer for both sizes; note the dimensions are not
// well-defined until we see the NDRanges's these are going to be used in
INP=random<12007>(0.0,1.0):r; OUP1=0:w; OUP2=0:w

// Make the calls
// NOTE: there can be some parallelism here if OOQ's are enabled
P.matmul_naive<1024x768>(OUP1,INP)
// But you could enforce a split here if needed
// barrier
P.matmul_slm<1024x768,16x16>(OUP2,INP)

// <1024,768>: the dimension gives the error messages some sanity
// 0.00001: the tolerance in comparisons (note: we'd throw a fit if
//          the buffer types were integral (given this parameter)
diff<1024,768>(OUP1,OUP2,0.001)

