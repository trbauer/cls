// Some stand-alone expressions one might pass in as a literal argument (-e).
// This could be used for timing or just testing.  This is similar to the
// clcall-style usage from days of yore.

#0`examples/vec.cl`add<4096x4096,16x16>(0:w,0.25:r,1.0:r)

// The above means the following:
//  On device 0, create a context, and command queue.  Compile and build
//  the program from from the file given without options; then create the
//  kernel vec from that program and invoke it over a 4k by 4k ND range
//  with a 16 by 16 workgroup size after setting the arguments as:
//  a writable zero buffer, a read-only buffer filled with 0.25, and a
//  readable buffer filled with 1.0



#GTX`examples/vec.cl[-DTYPE=int -cl-fast-relaxed-math]`add<4096x4096>(0:w,0.25:r,1.0:r)
// The above means:
//  On the device with name matching substring "GTX" compiles the file given,
//  but builds with options "-DTYPE=int -cl-fast-relaxed-math" passed in.
//  Run kernel matmul on a 4k by 4k NDR with a NULL workgroup size.
//  NOTE: if the kernel has a required workgroup size, we would use that.
