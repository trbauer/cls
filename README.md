# CL Script - *cls* on Unix *cls64.exe* or *cls32.exe* on Windows
## An OpenCL Scripting Language for Testing, Profiling, Debugging, and More

OpenCL programs require considerable boilerplate host-side source code to: load source; compile it; check and print errors; allocate and initialize buffers and images (surfaces) from constants, file, random data, or other source; bind those arguments to a kernel instance, and then dispatch kernels on a command queue.  In addition, more boilerplate code is required depending on the specific task be it: timing, debugging (printing intermediate data), saving or printing output, or refereeing output with reference values.  CL Script realizes many of these common tasks in a terse syntax.  Many typical uses result in "one-liners" that can be passed via command line (c.f. the `-e` option).  More complex scripts can be passed via a file containing a sequence of commands on separate lines.

## Applications
Some practical applications for CL Script include the following and are illustrated in more detail in further sections.

* Compiling and running a graph of kernels and observing the output buffer data or various statistics on the time each took to run.
* Compiling two different implementations of the same algorithm in different kernel sequences and diffing results for some input.
* Compiling the same kernel on different devices and observing output or performance differences.
* Observing the effect of different workgroup sizes on performance.
* The ability to dump compiled binaries (`-B`) as well as listing device properties (`-l` and `-l -v`).


## Integrated Command Line Help System
CL Script has a powerful integrated help system. Use `-h` on the command line to get started.  The syntax is accessible via -h=syn* (use -h to get started).

## File Structure
A CL Script program consists of a sequence of CL Script statements.  Statements by definitions (let expressions), commands such as *dispatches* (kernel enqueues), buffer *diff*ing (verification), buffer *print*ing and other such commands.

### Dispatch Command
A dispatch command represents a kernel ND Range enqueue on a command queue.  It captures specification of the OpenCL device, context, and command-queue creation, buffer and image creation and initialization, program and kernel compilation as well as argument specification and finally dispatch.

#### Example: demo.cl
```OpenCL
kernel void addK(
  global int *dst, global int *src0, global int *src1, int k)
{
  int id = get_global_id(0);
  dst[id] = src0[id] + src1[id] + k;
}
```

Testing this kernel can be performed by the compact CL Script given below.  This can be placed in a file argument to CL Script or passed as a literal string using the `-e` option.  Execute with `-h=e` for more information on that option.
```
#0`demo.cl`addK(0:w,seq(10):r,random<12007>(0,4):rw,3)<32,4>
```
This command breaks down into several sections separated by a \` (backtick).
  * `#0` creates a command queue on device 0 in a linear flattened order of the OpenCL platform/device tree.  (Run CL Script with the `-l` option to list devices by index on the current machine.)
  * The `demo.cl` part loads the OpenCL C source file and creates a program from that source.  This section can be suffixed with build options passed to `clBuildProgram`.  For example, `demo.cl[-DTYPE=int -cl-opt-disable]` would use the build option string `"-DTYPE=int -cl-opt-disable"`.
  * The `addK` section specifies the name of the kernel to create from the program compiled.
  * The kernel arguments constitute the rest of the command in parentheses `(0:w,seq(10):r,random<12007>(0,4):rw,3)`.  Arguments are separated by commas and apply in the order given in the kernel.  Buffers and images have dedicated syntax for surface initializers appearing as initializer expressions suffixed with a `:` and a string of surface attributes.  The next paragraph says more.
  * Finally, the angle bracket section `<32,4>` specifies the global and local sizes.   The local size may be omitted and CL Script will use `nullptr` as the local size letting the driver choose the workgroup size.  If the  `__attribute__((reqd_work_group_size(X,Y,Z)))` attribute is present, omission of the local size will cause CL Script to select the required workgroup size value.  Two or three dimensional sizes are also supported using an `x` as part of the lexical token. E.g. `<1024x768,16x16>` would be an example 2D dimension specification.  Finally, constant expressions are also supported if surrounded by parentheses.  For example `<(1024*2)x768>` would be the same as `<2048x768>`, indicating a global size of `2048` in the X-dimension (`get_global_id(0)`) and `768` in the Y (`get_global_id(1)`).


The specific meaning of the argument initializers in the above example follows.

  * The first argument `0:w` creates a buffer of zeros (`0`) with write-only (`w`) access (`CL_MEM_WRITE_ONLY`).  By default CL Script automatically chooses the buffer size by assuming one element per work item in the global range of the enqueue and the buffer type size.  CL script infers the type of buffer elements to be `int` by parsing the C preprocessed source code as well.  For a buffer unified with an kernel argument of type `global T *` CL Script will create a buffer of `get_global_size(0)*get_global_size(1)*get_global_size(2)*sizeof(T)`.  In the case of the example above this is `32*sizeof(int)`.

  The buffer size can be overridden with a specific size if needed. For example, `0:[64*4]w` would force the buffer to be 256 bytes.  Note well, the the buffer size expression is *bytes* not elements.  Custom sizes enable kernels that processes several element per workitem, pad a surface, or have a shared variable of some sort.
  * The second argument initializer `seq(10):r` creates a similar sized buffer, one per workitem, with an arithmetic sequence of integer values starting at 10 (i.e. 10, 11, ...).  The `:r` surface attribute specifies an access mode of `CL_MEM_READ_ONLY`.
  * The third argument creates another similar sized buffer with access mode `CL_MEM_READ_WRITE`. The contents are initialized to random values using a seed of `12007` and in the (inclusive) range `[0,4]` (inclusive).
  * The final argument is a scalar/uniform kernel argument of `3`.

More sophisticated expressions can be substituted anywhere an integer literal is expected. Many C++ STL numeric builtin functions are supported. E.g. `max(3,4)` evaluates to `4`.

More information can be accessed via `-h=syn` option.

### Printing Immediate Buffer Contents
Buffer contents can be printed before and after dispatch via the surface initializer attributes `P` and `p`.  In addition, the number of elements to emit per line is configurable as well.  For example, the suffix `...:rwP8p4` creates a read-write buffer `:..rw..`.  Before dispatches the contents are printed with 8 elements per line (`..:..P8..`) before kernel enqueue and afterwards with 4 elements per line (`..:..p4..`).  This can be useful for debugging.  Buffers can also be defined outside a dispatch statement, passed in by reference, and printed in separate statements.

### Image Support
CL Script contains nominal support for loading and saving images for kernels.
The kernel parameter should be an an OpenCL image type such as `image2d_t`.

CL Script can handle several formats:
* PPM
* BMP
* PNG (if built with LodePNG support)

The `image<..>(..)` argument initializer expression in CL Script will instantiate such an object.
A image initializer expression `image<rgba,u8>("foo.png"):r` would construct an image with the dimensions of the given image loaded from the file.  An empty image (e.g. for writing to) can be constructed by adding a dimension as in the example `image<rgba,u8,512x512>:w`.  See the *ImgExpr* rule in the help section on surface expressions (`-h=syn-sex`) for more details.

Consider the usual example image below.

![Lena](demos/lena.png)

And consider the simple blur kernel.
```OpenCL
#define FILTER_RAD 4
#define FILTER_SIZE 9

__constant float WEIGHTS[9] =
{
  0.008486917242407799f, 0.03807468712329865f, 0.111162662506103500f,
  0.211359992623329200f, 0.26183146238327030f, 0.211359992623329200f,
  0.111162669956684100f, 0.03807468712329865f, 0.008486918173730373f
};

__kernel void blur(
  __write_only image2d_t dstImg, __read_only image2d_t srcImg)
{
  const int pX = get_global_id(0);
  const int pY = get_global_id(1);

  const sampler_t sampler =
    CLK_NORMALIZED_COORDS_FALSE |
    CLK_ADDRESS_CLAMP_TO_EDGE |
    CLK_FILTER_NEAREST;

  float4 out = read_imagef(srcImg, sampler, (int2)(pX, pY));
  out *= WEIGHTS[FILTER_RAD];

  for(int r = 0; r < FILTER_RAD; ++r) {
    float4 c0 = read_imagef(srcImg, sampler, (int2)(pX + (r - FILTER_RAD), pY));
    float4 c1 = read_imagef(srcImg, sampler, (int2)(pX + (FILTER_RAD - r), pY));

    out += c0 * WEIGHTS[r];
    out += c1 * WEIGHTS[r];
  }

  write_imagef(dstImg, (int2)(pX,pY), out);
}
```
Finally, the CL Script given below.
```
let SRC=image<rgba,u8>('demos/lena.png'):r
let IMG1=image<rgba,u8,512x512>:rw
let IMG2=image<rgba,u8,512x512>:rw
let IMG3=image<rgba,u8,512x512>:rw
#1`demos/blur9.cl`blur(IMG1,SRC)<512x512>
#1`demos/blur9.cl`blur(IMG2,IMG1)<512x512>
#1`demos/blur9.cl`blur(IMG3,IMG2)<512x512>
save("b1.png",IMG1)
save("b2.png",IMG2)
save("b3.png",IMG3)
```
This loads the image and blurs it three times saving the result after each blur.
This results in the file output saved as `b3.png` illustrated below.

![Lena](demos/lena_blurred.png)


#
## Let Expressions and Shared Buffers
CL Script has a very power buffer initializer syntax as shown above, but often we need to name buffer for analysis (e.g. to compare results) or to reuse it in other dispatch statements.

Consider the CL Script given below (`demo.cls`).
```
let A=0:rwp8
#0`demo.cl`addK(A, A, 1:rw, 0)<16>
print<4>(A)
#0`demo.cl`addK(A, A, 2:rw, 0)<16>
print<uint,4>(A)
```
This results output similar to the following.
```
% cls64 demo.cls
PRINT<int>[0x0000014F9CA83930  (64 B)] =>
00000:             1             1             1             1
00010:             1             1             1             1
00020:             1             1             1             1
00030:             1             1             1             1
PRINT<uint>[0x0000014F9CA83930  (64 B)] =>
00000:  0x00000003  0x00000003  0x00000003  0x00000003
00010:  0x00000003  0x00000003  0x00000003  0x00000003
00020:  0x00000003  0x00000003  0x00000003  0x00000003
00030:  0x00000003  0x00000003  0x00000003  0x00000003
...
```
This enables us to reference the same buffer several times.  Also observe how the `print` command enables us to treat interpret buffer elements contents as another type.  (Unsigned types are printed in hex and signed in decimal in the `print` command.)  The integer argument to `print` is specifying the elements per line.

**NOTE:** buffers uses in multiple dispatch statements must produce the same inferred sizes.  E.g. had a dispatch above used a different global size, this would be an error.
**NOTE:** buffers used must only be used in the same context/command queue group.

#
## The `diff` Command
The `diff` command allows us to compare buffers to a specific value or another buffer.  If a difference is encountered, the `diff` command exits with an error (though this can be overridden via the `-Xno-exit-on-diff-fail` flag).

Integer types use binary comparison.  Floating-point also handle NaN comparison and also permit an absolute difference threshold.  Consider the following example (`demo2.cl`).
```OpenCL
kernel void div(global float *dst)
{
  int id = get_global_id(0);
  dst[id] /= (float)(get_local_id(0) - 1);
}
```
The kernel above will create a NaN value for local id 1 (0.0f/0.0f).  Different devices might produce differing `NaN` values, but are effectively the same.  We can test this with the following CL Script.

```
let A=0:rw
let B=0:rw
#0`demo2.cl`div(A)<8>
#1`demo2.cl`div(B)<8>
print<uint>(A)
print(A)
print<uint>(B)
print(B)
diff(A,B)
```

This script tests the same kernel on two different devices on the system.  We print the results both in floating point and binary (hex) to illustrate.
```
PRINT<uint>[0x0000020674F294A0  (32 B)] =>
00000:  0x00000000  0x7FFFFFFF  0x00000000  0x00000000  0x00000000  0x00000000  0x00000000  0x00000000
PRINT<float>[0x0000020674F294A0  (32 B)] =>
00000:     0.00000         nan     0.00000     0.00000     0.00000     0.00000     0.00000     0.00000
PRINT<uint>[0x000002067306D320  (32 B)] =>
00000:  0x00000000  0xFFC00000  0x00000000  0x00000000  0x00000000  0x00000000  0x00000000  0x00000000
PRINT<float>[0x000002067306D320  (32 B)] =>
00000:     0.00000   -nan(ind)     0.00000     0.00000     0.00000     0.00000     0.00000     0.00000
... program exits 0
```
Observe how different devices create different NaN values.  However, notice that the program exits successfully considering the buffer output equivalent.

### Approximate Floating-Point Diffs
Floating point buffers can diff with a "fudge factor" (maximal absolute difference).  For instance, two similar kernels might produce similar results, but with different floating point error.

Consider the following program, which applies a square root to each element of a buffer.
```
kernel void test(global float *dst)
{
  int id = get_global_id(0);
  dst[id] = sqrt(dst[id]);
}
```
The script below tests the effect of the `-cl-fp32-correctly-rounded-divide-sqrt` OpenCL build option.
```
let A=seq(10):rw, B=seq(10):rw
#0`demo3.cl`test(A)<8>
#0`demo3.cl[-cl-fp32-correctly-rounded-divide-sqrt]`test(B)<8>
diff(A,B)
```
The above program detects a difference on the second element in the buffer and emits something similar to the below.  Note, that the minor fractional difference does not show up with the default precision.
```
mismatch on buffer element 1 (type float)
value difference exceeds max allowable difference
============== vs. (SUT) ==============
   3.31662
============== vs. (REF) ==============
   3.31662
6.1: mismatch on element 1 (type float)
diff(A,B)
^^^^^^^^^
```
Unfortunately, the values look very close since the diff command doesn't print the values with sufficient precision.  However, insert some print statements to see the exact binary difference.  The script below will print the buffers in hex.
```
...
print<uint>(A)
print<uint>(B)
diff(A,B)
```
This produces output similar to that below.
```
PRINT<uint>[0x0000015FD63A6F00  (32 B)] =>
00000:  0x404A62C2  0x40544394  0x405DB3D7  0x4066C15A  0x406F7750  0x4077DEF5  0x40800000  0x4083F07B
PRINT<uint>[0x0000015FD63A7120  (32 B)] =>
00000:  0x404A62C2  0x40544395  0x405DB3D7  0x4066C15A  0x406F7751  0x4077DEF6  0x40800000  0x4083F07B
```
We can see that the second elements differ in the lowest bit of the mantissa (`0x40544394` and `0x40544395`).

Now suppose we don't care about a minor difference.  Change the script to following to solve this problem.
```
let A=seq(10):rw, B=seq(10):rw
#0`demo3.cl`test(A)<8>
#0`demo3.cl[-cl-fp32-correctly-rounded-divide-sqrt]`test(B)<8>
print<uint>(A); print<uint>(B)
diff<float,0.001>(A,B)
```
The `diff` command takes a type and we selected maximal absolute difference of `0.001`.
The diff will succeed and the program will exit 0 with no error message.

#
## Printing Buffers With the `print` Command
The `print` command, illustrated earlier, prints a surface's values.  The surface type can be overridden (re-interpreted) via an optional parameter.

#
## Saving Buffers and Images With `save`
The `save` command saves a binary buffer.  For example, `save("foo.dat",A)` saves surface A.
If the surface object is an image instead of a buffer, it will save as an image file.
The file extension implies the image format.  Only a few formats are supported, but typically png is included.

NOTE: binary buffers can be loaded from binary file via a surface initializer as in the example: `file<bin>("foo.dat")`.

#
## Saving Buffer With `save_image`
The `save_image` command saves a *buffer* as an image file.
The arguments to this command specify how to convert the underlying buffer to an RGBA image.
Currently, `float4` and `uchar4`, both in RGBA order, are supported.


#
# Execution Details
CL Script executes in three phases.
1. **Parsing**.  The CL Script syntax is parsed to an intermediate form either from a file given or via an immediate argument using the `-e` option.  The `-P` option stops processing after parsing.
2. **Compilation**.  Elements of the constructed IR are bound to actual objects representing those things. This includes constructing OpenCL contexts, command queues, buffers, and other elements.  Some additional checking is performed here.  Inferred surface sizes and properties are resolved in this phase.
3. **Execution**.  This final phase walks through the list of commands generated in the compilation phase and executes each in order. The `-i` option specifies the number of iterations to repeats this phase.  **NOTE:** Surfaces are initialized in this phase; consequently, *each successive iteration* will re-initialize buffers with an `-i` value greater than 1.  Consider using the `undef` value to suppress initialization of given buffers. (E.g. `undef:w` defines writable buffer of undefined values.)

# Other Usage Modes

## Performance Analysis
By default CL Script prints some timing information on each kernel executed and also information of that kernel as a proportion of the total.  Two options that control the timing source are `-tW` and `-tCL`.  The former uses wall times (host-side timers) and the latter uses the OpenCL Profiling Events, which generally are more accurate.

The `-i` option specifies how many iterations.  In particular when using wall time the first dispatch can be orders of magnitude more time consuming than in later iterations.  Be wary of this artifact and choose a robust statistic (e.g. a median or minimum).  The coefficient of variation (the `cfv%` field) will give one an estimate about the spread of all runtimes.

The raw times will be dumped if the verbose flag is enabled `-v`.


## Info
The `-l` option will make CL Script list information on the detected OpenCL devices found on the system (much like the classic `clinfo`)

Including the `-v` option will emit more output about devices.

Finally, the `-l` option takes an argument specifying which device to list information on.  E.g. `-l=0` lists information on device 0 (`#0` in CL Script syntax).