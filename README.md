# CLScript - *cls*
## An OpenCL Scripting Language for Testing, Profiling, Debugging, and Other Purposes.

OpenCL programs require considerable boilderplate host-side source code to compile (or load), setup arguments, and dispatch a kernel. CLScript enables one to specify this setup via a compact syntax.

A CLScript program consists of a sequence of CLScript commands such as *dispatches* (kernel enqueues), buffer *diff*ing (verification), buffer *print*ing and other such commands.

## CLScript Commands
CLScript has a powerful integrated help system. Use `-h=syntax` on the command line to list the formal syntax, but some of that will be repeated here.

### Dispatch Command
A dispatch command is the most important command.  It captures specification of the OpenCL device, context, and command-queue creation, buffer and image creation and initialization, program and kernel compilation as well as argument specifiction and finally dispatch.

#### Example: demo.cl
```C
kernel void addK(
  global int *dst, global int *src0, global int *src1, int k)
{
  int id = get_global_id(0);
  dst[id] = src0[id] + src1[id] + k;
}
```

Testing this kernel can be performed by the compact CLScript given below. (Note: this can be placed in a file argument to CLScript or passed as a literal string using the `-e` option.  Execute with `-h=e` for more information on that option.)
```
#0`demo.cl`addK<32,4>(0:w,seq(10):r,random<12007>(0,4):rw,3)
```
This command breaks down into several sections separated by a \` (backtick).
  * `#0` creates a command queue on device 0 in a linear flattened order of the OpenCL platform/device tree.  (Run CLScript with the `-l` option to list devices by index on the current machine.)
  * The `demo.cl` part loads the OpenCL C source file and creates a program from that source.  This section can be suffixed with build options passed to `clBuildProgram`.  For example, `demo.cl[-DTYPE=int -cl-opt-disable]` would use the build option string `"-DTYPE=int -cl-opt-disable"`.
  * The `addK` section specifies the name of the kernel to create from the program compiled.
  * The angle bracket section `<32,4>` specifies the global and local sizes.   The local size may be omitted and CLScript will use `nullptr` as the local size (letting the driver choose the workgroup size. If the  `__attribute__((reqd_work_group_size(X,Y,Z)))` attribute is present, omission of the local size will cause CLScript to select the required workgroup size value. Two or three dimensional sizes are also supported using an `x` as part of the lexical token. E.g. `<1024x768,16x16>` would be an example 2D dimension specification.  Finally, constant expressions are also supported. For example `<(1024*2)x768>` would be the same as `<2048x768>`.
  * Finally, the kernel arguments are constitute the rest of the command in parentheses `(0:w,seq(10):r,random<12007>(0,4):rw,3)`.  The general syntax for surface initializers (buffers or images) is an exprssion suffixed with a `:` and a string of surface attributes.

We now discuss the specific meaning of the argument initializers above.

  * The first argument `0:w` creates a buffer of zeros with `w` write-only access (`CL_MEM_WRITE_ONLY`).  CLScript chooses the buffer size by assuming one element per work item.  It infers the type of `int` from the source code as well.  For a buffer of type `T` CLScript will create a buffer of `get_global_size(0)*get_global_size(1)*get_global_size(2)*sizeof(T)`.  In the case of the example above this is `32*sizeof(int)` all initialized to 0's.  The buffer size can be overridden with a specific size if needed. For example, `0:[64*4]w` would force the buffer to be 256 bytes.  This can be useful if the kernel being tested processes several element per workitem.
  * The second argument initializer `seq(10):r` creates a similar sized buffer with a sequence of integer values starting at 10.  The `:r` surface attribute specifies an access mode of `CL_MEM_READ_ONLY`.
  * The third argument creates another similar sized buffer with access mode `CL_MEM_READ_WRITE`. The contents are initialized to random values using a seed of `12007` and in the (inclusive) range `[0,4]`.
  * The final argument is a scalar value `3`.

More sophisticated expressions can be substituted anywhere an integer literal is expected. Many C++ STL numeric builtin functions are supported. E.g. `max(3,4)` will be `4`.

More information can be accessed via `-h=syntax` option.  Confer with the `<Expr>` for more examples of constant expressions.

### Printing Immediate Buffer Contents
Buffer contents can be printed before and after dispatch via the surface initializer attributes `P` and `p`.  For example, the suffix `...:rwP8p4` creates a read-write buffer.  Before dispatches the contents are printed with 8 elements per line before kernel enqueue and afterwards with 4 elements per line.  This can be useful for debugging.

### Image Support
CLScript contains fairly decent support for loading and saving images for kernels.
The kernel parameter should be an OpenCL `image_t`.

CLScript can handle several formats:
* PPM
* BMP
* PNG, if built with LodePNG support.

The `image` argument initializer in CLScript will instantiate such an object.  See the *ImgExpr* rule in the help section (`-h=syntax`) for more details.

#
## Let Expressions and Shared Buffers
CLScript has a very power buffer initializer syntax as shown above, but often we need to name buffer for analysis (e.g. to compare results) or to reuse it in other dispatch statements.

Consider the CLScript given below (`demo.cls`).
```
let A=0:rwp8
#0`demo.cl`addK<16>(A, A, 1:rw, 0)
print<4>(A)
#0`demo.cl`addK<16>(A, A, 2:rw, 0)
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
The `diff` command allows us to compare buffers to a specific value or another buffer.  If a difference is encountered, the `diff` command exits with an error (though this can be overriden via the `-Xno-exit-on-diff-fail` flag).

Integer types use binary comparison.  Floating-point also handle NaN comparison and also permit an absolute difference threshold.  Consider the following example (`demo2.cl`).
```
kernel void div(global float *dst)
{
  int id = get_global_id(0);
  dst[id] /= (float)(get_local_id(0) - 1);
}
```
The kernel above will create a NaN value for local id 1 (0.0f/0.0f).  Different devices might produce differing `NaN` values, but are effectively the same.  We can test this with the following CLScript.

```
let A=0:rw
let B=0:rw
#0`demo2.cl`div<8>(A)
#1`demo2.cl`div<8>(B)
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
#0`demo3.cl`test<8>(A)
#0`demo3.cl[-cl-fp32-correctly-rounded-divide-sqrt]`test<8>(B)
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
Inserting some print statements illustrates the binary difference.  The script below will print the buffers in hex.
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
We can see that the second elements differ in the lowest bit of the manissa (`0x40544394` and `0x40544395`).

Now suppose we don't care about a minor difference.  We change the script to following to solve this problem.
```
let A=seq(10):rw, B=seq(10):rw
#0`demo3.cl`test<8>(A)
#0`demo3.cl[-cl-fp32-correctly-rounded-divide-sqrt]`test<8>(B)
print<uint>(A); print<uint>(B)
diff<float,0.001>(A,B)
```
The `diff` command takes a type and we selected maximal absolute difference of `0.001`.
The diff will succeed and the program will exit 0 with no error message.

#
## Printing Buffers With the `print` Command
The `print` command, illustrated earlier, prints a surface's values.  The surface type can be overriden (re-interpreted) via an optional parameter.

#
## Saving Buffers With `save`
The `save` command saves a binary buffer.  For example, `save("foo.dat",A)` saves surface A.

NOTE: binary buffers can be loaded via a surface initializer as in the example: `file<bin>("foo.dat")`.


#
# Execution Details
CLScript executes in three phases.
1. Parsing.  The CLScript syntax is parsed to an intermediate form either from a file given or via an immediate argument using the `-e` option.  The `-P` option stops processing after parsing.
2. Compilation.  Elements of the constructed IR are bound to actual objects representing those things. This includes contructing OpenCL contexts, command queues, buffers, and other elements.  Some additional checking is performed here.  Inferred surface sizes and properties are resolved in this phase.
3. Execution.  This final phase walks through the list of commands in linear order and executes each in order. The `-i` option specifying the number of iterations to run repeates this phase.  A value of 0 would skip it all together.  **NOTE:** Surfaces are initialized in this phase; consequently, *each successive iteration* will re-initialize buffers with an `-i` value greater than 1.

#
# Other CLS Usage Modes

## Performance Analyis
By default CLScript prints some timing information on each kernel executed and also information of that kernel as a proportion of the total.  Two options that control the timing source are `-tW` and `-tCL`.  The former uses wall times (host-side timers) and the latter uses the OpenCL Profiling Events, which generally are more accurate.

The `-i` option specifies how many iterations.  In particular when using wall time the first dispatch can be orders of magnitude more time consuming than in later iterations.  Be wary of this artifact and choose a robust statistic (e.g. a median or minimum).  The coefficient of variation (the `cfv%` field) will give one an estimate about the spread of all runtimes.

The raw times will be dumped if the verbose flag is enabled `-v`.


## Info [discuss info -l -v]
The `-l` option will make CLScript list information on the detected OpenCL devices found on the system (much like the classic CLS)

Including the `-v` option will emit more output about devices.

Finally, the `-l` option takes an argument specifying which device to list information on.  E.g. `-l=0` lists information on device 0 (`#0` in CLScript syntax).