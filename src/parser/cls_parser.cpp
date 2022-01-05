#define _USE_MATH_DEFINES
#include "cls_parser.hpp"
#include "parser.hpp"
#include "../system.hpp"

#include <cctype>
#include <cmath>
#include <iostream>
#include <limits>
#include <optional>
#include <sstream>
#include <string>

using namespace cls;

#define RESET "\033[0m"
//
#define SLIT_OPEN "\033[1;36m"
#define SLIT(X) SLIT_OPEN X RESET
//
#define SVAR_OPEN "\033[2;36m"
#define SVAR(X) SVAR_OPEN "<" X ">" RESET


///////////////////////////////////////////////////////////////////////////////////
std::string cls::CLS_SYNTAX_ALL() {
  std::stringstream ss;
  ss << CLS_SYN_SC();
  ss << CLS_SYN_ST();
  ss << CLS_SYN_PEX();
  ss << CLS_SYN_SEX();
  return ss.str();
}

std::string cls::CLS_SYN_SC()
{
  return
    "CLS Syntax\n"
    "Nonterminals are in " SVAR("nonterm") "\n"
    "terminals or literal syntax are given as " SLIT("term") " and "
    "meta syntax shows as normal"
    "\n"
    "************* COMMON SYNTAX *************\n"
    SVAR("PATH") " is an unescaped path; e.g. " SLIT("foo/bar/baz.cl") "\n"
    SVAR("IDENT") " an identifier (alphanumeric sequence); e.g. " SLIT("foo") "\n"
    SVAR("DIMS") " is 1d, 2d, or 3d dimension; e.g. "
    SLIT("16x16") " or " SLIT("800x600x2") "\n"
    SVAR("TYPE") " = is an OpenCL C built-in type\n"
    "  e.g." SLIT("float") " or " SLIT("ulong8") "\n"
    "\n"
    "************* SCRIPTS *************\n"
    SVAR("Script") " = a sequence " SVAR("Statement") " (-h=syn-st) deliminted by line or ;\n"
    "Comments can be omitted via /* ... */ comments or // comments\n"
    ;
}

std::string cls::CLS_SYN_ST()
{
  return
    "************* STATEMENTS *************\n"
    SVAR("Statement") " = "
      SVAR("Dispatch") " | " SVAR("Builtin") " | "
      SVAR("Let") " | " SVAR("DispatchLet") "\n"
      "\n"
    "************* DISPATCH STATEMENTS *************\n"
    SVAR("Dispatch") " =\n"
    "  (" SVAR("Device") SVAR("CommandQueueId") "?" SLIT("`") ")? "
    SVAR("Program") " ` "
    SVAR("KernelName") " " SVAR("KernelDims") " " SVAR("KernelArgs") "\n"
    "  - if the device is omitted, CLS uses first device\n"
    "  - if the command queue ID is omitted, we use a uniform"
    "\n"
    SVAR("Device") " = " SLIT("#") " " SVAR("INT") " | " SLIT("#") " " SVAR("STR") "\n"
    "  The " SVAR("INT") " variant selects the device by index\n"
    "    (linear order with platforms flattened out)\n"
    "    (use -l to list device indices)\n"
    "  " SVAR("STR") " matches based on a substring of CL_DEVICE_NAME\n"
    "    (the substring must be unique)\n"
    "  e.g. " SLIT("#0") " or " SLIT("#\"GTX\"") "\n"
    "\n"
    SVAR("CommandQueueId") " = " SLIT(":") SVAR("IDENT") "\n"
    "  A context identifier allows two dispatches to share the same context and\n"
    "  command queue\n"
    "  e.g. " SLIT("#0:A`prog1.cl`foo...`; #0:A`prog2.cl`bar...") "\n"
    "   dispatches two different kernels to the same queue\n"
    "\n"
    SVAR("Program") " = " SVAR("ProgramPath") " "
    SLIT("[") " " SVAR("BuildOpts") " " SLIT("]") "?\n"
    "   the build options may be omitted (CLS passes nullptr to clBuildProgram)\n"
    "  " SVAR("ProgramPath") " = " SVAR("STR") " | " SVAR("PATH") "\n"
    "    a program path doesn't need to be escaped as long as it doesn't conflict\n"
    "    with any other symbols\n"
    "     e.g. " SLIT("foo/bar/baz.cl") " or " SLIT("\"../foo/bar/baz.cl\"") "\n"
    "\n"
    "  " SVAR("BuildOpts") " = (anything but right bracket)* | " SVAR("STR") "\n"
    "    " SLIT("-cl-mad-enable -cl-fp32-correctly-rounded-divide-sqrt") "\n"
    "    " SLIT("\"-cl-denorms-are-zero -DTYPE=int4\"") "\n"
    "\n"
    SVAR("KernelName") " = " SVAR("IDENT") "\n"
    "  the name of the kernel within the program to dispatch\n"
    "\n"
    SVAR("KernelDims") " = "
    SLIT("<") " " SVAR("Dims") SLIT(">") " | "
    SLIT("<") " " SVAR("Dims") " " SLIT(",") " " SVAR("Dims") " " SLIT(">") "\n"
    "  the first argument specifies the global dimensions\n"
    "  the optional second argument specifies the local dimension\n"
    "  if local dimension are omitted, then we use nullptr (the driver chooses)\n"
    "  e.g. " SLIT("<1024x768>") " or " SLIT("<1024x768,32x16>") "\n"
    "  e.g. " SLIT("<1024*1024,32>") "\n"
    "\n"
    SVAR("Dims") " = " SVAR("DIMS") " | " SVAR("Expr") " " "\n"
    "  e.g. " SLIT("1024") " or " SLIT("1024x768x2") " or " SLIT("(2*1024)x(2*768)") "\n"
    "\n\n"
    SVAR("KernelArgs") " = a comma separated list of " SVAR("KernelArg") "\n"
    SVAR("KernelArg") " = "
    SVAR("MemInitExpr") " | "
    SVAR("Expr") " | "
    SVAR("LetVar") "\n"
    SVAR("LetVar") " = " SVAR("IDENT") "\n"
    "  a symbol defined in a " SLIT("let") " command"
    "\n"
    SVAR("DispatchLet") " = " SVAR("IDENT") "\n"
    "  executes a dispatch bound to a variable\n"
    "  e.g. " SLIT("let D=file.cl`kernel<1024>(...); D; D") " executes a dispatch twice\n"
    "\n"
    "************* BUILT-IN (non-dispatch) STATEMENTS *************\n"
    SVAR("BuiltinSt")
    " = " SVAR("DiffSt")
    " | " SVAR("PrintSt")
    " | " SVAR("SaveSt")
    "\n"
    "\n"
    "  " SVAR("DiffSt") "\n"
    "  = " SLIT("diff")
    SLIT("(") SVAR("IDENT") SLIT(", ") SVAR("IDENT") SLIT(")") "\n"
    "  | " SLIT("diff") SLIT("<") SVAR("TYPE") SLIT(">")
    SLIT("(") SVAR("IDENT") SLIT(", ") SVAR("IDENT") SLIT(")") "\n"
    "  | " SLIT("diff") SLIT("<") SVAR("TYPE") SLIT(", ") SVAR("FLOAT") SLIT(">")
    SLIT("(") SVAR("IDENT") SLIT(", ") SVAR("IDENT") SLIT(")") "\n"
    "  | " SLIT("diff")
    SLIT("(") SVAR("IDENT") SLIT(", ") SVAR("Expr") SLIT(")") "\n"
    "  | " SLIT("diff") SLIT("<") SVAR("TYPE") SLIT(">")
    SLIT("(") SVAR("IDENT") SLIT(", ") SVAR("Expr") SLIT(")") "\n"
    "  | " SLIT("diff") SLIT("<") SVAR("TYPE") SLIT(", ") SVAR("FLOAT") SLIT(">")
    SLIT("(") SVAR("IDENT") SLIT(", ") SVAR("Expr") SLIT(")") "\n"
    "  diffs two memory object or one against a replicated scalar value;\n"
    "  if a type argument is present, CLS will reinterpret elements as that type\n"
    "  NOTE: the program will exit with failure upon diff failure\n"
    "        use -Xno-exit-on-diff-fail to override this\n"
    "  e.g. " SLIT("let A=1:rw; #0`file.cl`kernel<1024>(A); diff<int>(A,0)") "\n"
    "       diffs all elements of buffer " SLIT("A") " with zeros\n"
    "  e.g. " SLIT("let A=1:rw; let B=2:rw; #0`file.cl`kernel<1024>(A,B); diff(A,B)") "\n"
    "       diffs buffers " SLIT("A") " and " SLIT("B") " element by element\n"
    "\n"
    "  " SVAR("PrintSt") "\n"
    "  = " SLIT("print") SLIT("(") SVAR("IDENT") SLIT(")") "\n"
    "  | " SLIT("print") SLIT("<") SVAR("INT") SLIT(">") SLIT("(") SVAR("IDENT") SLIT(")") "\n"
    "  | " SLIT("print") SLIT("<") SVAR("TYPE") SLIT(">") SLIT("(") SVAR("IDENT") SLIT(")") "\n"
    "  | " SLIT("print") SLIT("<") SVAR("TYPE") SLIT(",") SVAR("INT") SLIT(">") SLIT("(") SVAR("IDENT") SLIT(")") "\n"
    "  prints a memory object; an optional type interprets the surface's elements;\n"
    "  an optional integer argument overrides columns per line in output\n"
    "  e.g. " SLIT("let A = 1:w; #0`file.cl`kernel<1024>(A); print<float4>(A)") "\n"
    "       prints buffer A as float4's\n"
    "\n"
    "  " SVAR("SaveSt") " = " SLIT("save(") SVAR("STRLIT") SLIT(",") SVAR("IDENT") SLIT(")") "\n"
    "   saves a surface referenced by an identifier\n"
    "\n"
    "************* LET STATEMENTS *************\n"
    SVAR("Let")  " = " SLIT("let") SVAR("LetBinding") " (" SLIT(",") SVAR("LetBinding") ")*" "\n"
    "  a comma-separated list of bindings;\n"
    "  use this to share buffers in multiple dispatch statements\n"
    "  " SVAR("Binding") " =\n"
    "     " SVAR("IDENT") SLIT(" = ") SVAR("MemInitExpr") "\n"
    "   | " SVAR("IDENT") SLIT(" = ") SVAR("Dispatch") "\n"
    ;
}

std::string cls::CLS_SYN_PEX()
{
  std::stringstream ss;
  ss <<
    "************* SCALAR EXPRESSIONS *************\n"
    SVAR("Expr") " is a usual C-style expression\n"
    "   - most C operators as well as some many built-in C++ STL functions are allowed\n"
    "\n"
    "   - " SVAR("IntLitEx") " is an integer; e.g. " SLIT("32") " or " SLIT("0x20") "\n"
    "\n"
    "   - " SVAR("StrLitEx") " is a string literal in either single or double quotes);\n"
    "        e.g. " SLIT("\"foo\"") " or " SLIT("'foo'") " are identical\n"
    "\n"
    "   - the usual arithmetic operators are supported including:\n"
    "       bitwise logical, bit shifting, additive, multiplicative (incl. mod (" SLIT("%") "),\n"
    "       negation (" SLIT("-") "), and bitwise complement (" SLIT("~") ")\n"
    "\n"
    "   - vector types are represented via normal OpenCL-C syntax.\n"
    "       e.g. " SLIT("(float2)(1,4*5)") "\n"
    "\n"
    "   - custom structures types are represented via {..} syntax\n"
    "       e.g. " SLIT("{sqrt(3.1459), {3,4*5}}") "\n"
    "\n"
    "   - kernel arguments expressions may also reference some built-in variables\n"
    "       defined as the NDRange size\n"
    "       " SLIT("g.x") ", " SLIT("g.y") ", and " SLIT("g.z") " reference global dimensions\n"
    "       " SLIT("l.x") ", " SLIT("l.y") ", and " SLIT("l.z") " reference local dimensions\n"
    "\n"
    "   - " SLIT("undef") " means to use an undefined value; this can be useful for surface\n"
    "      initializers to be a don't care value\n"
    "\n"
    "   - type conversions and coercions attempt to mimic most C/C++ rules\n"
    "       at least: " SLIT("float(..)") ", " SLIT("int(..)") ", "
    SLIT("signed(..)") ", " SLIT("unsigned(..)") " are supported\n"
    "      e.g. " SLIT("unsigned(M_PI)") " evaluates to 3\n"
    "\n"
    "   - some OpenCL builtin constants are also defined (e.g. M_PI)\n"
    "     https://www.khronos.org/registry/OpenCL/sdk/2.0/docs/man/xhtml/mathConstants.html\n"
    "\n";
  bool first = true;
  const auto syms = builtin_symbols();
  ss <<
    "  - some built-in constants are the following:\n";
  ss <<
    "      ";
  size_t col = 6;
  for (size_t i = 0, len = syms.size(); i < len; i++) {
    if (i > 0) {
      ss << ",";
      col += 1;
      if (col > 70) {
        ss << "\n"
          "      ";
        col = 6;
      } else {
        ss << " ";
      }
    }
    if (i == len - 1)
      ss << "or ";
    ss << SLIT_OPEN << syms[i] << RESET;
    col += syms[i].size();
  }
  ss << "\n";

  ss <<
    "\n"
    "   - some unary C++ STL functions are supported:\n"
    "      " SLIT("abs") ", " SLIT("fabs") ", " SLIT("sqrt") ", " SLIT("cbrt") ",\n"
    "      " SLIT("exp") ", " SLIT("exp2") ", " SLIT("expm1") ", "
    SLIT("log") ", " SLIT("log2") ", " SLIT("log10") ", " SLIT("log1p") ",\n"
    "      " SLIT("sin") ", " SLIT("cos") ", " SLIT("tan") ", "
    SLIT("asin") ", " SLIT("acos") ", " SLIT("atan") ", "
    SLIT("sinh") ", " SLIT("cosh") ", " SLIT("tanh") ", "
    SLIT("asinh") ", " SLIT("acosh") ", " SLIT("atanh") ",\n"
    "      " SLIT("erf") ", " SLIT("erfc") ", " SLIT("tgamma") ", " SLIT("lgamma") ",\n"
    "      " SLIT("ceil") ", " SLIT("floor") ", " SLIT("trunc") ", " SLIT("round") ",\n"
    "      " SLIT("isfinite") ", " SLIT("isinf") ", " SLIT("isnan") ", " SLIT("isnormal") ",\n"
    "      " SLIT("nearbyint") ", " SLIT("nearbyint_rde") ", " SLIT("nearbyint_rdd") ", "
    SLIT("nearbyint_rdu") ", " SLIT("nearbyint_rtz") "\n"
    "\n"
    "   - some supported binary functions are:\n"
    "      " SLIT("fmod") ", " SLIT("atan2") ", " SLIT("fdim") ", "
    SLIT("hypot") ", " SLIT("pow") ", "
    SLIT("min") ", " SLIT("max") ", "
    SLIT("gcd") ", " SLIT("lcm") "\n"
    "\n"
    "   - some host-side OpenCL enumeration values may also be supported\n"
    "\n"
    "  Examples:\n"
    "       e.g. " SLIT("float(1<<10)") " generates 1024.0f\n"
    "       e.g. " SLIT("4*1024*(1024 + max(4,16)/sizeof(float))") "\n"
    "       e.g. " SLIT("int(max(pi*pi,2*e*e)") "\n"
    "\n"
    ;
  return ss.str();
}

std::string cls::CLS_SYN_SEX()
{
  return
    "************* MEMORY INITIALIZER EXPRESSIONS *************\n"
    "Buffer and image objects can be initialized in a number of ways.  These initializers\n"
    "consist of a per-component expression " SVAR("MemElementInitExpr")  " and a suffixing set\n"
    "of attributes including an optional sizing attribute.  For example, a writable buffer of\n"
    "zeros can be as simple as " SLIT("0:w") ".  If no size expression is given, then the\n"
    "surface size is deduced based on the dispatch size.\n"
    "\n"
    "\n"
    SVAR("MemInitExpr") " = "
      SVAR("MemElementInitExpr") SLIT(":") SVAR("MemObjSize") "?" SVAR("MemProps") "\n"
    "  Defines a memory object's initial contents;\n"
    "    if " SVAR("MemObjSize") " is absent, then CLS infers the size based on the\n"
    "    dimension (one element per global work item)\n"
    "\n"
    "  " SVAR("MemElementInitExpr") "\n"
    "    " " = " SVAR("ConstExpr") " | " SVAR("SeqExpr") " | " SVAR("CycExpr") " | " SVAR("RandExpr")
      " | " SVAR("FileExpr") " | " SVAR("ImgExpr") "\n"
    "    the memory object element initializer\n"
    "\n"
    "  " SVAR("MemObjSize") " = " SLIT("[") SVAR("Expr") SLIT("]") "\n"
    "      forces the buffer to be a specific size (in bytes)\n"
    "      e.g. " SLIT("0:[2*g.x*sizeof(float)]rw") " dedicates 2 floats per workitem\n"
    "           for a 1D kernel (e.g. instead of using float2)\n"
    "\n"
    "  " SVAR("MemProps") " = a sequence of characters specifying memory object properties\n"
    "    - access - " SLIT("r") ", " SLIT("w") ", or " SLIT("rw") " meaning:\n"
    "            CL_MEM_READ_ONLY, CL_MEM_READ_WRITE, and CL_MEM_READ_WRITE,\n"
    "            respectively\n"
    "               e.g. " SLIT("0:wr") " creates a zero-initialized buffer with\n"
    "            CL_MEM_READ_WRITE access\n"
  // TODO: enable
  //  "    - transfer - \n"
  //  "        " SLIT("m") " accesses the memory by buffer mapping (e.g. for zero copy)\n"
  //  "            e.g. " SLIT("1:mr") " creates a buffer initialized with 1's\n"
  //  "                 with CL_MEM_READ_ONLY access and initializes it via\n"
  //  "                 clEnqueue{Unmap,Map}Buffer\n"
  //  "        " SLIT("c") " uses buffer copy commands\n"
  //  "        " SLIT("sc") " uses coarse-grained SVM\n"
  //  "        " SLIT("sf") " uses fine-grained SVM\n"
    "    - saving - " SLIT("S") " - saves the memory contents before enqueue\n"
    "    - printing - " SLIT("P") " and " SLIT("p") " print memory objects before and after enqueue\n"
    "       these may be suffixed with a decimal integer sequence to override elements per row\n"
    "       e.g. " SLIT("..P16p8..") " prints a buffer before with 16 elements per console row\n"
    "            and 8 elements per row after\n"
    "       this is generally meant for debugging;\n"
    "       also see the " SLIT("print") " built-in statement\n"
    "\n"
    SVAR("ConstExpr")   " = " SVAR("Expr") "\n"
    "  initializes a memory object with each element to a constant value;\n"
    "  built-in dispatch variable sizes are permitted in the expression "
        "(e.g. " SLIT("g.x") " uses " SLIT("get_global_size(0)") ")\n"
    "\n"
    SVAR("SeqExpr")   " = "
    SLIT("seq(") SLIT(")")
    " | " SLIT("seq(") SVAR("Expr") SLIT(")")
    " | " SLIT("seq(") SVAR("Expr") SLIT(",") SVAR("Expr") SLIT(")") "\n"
    "  initializes a memory object to an arithmetic sequence of numbers;\n"
    "  an optional base and delta are permitted\n"
    "  e.g. " SLIT("seq()") " generates 0, 1, 2, ...\n"
    "  e.g. " SLIT("seq(17)") " generates 17, 18, ...\n"
    "  e.g. " SLIT("seq(1,3)") " generates 1, 4, 7, ...\n"
    "\n"
    SVAR("CycExpr")   " = "
    SLIT("cyc(") SVAR("Expr") "(" SLIT(",") SVAR("Expr") ")*" SLIT(")") "\n"
    "  initializes a memory object to an arithmetic sequence of numbers;\n"
    "  an optional base and delta are permitted\n"
    "  e.g. " SLIT("cyc(1)") " generates 1, 1, 1, ...\n"
    "  e.g. " SLIT("cyc(0,1)") " generates 0, 1, 0, 1, ...\n"
    "\n"
    SVAR("RandExpr")  " = "
    SLIT("random") SLIT("(") SVAR("RandBounds") "?" SLIT(")")
    " | " SLIT("random<") SVAR("EXPR") SLIT(">") SLIT("(") SVAR("RandBounds") "?" SLIT(")")
    "\n"
    "  randomly initializes a buffer with an optional seed and optional bounds\n"
    "  " SVAR("RandBounds") " = " SVAR("EXPR") "(" SLIT(",") " " SVAR("EXPR")")?\n"
    "  zero arguments defaults both low and high bounds\n"
    "    for integer types low is std::numeric_limits<T>::min() and\n"
    "       std::numeric_limits<T>::max()\n"
    "    for floating-point types low defaults to 0.0 and the high to 1.0\n"
    "  one argument set the high bound\n"
    "  two arguments sets both bounds\n"
    "  the pseudo random number generator starts with the given seed, implying\n"
    "    that between iterations, the same data is generated\n"
    "\n"
    SVAR("FileExpr")  " = " SVAR("FileBinExpr") " | " SVAR("FileTxtExpr")
          " | " SVAR("FileTxtColExpr") "\n"
    "  initializes a memory object's contents from a binary or text file\n"
    "  " SVAR("FileBinExpr") " = " SLIT("file<bin>(") SLIT("STR") SLIT(")") "\n"
  ///////////////////////////////////////////////////////////////////////////////////
    "    element width is determined by the memory objects's type\n"
    "    (e.g. global float4* would be 16B each);\n"
    "    file overflow or underflow lead to undefined results\n"
    "  " SVAR("FileTxtExpr")
    " = " SLIT("file<txt>(") SVAR("STR") SLIT(")")
    " | " SLIT("file<txt,") SVAR("INT") SLIT(">(") SVAR("STR") SLIT(")") "\n"
    "    loads from text file tokens ;elements are separated by spaces\n"
    "    parsing logic is influenced by the memory object's element type\n"
    "      e.g. a global float * will parse floats, and a global int * parses ints\n"
    "  " SVAR("FileTxtColExpr") "\n"
    "    = " SLIT("file<txt_col>") SLIT("(") SVAR("STR") SLIT(")") "\n"
    "    | " SLIT("file<txt_col") SLIT(",") SVAR("INT") SLIT(">")
              SLIT("(") SVAR("STR") SLIT(")") "\n"
    "    | " SLIT("file<txt_col") SLIT(",") SVAR("INT") SLIT(",") SVAR("STR") SLIT(">")
              SLIT("(") SVAR("STR") SLIT(")") "\n"
    "    loads from a column of a text file\n"
    "      e.g. " SLIT("<txt_col>") " uses column 0 and ' ' as the column separator\n"
    "      e.g. " SLIT("<txt_col,2>") " uses column 2 and ' ' as the column separator\n"
    "      e.g. " SLIT("<txt_col,2,','>") " uses column 2 and ',' as the column separator\n"
    "                      (i.e. simple CSV)\n"
    "\n"
    SVAR("ImgExpr")
    " = " SLIT("image<") " " SVAR("ChannelOrder") SLIT(", ") SVAR("DataType") " " SLIT(">")
            SLIT("(") SVAR("STR") SLIT(")") "\n"
    "   " SVAR("ChannelOrder") " = an image channel order\n"
    "     https://www.khronos.org/registry/OpenCL/sdk/2.1/docs/man/xhtml/cl_image_format.html\n"
    "     as well as some shorthands; drop 3 characters and switch to lower case\n"
    "     e.g. both " SLIT("CL_sRGBx") " and " SLIT("srgbx") " mean the same thing\n"
    "   " SVAR("DataType") " = is a image data type; the usual OpenCL enums are permitted\n"
    "     as well as shorthands such as:\n"
    "       " SLIT("u16") " meaning CL_UNSIGNED_INT16)\n"
    "       " SLIT("un8") " meaning CL_UNORM_INT8)\n"
    "       " SLIT("s32") " meaning CL_SIGNED_INT32)\n"
    "       " SLIT("un565") " meaning CL_UNORM_SHORT_565)\n"
    "       " SLIT("un101010_2") " meaning CL_UNORM_INT_101010_2)\n"
    "       " SLIT("f32") " meaning CL_FLOAT\n"
    "       " SLIT("f16") " meaning CL_HALF_FLOAT\n"
    "       " "... and many more (c.f. cls_parser.cpp)\n"
    "\n"
    "  e.g. " SLIT("image<rgb,un8>('foo.bmp')") " and "
    SLIT("image<CL_RGB,CL_UNORM_INT8>('foo.bmp')") "\n"
    "       both load image foo.bmp and convert it to an CL_RGB image with CL_UNORM_INT8\n"
    "       .bmp will load as a bitmap\n"
    "       .ppm will load a PPM file\n"
#ifdef USE_LODE_PNG
    "       .png loads as PNG format\n"
#else
    "       (compiled without .png support)\n"
#endif
    "\n"
  ;
}


struct cls_parser: parser
{
  script &s;

  cls_parser(
    diagnostics &ds, const std::string &inp, script &_s)
    : parser(ds, inp), s(_s) { }

  // a rough solution to enable use to read tokens including spaces
  // e.g. `path/has spaces/baz.cl[-DTYPE=int -cl-some-option]`kernel
  //       ^^^^^^^^^^^^^^^^^^^^^^
  //                              ^^^^^^^^^^^^^^^^^^^^^^^^^^
  std::string consumeToChar(const char *set)
  {
    const std::string &s = input();
    size_t start = nextLoc().offset;
    size_t len = 0;
    while (start + len < s.length()) {
      if (strchr(set, s[start + len])) {
        break;
      }
      len++;
    }
    while (nextLoc().offset != start + len)
      skip();
    return s.substr(start, len);
  }

  // 1024[1200]x768[800]x4
  //
  // similar to dimensions, but includes pitch
  void parseImageDimensionExpressions(
    std::vector<init_spec_atom *> &dims,
    std::vector<init_spec_atom *> &pitches)
  {
    dims.push_back(parseInitAtom());
    if (consumeIf(LBRACK)) {
      pitches.push_back(parseInitAtom());
      consume(RBRACK);
    } else {
      pitches.push_back(nullptr);
    }
    if (dims.size() == 3)
      return; // at the end

    if (consumeIfIdentEq("x")) {
      // EXPR IDENT("x") ...
      parseImageDimensionExpressions(dims, pitches);
    } else {
      while (lookingAtIdent()) {
        if (dims.size() == 3)
          fatal("syntax error in image dimension constant");
        // 1024x768x3 or 1024x768x(...) or 1024x768 x(...)
        //     ^^^^^^        ^^^^^             ^^^^
        // need to split x768x3 into: "x 768 x 3"
        loc at = nextLoc();
        std::string lxm = tokenString();
        skip();
        if (lxm[0] != 'x') {
          fatalAt(at, "syntax error in image dimensions");
        }
        size_t off = 0;
        while (off < lxm.size()) {
          if (lxm[off] != 'x') {
            at.column += (uint32_t)off;
            at.offset += (uint32_t)off;
            fatalAt(at, "syntax error in image dimensions");
          }
          off++;
          size_t end = off;
          while (end < lxm.size() && isdigit(lxm[end]))
            end++;
          if (end == off) {
            // it must be a new dimension
            // 2048x1024x(...)
            //          ^
            parseImageDimensionExpressions(dims, pitches);
            return;
          } else {
            // dimension is embedded in identifier
            // 2048x1024...
            //     ^^^^^
            try {
              auto val = (size_t)std::strtoull(
                lxm.substr(off, end-off).c_str(), nullptr, 10);
              loc this_int = at;
              this_int.column += (uint32_t)off;
              this_int.offset += (uint32_t)off;
              this_int.extent = (uint32_t)(end - off);
              dims.push_back(new init_spec_int(this_int, val));
              off = end;
            } catch (...) {
              at.offset += (uint32_t)off;
              at.column += (uint32_t)off;
              fatalAt(at, "syntax error in image dimensions");
            }
            // 2048x1024 x16x32
            //           ^^^^
            // 2048x1024 x16x(32)
            //           ^^^^
            // 2048x1024 x16
            //           ^^^
            // bail to the top loop and start the next token
            // that will handle expressions or fused numbers
            if (off == lxm.size()) {
              if (dims.size() == 2) {
                if (consumeIf(LBRACK)) {
                  pitches.push_back(parseInitAtom());
                  consume(RBRACK);
                } else {
                  pitches.push_back(nullptr);
                }
              }
              break;
            }
            // OTHERWISE
            // we are still within the same identifier token and hoping for
            // the next constant
            //
            // 2048x1024x16
            //          ^^^
            //   => next iteration of the innermost loop
            //
            // 2048x1024x(16)
            //          ^
            //   => next iteration of this while will break out
            //
            if (dims.size() == 3) {
              at.offset += (uint32_t)off;
              at.column += (uint32_t)off;
              fatalAt(at, "syntax error in image dimensions (too many)");
            }
          }
        } // while identifier
      } // while looking at successive identifiers
    } // if
  }

  init_spec_atom *parseInitAtomPrim()
  {
    auto at = nextLoc();
    if (lookingAt(STRLIT)) {
      fatal("bare strings not allowed for files anymore "
        "(use bin('...') and txt('...'))");
      return nullptr;
    } else if (lookingAtFloat()) {
      return new init_spec_float(at, consumeFloat());
    } else if (lookingAtInt()) {
      // we parse imm's as unsigned to enable u64 values, the cast will
      // preserve the bit patterns
      int64_t imm = (int64_t)consumeIntegral<uint64_t>();
      return new init_spec_int(at, imm);
    } else if (lookingAtIdent()) {
      // e.g. "X" or "g.x" or "pow(...)"
      auto id = tokenString();
      skip();
      if (lookingAt(DOT)) {
        // e.g. "g.x"
        while (consumeIf(DOT)) {
          id += '.';
          if (!lookingAt(IDENT))
            fatal("syntax error in initializer expression field access");
          id += tokenString();
          skip();
        }
        for (int biv = init_spec_builtin::BIV_FIRST;
          biv <= init_spec_builtin::BIV_LAST;
          biv++)
        {
          if (id == init_spec_builtin::syntax_for((init_spec_builtin::biv_kind)biv)) {
            at.extend_to(nextLoc());
            return new init_spec_builtin(at, (init_spec_builtin::biv_kind)biv);
          }
        }
        at.extend_to(nextLoc());
        return new init_spec_symbol(at, id);
      } else if (lookingAt(LPAREN) || lookingAt(LANGLE) ||
        id == "sizeof" || id == "random" ||
        id == "seq" || id == "cyc" || id == "file" || id == "image")
      {
        // foo<...  (e.g. random<12007>(...))
        // or
        // foo(...
        //
        // TODO: generalize function parsing to
        //    F<...>(....)
        // then match by template arguments
        if (id == "sizeof") {
          return parseInitAtomPrimSizeof(at);
        } else if (id == "random") {
          return parseInitAtomPrimRandom(at);
        } else if (id == "file") {
          return parseInitAtomPrimFile(at);
        } else if (id == "image") {
          return parseInitAtomPrimImage(at);
        } else {
          ///////////////////////////////////////////////////
          // generic function syntax (could be seq still)
          std::vector<init_spec_atom *> args;
          consume(LPAREN);
          while (!lookingAt(RPAREN)) {
            args.push_back(parseInitAtom());
            if (!consumeIf(COMMA))
              break;
          }
          consume(RPAREN);

          ///////////////////////////////////////////////////
          // special functions (pseudo functions)
          if (id == "seq") {
            init_spec_seq *iss = nullptr;
            switch (args.size()) {
            case 0: iss = new init_spec_seq(at, nullptr, nullptr); break;
            case 1: iss = new init_spec_seq(at, args[0], nullptr); break;
            case 2: iss = new init_spec_seq(at, args[0], args[1]); break;
            default: fatalAt(at, "wrong number of args to seq");
            }
            iss->defined_at.extend_to(nextLoc());
            return iss;
          } else if (id == "cyc") {
            init_spec_cyc *isc = new init_spec_cyc(at);
            if (args.empty()) {
              fatalAt(at, "cyc args must be non-empty");
            }
            for (const auto *arg : args) {
              isc->args.push_back(arg);
            }
            isc->defined_at.extend_to(nextLoc());
            return isc;
          }

          ///////////////////////////////////////////////////
          // regular arithmetic functions
          if (args.size() == 1) {
            if (id == "fabs")
              fatalAt(at, "use \"abs\" for the absolute value");
            else if (id == "sqt")
              fatalAt(at, "use \"sqrt\" for the square root");

            const auto *op = init_spec_uex::lookup_op(id.c_str());
            if (!op) {
              if (init_spec_bex::lookup_op(id.c_str())) {
                fatalAt(at, "function requires two arguments");
              } else {
                fatalAt(at, "not a unary function");
              }
            }
            return new init_spec_uex(at, *op, args[0]);
          } else if (args.size() == 2) {
            const auto *op = init_spec_bex::lookup_op(id.c_str());
            if (!op) {
              fatalAt(at, "not a binary function");
            }
            auto *isbe = new init_spec_bex(*op, args[0], args[1]);
            isbe->defined_at = at; // reset start loc to function name
            isbe->defined_at.extend_to(nextLoc());
            return isbe;
          } else {
            fatalAt(at, "undefined function");
            return nullptr;
          }
        } // end else not random
      } else if (id == "undef") {
        return new init_spec_undef(at);
      } else {
        return parseInitAtomPrimSymbol(at, id);
      }
    } else if (consumeIf(LBRACE)) {
      return parseInitAtomPrimStruct(at);
    } else if (consumeIf(LPAREN)) {
      // grouping expression or vector value
      if (lookingAtSeq(IDENT, RPAREN))
      {
        const type *et = lookupBuiltinType(tokenString(), 8);
        if (et == nullptr || !et->is<type_vector>()) {
          fatal("not a vector type");
          return nullptr;
        } else {
          return parseInitAtomPrimVector(at, et);
        }
      } else {
        // grouping expression (E)
        init_spec_atom *e = parseInitAtom();
        consume(RPAREN);
        return e;
      }
    } else {
      fatal("syntax error in initializer expression");
      return nullptr;
    }
  }
  init_spec_atom *parseInitAtomPrimSizeof(loc at)
  {
    bool has_parens = consumeIf(LPAREN);
    if (!lookingAtIdent())
      fatal("expected type name");
    loc nm_at = nextLoc();
    std::string sizeof_arg = tokenString();
    skip();
    if (has_parens)
      consume(RPAREN);
    at.extend_to(nextLoc());
    // manual dereference memobject
    auto itr = s.let_bindings.find(sizeof_arg);
    if (itr == s.let_bindings.end()) {
      // assume it's a type
      return new init_spec_sizeof(at, sizeof_arg);
    } else {
      let_spec *ls = itr->second;
      spec *rs = ls->value;
      if (rs->skind != spec::INIT_SPEC)
        fatal("symbol refers to non-memory object");
      init_spec *is = (init_spec *)rs;
      if (is->skind != init_spec::IS_MEM)
        fatal("symbol refers to non-memory object");
      return new init_spec_sizeof(at,
        refable<init_spec_mem>(nm_at, sizeof_arg, (init_spec_mem *)is));
    }
  }
  init_spec_atom *parseInitAtomPrimFile(loc at)
  {
    //
    // file('foo.bin'):r                 // binary
    // file<bin>('foo.bin'):r            // binary
    //
    // file<text>('foo.txt'):r           // all tokens using sep = ' '
    // file<text,','>('foo.txt'):r       // all tokens using sep = ','
    //
    //
    // file<text_col>('foo.txt'):r       // use column 0 with ' ' delimiter
    // file<text_col,0>('foo.txt'):r     // same as above
    // file<text_col,1,','>('foo.txt'):r // col 1 use , as separator
    auto flv = init_spec_file::BIN;
    int col = 0;
    std::string sep = " ";
    if (consumeIf(LANGLE)) {
      if (!lookingAt(RANGLE)) {
        auto fmt_loc = nextLoc();
        auto flv_str = consumeIdent("data format (identifier)");
        if (flv_str == "bin") {
          flv = init_spec_file::BIN;
        } else if (flv_str == "text") {
          flv = init_spec_file::TXT;
          if (consumeIf(COMMA)) {
            if (!lookingAt(STRLIT)) {
              fatal("expected separator string (literal)");
            }
            sep = tokenStringLiteral();
            skip();
          }
        } else if (flv_str == "text_col") {
          flv = init_spec_file::TXT_COL;
          if (consumeIf(COMMA)) {
            col = consumeIntegral<int>("column index (int)");
            if (consumeIf(COMMA)) {
              if (!lookingAt(STRLIT)) {
                fatal("expected separator string (literal)");
              }
              sep = tokenStringLiteral();
              skip();
            }
          }
        } else {
          fatalAt(fmt_loc, "unsupported file flavor; should be: bin, text, ...");
        }
      }
      consume(RANGLE);
    }
    skip();
    if (!lookingAt(STRLIT)) {
      fatalAt(at, "expected file path (string literal)");
    }
    auto s = tokenStringLiteral();
    skip();
    consume(RPAREN);
    at.extend_to(nextLoc());
    return new init_spec_file(at, s, flv, col, sep);
  }
  init_spec_atom *parseInitAtomPrimRandom(loc at)
  {
    auto func = new init_spec_rng(at);
    int64_t seed = 0;
    bool has_seed = false;
    if (consumeIf(LANGLE)) {
      if (!lookingAt(RANGLE)) {
        seed = consumeIntegral<int64_t>("seed (int)");
        has_seed = true;
      }
      consume(RANGLE);
    }
    if (consumeIf(LPAREN)) {
      if (!lookingAt(RPAREN)) {
        auto *arg1 = parseInitAtom();
        if (consumeIf(COMMA)) {
          auto *arg2 = parseInitAtom();
          func->e_lo = arg1;
          func->e_hi = arg2;
        } else {
          func->e_hi = arg1;
        }
      }
      consume(RPAREN);
    }
    if (has_seed)
      func->set_seed(seed);
    func->defined_at.extend_to(nextLoc());
    return func;
  }
  init_spec_atom *parseInitAtomPrimImage(loc at)
  {
    consume(LANGLE);
    auto ord = init_spec_image::RGB;
    if (consumeIfIdentEq("i") ||
      consumeIfIdentEq("CL_INTENSITY"))
    {
      ord = init_spec_image::I;
    } else if (consumeIfIdentEq("l") ||
      consumeIfIdentEq("CL_LUMINANCE"))
    {
      ord = init_spec_image::L;
    } else if (consumeIfIdentEq("r") ||
      consumeIfIdentEq("CL_R"))
    {
      ord = init_spec_image::R;
    } else if (consumeIfIdentEq("rx") ||
      consumeIfIdentEq("CL_Rx"))
    {
      ord = init_spec_image::Rx;
    } else if (consumeIfIdentEq("rg") ||
      consumeIfIdentEq("CL_RG"))
    {
      ord = init_spec_image::RG;
    } else if (consumeIfIdentEq("rgx") ||
      consumeIfIdentEq("CL_RGx"))
    {
      ord = init_spec_image::RGx;
    } else if (consumeIfIdentEq("rgb") ||
      consumeIfIdentEq("CL_RGB"))
    {
      ord = init_spec_image::RGB;
    } else if (consumeIfIdentEq("rgbx") ||
      consumeIfIdentEq("CL_RGBx"))
    {
      ord = init_spec_image::RGBx;
    } else if (consumeIfIdentEq("rgba") ||
      consumeIfIdentEq("CL_RGBA"))
    {
      ord = init_spec_image::RGBA;
    } else if (consumeIfIdentEq("argb") ||
      consumeIfIdentEq("CL_ARGB"))
    {
      ord = init_spec_image::ARGB;
    } else if (consumeIfIdentEq("bgra") ||
      consumeIfIdentEq("CL_BGRA"))
    {
      ord = init_spec_image::BGRA;

    } else if (consumeIfIdentEq("srgb") ||
      consumeIfIdentEq("CL_sRGB"))
    {
      ord = init_spec_image::sRGB;
    } else if (consumeIfIdentEq("srgbx") ||
      consumeIfIdentEq("CL_sRGBx"))
    {
      ord = init_spec_image::sRGBx;
    } else if (consumeIfIdentEq("srgba") ||
      consumeIfIdentEq("CL_sRGBA"))
    {
      ord = init_spec_image::sRGBA;
    } else if (consumeIfIdentEq("sbgra") ||
      consumeIfIdentEq("CL_sBGRA"))
    {
      ord = init_spec_image::sBGRA;
    } else {
      fatal("unrecognized channel order (try r, rg, rgb, rgba, ...)");
    }
    consume(COMMA);
    auto ty = init_spec_image::U8;
    ///////////////////////////////////////////////////////////////////////
    if (consumeIfIdentEq("un8") ||
      consumeIfIdentEq("CL_UNORM_INT8"))
    {
      ty = init_spec_image::UN8;
    } else if (consumeIfIdentEq("un16") ||
      consumeIfIdentEq("CL_UNORM_INT16"))
    {
      ty = init_spec_image::UN16;
    } else if (consumeIfIdentEq("un24") ||
      consumeIfIdentEq("CL_UNORM_INT24"))
    {
      ty = init_spec_image::UN24;
    } else if (consumeIfIdentEq("un565") ||
      consumeIfIdentEq("CL_UNORM_SHORT_565"))
    {
      ty = init_spec_image::UN565;
    } else if (consumeIfIdentEq("un555") ||
      consumeIfIdentEq("CL_UNORM_SHORT_555"))
    {
      ty = init_spec_image::UN555;
    } else if (consumeIfIdentEq("un101010") ||
      consumeIfIdentEq("CL_UNORM_INT_101010"))
    {
      ty = init_spec_image::UN101010;
    } else if (consumeIfIdentEq("un101010_2") ||
      consumeIfIdentEq("CL_UNORM_INT_101010_2"))
    {
      ty = init_spec_image::UN101010_2;
      ///////////////////////////////////////////////////////////////////////
    } else if (consumeIfIdentEq("sn8") ||
      consumeIfIdentEq("CL_SNORM_INT8"))
    {
      ty = init_spec_image::SN8;
    } else if (consumeIfIdentEq("sn16") ||
      consumeIfIdentEq("CL_SNORM_INT16"))
    {
      ty = init_spec_image::SN16;
      ///////////////////////////////////////////////////////////////////////
    } else if (consumeIfIdentEq("u8") ||
      consumeIfIdentEq("CL_UNSIGNED_INT8"))
    {
      ty = init_spec_image::U8;
    } else if (consumeIfIdentEq("u16") ||
      consumeIfIdentEq("CL_UNSIGNED_INT16"))
    {
      ty = init_spec_image::U16;
    } else if (consumeIfIdentEq("u32") ||
      consumeIfIdentEq("CL_UNSIGNED_INT32"))
    {
      ty = init_spec_image::U32;
      ///////////////////////////////////////////////////////////////////////
    } else if (consumeIfIdentEq("s8") ||
      consumeIfIdentEq("CL_SIGNED_INT8"))
    {
      ty = init_spec_image::S8;
    } else if (consumeIfIdentEq("s16") ||
      consumeIfIdentEq("CL_SIGNED_INT16"))
    {
      ty = init_spec_image::S16;
    } else if (consumeIfIdentEq("s32") ||
      consumeIfIdentEq("CL_SIGNED_INT32"))
    {
      ty = init_spec_image::S32;
      ///////////////////////////////////////////////////////////////////////
    } else if (consumeIfIdentEq("f32") ||
      consumeIfIdentEq("CL_FLOAT"))
    {
      ty = init_spec_image::F32;
    } else if (consumeIfIdentEq("f16") ||
      consumeIfIdentEq("CL_HALF_FLOAT"))
    {
      ty = init_spec_image::F16;
      ///////////////////////////////////////////////////////////////////////
    } else {
      fatal(
        "unrecognized image format (try: u8, u16, ..., f32, f16 ,...)");
    }

    init_spec_atom *width = nullptr, *height = nullptr, *depth = nullptr;
    init_spec_atom *row_pitch = nullptr, *slice_pitch = nullptr;
    if (consumeIf(COMMA)) {
      // image dimension:
      // W (RP)
      // W (RP) x H (SP)
      // W (RP) x H (SP) x D
      // the spacing makes this hard
      std::vector<init_spec_atom *> dims;
      std::vector<init_spec_atom *> pitches;
      parseImageDimensionExpressions(dims, pitches);
      //
      width = dims[0];
      height = dims.size() >= 2 ? dims[1] : nullptr;
      depth = dims.size() >= 3 ? dims[2] : nullptr;
      row_pitch = pitches[0];
      slice_pitch = pitches.size() >= 2 ? pitches[1] : nullptr;
    }
    consume(RANGLE);
    std::string file;
    if (consumeIf(LPAREN)) {
      if (!lookingAt(STRLIT)) {
        fatalAt(at, "expected file path (string literal)");
      }
      file = tokenStringLiteral(); skip();
      consume(RPAREN);
    }
    at.extend_to(nextLoc());
    return new init_spec_image(
      at, file, ord, ty, width, row_pitch, height, slice_pitch, depth);
  }
  init_spec_atom *parseInitAtomPrimStruct(loc at)
  {
    // {...}
    auto re = new init_spec_record(at);
    if (!lookingAt(RBRACE)) {
      re->children.push_back(parseInitAtom());
      while (consumeIf(COMMA))
        re->children.push_back(parseInitAtom());
    }
    consume(RBRACE);
    re->defined_at.extend_to(nextLoc());
    return re;
  }
  init_spec_atom *parseInitAtomPrimVector(loc at, const type *et)
  {
    // vector initializer
    if (!et->is<type_vector>()) {
      fatal("vector initializer type is not vector");
    }
    auto *vi = new init_spec_vector(at, et);
    skip();
    consume(RPAREN);
    //   vector:    (float4)(1,2,3,4)
    //   broadcast: (float4)(3.14f)
    if (lookingAt(LPAREN)) {
      consume(LPAREN);
      vi->children.push_back(parseInitAtom());
      while (consumeIf(COMMA))
        vi->children.push_back(parseInitAtom());
      consume(RPAREN);
      if (vi->children.size() == 1) {
        // broadcast
        for (int i = 0; i < (int)et->as<type_vector>().length; i++)
          vi->children.push_back(vi->children[i]);
      }
    } else {
      // broadcast: (float4)3.14f
      auto *e = parseInitAtom();
      for (int i = 0; i < (int)et->as<type_vector>().length; i++)
        vi->children.push_back(e);
    }
    return vi;
  }
  init_spec_atom *parseInitAtomPrimSymbol(loc at, const std::string &id)
  {
    auto mv = lookup_builtin_symbol(id);
    if (mv.has_value()) {
      val v = *mv;
      if (v.is_floating())
        return new init_spec_float(at, v.as<double>());
      else
        return new init_spec_int(at, v.as<int64_t>());
    } else {
      // some other symbol (may target a LET binding)
      return new init_spec_symbol(at, id);
    }
  }

  init_spec_atom *parseInitAtomUnr()
  {
    if (lookingAt(SUB) || lookingAt(TILDE)) {
      auto loc = nextLoc();
      bool isSub = lookingAt(SUB);
      const auto &op = isSub ?
        *init_spec_uex::lookup_op("-") :
        *init_spec_uex::lookup_op("~");

      skip();
      //
      // NOTE: we deal with 9223372036854775808 within atom as a second
      // try parse as uint64_t
      //
      // if (isSub && lookingAtIdentEq("9223372036854775808")) {
        // given "-9223372036854775808", we can't parse the atom
        // as 9223372036854775808 (out of bounds for int64_t)
      // }
      init_spec_atom *e = parseInitAtomUnr();
      return new init_spec_uex(loc, op, e);
    } else {
      return parseInitAtomPrim();
    }
  }
  init_spec_atom *parseInitAtomMul()
  {
    init_spec_atom *e = parseInitAtomUnr();
    while (lookingAt(MUL) || lookingAt(DIV) || lookingAt(MOD)) {
      const auto &op =
        lookingAt(MUL) ? *init_spec_bex::lookup_op("*") :
        lookingAt(DIV) ? *init_spec_bex::lookup_op("/") :
        *init_spec_bex::lookup_op("%");
      skip();
      init_spec_atom *t = parseInitAtomUnr();
      e = new init_spec_bex(op, e, t);
    }
    return e;
  }
  init_spec_atom *parseInitAtomAdd()
  {
    init_spec_atom *e = parseInitAtomMul();
    while (lookingAt(ADD) || lookingAt(SUB)) {
      const auto &op = lookingAt(ADD) ?
        *init_spec_bex::lookup_op("+") :
        *init_spec_bex::lookup_op("-");
      skip();
      init_spec_atom *t = parseInitAtomMul();
      e = new init_spec_bex(op, e, t);
    }
    return e;
  }
  init_spec_atom *parseInitAtomShift()
  {
    init_spec_atom *e = parseInitAtomAdd();
    while (lookingAt(LSH) || lookingAt(RSH)) {
      const auto &op = lookingAt(LSH) ?
        *init_spec_bex::lookup_op("<<") :
        *init_spec_bex::lookup_op(">>");
      skip();
      init_spec_atom *t = parseInitAtomAdd();
      e = new init_spec_bex(op, e, t);
    }
    return e;
  }
  init_spec_atom *parseInitAtomBitwiseAND()
  {
    init_spec_atom *e = parseInitAtomShift();
    while (consumeIf(AMP)) {
      init_spec_atom *t = parseInitAtomShift();
      e = new init_spec_bex(*init_spec_bex::lookup_op("&"), e, t);
    }
    return e;
  }
  init_spec_atom *parseInitAtomBitwiseXOR()
  {
    init_spec_atom *e = parseInitAtomBitwiseAND();
    while (consumeIf(CIRC)) {
      init_spec_atom *t = parseInitAtomBitwiseAND();
      e = new init_spec_bex(*init_spec_bex::lookup_op("^"), e, t);
    }
    return e;
  }
  init_spec_atom *parseInitAtomBitwiseOR()
  {
    init_spec_atom *e = parseInitAtomBitwiseXOR();
    while (consumeIf(PIPE)) {
      init_spec_atom *t = parseInitAtomBitwiseXOR();
      e = new init_spec_bex(*init_spec_bex::lookup_op("|"), e, t);
    }
    return e;
  }
  init_spec_atom *parseInitAtom()
  {
    return parseInitAtomBitwiseOR();
  }

  // starts
  //  p128...
  //  ^
  // ends
  //  p128...
  //     ^
  // and returns 128
  static int read_columns(const std::string &s, size_t &i) {
    int cols = 0;
    while (i + 1 < s.size() && ::isdigit(s[i + 1])) {
      cols = 10*cols + s[i + 1] - '0';
      i++;
    }
    return cols;
  }

  init_spec *parseInit()
  {
    auto l = nextLoc();
    init_spec_atom *e = parseInitAtom();

    if (consumeIf(COLON)) {
      // memory initializer
      init_spec_mem *m = new init_spec_mem(l);
      m->root = e;
      if (consumeIf(LBRACK)) {
        init_spec_atom *de = parseInitAtom();
        m->dimension = de;
        consume(RBRACK);
      }
      // attributes
      if (!lookingAt(IDENT)) {
        fatal("expected buffer/image attributes");
      }
      auto s = tokenString();
      skip();
      for (size_t i = 0; i < s.size(); i++) {
        auto setTx = [&](init_spec_mem::transfer t) {
          if (m->transfer_properties != init_spec_mem::transfer::TX_INVALID) {
            fatalAt(l, "memory transfer respecification");
          }
          m->transfer_properties = t;
        };

        switch (s[i]) {
        case 'r':
          m->access_properties = (init_spec_mem::access)(
            m->access_properties |
            init_spec_mem::access::INIT_SPEC_MEM_READ);
          break;
        case 'w':
          m->access_properties = (init_spec_mem::access)(
            m->access_properties |
            init_spec_mem::access::INIT_SPEC_MEM_WRITE);
          break;
        case 's': // SVM
          if (i < s.size() - 1) {
            i++;
            switch (s[i]) {
            case 'c':
            case 'f':
              if (m->transfer_properties != init_spec_mem::transfer::TX_INVALID)
                fatalAt(l,
                  "invalid svm memory attribute (must be :..sc.. or :..sf..)");
              setTx(s[i] == 'c' ?
                init_spec_mem::transfer::TX_SVM_COARSE :
                init_spec_mem::transfer::TX_SVM_FINE);
              break;
            default:
              // fatalAt(l, "invalid svm memory attribute (must be sc or sf)");
              // assume coarse if only one char given
              setTx(init_spec_mem::transfer::TX_SVM_COARSE);
            }
          } else {
            setTx(init_spec_mem::transfer::TX_SVM_COARSE);
          }
          break;
        case 'm':
          setTx(init_spec_mem::transfer::TX_MAP);
          break;
        case 'c':
          setTx(init_spec_mem::transfer::TX_COPY);
          break;
          // SPECIFY: do we consider deprecating these after stable development
          //          they are certainly nice for debugging
        case 'P':
          m->print_pre = true;
          m->print_pre_elems_per_row = read_columns(s, i);
          break;
        case 'p':
          m->print_post = true;
          m->print_post_elems_per_row = read_columns(s, i);
          break;
        case 'S':
          m->save_post = true;
          break;
        default:
          // adjust the location to the middle of this token for
          // the diganostic
          l.column += (uint32_t)i;
          l.offset += (uint32_t)i;
          fatalAt(l, "invalid memory attribute");
        }
      }
      if (m->transfer_properties == init_spec_mem::transfer::TX_INVALID)
        m->transfer_properties = init_spec_mem::transfer::TX_COPY; // default to copy
      m->defined_at.extend_to(nextLoc());
      return m;
    } else {
      // regular primitive
      return e;
    }
  }

  template <typename T>
  refable<T> dereferenceLet(
    enum spec::spec_kind skind,
    const char *what)
  {
    auto at = nextLoc();
    std::string name = consumeIdent();
    auto itr = s.let_bindings.find(name);
    if (itr == s.let_bindings.end()) {
      fatalAt(at, what, " not defined");
    }
    let_spec *ls = itr->second;
    spec *rs = ls->value;
    if (rs->skind != skind) {
      std::stringstream ss;
      ss << "identifier does not reference a " << what;
      ss << " (defined on line " << rs->defined_at.line << ")";
      fatalAt(at, ss.str());
    }
    return refable<T>(at, name, (T *)rs);
  }

  refable<init_spec_mem> dereferenceLetMem() {
    refable<init_spec> rf =
      dereferenceLet<init_spec>(spec::INIT_SPEC, "memory object");
    if ((*rf).skind != init_spec::IS_MEM) {
      fatalAt(rf.defined_at, "identifier does not reference a memory object");
    }
    return refable<init_spec_mem>(
      rf.defined_at,
      rf.identifier,
      (init_spec_mem *)rf.value);
  }

  // Dimensions are a lexically evil mess.  We do some massive hacking to make
  // it work, but this all requires the assumption that an identifier may not
  // follow a dimension.  So syntax such as
  //  1024x768 foo   [ILLEGAL in our language now]
  //
  // Previously I introduced a lexical pattern for dimensions, but I want to
  // support arithmetic within them. (4*1024)x4*1024 so I have to deal with this
  // mess at some point.
  //
  // EXAMPLE             PARSES AS
  //
  // 1024 x 25           INT(1024) IDENT("x")     INT
  // 1024x256            INT(1024) IDENT("x256")
  // 1024x256x3          INT(1024) IDENT("x256x3")
  // 1024x256 x3         INT(1024) IDENT("x256x3") IDENT("x3")
  // 1024x768x(2*4)      INT(1024) IDENT("x256x") EXPR
  // (2*1024)x768 x 3    EXPR      IDENT("x768") IDENT("x") INT(3)
  void parseDimensionExpressions(std::vector<init_spec_atom *> &dims) {
    dims.push_back(parseInitAtom());
    if (lookingAtIdentEq("x")) {
      // EXPR IDENT("x") ...
      skip();
      parseDimensionExpressions(dims);
    } else {
      while (lookingAtIdent()) {
        // 1024x768x3 or 1024x768x(...) or 1024x768 x(...)
        //     ^^^^^^        ^^^^^             ^^^^
        // need to split x768x3 into: "x 768 x 3"
        loc at = nextLoc();
        std::string lxm = tokenString();
        skip();
        if (lxm[0] != 'x') {
          fatalAt(at, "syntax error in dimension constant");
        }
        size_t off = 0;
        while (off < lxm.size()) {
          if (lxm[off] != 'x') {
            at.column += (uint32_t)off;
            at.offset += (uint32_t)off;
            fatalAt(at, "syntax error in dimension constant");
          }
          off++;
          size_t end = off;
          while (end < lxm.size() && isdigit(lxm[end]))
            end++;
          if (end == off) {
            // it must be a new dimension
            // 2048x1024x(...)
            //          ^
            parseDimensionExpressions(dims);
            return;
          } else {
            // dimension is embedded in identifier
            // 2048x1024...
            //     ^^^^^
            try {
              auto val = (size_t)std::strtoull(
                lxm.substr(off, end-off).c_str(), nullptr, 10);
              loc this_int = at;
              this_int.column += (uint32_t)off;
              this_int.offset += (uint32_t)off;
              this_int.extent = (uint32_t)(end - off);
              dims.push_back(new init_spec_int(this_int, val));
              off = end;
            } catch (...) {
              at.offset += (uint32_t)off;
              at.column += (uint32_t)off;
              fatalAt(at, "syntax error in dimension constant");
            }
            // 2048x1024 x16x32
            //           ^^^^
            // 2048x1024 x16x(32)
            //           ^^^^
            // 2048x1024 x16
            //           ^^^
            // bail to the top loop and start the next token
            // that will handle expressions or fused numbers
            if (off == lxm.size())
              break;
            // OTHERWISE
            // we are still within the same identifier token and hoping for
            // the next constant
            //
            // 2048x1024x16
            //          ^^^
            //   => next iteration of the innermost loop
            //
            // 2048x1024x(16)
            //          ^
            //   => next iteration of this while will break out
            //
          }
        } // while identifier
      } // while looking at successive identifiers
    } // if
  }

  // Three full forms
  // Full form:                   #1`path/foo.cl`kernel<128,16>(...)
  // Partially applied program:   BAR`baz<1024,128>(...)
  // Paritally applied kernel:    FOO<1024,128>(...)
  //
  // void parseDispatchStatement####(parser &p, script &s, dispatch_spec &ds)
  void parseDispatchStatementDimensions(dispatch_spec &ds) {
    // #1`path/foo.cl`kernel<1024x1024>(...)
    //                      ^^^^^^^^^^^
    // #1`path/foo.cl`kernel<1024x1024,16x16>(...)
    //                      ^^^^^^^^^^^^^^^^^
    consume(LANGLE);

    if (lookingAtIdentEq("nullptr") || lookingAtIdentEq("NULL"))
      fatal(tokenString(), " not allowed for global dimensions");
    loc at = nextLoc();
    parseDimensionExpressions(ds.global_size);
    if (consumeIf(COMMA)) {
      if (!consumeIfIdentEq("nullptr") && !consumeIfIdentEq("NULL"))
        parseDimensionExpressions(ds.local_size);
    }
    if (!ds.local_size.empty() &&
      ds.global_size.size() != ds.local_size.size())
    {
      at.extend_to(nextLoc());
      fatalAt(at, "global and local sizes have different dimensions");
    }

    consume(RANGLE);
  }

  // #1`path/foo.cl`kernel<1024x1024>(0:rw,1:r,33) where X = ..., Y = ...
  //                                 ^^^^^^^^^^^^^
  void parseDispatchStatementArguments(dispatch_spec &ds) {
    consume(LPAREN);
    while (!lookingAt(RPAREN)) {
      init_spec *is = parseInit();
      if (is->skind == init_spec::IS_SYM) {
        // make a reference argument
        ds.arguments.emplace_back(
          is->defined_at,
          ((const init_spec_symbol *)is)->identifier,
          nullptr);
        delete is; // delete the old object
      } else {
        // immediate value
        ds.arguments.push_back(is);
      }
      if (!consumeIf(COMMA))
        break;
    }
    consume(RPAREN);
    ds.defined_at.extend_to(nextLoc());
  }

  // #1`path/foo.cl`kernel<1024x1024>(0:rw,1:r,33) where X = ..., Y = ...
  //                                               ^^^^^^^^^^^^^^^^^^^^^^
  void parseDispatchStatementWhereClause(
    dispatch_spec &ds,
    let_spec *enclosing_let)
  {
    auto hasParam =
      [&](std::string nm) {
      if (enclosing_let)
        for (const std::string &arg : enclosing_let->params)
          if (arg == nm)
            return true;
      return false;
    };

    // resolve references after the where clause
    if (consumeIfIdentEq("where")) {
      while (lookingAt(IDENT)) {
        auto loc = nextLoc();
        auto name = consumeIdent("variable name");

        if (hasParam(name)) {
          fatalAt(loc, "where binding shadows let parameter");
        }
        auto itr = s.let_bindings.find(name);
        if (itr != s.let_bindings.end()) {
          warningAt(loc,
            "where binding shadows let binding (from line ",
            itr->second->defined_at.line, ")");
        }
        for (auto w : ds.where_bindings)
          if (std::get<0>(w) == name)
            fatalAt(loc, "repeated where binding name");
        consume(EQ);
        init_spec *i = parseInit();
        i->defined_at.extend_to(nextLoc());
        ds.where_bindings.emplace_back(name, i);
        bool where_used_at_least_once = false;
        for (size_t ai = 0; ai < ds.arguments.size(); ai++) {
          if (ds.arguments[ai].value == nullptr &&
            ds.arguments[ai].identifier == name)
          {
            ds.arguments[ai].value = i;
            where_used_at_least_once = true;
          }
        }
        if (!where_used_at_least_once) {
          warningAt(loc, "where binding never used");
        }
        if (lookingAtSeq(COMMA, IDENT)) {
          skip();
        } else {
          break;
        }
      }
    }

    // fail if anything is not defined
    for (size_t ai = 0; ai < ds.arguments.size(); ai++) {
      if (ds.arguments[ai].value == nullptr) {
        const auto &id = ds.arguments[ai].identifier;
        if (hasParam(id)) {
          // parameter passed to this let
          // e.g. let F(X) = ....<...>(...,X,...);
          auto &prs = enclosing_let->param_uses[id];
          prs.push_back(&ds.arguments[ai]);
        } else {
          // capture from the let binding above this statement
          auto itr = s.let_bindings.find(id);
          if (itr == s.let_bindings.end()) {
            fatalAt(ds.arguments[ai].defined_at, "undefined symbol");
          }
          let_spec *ls = itr->second;
          if (ls->value->skind != spec::INIT_SPEC) {
            fatalAt(ds.arguments[ai].defined_at,
              "symbol does not refer to a kernel argument (see line ",
              ls->defined_at.line,
              ")");
          }
          // otherwise make the replacement
          ds.arguments[ai].value = (init_spec *)ls->value; // replace with value
        }
      }
    }
  }

  // #1`path/foo.cl
  // #1:a`path/foo.cl
  program_spec *parseDispatchStatementDeviceProgramPart() {
    device_spec dev(nextLoc());
    if (consumeIf(HASH)) {
      if (lookingAt(STRLIT) || lookingAt(IDENT)) {
        if (lookingAt(STRLIT)) {
          dev.setSource(tokenStringLiteral());
        } else {
          dev.setSource(tokenString());
        }
        skip();
      } else if (lookingAtInt()) {
        dev.setSource(consumeIntegral<int32_t>("device index (integer)"));
      } else {
        fatal("invalid device specification");
      }
      if (consumeIf(COLON)) {
        dev.instance = consumeIdent("queue identifier");
      }
      dev.defined_at.extend_to(nextLoc());
      if (!lookingAt(BACKTICK))
        fatal("expected ` (dispatch program separator) or "
          "# (queue identifier)");
      consume(BACKTICK);
    } else {
      dev.skind = device_spec::BY_DEFAULT;
    }

    // #1`path/foo.cl`kernel<1024x1024,16x16>(...)
    //    ^^^^^^^^^^^
    // #GTX`"foo/spaces in path/prog.cl"`kernel<...>(...)
    //      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    program_spec *ps = new program_spec(nextLoc());
    ps->device = dev;
    if (lookingAt(STRLIT)) {
      ps->path = tokenStringLiteral(); skip();
    } else {
      ps->path = consumeToChar("[`");
    }

    // #1`path/foo.cl[-DTYPE=int]`kernel<1024x1024,16x16>(...)
    //               ^^^^^^^^^^^^
    if (consumeIf(LBRACK)) {
      ps->build_options = consumeToChar("]");
      consume(RBRACK);
    }
    ps->defined_at.extend_to(nextLoc());

    return ps;
  }

  kernel_spec *parseDispatchStatementKernelPart(program_spec *ps) {
    // #1`path/foo.cl`kernel<1024x1024,16x16>(...)
    //                ^^^^^^
    kernel_spec *ks = new kernel_spec(ps);
    if (lookingAt(IDENT)) {
      ks->name = tokenString(); skip();
    }
    ks->defined_at.extend_to(nextLoc());
    ks->program = ps;
    return ks;
  }


  // allows for default devices:
  //   EASY CSE:  #1`....
  //              ^ yes!
  //   HARD CASE: long/path with/spaces/foo.cl[long args]`kernel<...>()
  //                                          ^ YES!
  //              long/path with/spaces/foo.cl`kernel<...>()
  //                                          ^ YES!
  bool lookingAtImmediateDispatchStatement() const {
    if (lookingAt(HASH))
      return true;
    if (lookingAtIdentEq("let"))
      return false; // let foo = ...;
    if (lookingAtSeq(IDENT, LPAREN))
      return false; // e.g. seq(...); print(...)
    int i = 1;
    while (i < (int)tokensLeft()) {
      if (lookingAt(BACKTICK, i) || // correct dispatch
        lookingAt(LANGLE, i) || // malformed dispatch   foo.cl`bar<1024>(...)
                                 //                            ^
        lookingAt(LBRACK, i)) // malformed dispatchd    foo.cl[...]`bar(...)
                               //                              ^
      {
        return true;
      } else if (lookingAt(NEWLINE, i) || // malformed statement
        lookingAt(SEMI, i) || // malformed statement    .....; ....
        lookingAt(EQ, i)) // malformed let possibly     foo=BAR
      {
        break;
      }
      i++;
    }
    return false;
  }

  dispatch_spec *parseDispatchStatement()
  {
    auto loc = nextLoc();
    dispatch_spec *ds = new dispatch_spec(loc);
    if (lookingAtImmediateDispatchStatement()) {
      if (lookingAtSeq(IDENT, RANGLE)) {
        // named kernel invocation
        // KERNEL<...>(...)
        ds->kernel =
          dereferenceLet<kernel_spec>(spec::KERNEL_SPEC, "kernel");
      } else if (lookingAtSeq(IDENT, BACKTICK, IDENT, LANGLE)) {
        // PROG`kernel<...>(...)
        // 000012222223...
        refable<program_spec> ps =
          dereferenceLet<program_spec>(spec::PROGRAM_SPEC, "program");
        consume(BACKTICK);
        std::string kernel_name = consumeIdent("kernel name");
        ds->kernel = new kernel_spec(ps);
      } else {
        // FULLY immediate dispatch
        //
        // #1`path/foo.cl[-cl-opt]`kernel<1024x1024,16x16>(...)
        // ^^^^^^^^^^^^^^^^^^^^^^^
        program_spec *ps = parseDispatchStatementDeviceProgramPart();
        // #1`path/foo.cl[-cl-opt]`kernel<1024x1024,16x16>(...)
        //                        ^
        consume(BACKTICK);
        // #1`path/foo.cl`kernel<1024x1024,16x16>(...)
        //                ^^^^^^
        ds->kernel = parseDispatchStatementKernelPart(ps);
      }
    } else {
      fatal("expected statement");
    }

    // #1`path/foo.cl`kernel<1024x1024,16x16>(...)
    //                      ^^^^^^^^^^^^^^^^^
    parseDispatchStatementDimensions(*ds);

    // #1`path/foo.cl`kernel<1024x1024>(0:rw,1:r,33) where ...
    //                                 ^^^^^^^^^^^^^^^^^^^^^^^
    parseDispatchStatementArguments(*ds);
    // #1`path/foo.cl`kernel<1024x1024>(0:rw,1:r,33) where X = ..., Y = ...
    //                                               ^^^^^^^^^^^^^^^^^^^^^^
    parseDispatchStatementWhereClause(*ds, nullptr);
    ds->defined_at.extend_to(nextLoc());

    return ds;
  }

  init_spec_symbol *parseSymbol(parser &p)
  {
    auto loc = nextLoc();
    if (lookingAtIdent()) {
      auto ident = tokenString();
      return new init_spec_symbol(loc, ident);
    } else {
      fatal("expected identifier");
      return nullptr;
    }
  }

  // Given an arugment; we resolve symbol the symbol to a let target (or fail)
  refable<init_spec> parseInitResolved()
  {
    init_spec *is = parseInit();
    if (is->skind == init_spec::IS_SYM) {
      // make a reference argument
      auto itr = s.let_bindings.find(((const init_spec_symbol *)is)->identifier);
      if (itr == s.let_bindings.end()) {
        fatalAt(is->defined_at, "unbound identifier");
      } else if (itr->second->value->skind != spec::INIT_SPEC ||
        ((init_spec *)itr->second->value)->skind != init_spec::IS_MEM)
      {
        fatalAt(is->defined_at, "identifier does not reference a memory object");
      }
      refable<init_spec> ris(
        is->defined_at,
        ((const init_spec_symbol *)is)->identifier,
        (init_spec_mem *)itr->second->value);
      delete is; // delete the old object
      return ris;
    } else {
      // immediate value
      return refable<init_spec>(is);
    }
  }

  static bool isFloating(const type &elem_type) {
    if (elem_type.is<type_vector>()) {
      const type_vector &s = elem_type.as<type_vector>();
      return isFloating(s.element_type);
    }
    return elem_type.is<type_num>() &&
      elem_type.as<type_num>().skind == type_num::FLOATING;
  }

  // EXAMPLES
  // barrier
  // diff(X,Y) | diff<float>(X,Y) | diff<float,0.001> | diff<double2,0.0001>
  // print(X) | print<float>(X)
  // save(sym,X)
  bool parseBuiltIn()
  {
    auto loc = nextLoc();
    if (lookingAtIdentEq("barrier")) {
      s.statement_list.statements.push_back(new barrier_spec(loc));
      skip();
      if (consumeIf(LPAREN)) // optional ()
        consume(RPAREN);
      s.statement_list.statements.back()->defined_at.extend_to(nextLoc());
      return true;
    } else if (lookingAtIdentEq("diff")) {
      skip();
      const type *elem_type = nullptr;
      std::optional<double> max_diff;
      if (consumeIf(LANGLE)) {
        auto at = nextLoc();
        // HACK: need to know the pointer size; assume 64b for now
        // technically we should probably just save the string and deal with
        // it during compilation once devices are matched etc...
        const size_t HACK = 8;
        elem_type = lookupBuiltinType(consumeIdent("type name"), HACK);
        if (consumeIf(COMMA)) {
          auto diff_loc = nextLoc();
          max_diff = consumeFloat("floating point (max diff)");
          if (!isFloating(*elem_type)) {
            fatalAt(diff_loc, "diff max error requires floating point type");
          }
        }
        consume(RANGLE);
      }
      consume(LPAREN);
      auto ref = parseInitResolved();
      consume(COMMA);
      if (!lookingAt(IDENT))
        fatal("expected reference to memory object");
      refable<init_spec_mem> r_sut = dereferenceLetMem();
      consume(RPAREN);
      loc.extend_to(nextLoc());
      diff_spec *ds = new diff_spec(loc, ref, r_sut, elem_type);
      s.statement_list.statements.push_back(ds);
      if (max_diff.has_value()) {
        ds->max_diff = max_diff.value();
      }
      return true;
    } else if (lookingAtIdentEq("print")) {
      // print(X)
      // print<TYPE>(X)
      // print<INT>(X)
      // print<TYPE,INT>(X)
      skip();
      const type *elem_type = nullptr;
      int elems_per_row = 0;
      if (consumeIf(LANGLE)) {
        if (lookingAtInt()) {
          elems_per_row = consumeIntegral<int>("elements per column");
        } else {
          // HACK: we can't know the bits per ptr until we unify the
          // arguments with the owning context.
          const size_t HACK = 8;
          elem_type = lookupBuiltinType(consumeIdent("type name"), HACK);
          if (consumeIf(COMMA)) {
            elems_per_row = consumeIntegral<int>("elements per column");
          }
        }
        consume(RANGLE);
      }
      consume(LPAREN);
      if (!lookingAt(IDENT))
        fatal("expected reference to memory object");
      refable<init_spec_mem> r_surf = dereferenceLetMem();
      consume(RPAREN);
      loc.extend_to(nextLoc());
      s.statement_list.statements.push_back(
        new print_spec(loc, r_surf, elem_type, elems_per_row));
      return true;
    } else if (lookingAtIdentEq("save")) {
      skip();
      consume(LPAREN);
      if (!lookingAt(STRLIT))
        fatal("expected file name (string literal)");
      std::string file = tokenStringLiteral();
      skip();
      consume(COMMA);
      refable<init_spec_mem> r_surf = dereferenceLetMem();
      consume(RPAREN);
      loc.extend_to(nextLoc());
      s.statement_list.statements.push_back(new save_spec(loc, file, r_surf));
      return true;
    } else {
      return false;
    }
  }

  // X=...
  void parseLetBinding()
  {
    if (!lookingAt(IDENT)) {
      fatal("expected identifier");
    }
    auto at = nextLoc();
    auto name = tokenString(); // X
    if (s.let_bindings.find(name) != s.let_bindings.end()) {
      fatal(name, ": redefinition of let binding");
    }
    skip();      // X
    let_spec *ls = new let_spec(at, name);
    if (consumeIf(LPAREN)) {
      // let F(X,Y) = #1`foo.cl`bar<...>(X,Y)
      if (!lookingAt(RPAREN)) {
        do {
          ls->params.push_back(consumeIdent());
        } while (consumeIf(COMMA));
        consume(RPAREN);
        fatal("let arguments not supported yet");
      }
    }
    consume(EQ); // =

    spec *value = nullptr;
    loc value_loc = nextLoc();
    bool is_init_expr_start =
      // SPECIFY: does this handle built-in function (e.g. atan2)
      lookingAtFloat() ||
      lookingAtInt() ||
      lookingAt(LPAREN) ||
      lookingAtIdentEq("seq") ||
      lookingAtIdentEq("random") ||
      lookingAtIdentEq("file") ||
      lookingAtIdentEq("undef") ||
      lookingAtIdentEq("image");
    if (!is_init_expr_start && lookingAtSeq(IDENT, LANGLE)) {
      // let D = K<1024>(....) where ...
      dispatch_spec *ds = new dispatch_spec(value_loc);
      ds->kernel = dereferenceLet<kernel_spec>(spec::KERNEL_SPEC, "kernel");
      parseDispatchStatementDimensions(*ds);
      parseDispatchStatementArguments(*ds);
      parseDispatchStatementWhereClause(*ds, ls);
      ds->defined_at.extend_to(nextLoc());
      value = ds;
    } else if (
      !is_init_expr_start &&
      lookingAtSeq(IDENT, BACKTICK, IDENT, LANGLE))
    {
      // let D = P`kernel<...>(...) where ...
      dispatch_spec *ds = new dispatch_spec(value_loc);
      refable<program_spec> rps =
        dereferenceLet<program_spec>(spec::PROGRAM_SPEC, "programs");
      consume(BACKTICK);
      ds->kernel = new kernel_spec(rps.value);
      parseDispatchStatementDimensions(*ds);
      parseDispatchStatementArguments(*ds);
      parseDispatchStatementWhereClause(*ds, ls);
      ds->defined_at.extend_to(nextLoc());
      value = ds;
    } else if (!is_init_expr_start && lookingAtImmediateDispatchStatement()) {
      // let P = #1`foo/bar.cl
      // let K = foo/bar.cl`kernel
      // let D = foo/bar.cl`kernel<...>(...) where ...
      program_spec *ps = parseDispatchStatementDeviceProgramPart();
      if (consumeIf(BACKTICK)) {
        // includes the kernel
        kernel_spec *ks = parseDispatchStatementKernelPart(ps);
        ks->program = ps;
        if (lookingAt(LANGLE)) {
          // let D = ...<...>(...) where ...
          dispatch_spec *ds = new dispatch_spec(value_loc);
          ds->kernel = ks;
          parseDispatchStatementDimensions(*ds);
          parseDispatchStatementArguments(*ds);
          parseDispatchStatementWhereClause(*ds, ls);
          ds->defined_at.extend_to(nextLoc());
          value = ds;
        } else {
          // let P = #1`prog.cl`kernel
          value = ks;
        }
      } else {
        // let P = #1`prog.cl`kernel
        value = ps;
      }
    } else {
      // a regular initializer
      // let M = 0:w
      value = parseInit();
    }
    ls->value = value;
    s.let_bindings[name] = ls;
    s.statement_list.statements.push_back(ls);
    s.statement_list.statements.back()->defined_at.extend_to(nextLoc());
  }

  // let X=...
  // let X=..., Y=...
  void parseLetStatement()
  {
    auto let_loc = nextLoc();
    skip(); // let
    parseLetBinding();
    while (consumeIf(COMMA))
      parseLetBinding();
  }

  void parseStatementLine()
  {
    if (lookingAtIdentEq("let") && lookingAt(IDENT, 1)) {
      // let X = ...
      // let F(X,Y) = ...
      parseLetStatement();
    } else if (parseBuiltIn()) {
      // barrier
      // save('foo.bin',A);
      ;
    } else {
      // #1`foo/bar.cl
      s.statement_list.statements.emplace_back(parseDispatchStatement());
    }
  }

  ///////////////////////////////////////////////////////////
  void parseScript()
  {
    while (!endOfFile()) {
      while (consumeIf(NEWLINE))
        ;
      if (endOfFile())
        break;
      //
      parseStatementLine(); // S ((<NEWLINE> | ';') S)*
      //
      if (endOfFile())
        break;
      if (!consumeIf(SEMI) && !consumeIf(NEWLINE)) { // ';' S
        fatal("syntax error in statement");
      }
    }
  }
};

std::string cls::expand_input_variables(
  const opts &os,
  const std::string &inp,
  diagnostics &ds)
{
  std::stringstream ss;
  uint32_t off = 0;
  uint32_t ln = 1, col = 1;

  auto eos = [&] () {
    return off == inp.size();
  };
  auto lookingAt = [&] (char c) {
    return !eos() && inp[off] == c;
  };
  auto skip = [&](uint32_t n, bool copy = false) {
    for (uint32_t i = 0; i < n && off < inp.size(); i++) {
      if (inp[off] == '\n') {
        ln++; col = 1;
      } else {
        col++;
      }
      if (copy)
        ss << inp[off];
      off++;
    }
  };
  auto consumeIf = [&] (char c) {
    if (lookingAt(c)) {
      skip(1);
      return true;
    }
    return false;
  };

  while (!eos()) {
    if (lookingAt('$')) {
      loc inp_var_start(ln, col, off, 0);
      skip(1);
      size_t key_start = off;
      bool inBraces = consumeIf('{');
      if (inBraces)
        key_start++;
      if (!isalpha(inp[off]) && inp[off] != '_') {
        ds.fatalAt(loc(ln, col, off, 1),
          "invalid program input variable: "
          "should start with an identifier starting character");
      }
      while (!eos() && (isalnum(inp[off]) || inp[off] == '_')) {
        skip(1);
      }
      std::string inp_var = inp.substr(key_start, off - key_start);
      if (inBraces) {
        if (!lookingAt('}'))
          ds.fatalAt(loc(ln, col, off, 1), "expected '}'");
        skip(1);
      }
      inp_var_start.extent = off - inp_var_start.offset;
      bool matchedKey = false;
      for (const auto &iv : os.input_vars) {
        if (iv.first == inp_var) {
          ss << iv.second;
          matchedKey = true;
          break;
        }
      }
      if (!matchedKey) {
        ds.fatalAt(inp_var_start,
          "undefined input variable (-D", inp_var, "=..)");
      }
    } else {
      skip(1, true);
    }
  }
  return ss.str();
}

void cls::parse_script(
  const opts &os,
  const std::string &input,
  const std::string &filename,
  script &s,
  diagnostics &ds)
{
  cls_parser cp(ds, input, s);
  cp.parseScript();
}