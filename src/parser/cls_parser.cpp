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
    SVAR("SmplrExpr") " | "
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
    " | " SVAR("SaveImgSt")
    "\n"
    "\n"
    "  " SVAR("DiffSt") "\n"
    "    = " SLIT("diff")
    SLIT("(") SVAR("IDENT") SLIT(", ") SVAR("IDENT") SLIT(")") "\n"
    "    | " SLIT("diff") SLIT("<") SVAR("TYPE") SLIT(">")
    SLIT("(") SVAR("IDENT") SLIT(", ") SVAR("IDENT") SLIT(")") "\n"
    "    | " SLIT("diff") SLIT("<") SVAR("TYPE") SLIT(", ") SVAR("FLOAT") SLIT(">")
    SLIT("(") SVAR("IDENT") SLIT(", ") SVAR("IDENT") SLIT(")") "\n"
    "    | " SLIT("diff")
    SLIT("(") SVAR("IDENT") SLIT(", ") SVAR("Expr") SLIT(")") "\n"
    "    | " SLIT("diff") SLIT("<") SVAR("TYPE") SLIT(">")
    SLIT("(") SVAR("IDENT") SLIT(", ") SVAR("Expr") SLIT(")") "\n"
    "    | " SLIT("diff") SLIT("<") SVAR("TYPE") SLIT(", ") SVAR("FLOAT") SLIT(">")
    SLIT("(") SVAR("IDENT") SLIT(", ") SVAR("Expr") SLIT(")") "\n"
    "    diffs two memory object or one against a replicated scalar value;\n"
    "    if a type argument is present, CLS will reinterpret elements as that type\n"
    "    NOTE: the program will exit with failure upon diff failure\n"
    "         use -Xno-exit-on-diff-fail to override this\n"
    "    e.g. " SLIT("let A=1:rw; #0`file.cl`kernel<1024>(A); diff<int>(A,0)") "\n"
    "         diffs all elements of buffer " SLIT("A") " with zeros\n"
    "    e.g. " SLIT("let A=1:rw; let B=2:rw; #0`file.cl`kernel<1024>(A,B); diff(A,B)") "\n"
    "         diffs buffers " SLIT("A") " and " SLIT("B") " element by element\n"
    "\n"
    "  " SVAR("PrintSt") "\n"
    "    = " SLIT("print") SLIT("(") SVAR("IDENT") SLIT(")") "\n"
    "    | " SLIT("print") SLIT("<") SVAR("INT") SLIT(">") SLIT("(") SVAR("IDENT") SLIT(")") "\n"
    "    | " SLIT("print") SLIT("<") SVAR("TYPE") SLIT(">") SLIT("(") SVAR("IDENT") SLIT(")") "\n"
    "    | " SLIT("print") SLIT("<") SVAR("TYPE") SLIT(",") SVAR("INT") SLIT(">")
            SLIT("(") SVAR("IDENT") SLIT(")") "\n"
    "    prints a memory object; an optional type interprets the surface's elements;\n"
    "    an optional integer argument overrides columns per line in output\n"
    "    e.g. " SLIT("let A = 1:w; #0`file.cl`kernel<1024>(A); print<float4>(A)") "\n"
    "       prints buffer A as float4's\n"
    "\n"
    "  " SVAR("SaveSt") " = " SLIT("save(") SVAR("STRLIT") SLIT(",") SVAR("IDENT") SLIT(")") "\n"
    "   saves a surface referenced by an identifier;\n"
    "   if the surface is an image object, then this saves it to an image file;\n"
    "   the format is based on the file extension\n"
    "\n"
    "  " SVAR("SaveImgSt") "\n"
    "    = " SLIT("save_image<") SVAR("PxFmt") SLIT(">(") SVAR("STRLIT") SLIT(",")
              SVAR("IDENT") SLIT(")") "\n"
    "    | " SLIT("save_image<") SVAR("PxFmt") SLIT(",") SVAR("INT x INT") SLIT(">(")
              SVAR("STRLIT") SLIT(",") SVAR("IDENT") SLIT(")") "\n"
    "   saves a buffer surface referenced by an identifier converting it to an image file;\n"
    "   the file extension implies the image format (e.g. ppm, png, or bmp);\n"
    "   the pixel format " SVAR("PxFmt") " can be " SLIT("uchar4") " or " SLIT("float4") ", and\n"
    "   the channel order is always RGBA;\n"
    "   the optional image size is inferred by the buffer's use in 2d kernels, but the\n"
    "     optional dimension argument (INT x INT) overrides it\n"
    "\n"
    "************* LET STATEMENTS *************\n"
    SVAR("Let")  " = " SLIT("let") SVAR("LetBinding") " (" SLIT(",") SVAR("LetBinding") ")*" "\n"
    "  a comma-separated list of bindings;\n"
    "  use this to share buffers in multiple dispatch statements\n"
    "  " SVAR("Binding") " =\n"
    "       " SVAR("IDENT") SLIT(" = ") SVAR("MemInitExpr") "\n"
    "     | " SVAR("IDENT") SLIT(" = ") SVAR("Dispatch") "\n"
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
    "    " " = " SVAR("ConstExpr") " | " SVAR("SeqExpr") " | "
                SVAR("FiniteSeqExpr") " | " SVAR("CycExpr") " | " SVAR("RandExpr")
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
    SVAR("FiniteSeqExpr")   " = "
    SLIT("fseq(") SLIT(")")
    " | " SLIT("fseq(") SVAR("Expr") SLIT(")")
    " | " SLIT("fseq(") SVAR("Expr") "(" SLIT(",") SVAR("Expr") ")*" SLIT(")") "\n"
    "  initializes a memory object to a finite sequence of numbers;\n"
    "  the tail clamps to the final value\n"
    "  e.g. " SLIT("fseq(0,1,2)") " generates 0, 1, 2, 2, 2, ...\n"
    "\n"
    SVAR("CycExpr")   " = "
    SLIT("cyc(") SVAR("Expr") "(" SLIT(",") SVAR("Expr") ")*" SLIT(")") "\n"
    "  initializes a memory object to an cycle sequence of numbers;\n"
    "  an optional base and delta are permitted\n"
    "  e.g. " SLIT("cyc(0,1,2)") " generates 0, 1, 2, 0, 1, 2, ...\n"
    "  e.g. " SLIT("cyc(1)") " generates 1, 1, 1, ...\n"
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
    SVAR("SmplrExpr")
    " = " SLIT("sampler") SLIT("(") SVAR("CL_BOOL")
      SLIT(",") SVAR("cl_address_mode")
      SLIT(",") SVAR("cl_filter_mode")
      SLIT(")") "\n"
    "    calls clCreateSampler with given arguments for a host-side sampler\n"
    "    e.g. " SLIT("kernel(...,sampler(CL_FALSE,CL_ADDRMODE_CLAMP,CL_FILTER_LINEAR),...)") "\n"
    "\n"
  ;
}


struct cls_parser : parser {
  script &s;

  cls_parser(
    diagnostics &ds, const std::string &inp, script &_s)
    : parser(ds, inp), s(_s) { }

  // a rough solution to enable use to read tokens including spaces
  // e.g. `path/has spaces/baz.cl[-DTYPE=int -cl-some-option]`kernel
  //       ^^^^^^^^^^^^^^^^^^^^^^
  //                              ^^^^^^^^^^^^^^^^^^^^^^^^^^
  std::string consume_to_char(const char *set)
  {
    const std::string &s = input();
    size_t start = next_loc().offset;
    size_t len = 0;
    while (start + len < s.length()) {
      if (strchr(set, s[start + len])) {
        break;
      }
      len++;
    }
    while (next_loc().offset != start + len)
      skip();
    return s.substr(start, len);
  }

  // 1024[1200]x768[800]x4
  //
  // similar to dimensions, but includes pitch
  void parse_image_dimension_expressions(
    std::vector<init_spec_atom *> &dims,
    std::vector<init_spec_atom *> &pitches)
  {
    dims.push_back(parse_init_atom());
    if (consume_if(LBRACK)) {
      pitches.push_back(parse_init_atom());
      consume(RBRACK);
    } else {
      pitches.push_back(nullptr);
    }
    if (dims.size() == 3)
      return; // at the end

    if (consume_if_ident_eq("x")) {
      // EXPR IDENT("x") ...
      parse_image_dimension_expressions(dims, pitches);
    } else {
      while (looking_at_ident()) {
        if (dims.size() == 3)
          fatal("syntax error in image dimension constant");
        // 1024x768x3 or 1024x768x(...) or 1024x768 x(...)
        //     ^^^^^^        ^^^^^             ^^^^
        // need to split x768x3 into: "x 768 x 3"
        loc at = next_loc();
        std::string lxm = token_string();
        skip();
        if (lxm[0] != 'x') {
          fatal_at(at, "syntax error in image dimensions");
        }
        size_t off = 0;
        while (off < lxm.size()) {
          if (lxm[off] != 'x') {
            at.column += (uint32_t)off;
            at.offset += (uint32_t)off;
            fatal_at(at, "syntax error in image dimensions");
          }
          off++;
          size_t end = off;
          while (end < lxm.size() && isdigit(lxm[end]))
            end++;
          if (end == off) {
            // it must be a new dimension
            // 2048x1024x(...)
            //          ^
            parse_image_dimension_expressions(dims, pitches);
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
              fatal_at(at, "syntax error in image dimensions");
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
                if (consume_if(LBRACK)) {
                  pitches.push_back(parse_init_atom());
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
              fatal_at(at, "syntax error in image dimensions (too many)");
            }
          }
        } // while identifier
      } // while looking at successive identifiers
    } // if
  }

  init_spec_atom *parse_init_atom_prim()
  {
    auto at = next_loc();
    if (looking_at(STRLIT)) {
      fatal("bare strings not allowed for files anymore "
        "(use bin('...') and txt('...'))");
      return nullptr;
    } else if (looking_at_float()) {
      return new init_spec_float(at, consume_float());
    } else if (looking_at_int()) {
      // we parse imm's as unsigned to enable u64 values, the cast will
      // preserve the bit patterns
      int64_t imm = (int64_t)consume_integral<uint64_t>();
      return new init_spec_int(at, imm);
    } else if (looking_at_ident()) {
      // e.g. "X" or "g.x" or "pow(...)"
      auto id = token_string();
      skip();
      if (looking_at(DOT)) {
        // e.g. "g.x"
        while (consume_if(DOT)) {
          id += '.';
          if (!looking_at(IDENT))
            fatal("syntax error in initializer expression field access");
          id += token_string();
          skip();
        }
        for (int biv = init_spec_builtin::BIV_FIRST;
          biv <= init_spec_builtin::BIV_LAST;
          biv++)
        {
          if (id == init_spec_builtin::syntax_for((init_spec_builtin::biv_kind)biv)) {
            at.extend_to(next_loc());
            return new init_spec_builtin(at, (init_spec_builtin::biv_kind)biv);
          }
        }
        at.extend_to(next_loc());
        return new init_spec_symbol(at, id);
      } else if (looking_at(LPAREN) || looking_at(LANGLE) ||
        id == "sizeof" || id == "random" ||
        id == "seq" || id == "aseq" || id == "fseq" || id == "cyc" ||
        id == "file" || id == "image" || id == "sampler")
      {
        // foo<...  (e.g. random<12007>(...))
        // or
        // foo(...
        //
        // then match by template arguments
        if (id == "sizeof") {
          return parse_init_atom_prim_sizeof(at);
        } else if (id == "random") {
          return parse_init_atom_prim_random(at);
        } else if (id == "file") {
          return parse_init_atom_prim_file(at);
        } else if (id == "image") {
          return parse_init_atom_prim_image(at);
        } else if (id == "sampler") {
          return parse_init_sampler(at);
        } else {
          ///////////////////////////////////////////////////
          // generic function syntax (could be seq still)
          std::vector<init_spec_atom *> args;
          consume(LPAREN);
          while (!looking_at(RPAREN)) {
            args.push_back(parse_init_atom());
            if (!consume_if(COMMA))
              break;
          }
          consume(RPAREN);

          ///////////////////////////////////////////////////
          // special functions (pseudo functions)
          if (id == "seq" || id == "aseq") {
            init_spec_seq *iss = nullptr;
            switch (args.size()) {
            case 0: iss = new init_spec_seq(at, nullptr, nullptr); break;
            case 1: iss = new init_spec_seq(at, args[0], nullptr); break;
            case 2: iss = new init_spec_seq(at, args[0], args[1]); break;
            default: fatal_at(at, "wrong number of args to seq");
            }
            iss->defined_at.extend_to(next_loc());
            return iss;
          } else if (id == "fseq") {
            init_spec_fseq *isf = new init_spec_fseq(at);
            if (args.empty()) {
              fatal_at(at, "cyc args must be non-empty");
            }
            for (const auto *arg : args) {
              isf->args.push_back(arg);
            }
            isf->defined_at.extend_to(next_loc());
            return isf;
          } else if (id == "cyc") {
            init_spec_cyc *isc = new init_spec_cyc(at);
            if (args.empty()) {
              fatal_at(at, "cyc args must be non-empty");
            }
            for (const auto *arg : args) {
              isc->args.push_back(arg);
            }
            isc->defined_at.extend_to(next_loc());
            return isc;
          }

          ///////////////////////////////////////////////////
          // regular arithmetic functions
          if (args.size() == 1) {
            if (id == "fabs")
              fatal_at(at, "use \"abs\" for the absolute value");
            else if (id == "sqt")
              fatal_at(at, "use \"sqrt\" for the square root");

            const auto *op = init_spec_uex::lookup_op(id.c_str());
            if (!op) {
              if (init_spec_bex::lookup_op(id.c_str())) {
                fatal_at(at, "function requires two arguments");
              } else {
                fatal_at(at, "not a unary function");
              }
            }
            return new init_spec_uex(at, *op, args[0]);
          } else if (args.size() == 2) {
            const auto *op = init_spec_bex::lookup_op(id.c_str());
            if (!op) {
              fatal_at(at, "not a binary function");
            }
            auto *isbe = new init_spec_bex(*op, args[0], args[1]);
            isbe->defined_at = at; // reset start loc to function name
            isbe->defined_at.extend_to(next_loc());
            return isbe;
          } else {
            fatal_at(at, "undefined function");
            return nullptr;
          }
        } // end else not random
      } else if (id == "undef") {
        return new init_spec_undef(at);
      } else {
        return parse_init_atom_prim_symbol(at, id);
      }
    } else if (consume_if(LBRACE)) {
      return parse_init_atom_prim_struct(at);
    } else if (consume_if(LPAREN)) {
      // grouping expression or vector value
      if (looking_at_seq(IDENT, RPAREN))
      {
        const type *et = lookup_builtin_type(token_string(), 8);
        if (et == nullptr || !et->is<type_vector>()) {
          fatal("not a vector type");
          return nullptr;
        } else {
          return parse_init_atom_prim_vector(at, et);
        }
      } else {
        // grouping expression (E)
        init_spec_atom *e = parse_init_atom();
        consume(RPAREN);
        return e;
      }
    } else if (looking_at(NEWLINE)){
      fatal("unexpecetd newline initializer expression");
      return nullptr;
    } else {
      fatal("syntax error in initializer expression");
      return nullptr;
    }
  }

  init_spec_atom *parse_init_atom_prim_sizeof(loc at)
  {
    bool has_parens = consume_if(LPAREN);
    if (!looking_at_ident())
      fatal("expected type name");
    loc nm_at = next_loc();
    std::string sizeof_arg = token_string();
    skip();
    if (has_parens)
      consume(RPAREN);
    at.extend_to(next_loc());
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
  init_spec_atom *parse_init_atom_prim_file(loc at)
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
    if (consume_if(LANGLE)) {
      if (!looking_at(RANGLE)) {
        auto fmt_loc = next_loc();
        auto flv_str = consume_ident("data format (identifier)");
        if (flv_str == "bin") {
          flv = init_spec_file::BIN;
        } else if (flv_str == "text") {
          flv = init_spec_file::TXT;
          if (consume_if(COMMA)) {
            if (!looking_at(STRLIT)) {
              fatal("expected separator string (literal)");
            }
            sep = token_string_literal();
            skip();
          }
        } else if (flv_str == "text_col") {
          flv = init_spec_file::TXT_COL;
          if (consume_if(COMMA)) {
            col = consume_integral<int>("column index (int)");
            if (consume_if(COMMA)) {
              if (!looking_at(STRLIT)) {
                fatal("expected separator string (literal)");
              }
              sep = token_string_literal();
              skip();
            }
          }
        } else {
          fatal_at(fmt_loc, "unsupported file flavor; should be: bin, text, ...");
        }
      }
      consume(RANGLE);
    }
    skip();
    if (!looking_at(STRLIT)) {
      fatal_at(at, "expected file path (string literal)");
    }
    auto s = token_string_literal();
    skip();
    consume(RPAREN);
    at.extend_to(next_loc());
    return new init_spec_file(at, s, flv, col, sep);
  }
  init_spec_atom *parse_init_atom_prim_random(loc at)
  {
    auto func = new init_spec_rng(at);
    int64_t seed = 0;
    bool has_seed = false;
    if (consume_if(LANGLE)) {
      if (!looking_at(RANGLE)) {
        seed = consume_integral<int64_t>("seed (int)");
        has_seed = true;
      }
      consume(RANGLE);
    }
    if (consume_if(LPAREN)) {
      if (!looking_at(RPAREN)) {
        auto *arg1 = parse_init_atom();
        if (consume_if(COMMA)) {
          auto *arg2 = parse_init_atom();
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
    func->defined_at.extend_to(next_loc());
    return func;
  }
  init_spec_atom *parse_init_atom_prim_image(loc at)
  {
    consume(LANGLE);
    auto ord = init_spec_image::RGB;
    if (consume_if_ident_eq("i") ||
      consume_if_ident_eq("CL_INTENSITY"))
    {
      ord = init_spec_image::I;
    } else if (consume_if_ident_eq("l") ||
      consume_if_ident_eq("CL_LUMINANCE"))
    {
      ord = init_spec_image::L;
    } else if (consume_if_ident_eq("r") ||
      consume_if_ident_eq("CL_R"))
    {
      ord = init_spec_image::R;
    } else if (consume_if_ident_eq("rx") ||
      consume_if_ident_eq("CL_Rx"))
    {
      ord = init_spec_image::Rx;
    } else if (consume_if_ident_eq("rg") ||
      consume_if_ident_eq("CL_RG"))
    {
      ord = init_spec_image::RG;
    } else if (consume_if_ident_eq("rgx") ||
      consume_if_ident_eq("CL_RGx"))
    {
      ord = init_spec_image::RGx;
    } else if (consume_if_ident_eq("rgb") ||
      consume_if_ident_eq("CL_RGB"))
    {
      ord = init_spec_image::RGB;
    } else if (consume_if_ident_eq("rgbx") ||
      consume_if_ident_eq("CL_RGBx"))
    {
      ord = init_spec_image::RGBx;
    } else if (consume_if_ident_eq("rgba") ||
      consume_if_ident_eq("CL_RGBA"))
    {
      ord = init_spec_image::RGBA;
    } else if (consume_if_ident_eq("argb") ||
      consume_if_ident_eq("CL_ARGB"))
    {
      ord = init_spec_image::ARGB;
    } else if (consume_if_ident_eq("bgra") ||
      consume_if_ident_eq("CL_BGRA"))
    {
      ord = init_spec_image::BGRA;

    } else if (consume_if_ident_eq("srgb") ||
      consume_if_ident_eq("CL_sRGB"))
    {
      ord = init_spec_image::sRGB;
    } else if (consume_if_ident_eq("srgbx") ||
      consume_if_ident_eq("CL_sRGBx"))
    {
      ord = init_spec_image::sRGBx;
    } else if (consume_if_ident_eq("srgba") ||
      consume_if_ident_eq("CL_sRGBA"))
    {
      ord = init_spec_image::sRGBA;
    } else if (consume_if_ident_eq("sbgra") ||
      consume_if_ident_eq("CL_sBGRA"))
    {
      ord = init_spec_image::sBGRA;
    } else {
      fatal("unrecognized channel order (try r, rg, rgb, rgba, ...)");
    }
    consume(COMMA);
    auto ty = init_spec_image::U8;
    ///////////////////////////////////////////////////////////////////////
    if (consume_if_ident_eq("un8") ||
      consume_if_ident_eq("CL_UNORM_INT8"))
    {
      ty = init_spec_image::UN8;
    } else if (consume_if_ident_eq("un16") ||
      consume_if_ident_eq("CL_UNORM_INT16"))
    {
      ty = init_spec_image::UN16;
    } else if (consume_if_ident_eq("un24") ||
      consume_if_ident_eq("CL_UNORM_INT24"))
    {
      ty = init_spec_image::UN24;
    } else if (consume_if_ident_eq("un565") ||
      consume_if_ident_eq("CL_UNORM_SHORT_565"))
    {
      ty = init_spec_image::UN565;
    } else if (consume_if_ident_eq("un555") ||
      consume_if_ident_eq("CL_UNORM_SHORT_555"))
    {
      ty = init_spec_image::UN555;
    } else if (consume_if_ident_eq("un101010") ||
      consume_if_ident_eq("CL_UNORM_INT_101010"))
    {
      ty = init_spec_image::UN101010;
    } else if (consume_if_ident_eq("un101010_2") ||
      consume_if_ident_eq("CL_UNORM_INT_101010_2"))
    {
      ty = init_spec_image::UN101010_2;
      ///////////////////////////////////////////////////////////////////////
    } else if (consume_if_ident_eq("sn8") ||
      consume_if_ident_eq("CL_SNORM_INT8"))
    {
      ty = init_spec_image::SN8;
    } else if (consume_if_ident_eq("sn16") ||
      consume_if_ident_eq("CL_SNORM_INT16"))
    {
      ty = init_spec_image::SN16;
      ///////////////////////////////////////////////////////////////////////
    } else if (consume_if_ident_eq("u8") ||
      consume_if_ident_eq("CL_UNSIGNED_INT8"))
    {
      ty = init_spec_image::U8;
    } else if (consume_if_ident_eq("u16") ||
      consume_if_ident_eq("CL_UNSIGNED_INT16"))
    {
      ty = init_spec_image::U16;
    } else if (consume_if_ident_eq("u32") ||
      consume_if_ident_eq("CL_UNSIGNED_INT32"))
    {
      ty = init_spec_image::U32;
      ///////////////////////////////////////////////////////////////////////
    } else if (consume_if_ident_eq("s8") ||
      consume_if_ident_eq("CL_SIGNED_INT8"))
    {
      ty = init_spec_image::S8;
    } else if (consume_if_ident_eq("s16") ||
      consume_if_ident_eq("CL_SIGNED_INT16"))
    {
      ty = init_spec_image::S16;
    } else if (consume_if_ident_eq("s32") ||
      consume_if_ident_eq("CL_SIGNED_INT32"))
    {
      ty = init_spec_image::S32;
      ///////////////////////////////////////////////////////////////////////
    } else if (consume_if_ident_eq("f32") ||
      consume_if_ident_eq("CL_FLOAT"))
    {
      ty = init_spec_image::F32;
    } else if (consume_if_ident_eq("f16") ||
      consume_if_ident_eq("CL_HALF_FLOAT"))
    {
      ty = init_spec_image::F16;
      ///////////////////////////////////////////////////////////////////////
    } else {
      fatal(
        "unrecognized image format (try: u8, u16, ..., f32, f16 ,...)");
    }

    init_spec_atom *width = nullptr, *height = nullptr, *depth = nullptr;
    init_spec_atom *row_pitch = nullptr, *slice_pitch = nullptr;
    if (consume_if(COMMA)) {
      // image dimension:
      // W (RP)
      // W (RP) x H (SP)
      // W (RP) x H (SP) x D
      // the spacing makes this hard
      std::vector<init_spec_atom *> dims;
      std::vector<init_spec_atom *> pitches;
      parse_image_dimension_expressions(dims, pitches);
      //
      width = dims[0];
      height = dims.size() >= 2 ? dims[1] : nullptr;
      depth = dims.size() >= 3 ? dims[2] : nullptr;
      row_pitch = pitches[0];
      slice_pitch = pitches.size() >= 2 ? pitches[1] : nullptr;
    }
    consume(RANGLE);
    std::string file;
    if (consume_if(LPAREN)) {
      if (!looking_at(STRLIT)) {
        fatal_at(at, "expected file path (string literal)");
      }
      file = token_string_literal(); skip();
      consume(RPAREN);
    }
    at.extend_to(next_loc());
    return new init_spec_image(
      at, file, ord, ty, width, row_pitch, height, slice_pitch, depth);
  }
  init_spec_atom *parse_init_atom_prim_struct(loc at)
  {
    // {...}
    auto re = new init_spec_record(at);
    if (!looking_at(RBRACE)) {
      re->children.push_back(parse_init_atom());
      while (consume_if(COMMA))
        re->children.push_back(parse_init_atom());
    }
    consume(RBRACE);
    re->defined_at.extend_to(next_loc());
    return re;
  }
  init_spec_atom *parse_init_atom_prim_vector(loc at, const type *et)
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
    if (looking_at(LPAREN)) {
      consume(LPAREN);
      vi->children.push_back(parse_init_atom());
      while (consume_if(COMMA))
        vi->children.push_back(parse_init_atom());
      consume(RPAREN);
      if (vi->children.size() == 1) {
        // broadcast
        for (int i = 0; i < (int)et->as<type_vector>().length; i++)
          vi->children.push_back(vi->children[i]);
      }
    } else {
      // broadcast: (float4)3.14f
      auto *e = parse_init_atom();
      for (int i = 0; i < (int)et->as<type_vector>().length; i++)
        vi->children.push_back(e);
    }
    return vi;
  }

  init_spec_atom *parse_init_sampler(loc at) {
    consume(LPAREN);
    init_spec_sampler *iss = new init_spec_sampler(at);
    if (consume_if_ident_eq("CL_FALSE") || consume_if_ident_eq("false")) {
      iss->normalized = false;
    } else if (consume_if_ident_eq("CL_TRUE") || consume_if_ident_eq("true")) {
      iss->normalized = true;
    } else {
      fatal("expected CL_FALSE or CL_TRUE for normalized device coordinates");
    }
    consume(COMMA);
    if (consume_if_ident_eq("CL_ADDRESS_NONE")) {
      iss->addr_mode = init_spec_sampler::AM_NONE;
    } else if (consume_if_ident_eq("CL_ADDRESS_CLAMP_TO_EDGE")) {
      iss->addr_mode = init_spec_sampler::AM_CLAMP_EDGE;
    } else if (consume_if_ident_eq("CL_ADDRESS_CLAMP")) {
      iss->addr_mode = init_spec_sampler::AM_CLAMP;
    } else if (consume_if_ident_eq("CL_ADDRESS_REPEAT")) {
      iss->addr_mode = init_spec_sampler::AM_REPEAT;
    } else if (consume_if_ident_eq("CL_ADDRESS_MIRRORED_REPEAT")) {
      iss->addr_mode = init_spec_sampler::AM_MIRRORED_REPEAT;
    } else {
      fatal("expected cl_address_mode (e.g. CL_ADDRESS_CLAMP)");
    }
    consume(COMMA);
    if (consume_if_ident_eq("CL_FILTER_NEAREST")) {
      iss->filter = init_spec_sampler::FM_NEAREST;
    } else if (consume_if_ident_eq("CL_FILTER_LINEAR")) {
      iss->filter = init_spec_sampler::FM_LINEAR;
    } else {
      fatal("expected cl_filter_mode (e.g. CL_FILTER_NEAREST)");
    }
    consume(RPAREN);
    return iss;
  }

  init_spec_atom *parse_init_atom_prim_symbol(loc at, const std::string &id)
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

  init_spec_atom *parse_init_atom_unr()
  {
    if (looking_at(SUB) || looking_at(TILDE)) {
      auto loc = next_loc();
      bool is_sub = looking_at(SUB);
      const auto &op = is_sub ?
        *init_spec_uex::lookup_op("-") :
        *init_spec_uex::lookup_op("~");

      skip();
      //
      // NOTE: we deal with 9223372036854775808 within atom as a second
      // try parse as uint64_t
      //
      // if (is_sub && looking_at_ident_eq("9223372036854775808")) {
        // given "-9223372036854775808", we can't parse the atom
        // as 9223372036854775808 (out of bounds for int64_t)
      // }
      init_spec_atom *e = parse_init_atom_unr();
      return new init_spec_uex(loc, op, e);
    } else {
      return parse_init_atom_prim();
    }
  }
  init_spec_atom *parse_init_atom_mul()
  {
    init_spec_atom *e = parse_init_atom_unr();
    while (looking_at(MUL) || looking_at(DIV) || looking_at(MOD)) {
      const auto &op =
        looking_at(MUL) ? *init_spec_bex::lookup_op("*") :
        looking_at(DIV) ? *init_spec_bex::lookup_op("/") :
        *init_spec_bex::lookup_op("%");
      skip();
      init_spec_atom *t = parse_init_atom_unr();
      e = new init_spec_bex(op, e, t);
    }
    return e;
  }
  init_spec_atom *parse_init_atom_add()
  {
    init_spec_atom *e = parse_init_atom_mul();
    while (looking_at(ADD) || looking_at(SUB)) {
      const auto &op = looking_at(ADD) ?
        *init_spec_bex::lookup_op("+") :
        *init_spec_bex::lookup_op("-");
      skip();
      init_spec_atom *t = parse_init_atom_mul();
      e = new init_spec_bex(op, e, t);
    }
    return e;
  }
  init_spec_atom *parse_init_atom_shift()
  {
    init_spec_atom *e = parse_init_atom_add();
    while (looking_at(LSH) || looking_at(RSH)) {
      const auto &op = looking_at(LSH) ?
        *init_spec_bex::lookup_op("<<") :
        *init_spec_bex::lookup_op(">>");
      skip();
      init_spec_atom *t = parse_init_atom_add();
      e = new init_spec_bex(op, e, t);
    }
    return e;
  }
  init_spec_atom *parse_init_atom_bitwise_and()
  {
    init_spec_atom *e = parse_init_atom_shift();
    while (consume_if(AMP)) {
      init_spec_atom *t = parse_init_atom_shift();
      e = new init_spec_bex(*init_spec_bex::lookup_op("&"), e, t);
    }
    return e;
  }
  init_spec_atom *parse_init_atom_bitwise_xor()
  {
    init_spec_atom *e = parse_init_atom_bitwise_and();
    while (consume_if(CIRC)) {
      init_spec_atom *t = parse_init_atom_bitwise_and();
      e = new init_spec_bex(*init_spec_bex::lookup_op("^"), e, t);
    }
    return e;
  }
  init_spec_atom *parse_init_atom_bitwise_or()
  {
    init_spec_atom *e = parse_init_atom_bitwise_xor();
    while (consume_if(PIPE)) {
      init_spec_atom *t = parse_init_atom_bitwise_xor();
      e = new init_spec_bex(*init_spec_bex::lookup_op("|"), e, t);
    }
    return e;
  }
  init_spec_atom *parse_init_atom()
  {
    return parse_init_atom_bitwise_or();
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

  init_spec *parse_init()
  {
    auto l = next_loc();
    init_spec_atom *e = parse_init_atom();

    if (consume_if(COLON)) {
      // memory initializer
      init_spec_mem *m = new init_spec_mem(l);
      m->root = e;
      if (consume_if(LBRACK)) {
        init_spec_atom *de = parse_init_atom();
        m->dimension = de;
        consume(RBRACK);
      }
      // attributes
      if (!looking_at(IDENT)) {
        fatal("expected buffer/image attributes");
      }
      auto s = token_string();
      skip();
      for (size_t i = 0; i < s.size(); i++) {
        auto set_tx = [&](init_spec_mem::transfer t) {
          if (m->transfer_properties != init_spec_mem::transfer::TX_INVALID) {
            fatal_at(l, "memory transfer respecification");
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
                fatal_at(l,
                  "invalid svm memory attribute (must be :..sc.. or :..sf..)");
              set_tx(s[i] == 'c' ?
                init_spec_mem::transfer::TX_SVM_COARSE :
                init_spec_mem::transfer::TX_SVM_FINE);
              break;
            default:
              // fatal_at(l, "invalid svm memory attribute (must be sc or sf)");
              // assume coarse if only one char given
              set_tx(init_spec_mem::transfer::TX_SVM_COARSE);
            }
          } else {
            set_tx(init_spec_mem::transfer::TX_SVM_COARSE);
          }
          break;
        case 'm':
          set_tx(init_spec_mem::transfer::TX_MAP);
          break;
        case 'c':
          set_tx(init_spec_mem::transfer::TX_COPY);
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
          fatal_at(l, "invalid memory attribute");
        }
      }
      if (m->transfer_properties == init_spec_mem::transfer::TX_INVALID)
        m->transfer_properties = init_spec_mem::transfer::TX_COPY; // default to copy
      m->defined_at.extend_to(next_loc());
      return m;
    } else {
      // regular primitive
      return e;
    }
  }

  template <typename T>
  refable<T> dereference_let(
    enum spec::spec_kind skind,
    const char *what)
  {
    auto at = next_loc();
    std::string name = consume_ident();
    auto itr = s.let_bindings.find(name);
    if (itr == s.let_bindings.end()) {
      fatal_at(at, what, " not defined");
    }
    let_spec *ls = itr->second;
    spec *rs = ls->value;
    if (rs->skind != skind) {
      std::stringstream ss;
      ss << "identifier does not reference a " << what;
      ss << " (defined on line " << rs->defined_at.line << ")";
      fatal_at(at, ss.str());
    }
    return refable<T>(at, name, (T *)rs);
  }

  refable<init_spec_mem> dereference_let_mem() {
    refable<init_spec> rf =
      dereference_let<init_spec>(spec::INIT_SPEC, "memory object");
    if ((*rf).skind != init_spec::IS_MEM) {
      fatal_at(rf.defined_at, "identifier does not reference a memory object");
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
  void parse_dimension_expressions(std::vector<init_spec_atom *> &dims) {
    dims.push_back(parse_init_atom());
    if (looking_at_ident_eq("x")) {
      // EXPR IDENT("x") ...
      skip();
      parse_dimension_expressions(dims);
    } else {
      while (looking_at_ident()) {
        // 1024x768x3 or 1024x768x(...) or 1024x768 x(...)
        //     ^^^^^^        ^^^^^             ^^^^
        // need to split x768x3 into: "x 768 x 3"
        loc at = next_loc();
        std::string lxm = token_string();
        skip();
        if (lxm[0] != 'x') {
          fatal_at(at, "syntax error in dimension constant");
        }
        size_t off = 0;
        while (off < lxm.size()) {
          if (lxm[off] != 'x') {
            at.column += (uint32_t)off;
            at.offset += (uint32_t)off;
            fatal_at(at, "syntax error in dimension constant");
          }
          off++;
          size_t end = off;
          while (end < lxm.size() && isdigit(lxm[end]))
            end++;
          if (end == off) {
            // it must be a new dimension
            // 2048x1024x(...)
            //          ^
            parse_dimension_expressions(dims);
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
              fatal_at(at, "syntax error in dimension constant");
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
  // void parse_dispatch_statement####(parser &p, script &s, dispatch_spec &ds)
  void parse_dispatch_statement_dimensions(dispatch_spec &ds) {
    // #1`path/foo.cl`kernel<1024x1024>(...)
    //                      ^^^^^^^^^^^
    // #1`path/foo.cl`kernel<1024x1024,16x16>(...)
    //                      ^^^^^^^^^^^^^^^^^
    consume(LANGLE);

    if (looking_at_ident_eq("nullptr") || looking_at_ident_eq("NULL"))
      fatal(token_string(), " not allowed for global dimensions");
    loc at = next_loc();
    parse_dimension_expressions(ds.global_size);
    if (consume_if(COMMA)) {
      if (!consume_if_ident_eq("nullptr") && !consume_if_ident_eq("NULL"))
        parse_dimension_expressions(ds.local_size);
    }
    if (!ds.local_size.empty() &&
      ds.global_size.size() != ds.local_size.size())
    {
      at.extend_to(next_loc());
      fatal_at(at, "global and local sizes have different dimensions");
    }

    consume(RANGLE);
  }

  // #1`path/foo.cl`kernel<1024x1024>(0:rw,1:r,33) where X = ..., Y = ...
  //                                 ^^^^^^^^^^^^^
  void parse_dispatch_statement_arguments(dispatch_spec &ds) {
    consume(LPAREN);
    while (!looking_at(RPAREN)) {
      init_spec *is = parse_init();
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
      if (!consume_if(COMMA))
        break;
    }
    consume(RPAREN);
    ds.defined_at.extend_to(next_loc());
  }

  // #1`path/foo.cl`kernel<1024x1024>(0:rw,1:r,33) where X = ..., Y = ...
  //                                               ^^^^^^^^^^^^^^^^^^^^^^
  void parse_dispatch_statement_where_clause(
    dispatch_spec &ds,
    let_spec *enclosing_let)
  {
    auto has_param =
      [&](std::string nm) {
      if (enclosing_let)
        for (const std::string &arg : enclosing_let->params)
          if (arg == nm)
            return true;
      return false;
    };

    // resolve references after the where clause
    if (consume_if_ident_eq("where")) {
      while (looking_at(IDENT)) {
        auto loc = next_loc();
        auto name = consume_ident("variable name");

        if (has_param(name)) {
          fatal_at(loc, "where binding shadows let parameter");
        }
        auto itr = s.let_bindings.find(name);
        if (itr != s.let_bindings.end()) {
          warning_at(loc,
            "where binding shadows let binding (from line ",
            itr->second->defined_at.line, ")");
        }
        for (auto w : ds.where_bindings)
          if (std::get<0>(w) == name)
            fatal_at(loc, "repeated where binding name");
        consume(EQ);
        init_spec *i = parse_init();
        i->defined_at.extend_to(next_loc());
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
          warning_at(loc, "where binding never used");
        }
        if (looking_at_seq(COMMA, IDENT)) {
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
        if (has_param(id)) {
          // parameter passed to this let
          // e.g. let F(X) = ....<...>(...,X,...);
          auto &prs = enclosing_let->param_uses[id];
          prs.push_back(&ds.arguments[ai]);
        } else {
          // capture from the let binding above this statement
          auto itr = s.let_bindings.find(id);
          if (itr == s.let_bindings.end()) {
            fatal_at(ds.arguments[ai].defined_at, "undefined symbol");
          }
          let_spec *ls = itr->second;
          if (ls->value->skind != spec::INIT_SPEC) {
            fatal_at(ds.arguments[ai].defined_at,
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
  program_spec *parse_dispatch_statement_device_program_part() {
    device_spec dev(next_loc());
    if (consume_if(HASH)) {
      if (looking_at(STRLIT) || looking_at(IDENT)) {
        if (looking_at(STRLIT)) {
          dev.set_source(token_string_literal());
        } else {
          dev.set_source(token_string());
        }
        skip();
      } else if (looking_at_int()) {
        dev.set_source(consume_integral<int32_t>("device index (integer)"));
      } else {
        fatal("invalid device specification");
      }
      if (consume_if(COLON)) {
        dev.instance = consume_ident("queue identifier");
      }
      dev.defined_at.extend_to(next_loc());
      if (!looking_at(BACKTICK))
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
    program_spec *ps = new program_spec(next_loc());
    ps->device = dev;
    if (looking_at(STRLIT)) {
      ps->path = token_string_literal(); skip();
    } else {
      ps->path = consume_to_char("[`");
    }

    // #1`path/foo.cl[-DTYPE=int]`kernel<1024x1024,16x16>(...)
    //               ^^^^^^^^^^^^
    if (consume_if(LBRACK)) {
      ps->build_options = consume_to_char("]");
      consume(RBRACK);
    }
    ps->defined_at.extend_to(next_loc());

    return ps;
  }

  kernel_spec *parse_dispatch_statement_kernel_part(program_spec *ps) {
    // #1`path/foo.cl`kernel<1024x1024,16x16>(...)
    //                ^^^^^^
    kernel_spec *ks = new kernel_spec(ps);
    if (looking_at(IDENT)) {
      ks->name = token_string(); skip();
    }
    ks->defined_at.extend_to(next_loc());
    ks->program = ps;
    return ks;
  }


  // allows for default devices:
  //   EASY CASE: #1`....
  //              ^ yes!
  //   HARD CASE: long/path with/spaces/foo.cl[long args]`kernel<...>()
  //                                          ^ YES!
  //              long/path with/spaces/foo.cl`kernel<...>()
  //                                          ^ YES!
  bool looking_at_immediate_dispatch_statement() const {
    if (looking_at(HASH))
      return true;
    if (looking_at_ident_eq("let"))
      return false; // let foo = ...;
    if (looking_at_seq(IDENT, LPAREN))
      return false; // e.g. seq(...); print(...)
    int i = 1;
    while (i < (int)tokens_left()) {
      if (looking_at(BACKTICK, i) || // correct dispatch
        looking_at(LANGLE, i) || // malformed dispatch   foo.cl`bar<1024>(...)
                                 //                            ^
        looking_at(LBRACK, i)) // malformed dispatchd    foo.cl[...]`bar(...)
                               //                              ^
      {
        return true;
      } else if (looking_at(NEWLINE, i) || // malformed statement
        looking_at(SEMI, i) || // malformed statement    .....; ....
        looking_at(EQ, i)) // malformed let possibly     foo=BAR
      {
        break;
      }
      i++;
    }
    return false;
  }

  dispatch_spec *parse_dispatch_statement()
  {
    auto loc = next_loc();
    dispatch_spec *ds = new dispatch_spec(loc);
    if (looking_at_immediate_dispatch_statement()) {
      if (looking_at_seq(IDENT, RANGLE)) {
        // named kernel invocation
        // KERNEL<...>(...)
        ds->kernel =
          dereference_let<kernel_spec>(spec::KERNEL_SPEC, "kernel");
      } else if (looking_at_seq(IDENT, BACKTICK, IDENT, LANGLE)) {
        // PROG`kernel<...>(...)
        // 000012222223...
        refable<program_spec> ps =
          dereference_let<program_spec>(spec::PROGRAM_SPEC, "program");
        consume(BACKTICK);
        std::string kernel_name = consume_ident("kernel name");
        ds->kernel = new kernel_spec(ps);
      } else {
        // FULLY immediate dispatch
        //
        // #1`path/foo.cl[-cl-opt]`kernel<1024x1024,16x16>(...)
        // ^^^^^^^^^^^^^^^^^^^^^^^
        program_spec *ps = parse_dispatch_statement_device_program_part();
        // #1`path/foo.cl[-cl-opt]`kernel<1024x1024,16x16>(...)
        //                        ^
        consume(BACKTICK);
        // #1`path/foo.cl`kernel<1024x1024,16x16>(...)
        //                ^^^^^^
        ds->kernel = parse_dispatch_statement_kernel_part(ps);
      }
    } else {
      fatal("expected statement");
    }

    // #1`path/foo.cl`kernel<1024x1024,16x16>(...)
    //                      ^^^^^^^^^^^^^^^^^
    parse_dispatch_statement_dimensions(*ds);

    // #1`path/foo.cl`kernel<1024x1024>(0:rw,1:r,33) where ...
    //                                 ^^^^^^^^^^^^^^^^^^^^^^^
    parse_dispatch_statement_arguments(*ds);
    // #1`path/foo.cl`kernel<1024x1024>(0:rw,1:r,33) where X = ..., Y = ...
    //                                               ^^^^^^^^^^^^^^^^^^^^^^
    parse_dispatch_statement_where_clause(*ds, nullptr);
    ds->defined_at.extend_to(next_loc());

    return ds;
  }

  init_spec_symbol *parse_symbol(parser &p)
  {
    auto loc = next_loc();
    if (looking_at_ident()) {
      auto ident = token_string();
      return new init_spec_symbol(loc, ident);
    } else {
      fatal("expected identifier");
      return nullptr;
    }
  }

  // Given an arugment; we resolve symbol the symbol to a let target (or fail)
  refable<init_spec> parse_init_resolved()
  {
    init_spec *is = parse_init();
    if (is->skind == init_spec::IS_SYM) {
      // make a reference argument
      auto itr = s.let_bindings.find(((const init_spec_symbol *)is)->identifier);
      if (itr == s.let_bindings.end()) {
        fatal_at(is->defined_at, "unbound identifier");
      } else if (itr->second->value->skind != spec::INIT_SPEC ||
        ((init_spec *)itr->second->value)->skind != init_spec::IS_MEM)
      {
        fatal_at(is->defined_at, "identifier does not reference a memory object");
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

  static bool is_floating(const type &elem_type) {
    if (elem_type.is<type_vector>()) {
      const type_vector &s = elem_type.as<type_vector>();
      return is_floating(s.element_type);
    }
    return elem_type.is<type_num>() &&
      elem_type.as<type_num>().skind == type_num::FLOATING;
  }


  // EXAMPLES
  // barrier
  // diff(X,Y) | diff<float>(X,Y) | diff<float,0.001> | diff<double2,0.0001>
  // print(X) | print<float>(X)
  // save(sym,X) | save_image<...>(...)
  bool parse_builtin()
  {
    auto loc = next_loc();
    if (looking_at_ident_eq("barrier")) {
      s.statement_list.statements.push_back(new barrier_spec(loc));
      skip();
      if (consume_if(LPAREN)) // optional ()
        consume(RPAREN);
      s.statement_list.statements.back()->defined_at.extend_to(next_loc());
      return true;
    } else if (looking_at_ident_eq("diff")) {
      skip();
      const type *elem_type = nullptr;
      std::optional<double> max_diff;
      if (consume_if(LANGLE)) {
        auto at = next_loc();
        // HACK: need to know the pointer size; assume 64b for now
        // technically we should probably just save the string and deal with
        // it during compilation once devices are matched etc...
        const size_t HACK = 8;
        elem_type = lookup_builtin_type(consume_ident("type name"), HACK);
        if (consume_if(COMMA)) {
          auto diff_loc = next_loc();
          max_diff = consume_float("floating point (max diff)");
          if (!is_floating(*elem_type)) {
            fatal_at(diff_loc, "diff max error requires floating point type");
          }
        }
        consume(RANGLE);
      }
      consume(LPAREN);
      auto ref = parse_init_resolved();
      consume(COMMA);
      if (!looking_at(IDENT))
        fatal("expected reference to memory object");
      refable<init_spec_mem> r_sut = dereference_let_mem();
      consume(RPAREN);
      loc.extend_to(next_loc());
      diff_spec *ds = new diff_spec(loc, ref, r_sut, elem_type);
      s.statement_list.statements.push_back(ds);
      if (max_diff.has_value()) {
        ds->max_diff = max_diff.value();
      }
      return true;
    } else if (looking_at_ident_eq("print")) {
      // print(X)
      // print<TYPE>(X)
      // print<INT>(X)
      // print<TYPE,INT>(X)
      skip();
      const type *elem_type = nullptr;
      int elems_per_row = 0;
      if (consume_if(LANGLE)) {
        if (looking_at_int()) {
          elems_per_row = consume_integral<int>("elements per column");
        } else {
          // HACK: we can't know the bits per ptr until we unify the
          // arguments with the owning context.
          const size_t HACK = 8;
          elem_type = lookup_builtin_type(consume_ident("type name"), HACK);
          if (consume_if(COMMA)) {
            elems_per_row = consume_integral<int>("elements per column");
          }
        }
        consume(RANGLE);
      }
      consume(LPAREN);
      if (!looking_at(IDENT))
        fatal("expected reference to memory object");
      refable<init_spec_mem> r_surf = dereference_let_mem();
      consume(RPAREN);
      loc.extend_to(next_loc());
      s.statement_list.statements.push_back(
        new print_spec(loc, r_surf, elem_type, elems_per_row));
      return true;
    } else if (looking_at_ident_eq("save_buffer") || looking_at_ident_eq("save")) {
      skip();
      consume(LPAREN);
      if (!looking_at(STRLIT))
        fatal("expected file name (string literal)");
      std::string file = token_string_literal();
      skip();
      consume(COMMA);
      refable<init_spec_mem> r_surf = dereference_let_mem();
      consume(RPAREN);
      loc.extend_to(next_loc());
      s.statement_list.statements.push_back(new save_spec(loc, file, r_surf));
      return true;
    } else if (looking_at_ident_eq("save_image")) {
      skip();
      save_image_spec::data_format fmt = save_image_spec::data_format::INVALID;
      consume(LANGLE);
      bool float4 = false;
      if (consume_if_ident_eq("float4")) {
        float4 = true;
      } else if (consume_if_ident_eq("uchar4")) {
        float4 = false;
      } else {
        fatal("expected image format type (float4,rgba)");
      }
      if (looking_at(COMMA) && looking_at_ident(1)) {
        skip();
        if (!consume_if_ident_eq("rgba"))
          fatal("expected rgba or width");
        // only RGBA is supported
      } // else implied
      fmt = float4 ?
        save_image_spec::data_format::FLOAT4_RGBA :
        save_image_spec::data_format::UCHAR4_RGBA;
      size_t width = 0, height = 0;
      if (consume_if(COMMA)) {
        // "200x400" lexes as:   INT(200) IDENT("x400")
        // "200 x 400" lexes as: INT(200) IDENT("x") INT(400)
        width = consume_integral<size_t>("image width");
        auto at = next_loc();
        std::string hstr = consume_ident("image height"); // "x200" or "x 200"
        if (hstr == "x") {
          height = consume_integral<size_t>("image height");
        } else {
          if (hstr.size() < 2 || hstr[0] != 'x' || !isdigit(hstr[1]))
            fatal_at(at, "malformed image height");
          size_t off = 1;
          while (off < hstr.size() && isdigit(hstr[off])) {
            height = 10 * height + hstr[off++] - '0';
          }
          if (off != hstr.size())
            fatal_at(at, "malformed image height");
        }
      }
      consume(RANGLE);
      consume(LPAREN);
      if (!looking_at(STRLIT))
        fatal("expected file name (string literal)");
      std::string file = token_string_literal();
      skip();
      consume(COMMA);
      refable<init_spec_mem> r_surf = dereference_let_mem();
      consume(RPAREN);
      loc.extend_to(next_loc());
      s.statement_list.statements.push_back(
        new save_image_spec(loc, fmt, width, height, file, r_surf));
      return true;
    } else {
      return false;
    }
  }

  // X=...
  void parse_let_binding()
  {
    if (!looking_at(IDENT)) {
      fatal("expected identifier");
    }
    auto at = next_loc();
    auto name = token_string(); // X
    if (s.let_bindings.find(name) != s.let_bindings.end()) {
      fatal(name, ": redefinition of let binding");
    }
    skip();      // X
    let_spec *ls = new let_spec(at, name);
    if (consume_if(LPAREN)) {
      // let F(X,Y) = #1`foo.cl`bar<...>(X,Y)
      if (!looking_at(RPAREN)) {
        do {
          ls->params.push_back(consume_ident());
        } while (consume_if(COMMA));
        consume(RPAREN);
        fatal("let arguments not supported yet");
      }
    }
    consume(EQ); // =

    spec *value = nullptr;
    loc value_loc = next_loc();
    bool is_init_expr_start =
      // SPECIFY: does this handle built-in function (e.g. atan2)
      looking_at_float() ||
      looking_at_int() ||
      looking_at(LPAREN) ||
      looking_at_ident_eq("seq") ||
      looking_at_ident_eq("random") ||
      looking_at_ident_eq("file") ||
      looking_at_ident_eq("undef") ||
      looking_at_ident_eq("image");
    if (!is_init_expr_start && looking_at_seq(IDENT, LANGLE)) {
      // let D = K<1024>(....) where ...
      dispatch_spec *ds = new dispatch_spec(value_loc);
      ds->kernel = dereference_let<kernel_spec>(spec::KERNEL_SPEC, "kernel");
      parse_dispatch_statement_dimensions(*ds);
      parse_dispatch_statement_arguments(*ds);
      parse_dispatch_statement_where_clause(*ds, ls);
      ds->defined_at.extend_to(next_loc());
      value = ds;
    } else if (
      !is_init_expr_start &&
      looking_at_seq(IDENT, BACKTICK, IDENT, LANGLE))
    {
      // let D = P`kernel<...>(...) where ...
      dispatch_spec *ds = new dispatch_spec(value_loc);
      refable<program_spec> rps =
        dereference_let<program_spec>(spec::PROGRAM_SPEC, "programs");
      consume(BACKTICK);
      ds->kernel = new kernel_spec(rps.value);
      parse_dispatch_statement_dimensions(*ds);
      parse_dispatch_statement_arguments(*ds);
      parse_dispatch_statement_where_clause(*ds, ls);
      ds->defined_at.extend_to(next_loc());
      value = ds;
    } else if (!is_init_expr_start && looking_at_immediate_dispatch_statement()) {
      // let P = #1`foo/bar.cl
      // let K = foo/bar.cl`kernel
      // let D = foo/bar.cl`kernel<...>(...) where ...
      program_spec *ps = parse_dispatch_statement_device_program_part();
      if (consume_if(BACKTICK)) {
        // includes the kernel
        kernel_spec *ks = parse_dispatch_statement_kernel_part(ps);
        ks->program = ps;
        if (looking_at(LANGLE)) {
          // let D = ...<...>(...) where ...
          dispatch_spec *ds = new dispatch_spec(value_loc);
          ds->kernel = ks;
          parse_dispatch_statement_dimensions(*ds);
          parse_dispatch_statement_arguments(*ds);
          parse_dispatch_statement_where_clause(*ds, ls);
          ds->defined_at.extend_to(next_loc());
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
      value = parse_init();
    }
    ls->value = value;
    s.let_bindings[name] = ls;
    s.statement_list.statements.push_back(ls);
    s.statement_list.statements.back()->defined_at.extend_to(next_loc());
  }

  // let X=...
  // let X=..., Y=...
  void parse_let_statement()
  {
    auto let_loc = next_loc();
    skip(); // let
    parse_let_binding();
    while (consume_if(COMMA))
      parse_let_binding();
  }

  void parse_statement_line()
  {
    if (looking_at_ident_eq("let") && looking_at(IDENT, 1)) {
      // let X = ...
      // let F(X,Y) = ...
      parse_let_statement();
    } else if (parse_builtin()) {
      // barrier
      // save('foo.bin',A);
      ;
    } else {
      // #1`foo/bar.cl
      s.statement_list.statements.emplace_back(parse_dispatch_statement());
    }
  }

  ///////////////////////////////////////////////////////////
  void parse_script()
  {
    while (!end_of_file()) {
      while (consume_if(NEWLINE))
        ;
      if (end_of_file())
        break;
      //
      parse_statement_line(); // S ((<NEWLINE> | ';') S)*
      //
      if (end_of_file())
        break;
      if (!consume_if(SEMI) && !consume_if(NEWLINE)) { // ';' S
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
  auto looking_at = [&] (char c) {
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
  auto consume_if = [&] (char c) {
    if (looking_at(c)) {
      skip(1);
      return true;
    }
    return false;
  };

  while (!eos()) {
    if (looking_at('$')) {
      loc inp_var_start(ln, col, off, 0);
      skip(1);
      size_t key_start = off;
      bool in_braces = consume_if('{');
      if (in_braces)
        key_start++;
      if (!std::isalpha(inp[off]) && inp[off] != '_') {
        ds.fatal_at(loc(ln, col, off, 1),
          "invalid program input variable: "
          "should start with an identifier starting character");
      }
      while (!eos() && (isalnum(inp[off]) || inp[off] == '_')) {
        skip(1);
      }
      std::string inp_var = inp.substr(key_start, off - key_start);
      if (in_braces) {
        if (!looking_at('}'))
          ds.fatal_at(loc(ln, col, off, 1), "expected '}'");
        skip(1);
      }
      inp_var_start.extent = off - inp_var_start.offset;
      bool matched_key = false;
      for (const auto &iv : os.input_vars) {
        if (iv.first == inp_var) {
          ss << iv.second;
          matched_key = true;
          break;
        }
      }
      if (!matched_key) {
        ds.fatal_at(inp_var_start,
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
  cp.parse_script();
}