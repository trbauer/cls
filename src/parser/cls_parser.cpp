#define _USE_MATH_DEFINES
#include "cls_parser.hpp"
#include "parser.hpp"
#include "../system.hpp"

#include <cmath>
#include <iostream>
#include <sstream>

using namespace cls;

#define SLIT(X) "\033[1;36m" X "\033[0m"
#define SVAR(X) "\033[2;36m" "<" X ">" "\033[0m"

///////////////////////////////////////////////////////////////////////////////////
const char *cls::CLS_SYNTAX =
  "CLS Syntax\n"
  "Nonterminals are in " SVAR("nonterm") "\n"
  "terminals or literal syntax are given as " SLIT("term") " and "
  "meta syntax shows as normal"
  "\n"
  "************* COMMON SYNTAX *************\n"
  SVAR("INT") " is an integer; e.g. " SLIT("32") " or " SLIT("0x20") "\n"
  SVAR("STR") " is a string literal in either single or double quotes);\n"
  "      e.g. " SLIT("\"foo\"") " or " SLIT("'foo'") "\n"
  SVAR("PATH") " is an unescaped path; e.g. " SLIT("foo/bar/baz.cl") "\n"
  SVAR("IDENT") " an identifier (alphanumeric sequence); e.g. " SLIT("foo") "\n"
  SVAR("DIMS") " is 1d, 2d, or 3d dimension; e.g. "
      SLIT("16x16") " or " SLIT("800x600x2") "\n"
  SVAR("TYPE") " = is an OpenCL C built-in type\n"
  "  e.g." SLIT("float") " or " SLIT("ulong8") "\n"
  "\n"
  "************* STATEMENTS *************\n"
  SVAR("Script")    " = a sequence of lines or ; separated <CLSStatement>s\n"
  SVAR("Statement") " = "
    SVAR("Dispatch") " | " SVAR("Builtin") " | "
    SVAR("Let") " | " SVAR("DispatchLet") "\n"
  "\n"
  "************* DISPATCH STATEMENTS *************\n"
  SVAR("Dispatch") " = (" SVAR("Device") SVAR("CommandQueueId") "?" SLIT("`") ")? " SVAR("Program") " ` "
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
  SVAR("CommandQueueId") " = " SLIT("#") SVAR("IDENT") "\n"
  "  A context identifier allows two dispatches to share the same context and\n"
  "command queue\n"
  "  e.g. " SLIT("#0#A`prog1.cl`foo...`; #0#A`prog2.cl`bar...")
  " dispatches two different kernels to the same queue\n"
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
  "************* SCALAR EXPRESSIONS *************\n"
  SVAR("Expr") " a usual C-style numeric expression\n"
  "  - most C operators as well as some many built-in C++ STL functions are\n"
  "    allowed"
  "  - the usual arithmetic operators are supported including:\n"
  "    bitwise logical, bit shifting, additive, multiplicative (incl. mod (" SLIT("%") "),\n"
  "    negation (" SLIT("-") "), and bitwise complement (" SLIT("~") ")\n"
  "  - type conversions and coercions attempt to mimic most C/C++ rules\n"
  "    at least: " SLIT("float(..)") ", " SLIT("int(..)") ", "
                   SLIT("signed(..)") ", " SLIT("unsigned(..)") " are supported\n"
  "      e.g. " SLIT("unsigned(pi)") " evaluates to 3\n"
  "    we also permit type conversions; e.g. " SLIT("unsigned(x)") "\n"
  "    the constants " SLIT("E") " and " SLIT("PI") " are also defined\n"
  "  - the unary C++ STL functions are:\n"
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
  "  - the binary functions are:\n"
  "      " SLIT("fmod") ", " SLIT("atan2") ", " SLIT("fdim") ", "
           SLIT("hypot") ", " SLIT("pow") ", "
           SLIT("min") ", " SLIT("max") ", "
           SLIT("gcd") ", " SLIT("lcm") "\n"
  "       e.g. " SLIT("float(1<<10)") " generates 1024.0f\n"
  "       e.g. " SLIT("4*1024*(1024 + max(4,16)/sizeof(float))") "\n"
  "       e.g. " SLIT("int(max(pi*pi,2*e*e)") "\n"
  "   - structures and vector types (e.g. float4) are represented via {..} syntax\n"
  "       e.g. " SLIT("{1,2,3,4*5}") "\n"
  "   - some host-side OpenCL enumerations may also be supported\n"
  "   - kernel arguments expressions may also refernce some built-in variables\n"
  "     defined as the NDRange size\n"
  "     " SLIT("g.x") ", " SLIT("g.y") ", and " SLIT("g.z") " reference global dimensions\n"
  "     " SLIT("l.x") ", " SLIT("l.y") ", and " SLIT("l.z") " reference local dimensions\n"
  "\n"
  "************* MEMORY INITIALIZER EXPRESSIONS *************\n"
  SVAR("MemInitExpr") " = "
    SVAR("MemElementInitExpr") SLIT(":") SVAR("MemObjSize") "?" SVAR("MemProps") "\n"
  "  Defines a memory object's initial contents;\n"
  "    if " SVAR("MemObjSize") " is absent, then CLS infers the size based on the\n"
  "    dimension (one element per global work item)\n"
  "\n"
  "  " SVAR("MemElementInitExpr") "\n"
  "    " " = " SVAR("ConstExpr") " | " SVAR("SeqExpr") " | " SVAR("RandExpr")
    " | " SVAR("FileExpr") " | " SVAR("ImgExpr") "\n"
  "    the memory object element initializer\n"
  "\n"
  "  " SVAR("MemObjSize") " = " SLIT("[") SVAR("Expr") SLIT("]") "\n"
  "      forces the buffer to be a specific size (in bytes)"
  "      e.g. " SLIT("0:[2*g.x*sizeof(float)]rw") " dedicates 2 floats per workitem"
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
  "       these may be suffixed with an integer to override elements per row\n"
  "       e.g. " SLIT("..P16p8..") " prints a buffer before with 16 elements per console row\n"
  "            and 8 elements per row after\n"
  "       this is generally meant for debugging;\n"
  "       also see the " SLIT("print") " built-in command\n"
  "\n"
  SVAR("ConstExpr")   " = " SVAR("Expr") "\n"
  "  initializes a memory object a constant value\n"
  "  built-in dispatch variable sizes are permitted in the expression\n"
  "\n"
  SVAR("SeqExpr")   " = "
  SLIT("seq(") SLIT(")")
  " | " SLIT("seq(") SVAR("Expr") SLIT(")")
  " | " SLIT("seq(") SVAR("Expr") SLIT(",") SVAR("Expr") SLIT(")") "\n"
  "  initializes a memory object to an arithmetic sequence of numbers;\n"
  "  an optional base and delta are given\n"
  "  e.g. " SLIT("seq()") " generates 0, 1, 2, ...\n"
  "  e.g. " SLIT("seq(17)") " generates 17, 18, ...\n"
  "  e.g. " SLIT("seq(1,3)") " generates 1, 4, 7, ...\n"
  "\n"
  SVAR("RandExpr")  " = "
  SLIT("random") SLIT("(") SVAR("RandBounds") "?" SLIT(")")
  " | " SLIT("random<") SVAR("EXPR") SLIT(">") SLIT("(") SVAR("RandBounds") "?" SLIT(")")
  "\n"
  "  randomly initializes a buffer with an optional seed and bounds\n"
  "  " SVAR("RandBounds") " = " SVAR("EXPR") "(" SLIT(",") " " SVAR("EXPR")")?\n"
  "  zero arguments lets CLS specify the low and high bounds\n"
  "  one argument sets the high bound and uses zero as the low bound\n"
  "  two argument sets the bounds as low and high bounds\n"
  "\n"
  SVAR("FileExpr")  " =\n"
  "    " SVAR("FileBinExpr") " | " SVAR("FileTxtExpr") " | " SVAR("FileTxtColExpr") "\n"
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
  " = " SLIT("image<") " " SVAR("ChannelOrder") SLIT(", ") SVAR("DataType") " " SLIT(">") "\n"
  "   <ChannelOrder> = an image channel order\n"
  "     https://www.khronos.org/registry/OpenCL/sdk/2.1/docs/man/xhtml/cl_image_format.html\n"
  "     as well as some shorthands; drop 3 characters and switch to lower case\n"
  "     e.g. both " SLIT("CL_sRGBx") " and " SLIT("srgbx") " mean the same thing\n"
  "   <DataType> = is a image data type; the usual OpenCL enums are permitted\n"
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
  "       both load foo.png and convert it to an CL_RGB image with CL_UNORM_INT8\n"
  "       .bmp will load as a bitmap\n"
  "       .ppm will load a PPM file\n"
#ifdef USE_LODE_PNG
  "       .png loads as PNG format\n"
#endif
  "\n"
  "************* BUILT-IN STATEMENTS *************\n"
  SVAR("Builtin")
  " = " SVAR("Barrier")
  " | " SVAR("Diff")
  " | " SVAR("Print")
  " | " SVAR("Save")
  "\n"
  "  " SVAR("Barrier") " = " SLIT("barrier") " | " SLIT("barrier") SLIT("(") SLIT(")") "\n"
  "  waits until all dispatches complete before proceeding\n"
// TODO: update this
  "  (currently a nop because kernel dispatches are currently synchronous)\n"
    "\n"
    "  " SVAR("Diff") "\n"
    "  = " SLIT("diff")
            SLIT("(") SVAR("IDENT") SLIT(", ") SVAR("IDENT") SLIT(")") "\n"
    "  | " SLIT("diff") SLIT("<") SVAR("TYPE") SLIT(">")
            SLIT("(") SVAR("IDENT") SLIT(", ") SVAR("IDENT") SLIT(")") "\n"
// TODO: update this when max diffs are allowed
    "  diffs two memory object or one against a replicated scalar value;\n"
    "  if a type argument is present, CLS will reinterpret elements as that type\n"
    "  NOTE: the program will exit with failure upon diff failure\n"
    "        use -Xno-exit-on-diff-fail to override this\n"
    "  e.g. " SLIT("let A=1:rw; #0`file.cl`kernel<1024>(A); diff<int>(A,0)") "\n"
    "       diffs buffer " SLIT("A") " with 0 as integer's\n"
    "  e.g. " SLIT("let A=1:rw; let B=2:rw; #0`file.cl`kernel<1024>(A,B); diff(A,B)") "\n"
    "       diffs buffers " SLIT("A") " and " SLIT("B") " element by element\n"
    "\n"
    "  " SVAR("Print") "\n"
    "  = " SLIT("print") SLIT("(") SVAR("IDENT") SLIT(")") "\n"
    "  | " SLIT("print") SLIT("<") SVAR("INT") SLIT(">") SLIT("(") SVAR("IDENT") SLIT(")") "\n"
    "  | " SLIT("print") SLIT("<") SVAR("TYPE") SLIT(">") SLIT("(") SVAR("IDENT") SLIT(")") "\n"
    "  | " SLIT("print") SLIT("<") SVAR("TYPE") SLIT(",") SVAR("INT") SLIT(">") SLIT("(") SVAR("IDENT") SLIT(")") "\n"
    "  prints a memory object; an optional type interprets the surface's elements;\n"
    "  an optional integer argument overrides columns per line in output\n"
    "  e.g. " SLIT("let A = 1:w; #0`file.cl`kernel<1024>(A); print<float4>(A)") "\n"
    "       prints buffer A as float4's\n"
  "\n"
  "************* LET STATEMENTS *************\n"
  SVAR("Let") " = " SLIT("let") " " SVAR("IDENT") SLIT(" = ") SVAR("Dispatch") "\n"
  "  binds a symbol to a value;\n"
  "  use this to share buffers in multiple dispatch statements\n"
  ;


// a rough solution to enable use to read tokens including spaces
// e.g. `path/has spaces/baz.cl[-DTYPE=int -cl-some-option]`kernel
//       ^^^^^^^^^^^^^^^^^^^^^^
//                              ^^^^^^^^^^^^^^^^^^^^^^^^^^
static std::string consumeToChar(parser &p, const char *set)
{
  const std::string &s = p.input();
  size_t start = p.nextLoc().offset;
  size_t len = 0;
  while (start + len < s.length()) {
    if (strchr(set, s[start + len])) {
      break;
    }
    len++;
  }
  while(p.nextLoc().offset != start + len)
    p.skip();
  return s.substr(start, len);
}

static init_spec_atom *parseInitAtom(parser &p, script &s);

// 1024[1200]x768[800]x4
//
// similar to dimensions, but includes pitch
static void parseImageDimensionExpressions(
  parser &p,
  script &s,
  std::vector<init_spec_atom *> &dims,
  std::vector<init_spec_atom *> &pitches)
{
  dims.push_back(parseInitAtom(p, s));
  if (p.consumeIf(LBRACK)) {
    pitches.push_back(parseInitAtom(p, s));
    p.consume(RBRACK);
  } else {
    pitches.push_back(nullptr);
  }
  if (dims.size() == 3)
    return; // at the end

  if (p.consumeIfIdentEq("x")) {
    // EXPR IDENT("x") ...
    parseImageDimensionExpressions(p, s, dims, pitches);
  } else {
    while (p.lookingAtIdent()) {
      if (dims.size() == 3)
        p.fatal("syntax error in image dimension constant");
      // 1024x768x3 or 1024x768x(...) or 1024x768 x(...)
      //     ^^^^^^        ^^^^^             ^^^^
      // need to split x768x3 into: "x 768 x 3"
      loc at = p.nextLoc();
      std::string lxm = p.tokenString();
      p.skip();
      if (lxm[0] != 'x') {
        p.fatalAt(at,"syntax error in image dimensions");
      }
      size_t off = 0;
      while (off < lxm.size()) {
        if (lxm[off] != 'x') {
          at.column += (uint32_t)off;
          at.offset += (uint32_t)off;
          p.fatalAt(at,"syntax error in image dimensions");
        }
        off++;
        size_t end = off;
        while (end < lxm.size() && isdigit(lxm[end]))
          end++;
        if (end == off) {
          // it must be a new dimension
          // 2048x1024x(...)
          //          ^
          parseImageDimensionExpressions(p, s, dims, pitches);
          return;
        } else {
          // dimension is embedded in identifier
          // 2048x1024...
          //     ^^^^^
          try {
            auto val = (size_t)std::strtoull(
              lxm.substr(off,end-off).c_str(),nullptr,10);
            loc this_int = at;
            this_int.column += (uint32_t)off;
            this_int.offset += (uint32_t)off;
            this_int.extent = (uint32_t)(end - off);
            dims.push_back(new init_spec_int(this_int, val));
            off = end;
          } catch (...) {
            at.offset += (uint32_t)off;
            at.column += (uint32_t)off;
            p.fatalAt(at,"syntax error in image dimensions");
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
              if (p.consumeIf(LBRACK)) {
                pitches.push_back(parseInitAtom(p, s));
                p.consume(RBRACK);
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
            p.fatalAt(at, "syntax error in image dimensions (too many)");
          }
        }
      } // while identifier
    } // while looking at successive identifiers
  } // if
}

static init_spec_atom *parseInitAtomPrim(parser &p, script &s)
{
  auto at = p.nextLoc();
  if (p.lookingAt(STRLIT)) {
    p.fatal("bare strings not allowed for files anymore "
      "(use bin('...') and txt('...'))");
    return nullptr;
  } else if (p.lookingAtFloat()) {
    return new init_spec_float(at, p.consumeFloat());
  } else if (p.lookingAtInt()) {
    return new init_spec_int(at, p.consumeIntegral<int64_t>());
  } else if (p.lookingAtIdent()) {
    // e.g. "X" or "g.x" or "pow(...)"
    auto id = p.tokenString();
    p.skip();
    if (p.lookingAt(DOT)) {
      // e.g. "g.x"
      while (p.consumeIf(DOT)) {
        id += '.';
        if (!p.lookingAt(IDENT))
          p.fatal("syntax error in initializer expression field access");
        id += p.tokenString();
        p.skip();
      }
      for (int biv = init_spec_builtin::BIV_FIRST;
        biv <= init_spec_builtin::BIV_LAST;
        biv++)
      {
        if (id == init_spec_builtin::syntax_for((init_spec_builtin::biv_kind)biv)) {
          at.extend_to(p.nextLoc());
          return new init_spec_builtin(at, (init_spec_builtin::biv_kind)biv);
        }
      }
      at.extend_to(p.nextLoc());
      return new init_spec_symbol(at, id);
    } else if (p.lookingAt(LPAREN) || p.lookingAt(LANGLE) ||
      id == "sizeof" || id == "random" ||
      id == "seq" || id == "file" || id == "image")
    {
      // foo<...  (e.g. random<12007>(...))
      // or
      // foo(...
      //
      // TODO: generalize function parsing to
      //    F<...>(....)
      // then match by template arguments
      if (id == "sizeof") {
        bool has_parens = p.consumeIf(LPAREN);
        if (!p.lookingAtIdent())
          p.fatal("expected type name");
        loc nm_at = p.nextLoc();
        std::string sizeof_arg = p.tokenString();
        p.skip();
        if (has_parens)
          p.consume(RPAREN);
        at.extend_to(p.nextLoc());
        // manual dereference memobject
        auto itr = s.let_bindings.find(sizeof_arg);
        if (itr == s.let_bindings.end()) {
          // assume it's a type
          return new init_spec_sizeof(at, sizeof_arg);
        } else {
          let_spec *ls = itr->second;
          spec *rs = ls->value;
          if (rs->skind != spec::INIT_SPEC)
            p.fatal("symbol refers to non-memory object");
          init_spec *is = (init_spec *)rs;
          if (is->skind != init_spec::IS_MEM)
            p.fatal("symbol refers to non-memory object");
          return new init_spec_sizeof(at,
            refable<init_spec_mem>(nm_at, sizeof_arg, (init_spec_mem *)is));
        }
      } else if (id == "random") {
        auto func = new init_spec_rng(at);
        int64_t seed = 0;
        bool has_seed = false;
        if (p.consumeIf(LANGLE)) {
          if (!p.lookingAt(RANGLE)) {
            seed = p.consumeIntegral<int64_t>("seed (int)");
            has_seed = true;
          }
          p.consume(RANGLE);
        }
        if (p.consumeIf(LPAREN)) {
          if (!p.lookingAt(RPAREN)) {
            auto *arg1 = parseInitAtom(p,s);
            if (p.consumeIf(COMMA)) {
              auto *arg2 = parseInitAtom(p,s);
              func->e_lo = arg1;
              func->e_hi = arg2;
            } else {
              func->e_hi = arg1;
            }
          }
          p.consume(RPAREN);
        }
        if (has_seed)
          func->set_seed(seed);
        func->defined_at.extend_to(p.nextLoc());
        return func;
      } else if (id == "file") {
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
        if (p.consumeIf(LANGLE)) {
          if (!p.lookingAt(RANGLE)) {
            auto fmt_loc = p.nextLoc();
            auto flv_str = p.consumeIdent("data format (identifier)");
            if (flv_str == "bin") {
              flv = init_spec_file::BIN;
            } else if (flv_str == "text") {
              flv = init_spec_file::TXT;
              if (p.consumeIf(COMMA)) {
                if (!p.lookingAt(STRLIT)) {
                  p.fatal("expected separator string (literal)");
                }
                sep = p.tokenStringLiteral();
                p.skip();
              }
            } else if (flv_str == "text_col") {
              flv = init_spec_file::TXT_COL;
              if (p.consumeIf(COMMA)) {
                col = p.consumeIntegral<int>("column index (int)");
                if (p.consumeIf(COMMA)) {
                  if (!p.lookingAt(STRLIT)) {
                    p.fatal("expected separator string (literal)");
                  }
                  sep = p.tokenStringLiteral();
                  p.skip();
                }
              }
            } else {
              p.fatalAt(fmt_loc,"unsupported file flavor; should be: bin, text, ...");
            }
          }
          p.consume(RANGLE);
        }
        p.skip();
        if (!p.lookingAt(STRLIT)) {
          p.fatalAt(at,"expected file path (string literal)");
        }
        auto s = p.tokenStringLiteral();
        p.skip();
        p.consume(RPAREN);
        at.extend_to(p.nextLoc());
        return new init_spec_file(at, s, flv, col, sep);
      } else if (id == "image") {
        p.consume(LANGLE);
        auto ord = init_spec_image::RGB;
        if (p.consumeIfIdentEq("i") ||
          p.consumeIfIdentEq("CL_INTENSITY"))
        {
          ord = init_spec_image::I;
        } else if (p.consumeIfIdentEq("l") ||
          p.consumeIfIdentEq("CL_LUMINANCE"))
        {
          ord = init_spec_image::L;
        } else if (p.consumeIfIdentEq("r") ||
          p.consumeIfIdentEq("CL_R"))
        {
          ord = init_spec_image::R;
        } else if (p.consumeIfIdentEq("rx") ||
          p.consumeIfIdentEq("CL_Rx"))
        {
          ord = init_spec_image::Rx;
        } else if (p.consumeIfIdentEq("rg") ||
          p.consumeIfIdentEq("CL_RG"))
        {
          ord = init_spec_image::RG;
        } else if (p.consumeIfIdentEq("rgx") ||
          p.consumeIfIdentEq("CL_RGx"))
        {
          ord = init_spec_image::RGx;
        } else if (p.consumeIfIdentEq("rgb") ||
          p.consumeIfIdentEq("CL_RGB"))
        {
          ord = init_spec_image::RGB;
        } else if (p.consumeIfIdentEq("rgbx") ||
          p.consumeIfIdentEq("CL_RGBx"))
        {
          ord = init_spec_image::RGBx;
        } else if (p.consumeIfIdentEq("rgba") ||
          p.consumeIfIdentEq("CL_RGBA"))
        {
          ord = init_spec_image::RGBA;
        } else if (p.consumeIfIdentEq("argb") ||
          p.consumeIfIdentEq("CL_ARGB"))
        {
          ord = init_spec_image::ARGB;
        } else if (p.consumeIfIdentEq("bgra") ||
          p.consumeIfIdentEq("CL_BGRA"))
        {
          ord = init_spec_image::BGRA;

        } else if (p.consumeIfIdentEq("srgb") ||
          p.consumeIfIdentEq("CL_sRGB"))
        {
          ord = init_spec_image::sRGB;
        } else if (p.consumeIfIdentEq("srgbx") ||
          p.consumeIfIdentEq("CL_sRGBx"))
        {
          ord = init_spec_image::sRGBx;
        } else if (p.consumeIfIdentEq("srgba") ||
          p.consumeIfIdentEq("CL_sRGBA"))
        {
          ord = init_spec_image::sRGBA;
        } else if (p.consumeIfIdentEq("sbgra") ||
          p.consumeIfIdentEq("CL_sBGRA"))
        {
          ord = init_spec_image::sBGRA;
        } else {
          p.fatal("unrecognized channel order (try r, rg, rgb, rgba, ...)");
        }
        p.consume(COMMA);
        auto ty = init_spec_image::U8;
        ///////////////////////////////////////////////////////////////////////
        if (p.consumeIfIdentEq("un8") ||
          p.consumeIfIdentEq("CL_UNORM_INT8"))
        {
          ty = init_spec_image::UN8;
        } else if (p.consumeIfIdentEq("un16") ||
          p.consumeIfIdentEq("CL_UNORM_INT16"))
        {
          ty = init_spec_image::UN16;
        } else if (p.consumeIfIdentEq("un24") ||
          p.consumeIfIdentEq("CL_UNORM_INT24"))
        {
          ty = init_spec_image::UN24;
        } else if (p.consumeIfIdentEq("un565") ||
          p.consumeIfIdentEq("CL_UNORM_SHORT_565"))
        {
          ty = init_spec_image::UN565;
        } else if (p.consumeIfIdentEq("un555") ||
          p.consumeIfIdentEq("CL_UNORM_SHORT_555"))
        {
          ty = init_spec_image::UN555;
        } else if (p.consumeIfIdentEq("un101010") ||
          p.consumeIfIdentEq("CL_UNORM_INT_101010"))
        {
          ty = init_spec_image::UN101010;
        } else if (p.consumeIfIdentEq("un101010_2") ||
          p.consumeIfIdentEq("CL_UNORM_INT_101010_2"))
        {
          ty = init_spec_image::UN101010_2;
        ///////////////////////////////////////////////////////////////////////
        } else if (p.consumeIfIdentEq("sn8") ||
          p.consumeIfIdentEq("CL_SNORM_INT8"))
        {
          ty = init_spec_image::SN8;
        } else if (p.consumeIfIdentEq("sn16") ||
          p.consumeIfIdentEq("CL_SNORM_INT16"))
        {
          ty = init_spec_image::SN16;
        ///////////////////////////////////////////////////////////////////////
        } else if (p.consumeIfIdentEq("u8") ||
          p.consumeIfIdentEq("CL_UNSIGNED_INT8"))
        {
          ty = init_spec_image::U8;
        } else if (p.consumeIfIdentEq("u16") ||
          p.consumeIfIdentEq("CL_UNSIGNED_INT16"))
        {
          ty = init_spec_image::U16;
        } else if (p.consumeIfIdentEq("u32") ||
          p.consumeIfIdentEq("CL_UNSIGNED_INT32"))
        {
          ty = init_spec_image::U32;
        ///////////////////////////////////////////////////////////////////////
        } else if (p.consumeIfIdentEq("s8") ||
          p.consumeIfIdentEq("CL_SIGNED_INT8"))
        {
          ty = init_spec_image::S8;
        } else if (p.consumeIfIdentEq("s16") ||
          p.consumeIfIdentEq("CL_SIGNED_INT16"))
        {
          ty = init_spec_image::S16;
        } else if (p.consumeIfIdentEq("s32") ||
          p.consumeIfIdentEq("CL_SIGNED_INT32"))
        {
          ty = init_spec_image::S32;
        ///////////////////////////////////////////////////////////////////////
        } else if (p.consumeIfIdentEq("f32") ||
          p.consumeIfIdentEq("CL_FLOAT"))
        {
          ty = init_spec_image::F32;
        } else if (p.consumeIfIdentEq("f16") ||
          p.consumeIfIdentEq("CL_HALF_FLOAT"))
        {
          ty = init_spec_image::F16;
        ///////////////////////////////////////////////////////////////////////
        } else {
          p.fatal(
            "unrecognized image format (try: u8, u16, ..., f32, f16 ,...)");
        }

        init_spec_atom *width = nullptr, *height = nullptr, *depth = nullptr;
        init_spec_atom *row_pitch = nullptr, *slice_pitch = nullptr;
        if (p.consumeIf(COMMA)) {
          // image dimension:
          // W (RP)
          // W (RP) x H (SP)
          // W (RP) x H (SP) x D
          // the spacing makes this hard
          std::vector<init_spec_atom *> dims;
          std::vector<init_spec_atom *> pitches;
          parseImageDimensionExpressions(p, s, dims, pitches);
          //
          width = dims[0];
          height = dims.size() >= 2 ? dims[1] : nullptr;
          depth = dims.size() >= 3 ? dims[2] : nullptr;
          row_pitch = pitches[0];
          slice_pitch = pitches.size() >= 2 ? pitches[1] : nullptr;
        }
        p.consume(RANGLE);
        std::string file;
        if (p.consumeIf(LPAREN)) {
          if (!p.lookingAt(STRLIT)) {
            p.fatalAt(at,"expected file path (string literal)");
          }
          file = p.tokenStringLiteral(); p.skip();
          p.consume(RPAREN);
        }
        at.extend_to(p.nextLoc());
        return new init_spec_image(
          at, file, ord, ty, width, row_pitch, height, slice_pitch, depth);
      } else {
        ///////////////////////////////////////////////////
        // generic function
        std::vector<init_spec_atom *> args;
        p.consume(LPAREN);
        while (!p.lookingAt(RPAREN)) {
          args.push_back(parseInitAtom(p,s));
          if (!p.consumeIf(COMMA))
            break;
        }
        p.consume(RPAREN);

        ///////////////////////////////////////////////////
        // special functions (pseudo functions)
        if (id == "seq") {
          init_spec_seq *iss = nullptr;
          switch (args.size()) {
          case 0: iss = new init_spec_seq(at,nullptr,nullptr); break;
          case 1: iss = new init_spec_seq(at,args[0],nullptr); break;
          case 2: iss = new init_spec_seq(at,args[0],args[1]); break;
          default: p.fatalAt(at,"wrong number of args to seq");
          }
          iss->defined_at.extend_to(p.nextLoc());
          return iss;
        }

        ///////////////////////////////////////////////////
        // regular arithmetic functions
        if (args.size() == 1) {
          if (id == "fabs")
            p.fatalAt(at,"use \"abs\" for the absolute value");
          else if (id == "sqt")
            p.fatalAt(at,"use \"sqrt\" for the square root");

          const auto *op = init_spec_uex::lookup_op(id.c_str());
          if (!op) {
            if (init_spec_bex::lookup_op(id.c_str())) {
              p.fatalAt(at, "function requires two arguments");
            } else {
              p.fatalAt(at, "not a unary function");
            }
          }
          return new init_spec_uex(at,*op,args[0]);
        } else if (args.size() == 2) {
          const auto *op = init_spec_bex::lookup_op(id.c_str());
          if (!op) {
            p.fatalAt(at, "not a binary function");
          }
          auto *isbe = new init_spec_bex(*op,args[0],args[1]);
          isbe->defined_at = at; // reset start loc to function name
          isbe->defined_at.extend_to(p.nextLoc());
          return isbe;
        } else {
          p.fatalAt(at,"undefined function");
        }
        // fallback
        return nullptr; // unreachable
      } // end else not random
    } else {
      if (id == "E")
        return new init_spec_float(at, M_E);
      else if (id == "PI")
        return new init_spec_float(at, M_PI);
      // some other symbol (may target a LET binding)
      return new init_spec_symbol(at, id);
    }
  } else if (p.consumeIf(LBRACE)) {
    // {...}
    auto re = new init_spec_record(at);
    if (!p.lookingAt(RBRACE)) {
      re->children.push_back(parseInitAtom(p,s));
      while (p.consumeIf(COMMA))
        re->children.push_back(parseInitAtom(p,s));
    }
    p.consume(RBRACE);
    re->defined_at.extend_to(p.nextLoc());
    return re;
  } else if (p.consumeIf(LPAREN)) {
    init_spec_atom *e = parseInitAtom(p,s);
    p.consume(RPAREN);
    return e;
  } else {
    p.fatal("syntax error in initializer expression");
    return nullptr;
  }
}
static init_spec_atom *parseInitAtomUnr(parser &p,script &s)
{
  if (p.lookingAt(SUB) || p.lookingAt(TILDE)) {
    auto loc = p.nextLoc();
    const auto &op =
      p.lookingAt(SUB) ?
        *init_spec_uex::lookup_op("-") :
        *init_spec_uex::lookup_op("~");
    p.skip();
    init_spec_atom *e = parseInitAtomUnr(p,s);
    return new init_spec_uex(loc, op, e);
  } else {
    return parseInitAtomPrim(p,s);
  }
}
static init_spec_atom *parseInitAtomMul(parser &p,script &s)
{
  init_spec_atom *e = parseInitAtomUnr(p,s);
  while (p.lookingAt(MUL) || p.lookingAt(DIV) || p.lookingAt(MOD)) {
    const auto &op =
      p.lookingAt(MUL) ? *init_spec_bex::lookup_op("*") :
      p.lookingAt(DIV) ? *init_spec_bex::lookup_op("/") :
      *init_spec_bex::lookup_op("%");
    p.skip();
    init_spec_atom *t = parseInitAtomUnr(p,s);
    e = new init_spec_bex(op, e, t);
  }
  return e;
}
static init_spec_atom *parseInitAtomAdd(parser &p,script &s)
{
  init_spec_atom *e = parseInitAtomMul(p,s);
  while (p.lookingAt(ADD) || p.lookingAt(SUB)) {
    const auto &op = p.lookingAt(ADD) ?
      *init_spec_bex::lookup_op("+") :
      *init_spec_bex::lookup_op("-");
    p.skip();
    init_spec_atom *t = parseInitAtomMul(p,s);
    e = new init_spec_bex(op, e, t);
  }
  return e;
}
static init_spec_atom *parseInitAtomShift(parser &p,script &s)
{
  init_spec_atom *e = parseInitAtomAdd(p,s);
  while (p.lookingAt(LSH) || p.lookingAt(RSH)) {
    const auto &op = p.lookingAt(LSH) ?
      *init_spec_bex::lookup_op("<<") :
      *init_spec_bex::lookup_op(">>");
    p.skip();
    init_spec_atom *t = parseInitAtomAdd(p,s);
    e = new init_spec_bex(op, e, t);
  }
  return e;
}
static init_spec_atom *parseInitAtomBitwiseAND(parser &p,script &s)
{
  init_spec_atom *e = parseInitAtomShift(p,s);
  while (p.consumeIf(AMP)) {
    init_spec_atom *t = parseInitAtomShift(p,s);
    e = new init_spec_bex(*init_spec_bex::lookup_op("&"), e, t);
  }
  return e;
}
static init_spec_atom *parseInitAtomBitwiseXOR(parser &p,script &s)
{
  init_spec_atom *e = parseInitAtomBitwiseAND(p,s);
  while (p.consumeIf(CIRC)) {
    init_spec_atom *t = parseInitAtomBitwiseAND(p,s);
    e = new init_spec_bex(*init_spec_bex::lookup_op("^"), e, t);
  }
  return e;
}
static init_spec_atom *parseInitAtomBitwiseOR(parser &p,script &s)
{
  init_spec_atom *e = parseInitAtomBitwiseXOR(p,s);
  while (p.consumeIf(PIPE)) {
    init_spec_atom *t = parseInitAtomBitwiseXOR(p,s);
    e = new init_spec_bex(*init_spec_bex::lookup_op("|"), e, t);
  }
  return e;
}
static init_spec_atom *parseInitAtom(parser &p,script &s)
{
  return parseInitAtomBitwiseOR(p,s);
}

static init_spec *parseInit(parser &p,script &s)
{
  auto l = p.nextLoc();
  init_spec_atom *e = parseInitAtom(p,s);

  if (p.consumeIf(COLON)) {
    // memory initializer
    init_spec_mem *m = new init_spec_mem(l);
    m->root = e;
    if (p.consumeIf(LBRACK)) {
      init_spec_atom *de = parseInitAtom(p,s);
      m->dimension = de;
      p.consume(RBRACK);
    }
    // attributes
    if (!p.lookingAt(IDENT)) {
      p.fatal("expected buffer/image attributes");
    }
    auto s = p.tokenString();
    p.skip();
    for (size_t i = 0; i < s.size(); i++) {
      auto setTx = [&] (init_spec_mem::transfer t) {
        if (m->transfer_properties != init_spec_mem::transfer::TX_INVALID) {
          p.fatalAt(l, "memory transfer respecification");
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
              p.fatalAt(l, "invalid svm memory attribute (must be :..sc.. or :..sf..)");
            setTx(s[i] == 'c' ?
              init_spec_mem::transfer::TX_SVM_COARSE :
              init_spec_mem::transfer::TX_SVM_FINE);
            break;
          default:
            // p.fatalAt(l, "invalid svm memory attribute (must be sc or sf)");
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
        if (i + 1 < s.size() && ::isdigit(s[i+1])) {
          i++;
          m->print_pre_elems_per_row = 0;
          if (i < s.size() && ::isdigit(s[i])) {
            m->print_pre_elems_per_row =
              10 * m->print_pre_elems_per_row + s[i] - '0';
          }
        }
        break;
      case 'p':
        m->print_post = true;
        if (i + 1 < s.size() && ::isdigit(s[i+1])) {
          i++;
          m->print_post_elems_per_row = 0;
          if (i < s.size() && ::isdigit(s[i])) {
            m->print_post_elems_per_row =
              10 * m->print_post_elems_per_row + s[i] - '0';
          }
        }
        break;
      case 'S':
        m->save_post = true;
        break;
      default:
        // adjust the location to the middle of this token for
        // the diganostic
        l.column += (uint32_t)i;
        l.offset += (uint32_t)i;
        p.fatalAt(l, "invalid memory attribute");
      }
    }
    if (m->transfer_properties == init_spec_mem::transfer::TX_INVALID)
      m->transfer_properties = init_spec_mem::transfer::TX_COPY; // default to copy
    m->defined_at.extend_to(p.nextLoc());
    return m;
  } else {
    // regular primitive
    return e;
  }
}

template <typename T>
static refable<T> dereferenceLet(
  parser &p,
  script &s,
  enum spec::spec_kind skind,
  const char *what)
{
  auto at = p.nextLoc();
  std::string name = p.consumeIdent();
  auto itr = s.let_bindings.find(name);
  if (itr == s.let_bindings.end()) {
    p.fatalAt(at,what," not defined");
  }
  let_spec *ls = itr->second;
  spec *rs = ls->value;
  if (rs->skind != skind) {
    std::stringstream ss;
    ss << "identifier does not reference a " << what;
    ss << " (defined on line " << rs->defined_at.line << ")";
    p.fatalAt(at,ss.str());
  }
  return refable<T>(at,name,(T *)rs);
}

static refable<init_spec_mem> dereferenceLetMem(
  parser &p,
  script &s)
{
  refable<init_spec> rf =
    dereferenceLet<init_spec>(p, s, spec::INIT_SPEC, "memory object");
  if ((*rf).skind != init_spec::IS_MEM) {
    p.fatalAt(rf.defined_at,"identifier does not reference a memory object");
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
static void parseDimensionExpressions(
  parser &p,
  script &s,
  std::vector<init_spec_atom *> &dims)
{
  dims.push_back(parseInitAtom(p, s));
  if (p.lookingAtIdentEq("x")) {
    // EXPR IDENT("x") ...
    p.skip();
    parseDimensionExpressions(p, s, dims);
  } else {
    while (p.lookingAtIdent()) {
      // 1024x768x3 or 1024x768x(...) or 1024x768 x(...)
      //     ^^^^^^        ^^^^^             ^^^^
      // need to split x768x3 into: "x 768 x 3"
      loc at = p.nextLoc();
      std::string lxm = p.tokenString();
      p.skip();
      if (lxm[0] != 'x') {
        p.fatalAt(at,"syntax error in dimension constant");
      }
      size_t off = 0;
      while (off < lxm.size()) {
        if (lxm[off] != 'x') {
          at.column += (uint32_t)off;
          at.offset += (uint32_t)off;
          p.fatalAt(at,"syntax error in dimension constant");
        }
        off++;
        size_t end = off;
        while (end < lxm.size() && isdigit(lxm[end]))
          end++;
        if (end == off) {
          // it must be a new dimension
          // 2048x1024x(...)
          //          ^
          parseDimensionExpressions(p, s, dims);
          return;
        } else {
          // dimension is embedded in identifier
          // 2048x1024...
          //     ^^^^^
          try {
            auto val = (size_t)std::strtoull(
              lxm.substr(off,end-off).c_str(),nullptr,10);
            loc this_int = at;
            this_int.column += (uint32_t)off;
            this_int.offset += (uint32_t)off;
            this_int.extent = (uint32_t)(end - off);
            dims.push_back(new init_spec_int(this_int, val));
            off = end;
          } catch (...) {
            at.offset += (uint32_t)off;
            at.column += (uint32_t)off;
            p.fatalAt(at,"syntax error in dimension constant");
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
// static void parseDispatchStatement####(parser &p, script &s, dispatch_spec &ds)
static void parseDispatchStatementDimensions(
  parser &p,
  script &s,
  dispatch_spec &ds)
{
  // #1`path/foo.cl`kernel<1024x1024>(...)
  //                      ^^^^^^^^^^^
  // #1`path/foo.cl`kernel<1024x1024,16x16>(...)
  //                      ^^^^^^^^^^^^^^^^^
  if (p.consumeIf(LANGLE)) {
    if (p.lookingAtIdentEq("nullptr") || p.lookingAtIdentEq("NULL"))
      p.fatal(p.tokenString(), " not allowed for global dimensions");
    loc at = p.nextLoc();
    parseDimensionExpressions(p, s, ds.global_size);
    if (p.consumeIf(COMMA)) {
      if (!p.consumeIfIdentEq("nullptr") && !p.consumeIfIdentEq("NULL"))
        parseDimensionExpressions(p, s, ds.local_size);
    }
    if (!ds.local_size.empty() &&
      ds.global_size.size() != ds.local_size.size())
    {
      at.extend_to(p.nextLoc());
      p.fatalAt(at, "global and local sizes have different dimensions");
    }
    p.consume(RANGLE);
  } // end dimension part <...>
}

// #1`path/foo.cl`kernel<1024x1024>(0:rw,1:r,33) where X = ..., Y = ...
//                                 ^^^^^^^^^^^^^
static void parseDispatchStatementArguments(
  parser &p,
  script &s,
  dispatch_spec &ds)
{
  p.consume(LPAREN);
  while (!p.lookingAt(RPAREN)) {
    init_spec *is = parseInit(p,s);
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
    if (!p.consumeIf(COMMA))
      break;
  }
  p.consume(RPAREN);
  ds.defined_at.extend_to(p.nextLoc());
}

// #1`path/foo.cl`kernel<1024x1024>(0:rw,1:r,33) where X = ..., Y = ...
//                                               ^^^^^^^^^^^^^^^^^^^^^^
static void parseDispatchStatementWhereClause(
  parser &p,
  script &s,
  dispatch_spec &ds,
  let_spec *enclosing_let)
{
  auto hasParam =
    [&] (std::string nm) {
      if (enclosing_let)
        for (const std::string &arg : enclosing_let->params)
          if (arg == nm)
            return true;
      return false;
    };

  // resolve references after the where clause
  if (p.consumeIfIdentEq("where")) {
    while (p.lookingAt(IDENT)) {
      auto loc = p.nextLoc();
      auto name = p.consumeIdent("variable name");

      if (hasParam(name)) {
        p.fatalAt(loc,"where binding shadows let parameter");
      }
      auto itr = s.let_bindings.find(name);
      if (itr != s.let_bindings.end()) {
        p.warningAt(loc,
          "where binding shadows let binding (from line ",
          itr->second->defined_at.line, ")");
      }
      for (auto w : ds.where_bindings)
        if (std::get<0>(w) == name)
          p.fatalAt(loc,"repeated where binding name");
      p.consume(EQ);
      init_spec *i = parseInit(p,s);
      i->defined_at.extend_to(p.nextLoc());
      ds.where_bindings.emplace_back(name,i);
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
        p.warningAt(loc, "where binding never used");
      }
      if (p.lookingAtSeq(COMMA,IDENT)) {
        p.skip();
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
          p.fatalAt(ds.arguments[ai].defined_at,"undefined symbol");
        }
        let_spec *ls = itr->second;
        if (ls->value->skind != spec::INIT_SPEC) {
          p.fatalAt(ds.arguments[ai].defined_at,
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
// #1#a`path/foo.cl
static program_spec *parseDispatchStatementDeviceProgramPart(
  parser &p,
  script &s)
{
  device_spec dev(p.nextLoc());
  if (p.consumeIf(HASH)) {
    if (p.lookingAt(STRLIT) || p.lookingAt(IDENT)) {
      if (p.lookingAt(STRLIT)) {
        dev.setSource(p.tokenStringLiteral());
      } else {
        dev.setSource(p.tokenString());
      }
      p.skip();
    } else if (p.lookingAtInt()) {
      dev.setSource(p.consumeIntegral<int32_t>("device index (integer)"));
    } else {
      p.fatal("invalid device specification");
    }
    if (p.consumeIf(HASH)) {
      dev.instance = p.consumeIdent("instance identifier");
    }
    dev.defined_at.extend_to(p.nextLoc());
    if (!p.lookingAt(BACKTICK))
      p.fatal("expected ` (dispatch program separator) or "
        "# (instance identifier)");
    p.consume(BACKTICK);
  } else {
    dev.skind = device_spec::BY_DEFAULT;
  }

  // #1`path/foo.cl`kernel<1024x1024,16x16>(...)
  //    ^^^^^^^^^^^
  // #GTX`"foo/spaces in path/prog.cl"`kernel<...>(...)
  //      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  program_spec *ps = new program_spec(p.nextLoc());
  ps->device = dev;
  if (p.lookingAt(STRLIT)) {
    ps->path = p.tokenStringLiteral(); p.skip();
  } else {
    ps->path = consumeToChar(p, "[`");
  }

  // #1`path/foo.cl[-DTYPE=int]`kernel<1024x1024,16x16>(...)
  //               ^^^^^^^^^^^^
  if (p.consumeIf(LBRACK)) {
    ps->build_options = consumeToChar(p, "]");
    p.consume(RBRACK);
  }
  ps->defined_at.extend_to(p.nextLoc());

  return ps;
}

static kernel_spec *parseDispatchStatementKernelPart(
  program_spec *ps, parser &p, script &s)
{
  // #1`path/foo.cl`kernel<1024x1024,16x16>(...)
  //                ^^^^^^
  kernel_spec *ks = new kernel_spec(ps);
  if (p.lookingAt(IDENT)) {
    ks->name = p.tokenString(); p.skip();
  }
  ks->defined_at.extend_to(p.nextLoc());
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
static bool lookingAtImmediateDispatchStatement(parser &p) {
    if (p.lookingAt(HASH))
      return true;
    if (p.lookingAtIdentEq("let"))
      return false; // let foo = ...;
    if (p.lookingAtSeq(IDENT, LPAREN))
      return false; // e.g. seq(...); print(...)
    int i = 1;
    while (i < (int)p.tokensLeft()) {
      if (p.lookingAt(BACKTICK,i) || // correct dispatch
        p.lookingAt(LANGLE,i) || // malformed dispatch   foo.cl`bar<1024>(...)
                                 //                            ^
        p.lookingAt(LBRACK,i)) // malformed dispatchd    foo.cl[...]`bar(...)
                               //                              ^
      {
        return true;
      } else if (p.lookingAt(NEWLINE,i) || // malformed statement
        p.lookingAt(SEMI,i) || // malformed statement    .....; ....
        p.lookingAt(EQ,i)) // malformed let possibly     foo=BAR
      {
        break;
      }
      i++;
    }
    return false;
}

static dispatch_spec *parseDispatchStatement(parser &p, script &s)
{
  auto loc = p.nextLoc();
  dispatch_spec *ds = new dispatch_spec(loc);
  if (lookingAtImmediateDispatchStatement(p)) {
    if (p.lookingAtSeq(IDENT,RANGLE)) {
      // named kernel invocation
      // KERNEL<...>(...)
      ds->kernel =
        dereferenceLet<kernel_spec>(p,s,spec::KERNEL_SPEC,"kernel");
    } else if (p.lookingAtSeq(IDENT,BACKTICK,IDENT,LANGLE)) {
      // PROG`kernel<...>(...)
      // 000012222223...
      refable<program_spec> ps =
        dereferenceLet<program_spec>(p,s,spec::PROGRAM_SPEC,"program");
      p.consume(BACKTICK);
      std::string kernel_name = p.consumeIdent("kernel name");
      ds->kernel = new kernel_spec(ps);
    } else {
      // FULLY immediate dispatch
      //
      // #1`path/foo.cl[-cl-opt]`kernel<1024x1024,16x16>(...)
      // ^^^^^^^^^^^^^^^^^^^^^^^
      program_spec *ps = parseDispatchStatementDeviceProgramPart(p,s);
      // #1`path/foo.cl[-cl-opt]`kernel<1024x1024,16x16>(...)
      //                        ^
      p.consume(BACKTICK);
      // #1`path/foo.cl`kernel<1024x1024,16x16>(...)
      //                ^^^^^^
      ds->kernel = parseDispatchStatementKernelPart(ps, p, s);
    }
  } else {
    p.fatal("expected statement");
  }

  // #1`path/foo.cl`kernel<1024x1024,16x16>(...)
  //                      ^^^^^^^^^^^^^^^^^
  parseDispatchStatementDimensions(p,s,*ds);

  // #1`path/foo.cl`kernel<1024x1024>(0:rw,1:r,33) where ...
  //                                 ^^^^^^^^^^^^^^^^^^^^^^^
  parseDispatchStatementArguments(p,s,*ds);
  // #1`path/foo.cl`kernel<1024x1024>(0:rw,1:r,33) where X = ..., Y = ...
  //                                               ^^^^^^^^^^^^^^^^^^^^^^
  parseDispatchStatementWhereClause(p,s,*ds,nullptr);
  ds->defined_at.extend_to(p.nextLoc());

  return ds;
}

static init_spec_symbol *parseSymbol(parser &p)
{
  auto loc = p.nextLoc();
  if (p.lookingAtIdent()) {
    auto ident = p.tokenString();
    return new init_spec_symbol(loc, ident);
  } else {
    p.fatal("expected identifier");
    return nullptr;
  }
}

// Given an arugment; we resolve symbol the symbol to a let target (or fail)
static refable<init_spec> parseInitResolved(parser &p, script &s)
{
  init_spec *is = parseInit(p,s);
  if (is->skind == init_spec::IS_SYM) {
    // make a reference argument
    auto itr = s.let_bindings.find(((const init_spec_symbol *)is)->identifier);
    if (itr == s.let_bindings.end()) {
      p.fatalAt(is->defined_at,"unbound identifier");
    } else if (itr->second->value->skind != spec::INIT_SPEC ||
      ((init_spec *)itr->second->value)->skind != init_spec::IS_MEM)
    {
      p.fatalAt(is->defined_at,"identifier does not reference a memory object");
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


// EXAMPLES
// barrier
// diff(X,Y) | diff<float>(X,Y)
// print(X) | print<float>(X)
// save(sym,X)
static bool parseBuiltIn(parser &p, script &s)
{
  auto loc = p.nextLoc();
  if (p.lookingAtIdentEq("barrier")) {
      s.statement_list.statements.push_back(new barrier_spec(loc));
      p.skip();
      if (p.consumeIf(LPAREN)) // optional ()
        p.consume(RPAREN);
    s.statement_list.statements.back()->defined_at.extend_to(p.nextLoc());
    return true;
  } else if (p.lookingAtIdentEq("diff")) {
    p.skip();
    const type *elem_type = nullptr;
    if (p.consumeIf(LANGLE)) {
      auto at = p.nextLoc();
      // HACK: need to know the pointer size; assume 64b for now
      // technically we should probably just save the string and deal with
      // it during compilation once devices are matched etc...
      const size_t HACK = 8;
      elem_type = lookupBuiltinType(p.consumeIdent("type name"),HACK);
      p.consume(RANGLE);
    }
    p.consume(LPAREN);
    auto ref = parseInitResolved(p,s);
    p.consume(COMMA);
    if (!p.lookingAt(IDENT))
      p.fatal("expected reference to memory object");
    refable<init_spec_mem> r_sut = dereferenceLetMem(p, s);
    p.consume(RPAREN);
    loc.extend_to(p.nextLoc());
    s.statement_list.statements.push_back(
      new diff_spec(loc, ref, r_sut, elem_type));
    return true;
  } else if (p.lookingAtIdentEq("print")) {
    // print(X)
    // print<TYPE>(X)
    // print<INT>(X)
    // print<TYPE,INT>(X)
    p.skip();
    const type *elem_type = nullptr;
    int elems_per_row = 0;
    if (p.consumeIf(LANGLE)) {
      if (p.lookingAtInt()) {
        elems_per_row = p.consumeIntegral<int>("elements per column");
      } else {
        // HACK: we can't know the bits per ptr until we unify the
        // arguments with the owning context.
        const size_t HACK = 8;
        elem_type = lookupBuiltinType(p.consumeIdent("type name"),HACK);
        if (p.consumeIf(COMMA)) {
          elems_per_row = p.consumeIntegral<int>("elements per column");
        }
      }
      p.consume(RANGLE);
    }
    p.consume(LPAREN);
    if (!p.lookingAt(IDENT))
      p.fatal("expected reference to memory object");
    refable<init_spec_mem> r_surf = dereferenceLetMem(p,s);
    p.consume(RPAREN);
    loc.extend_to(p.nextLoc());
    s.statement_list.statements.push_back(
      new print_spec(loc, r_surf, elem_type, elems_per_row));
    return true;
  } else if (p.lookingAtIdentEq("save")) {
    p.skip();
    p.consume(LPAREN);
    if (!p.lookingAt(STRLIT))
      p.fatal("expected file name (string literal)");
    std::string file = p.tokenStringLiteral();
    p.skip();
    p.consume(COMMA);
    refable<init_spec_mem> r_surf = dereferenceLetMem(p,s);
    p.consume(RPAREN);
    loc.extend_to(p.nextLoc());
    s.statement_list.statements.push_back(new save_spec(loc, file, r_surf));
    return true;
  } else {
    return false;
  }
}

// let X=...
static void parseLetStatement(parser &p, script &s)
{
  auto let_loc = p.nextLoc();
  p.skip();      // let
  auto name = p.tokenString(); // X
  if (s.let_bindings.find(name) != s.let_bindings.end()) {
    p.fatal(name, ": redefinition of let binding");
  }
  p.skip();      // X
  let_spec *ls = new let_spec(let_loc, name);
  if (p.consumeIf(LPAREN)) {
    // let F(X,Y) = #1`foo.cl`bar<...>(X,Y)
    if (!p.lookingAt(RPAREN)) {
      do {
        ls->params.push_back(p.consumeIdent());
      } while(p.consumeIf(COMMA));
      p.consume(RPAREN);
      p.fatal("let arguments not supported yet");
    }
  }
  p.consume(EQ); // =

  spec *value = nullptr;
  loc value_loc = p.nextLoc();
  bool is_init_expr_start =
    p.lookingAtFloat() ||
    p.lookingAtInt() ||
    p.lookingAt(LPAREN) ||
    p.lookingAtIdentEq("seq") ||
    p.lookingAtIdentEq("random") ||
    p.lookingAtIdentEq("file");
  if (!is_init_expr_start && p.lookingAtSeq(IDENT,LANGLE)) {
    // let D = K<1024>(....) where ...
    dispatch_spec *ds = new dispatch_spec(value_loc);
    ds->kernel = dereferenceLet<kernel_spec>(p,s,spec::KERNEL_SPEC,"kernel");
    parseDispatchStatementDimensions(p,s,*ds);
    parseDispatchStatementArguments(p,s,*ds);
    parseDispatchStatementWhereClause(p,s,*ds,ls);
    ds->defined_at.extend_to(p.nextLoc());
    value = ds;
  } else if (!is_init_expr_start && p.lookingAtSeq(IDENT,BACKTICK,IDENT,LANGLE)) {
    // let D = P`kernel<...>(...) where ...
    dispatch_spec *ds = new dispatch_spec(value_loc);
    refable<program_spec> rps =
      dereferenceLet<program_spec>(p,s,spec::PROGRAM_SPEC,"programs");
    p.consume(BACKTICK);
    ds->kernel = new kernel_spec(rps.value);
    parseDispatchStatementDimensions(p,s,*ds);
    parseDispatchStatementArguments(p,s,*ds);
    parseDispatchStatementWhereClause(p,s,*ds,ls);
    ds->defined_at.extend_to(p.nextLoc());
    value = ds;
  } else if (!is_init_expr_start && lookingAtImmediateDispatchStatement(p)) {
    // let P = #1`foo/bar.cl
    // let K = foo/bar.cl`kernel
    // let D = foo/bar.cl`kernel<...>(...) where ...
    program_spec *ps = parseDispatchStatementDeviceProgramPart(p,s);
    if (p.consumeIf(BACKTICK)) {
      // includes the kernel
      kernel_spec *ks = parseDispatchStatementKernelPart(ps, p, s);
      ks->program = ps;
      if (p.lookingAt(LANGLE)) {
        // let D = ...<...>(...) where ...
        dispatch_spec *ds = new dispatch_spec(value_loc);
        ds->kernel = ks;
        parseDispatchStatementDimensions(p,s,*ds);
        parseDispatchStatementArguments(p,s,*ds);
        parseDispatchStatementWhereClause(p,s,*ds,ls);
        ds->defined_at.extend_to(p.nextLoc());
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
    value = parseInit(p,s);
  }
  ls->value = value;
  s.let_bindings[name] = ls;
  s.statement_list.statements.push_back(ls);
  s.statement_list.statements.back()->defined_at.extend_to(p.nextLoc());
}

static void parseStatementLine(parser &p, script &s)
{
  if (p.lookingAtIdentEq("let") && p.lookingAt(IDENT,1)) {
    // let X = ...
    // let F(X,Y) = ...
    parseLetStatement(p,s);
  } else if (parseBuiltIn(p,s)) {
    // barrier
    // save('foo.bin',A);
    ;
  } else {
    // #1`foo/bar.cl
    s.statement_list.statements.emplace_back(parseDispatchStatement(p,s));
  }
}

void cls::parse_script(
  const opts &os,
  const std::string &input,
  const std::string &filename,
  script &s,
  warning_list &wl)
{
  parser p(input);
  while (p.consumeIf(NEWLINE))
    ;
  while (!p.endOfFile()) {
    parseStatementLine(p,s); // S ((<NEWLINE> | ';') S)*
    if (p.consumeIf(SEMI)) { // ';' S
      parseStatementLine(p, s);
    } else if (!p.endOfFile()) { // '<NEWLINE>' S
      p.consume(NEWLINE);
      while (p.consumeIf(NEWLINE))
       ;
    }
  }
  wl = p.warnings();
}