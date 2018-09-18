#include "cls_parser.hpp"
#include "parser.hpp"
#include "../cls.hpp"
// TODO: remove (since we now use cl2.hpp
#include "../svm.hpp"
#include "../system.hpp"

#include <sstream>
#include <iostream>
// #include <filesystem>
// using namespace std::tr2::sys;
// namespace fs = std::tr2::sys;
// #include <experimental/filesystem>
// namespace fs = std::experimental::filesystem;

#if 0


std::string arg::toSyntax() const {
  std::stringstream ss;
  if (has_const) {
    ss << "const ";
  }
  switch (addr_qual) {
  case CL_KERNEL_ARG_ADDRESS_GLOBAL:
    ss << "global ";  break;
  case CL_KERNEL_ARG_ADDRESS_LOCAL:
    ss << "local ";  break;
  case CL_KERNEL_ARG_ADDRESS_CONSTANT:
    ss << "constant ";  break;
  case CL_KERNEL_ARG_ADDRESS_PRIVATE:
    break;
  }
  switch (accs_qual) {
  case CL_KERNEL_ARG_ACCESS_READ_ONLY:
    ss << "read_only "; break;
  case CL_KERNEL_ARG_ACCESS_WRITE_ONLY:
    ss << "write_only "; break;
  case CL_KERNEL_ARG_ACCESS_READ_WRITE:
    ss << "read_write "; break;
  case CL_KERNEL_ARG_ACCESS_NONE:
    break;
  }
  ss << type_name;
  if (has_restrict) {
    ss << " restrict";
  }
  ss << " " << name;
  return ss.str();
}

// used to parse raw opencl code
struct CLParser : parser {
  ndr &clc;
  const size_t device_ptr_size;
  CLParser(ndr &_clc, size_t ptr_size)
    : parser(_clc.prg.source), clc(_clc), device_ptr_size(ptr_size)
  {
  }

  void parse() {
    while (!endOfFile()) {
      findNextKernel();
    }
  }
  void findNextKernel() {
    // TODO: handle preprocessor line adjustments so we can report good
    // errors on pre-processed results
    // e.g.
    // # 1 "<built-in>" 2
    // # 1 "..\\..\\..\\tests\\pp.cl" 2
    while (!lookingAtIdent("__kernel","kernel")) {
      if (lookingAt(END_OF_FILE)) {
        return;
      }
      skip(); // skip until kernel
    }

    loc kloc = nextLoc();
    clc.prg.kernels.emplace_back(kloc,&clc.prg);
    kernel &k = clc.prg.kernels.back();
    skip(); // kernel
    consumeIdentAs("void","void");
    k.name = consumeIdentStringAs("kernel name");
    consume(LPAREN);
    if (!lookingAt(RPAREN)) {
      parseArg(k);
      while (consumeIf(COMMA)) {
        parseArg(k);
      }
    }
    consume(RPAREN);
  }

  void parseArg(kernel &k) {
    k.args.emplace_back(nextLoc());
    arg &ka = k.args.back();

    ka.has_const = consumeIfIdent("const");

    if (consumeIfIdent("global","__global")) {
      ka.addr_qual = CL_KERNEL_ARG_ADDRESS_GLOBAL;
    } else if (consumeIfIdent("constant","__constant")) {
      ka.addr_qual = CL_KERNEL_ARG_ADDRESS_CONSTANT;
    } else if (consumeIfIdent("local","__local")) {
      ka.addr_qual = CL_KERNEL_ARG_ADDRESS_LOCAL;
    } else if (consumeIfIdent("private","__private")) {
      ka.addr_qual = CL_KERNEL_ARG_ADDRESS_LOCAL;
    } else {
      ka.addr_qual = CL_KERNEL_ARG_ADDRESS_PRIVATE;
    }

    ka.has_const |= consumeIfIdent("const");

    if (consumeIfIdent("read_only") || consumeIfIdent("__read_only")) {
      ka.accs_qual = CL_KERNEL_ARG_ACCESS_READ_ONLY;
    } else if (consumeIfIdent("write_only") || consumeIfIdent("__write_only")) {
      ka.accs_qual = CL_KERNEL_ARG_ACCESS_WRITE_ONLY;
    } else if (consumeIfIdent("read_write") || consumeIfIdent("__read_write")) {
      ka.accs_qual = CL_KERNEL_ARG_ACCESS_READ_WRITE;
    } else {
      ka.accs_qual = CL_KERNEL_ARG_ACCESS_NONE;
    }

    bool pointer = false;
    ka.elem_name = consumeIdentStringAs("qualifier or type");
    bool explicit_signed_or_unsigned = false;
    if (ka.elem_name == "unsigned" || ka.elem_name == "signed") {
      explicit_signed_or_unsigned = true;
      std::string type = consumeIdentStringAs("type");
      ka.elem_name += ' ';
      ka.elem_name += type;
    }

    ka.elem_class = INVALID;
    ka.vec_width = 1;
    ka.elem_signed = false;
    if (ka.elem_name == "bool") {
      ka.elem_class = OTHER;
    } else if (ka.elem_name == "char" ||
      ka.elem_name == "uchar" ||
      ka.elem_name == "unsigned char")
    {
      ka.elem_class = INTEGRAL;
      ka.elem_size = 1;
      ka.elem_signed = ka.elem_name[0] != 'u';
    } else if (ka.elem_name == "short" ||
      ka.elem_name == "ushort" ||
      ka.elem_name == "unsigned short")
    {
      ka.elem_class = INTEGRAL;
      ka.elem_size = 2;
      ka.elem_signed = ka.elem_name[0] != 'u';
    } else if (ka.elem_name == "int" ||
      ka.elem_name == "uint" ||
      ka.elem_name == "unsigned int")
    {
      ka.elem_class = INTEGRAL;
      ka.elem_size = 4;
      ka.elem_signed = ka.elem_name[0] != 'u';
    } else if (ka.elem_name == "long" ||
      ka.elem_name == "ulong" ||
      ka.elem_name == "unsigned long")
    {
      ka.elem_class = INTEGRAL;
      ka.elem_size = 8;
      ka.elem_signed = ka.elem_name[0] != 'u';
    } else if (ka.elem_name == "float") {
      ka.elem_class = FLOATING;
      ka.elem_size = 4;
    } else if (ka.elem_name == "double") {
      ka.elem_class = FLOATING;
      ka.elem_size = 8;
    } else if (ka.elem_name == "half") {
      ka.elem_class = FLOATING;
      ka.elem_size = 2;
    } else if (ka.elem_name == "size_t" ||
      ka.elem_name == "ptrdiff_t" ||
      ka.elem_name == "intptr_t" ||
      ka.elem_name == "uintptr_t")
    {
      ka.elem_class = INTEGRAL;
      ka.elem_size = device_ptr_size;
      ka.elem_signed =
        ka.elem_name[0] != 's' && ka.elem_name[0] != 'u';
    } else if (ka.elem_name == "void") {
      ka.elem_class = OTHER;
      ka.elem_size = 0;
    } else if (ka.elem_name == "image2d_t") {
      ka.elem_class = IMAGE2D;
      ka.elem_size = 0;
    } else if (ka.elem_name == "image3d_t") {
      ka.elem_class = IMAGE3D;
      ka.elem_size = 0;
    } else if (ka.elem_name == "image2d_array_t") {
      ka.elem_class = IMAGE2D_ARRAY;
      ka.elem_size = 0;
    } else if (ka.elem_name == "image1d_t") {
      ka.elem_class = IMAGE1D;
      ka.elem_size = 0;
    } else if (ka.elem_name == "image1d_buffer_t") {
      ka.elem_class = IMAGE1D_BUFFER;
      ka.elem_size = 0;
    } else if (ka.elem_name == "image1d_array_t") {
      ka.elem_class = IMAGE1D_ARRAY;
      ka.elem_size = 0;
    } else if (ka.elem_name == "sampler_t") {
      ka.elem_class = SAMPLER;
      ka.elem_size = sizeof(cl_sampler);
    } else if (ka.elem_name == "event_t") {
      ka.elem_class = EVENT;
      ka.elem_size = sizeof(cl_event);
    } else {
      // Try it as a vector type (e.g. uint4)
      if (!explicit_signed_or_unsigned) {
        std::string vectype;
        size_t toff = 0;
        while (toff < ka.elem_name.size() && isalpha(ka.elem_name[toff])) {
          vectype += ka.elem_name[toff++];
        }
        size_t width = 0;
        while (toff < ka.elem_name.size() && isdigit(ka.elem_name[toff])) {
          width = width * 10 + ka.elem_name[toff++] - '0';
        }

        ka.elem_size = 0;
        bool is_vec_type = true;
        if (width == 2 || width == 3 || width == 4 || width == 8 || width == 16) {
          if (vectype == "char" || vectype == "uchar") {
            ka.elem_class = INTEGRAL;
            ka.type_class = VECTOR;
            ka.elem_size = 1;
          } else if (vectype == "short" || vectype == "ushort") {
            ka.elem_class = INTEGRAL;
            ka.elem_size = 2;
          } else if (vectype == "int" || vectype == "uint") {
            ka.elem_class = INTEGRAL;
            ka.elem_size = 4;
          } else if (vectype == "long" || vectype == "ulong") {
            ka.elem_class = INTEGRAL;
            ka.elem_size = 8;
          } else if (vectype == "half") {
            ka.elem_class = FLOATING;
            ka.elem_size = 2;
          } else if (vectype == "float") {
            ka.elem_class = FLOATING;
            ka.elem_size = 4;
          } else if (vectype == "double") {
            ka.elem_class = FLOATING;
            ka.elem_size = 8;
          } else {
            // some other type (e.g. foo8)
            ka.elem_class = STRUCT;
            ka.elem_size = 0;
            is_vec_type = false;
            fatalAt(ka.location, "user struct type unsupported");
          }
        } else {
          // user-define struct type because of invalid vec size E.g. "uint7"
          ka.elem_size = 0;
          is_vec_type = false;
          fatalAt(ka.location, "user struct type unsupported");
        }
        if (is_vec_type) {
          ka.type_class = VECTOR;
          if (sizeof(cl_uint3) == sizeof(cl_uint4) && width == 3)
            width = 4; // vec3 == really 4 wide
          ka.elem_signed = vectype[0] != 'u';
          ka.vec_width = width;
        } else {
          ka.type_class = STRUCT;
          ka.vec_width = 1;
        }
      } // vector type
    } // else: type
    ka.type_class = ka.elem_class;
    ka.type_name = ka.elem_name;

    if (consumeIf(MUL)) { // *
      pointer = true;
      ka.type_name += '*';
      ka.type_class = BUFFER;
    } // if '*'

    ka.name = consumeIdentStringAs("argument name");
    // global const uchar4 *restrict input
    if (ka.name == "restrict") {
      ka.has_restrict = true;
      ka.name = consumeIdentStringAs("argument name");
    } else {
      ka.has_restrict = false;
    }

    if (ka.elem_size == 0 && ka.type_class != IMAGE2D) {
      fatalAt(ka.location, "INTERNAL ERROR: unable to parse non-zero type size");
    }
  } // parseArg
};


static const char *FindClangExe()
{
  static const char *paths [] = {
    "C:\\Progra~2\\LLVM\\bin\\clang.exe", // 32-bit Program Files (x86)
    "C:\\Progra~1\\LLVM\\bin\\clang.exe", // 64-bit
  };
  static const char *good_clang;
  if (good_clang == nullptr) {
    for (size_t i = 0; i < sizeof(paths)/sizeof(paths[0]); i++) {
      if (sys::file_exists(paths[i])) {
        good_clang = paths[i];
        break;
      }
    }
    if (good_clang == nullptr) {
      good_clang = "clang";
    }
  }

  return good_clang;
}

#if 0
std::string PreProcessPath(
  const std::string &inp_path,
  const std::string &args)
{
  // use clang.exe
  // text::ReadFileToString
  std::string temp_out_path = sys::get_temp_path("pp.cl");
  auto clang_exe = FindClangExe();
  std::stringstream ss;
  ss << clang_exe << " -E -o " <<
    temp_out_path << " " << args << " " << inp_path;
  int e = system(ss.str().c_str());
  std::string outp;
  if (e != 0) {
    WARNING("PreProcessPath: unable to preprocess with clang.exe\n");
    // punt, and just return the .cl contents
    outp = sys::read_file_text(inp_path);
  } else {
    outp = sys::read_file_text(temp_out_path);
  }
  (void)remove(temp_out_path.c_str());
  return outp;
}
#endif


// used to parse cl scripts
struct CLSParser : parser {
  ndr *clc = nullptr;
  const cl::Device &device;
  cl::Context &context;

  CLSParser(cl::Context &c, const cl::Device &d, const std::string &inp)
    : parser(inp), context(c), device(d)
  { }

  // foo-bar/baz.cl[-DT=int]`kernel<...>(...)
  // baz.bin`kernel<...>(...)
  ndr *parse() {
    clc = nullptr;
    try {
      auto cl_call_start = nextLoc();
      clc = new ndr;
      // cl_call_start
      clc->source = input();
      fs::path prog_path = parsePath(BACKTICK);
      clc->prg.build_opts = parseOpts();
      consume(BACKTICK);
      auto kLoc = nextLoc();
      auto knm = consumeIdentStringAs("kernel name");
      loadProgram(cl_call_start, *clc, prog_path, kLoc, knm);

      // dimension
      consume(LANGLE);
      clc->global_size = parseNDRange();
      if (consumeIf(COMMA)) {
        if (consumeIfIdent("nullptr") ||
          consumeIfIdent("NULL"))
        {
          clc->local_size = cl::NullRange;
        } else {
          clc->local_size = parseNDRange();
        }
      } else {
        clc->local_size = cl::NullRange;
      }
      consume(RANGLE);
      // arguments
      consume(LPAREN);
      int arg_ix = 0;
      if (!lookingAt(RPAREN)) {
        parseInit(*clc, arg_ix++);
        while (consumeIf(COMMA)) {
          parseInit(*clc, arg_ix++);
        }
      }
      consume(RPAREN);

      if (clc->inits.size() < clc->entry->args.size()) {
        fatal("expected more initializers");
      } else if(clc->inits.size() > clc->entry->args.size()) {
        fatal("too many initializers for kernel arguments");
      } // else: just right

      setKernelArgs(*clc);
    } catch (...) {
      if (clc) {
        delete clc;
        clc = nullptr;
      }
      throw;
    }
    return clc;
  }
 private:
  void loadProgram(
    loc ndr_loc,
    ndr &clc,
    const fs::path &prog_path,
    loc kLoc,
    const std::string &kName)
  {
    auto st = fs::status(prog_path);
    if (!fs::exists(st)) {
      fatalAt(ndr_loc, prog_path.string(), ": file not found");
    } else if (!is_regular_file(st)) {
      fatalAt(ndr_loc, prog_path.string(), ": not a regular file");
    }
    clc.prg.source = text::load_c_preprocessed(
      prog_path.string(),
      filterDefines(clc.prg.build_opts));
    try {
      CLParser p(clc, device.getInfo<CL_DEVICE_ADDRESS_BITS>()/8);
      p.parse();
      clc.prg.cl_program = new cl::Program(context, clc.prg.source);
      try {
        clc.prg.cl_program->build(clc.prg.build_opts.c_str());
        auto log = clc.prg.cl_program->getBuildInfo<CL_PROGRAM_BUILD_LOG>(device);
        VERBOSE("BUILD LOG:\n%s\n",log.c_str());
      } catch (const cl::Error &) {
        auto log = clc.prg.cl_program->getBuildInfo<CL_PROGRAM_BUILD_LOG>(device);
        fatalAt(ndr_loc, "failed to compile program:\n", log);
      }
      clc.prg.cl_program->createKernels(&clc.prg.cl_kernels);
      for (auto &k : clc.prg.kernels) {
        bool found = false;
        for (auto &cl_k : clc.prg.cl_kernels) {
          std::string knm = cl_k.getInfo<CL_KERNEL_FUNCTION_NAME>().c_str(); // WA for cl.hpp bug
          if (knm == k.name) {
            found = true;
            k.cl_kernel = &cl_k;
            if (knm == kName) {
              clc.entry = &k;
            }
            break;
          }
        }
        if (!found) {
          fatalAt(kLoc, "could not find kernel ", k.name, " in program");
        }
      }
    } catch (const diag &d) {
      std::cerr << d.toString() << "\n";
      fatalAt(ndr_loc, prog_path.string().c_str(), " error reading program");
    }
    if (!clc.entry) {
      fatalAt(kLoc, "could not find entry kernel ", kName);
    }
  }

  std::string filterDefines(const std::string &bos) const
  {
    std::stringstream ss;
    size_t i = 0, slen = bos.size();
    while (i < slen) {
      // -cl-fast-relaxed-math -D USE_UCHAR4 -D FILTER_RAD=2 -D FILTER_SIZE=5
      if (i < slen - 1 && bos[i] == '-' && bos[i+1] == 'D') {
        // -D USE_UCHAR4
        // or
        // -DFILTER_RAD=2
        if (ss.tellp() > 0) {
          ss << ' ';
        }
        ss << "-D";
        i += 2;
        while (i < slen && isspace(bos[i]))
          i++;
        while (i < slen && !isspace(bos[i]))
          ss << bos[i++];
      } else {
        i++;
      }
    }
    return ss.str();
  }

  // e.g. "foo-bar/baz.cl/"
  // or "../foo/bar/baz.cl"
  // or "/foo/bar/baz"
  std::string parsePath(cls::lexeme endLxm) {
    if (lookingAt(STRLIT)) {
      return tokenStringLiteral();
    } else {
      // must be looking at identifiers or .
      // ./foo/bar
      //
      auto start = nextLoc();
      auto end = nextLoc();
      consumeOneOf("program (path or identifier)",IDENT,DOT,DIV);
      while (!lookingAt(endLxm) && !lookingAt(LBRACK)) {
        if (endOfFile()) {
          fatalAt(start, "unable to find end of path: `");
        }
        skip();
        end = nextLoc();
      }
      return input().substr(start.offset, end.offset - start.offset);
    }
  }

  std::string parseOpts() {
    auto start = nextLoc();
    if (consumeIf(LBRACK)) {
      auto end = nextLoc();
      while (!consumeIf(RBRACK)) {
        if (endOfFile()) {
          fatalAt(start, "unclosed build options [");
        }
        skip();
        end = nextLoc();
      }
      auto opts = input().substr(
        (start.offset + 1),
        end.offset - start.offset - 1);
      return opts;
    } else {
      return "";
    }
  }
  // 1024x1024
  // 1024 x 1024
  cl::NDRange parseNDRange() {
    size_t dims[3] = {0,0,0};

    auto xDimStr = tokenString();
    consume(INTLIT10);
    dims[0] = atoi(xDimStr.c_str());

    int rank = 1;
    while (rank < 3) {
      if (consumeIfIdent("x") && lookingAt(INTLIT10)) {
        dims[rank++] = atoi(tokenString().c_str());
        skip();
        // 1024 x 1024
        //      ^ ident x
      } else if (lookingAt(IDENT)) {
        // could be 1024x1024
        //              ^^^^^
        // strip the x off and evaluate 1024
        std::string str = tokenString();
        skip();
        if (str.size() > 0 && str[0] == 'x') {
          for (size_t i = 1; i < str.size(); i++) {
            if (!isdigit(str[i])) {
              fatal("expected y dimention value");
            }
          }
          dims[rank++] = atoi(str.substr(1).c_str());
        } else {
          fatal("expected y dimention value");
        }
      } else {
        break;
      }
    } // while

    if (rank == 3) {
      return cl::NDRange(dims[0], dims[1], dims[2]);
    } else if (rank == 2) {
      return cl::NDRange(dims[0], dims[1]);
    } else {
      return cl::NDRange(dims[0]);
    }
  }


  // expr = term (('+'|'-') term)*
  // term = fact (('*'|'/'|'%') fact)*
  // fact = ('-')? prim
  // prim = ... see below ...
  //
  void parseInit(ndr &c, size_t arg_ix) {
    if (arg_ix >= c.entry->args.size()) {
      fatal("kernel prototype only defines ", c.entry->args.size(),
        " arguments");
    }
    arg &a = c.entry->args[arg_ix];
    init *i = parseExpr(c,a);
    c.inits.push_back(i);
    if (consumeIf(COLON)) {
      // :rwp
      auto location = nextLoc();
      if (consumeIf(LBRACK)) {
        // :[1024*1024]r
        //  ^
        auto sz = parseIntegralExpr(c);
        if (sz <= 0) {
          fatalAt(location, "memory size expression must be positive");
        }
        i->explicit_element_count = (size_t)sz;
        consume(RBRACK);
      }
      auto attrsLoc = nextLoc();
      if (consumeIf(IDENT)) {
        const char *astr = input().c_str() + attrsLoc.offset;
        for (size_t ci = 0; ci < attrsLoc.extent; ci++) {
          switch (astr[ci]) {
          case 'r':
            i->buffer_r = true;
            break;
          case 'w':
            i->buffer_w = true;
            break;
          case 'p':
            i->display_pre = true;
            if (ci + 1 < attrsLoc.extent && astr[ci + 1] == 'x') {
              ++ci;
              i->display_pst_as_hex = true;
            }
            break;
          case 'P':
            i->display_pst = true;
            if (ci + 1 < attrsLoc.extent && astr[ci + 1] == 'x') {
              ++ci;
              i->display_pre_as_hex = true;
            }
            break;
          case 'S':
            i->save_post = true;
            break;
          case 'm':
            i->transfer = init::TRANS_MAP;
            break;
          case 'c':
            i->transfer = init::TRANS_COPY;
            break;
          case 'v':
            i->transfer = init::TRANS_SVM;
            if (ci + 1 < attrsLoc.extent && astr[ci + 1] == 'f') {
              ci++;
              i->use_svm_fine_grained = true;
            }
            if (ci + 1 < attrsLoc.extent && astr[ci + 1] == 'a') {
              ci++;
              i->use_svm_atomics = true;
            }
            break;
          default:
            fatalAt(
              loc(attrsLoc.line,
                attrsLoc.column + (uint32_t)ci, attrsLoc.offset +  (uint32_t)ci, 1),
              "invalid buffer attribute");
          }  // for switch
        } // for attr chars
        if (i->transfer == init::TRANS_INVALID) {
          // default to map :m
          i->transfer = init::TRANS_MAP;
        }
      }
    }
  }
  int64_t parseIntegralExpr(const ndr &c) {
    auto e = parseIntegralTerm(c);
    while (true) {
      if (consumeIf(ADD)) {
        e += parseIntegralTerm(c);
      } else if (consumeIf(SUB)) {
        e -= parseIntegralTerm(c);
      } else {
        break;
      }
    }
    return e;
  }
  int64_t parseIntegralTerm(const ndr &c) {
    auto e = parseIntegralFactor(c);
    while (true) {
      auto loc = nextLoc();
      if (consumeIf(MUL)) {
        e *= parseIntegralTerm(c);
      } else if (consumeIf(DIV)) {
        auto rhs = parseIntegralFactor(c);
        if (rhs == 0) {
          fatalAt(loc, "division by 0");
        }
        e /= parseIntegralTerm(c);
      } else if (consumeIf(MOD)) {
        auto rhs = parseIntegralFactor(c);
        if (rhs == 0) {
          fatalAt(loc, "mod by 0");
        }
        e %= parseIntegralFactor(c);
      } else {
        break;
      }
    }
    return e;
  }
  int64_t parseIntegralFactor(const ndr &c) {
    bool neg = consumeIf(SUB);
    auto e = parseIntegralAtom(c);
    if (neg) {
      e = -e;
    }
    return e;
  }
  int64_t parseIntegralAtom(const ndr &c) {
    if (lookingAtInt()) {
      return consumeInt();
    } else if (consumeIfIdent("g")) {
      return parseNDRange(c.global_size);
    } else if (consumeIfIdent("l")) {
      return parseNDRange(c.local_size);
    } else {
      fatal("unbound symbol");
      return 0;
    }
  }
  int64_t parseNDRange(const cl::NDRange &ndr) {
    consume(DOT);
    auto loc = nextLoc();
    if (consumeIfIdent("x")) {
      if (ndr.dimensions() <= 0) {
        fatalAt(loc, "not enough dimensions for .x");
      }
      return (int64_t)ndr[0];
    } else if (consumeIfIdent("y")) {
      if (ndr.dimensions() <= 1) {
        fatalAt(loc, "not enough dimensions for .y");
      }
      return (int64_t)ndr[1];
    } else if (consumeIfIdent("z")) {
      if (ndr.dimensions() <= 2) {
        fatalAt(loc, "not enough dimensions for .z");
      }
      return (int64_t)ndr[2];
    } else {
      fatalAt(loc, "invalid dimension name for ndrange");
      return 0;
    }
  }
  void toFloat(init *ci) {
    if (ci->type == init::LIT_FLT) {
      return;
    }
    if (ci->type != init::LIT_INT) {
      fatalAt(ci->location, "cannot promote expression to floating point");
    }
    ci->type = init::LIT_FLT;
    ci->fltval = (double)ci->intval;
  }

  void toVector(init *ci, size_t elems) {
    for (size_t k = 0; k < elems; k++) {
      auto ptr = new init(ci->location);
      ptr->type = ci->type;
      if (ci->type == ci->LIT_FLT) {
        ptr->fltval = ci->fltval;
      } else if (ci->type == ci->LIT_INT) {
        ptr->intval = ci->intval;
      }
      ci->children.push_back(ptr);
    }
    ci->type = init::LIT_VEC;
  }

  void applyBinary(const token &op, init *lhs, init *rhs) {
    if (lhs->type == init::LIT_VEC || rhs->type == init::LIT_VEC) {
      // ensure both are vectors, promote if needed
      if (lhs->type != init::LIT_VEC) {
        toVector(lhs,rhs->children.size());
      } else if (rhs->type != init::LIT_VEC) {
        toVector(rhs,lhs->children.size());
      } else if (lhs->children.size() != rhs->children.size()) {
        fatalAt(op.loc, "vectors operands different sizes");
      } // else: length matches
      for (size_t i = 0; i < lhs->children.size(); i++) {
        applyBinary(op, lhs->children[i], rhs->children[i]);
      }
    }  else if (lhs->type == init::LIT_FLT || rhs->type == init::LIT_FLT) {
      // floating point scalar op
      toFloat(lhs);
      toFloat(rhs);
      switch (op.lexeme) {
      case ADD: lhs->fltval += rhs->fltval; break;
      case SUB: lhs->fltval -= rhs->fltval; break;
      case MUL: lhs->fltval *= rhs->fltval; break;
      case DIV: lhs->fltval /= rhs->fltval; break;
      case MOD: fatalAt(op.loc, "cannot apply mod to floating point operands");
      default:
        fatalAt(op.loc, "unsupported binary operator");
      }
    } else if (lhs->type == init::LIT_INT && rhs->type == init::LIT_INT) {
      switch (op.lexeme) {
      case ADD: lhs->intval += rhs->intval; break;
      case SUB: lhs->intval -= rhs->intval; break;
      case MUL: lhs->intval *= rhs->intval; break;
      case DIV:
        if (rhs->intval == 0) {
          fatalAt(op.loc, "division by zero");
        }
        lhs->intval /= rhs->intval;
        break;
      case MOD:
        if (rhs->intval == 0) {
          fatalAt(op.loc, "mod by zero");
        }
        lhs->intval %= rhs->intval;
        break;
      default:
        fatalAt(op.loc, "unsupported binary operator");
      }
    } else {
      // unsupported type
      fatalAt(op.loc, "unsupported types for operator");
    }
  }

  init *parseExpr(const ndr &c, arg &a) {
    init *lhs = parseTerm(c,a);
    while (true) {
      auto op = next();
      if (consumeIf(ADD) || consumeIf(SUB)) {
        init *rhs = parseTerm(c,a);
        applyBinary(op, lhs, rhs);
        delete rhs;
      } else {
        break;
      }
    }
    return lhs;
  }
  init *parseTerm(const ndr &c, arg &a) {
    init *lhs = parseFact(c,a);
    while (true) {
      auto op = next();
      if (consumeIf(MUL) || consumeIf(DIV) || consumeIf(MOD)) {
        init *rhs = parseFact(c,a);
        applyBinary(op, lhs, rhs);
        delete rhs;
      } else {
        break;
      }
    }
    return lhs;
  }
  init *parseFact(const ndr &c, arg &a) {
    auto loc = nextLoc();
    bool neg = consumeIf(SUB);
    init *e = parsePrim(c, a);
    if (neg) {
      init negOne(loc);
      negOne.intval = -1;
      negOne.type = init::LIT_INT;
      if (e->type == init::LIT_VEC) {
        // e.g. -(1,2) == (-1,-2)
        // need to treat this is -1 * (-1,-2)
        // so the type of -1 must be LIT_INT or LIT_FLT
        negOne.type = e->children.front()->type;
        if (negOne.type == init::LIT_FLT) {
          negOne.fltval = -1.0f;
        }
        toVector(&negOne, e->children.size());
      }
      token t(MUL, loc.line, loc.column, loc.offset, loc.extent);
      applyBinary(t, e, &negOne);
    }
    return e;
  }
  // prim = ilit
  //      | flit
  //      | '{' expr (',' expr)* '}'            -- struct
  //      | '(' expr ')'                        -- group
  //      | '(' expr (',' expr)* ')'            -- vector
  //      | func '(' (expr (',' expr)* )* ')'   -- function call
  //      | ('g'|'l') '.' ('x'|'y'|'z')         -- dimension
  init *parsePrim(const ndr &c, arg &a) {
    auto loc = nextLoc();
    if (lookingAtFloat()) {
      init *ci = new init(loc);
      ci->type = init::LIT_FLT;
      ci->fltval = consumeFloat();
      return ci;
    } else if (lookingAtInt()) {
      init *ci = new init(loc);
      ci->type = init::LIT_INT;
      ci->intval = consumeInt();
      return ci;
    } else if (consumeIf(LPAREN)) {
      // either grouping or vector
      init *ci_arg0 = parseExpr(c, a);
      if (consumeIf(RPAREN)) {
        // grouping expression
        return ci_arg0;
      } else if (!lookingAt(COMMA)) {
        fatal("expected , or )");
      }
      init *ci = new init(loc);
      ci->type = init::LIT_VEC;
      ci->children.push_back(ci_arg0);
      while (consumeIf(COMMA)) {
        ci->children.push_back(parseExpr(c, a));
      }
      consume(RPAREN);
      return ci;
    } else if (consumeIfIdent("g")) {
      init *ci = new init(loc);
      ci->type = init::LIT_INT;
      ci->intval = parseNDRange(c.global_size);
      return ci;
    } else if (consumeIfIdent("l")) {
      init *ci = new init(loc);
      ci->type = init::LIT_INT;
      ci->intval = parseNDRange(c.local_size);
      return ci;
    } else if (consumeIfIdent("rnd") || consumeIfIdent("rand")) {
      init *ci = new init(loc);
      ci->type = init::LIT_RND;
      if (consumeIf(LPAREN)) {
        if (lookingAtInt()) {
          ci->rand_seed = (int32_t)parseIntegralExpr(c);
        }
        consume(RPAREN);
      } else {
        ci->rand_seed = 0;
      }
      return ci;
    } else if (consumeIfIdent("cyc")) {
      init *ci = new init(loc);
      ci->type = init::LIT_CYC;
      consume(LPAREN);
      ci->cycle_values.push_back(parseIntegralExpr(c));
      while (consumeIf(COMMA)) {
        ci->cycle_values.push_back(parseIntegralExpr(c));
      }
      consume(RPAREN);
      return ci;
    } else if (consumeIfIdent("seq")) {
      init *ci = new init(loc);
      ci->type = init::LIT_SEQ;
      ci->seq_start = 0;
      ci->seq_delta = 1;
      if (consumeIf(LPAREN)) {
        ci->seq_start = parseIntegralExpr(c);
        if (consumeIf(COMMA)) {
          ci->seq_delta = parseIntegralExpr(c);
        }
        consume(RPAREN);
      }
      return ci;
    } else if (lookingAt(STRLIT)) {
      init *ci = new init(loc);
      ci->type = init::LIT_FILE;
      ci->fileval = parsePath(COLON);
      skip();
      return ci;
    } else {
      fatal("syntax error in argument initializer");
      return nullptr;
    }
  } // parsePrim

  void setKernelArgs(ndr &c) {
    kernel &k = *c.entry;
    for (cl_uint arg_ix = 0, len = (cl_uint)k.args.size();
      arg_ix < len;
      arg_ix++)
    {
      arg &a = k.args[arg_ix];
      init &i = *c.inits[arg_ix];
      ErrorHandler eh = [&](const char *msg) {
        fatalAt(i.location, "setting kernel arg: ", msg);
      };
      i.setKernelArg(c, k, a, arg_ix, &context, eh);
    } // for arg_ix
  } // setKernelArgs
};

ndr *cls::ParseCLS(
  cl::Context &ctx,
  const cl::Device &dev,
  const std::string& inp)
{
  CLSParser p(ctx, dev, inp);
  return p.parse();
}
#endif

// a hacky solution to enable use to read tokens including spaces
// e.g. `path/has spaces/baz.cl[-DTYPE=int -cl-some-option]`kernel
//       ^^^^^^^^^^^^^^^^^^^^^^
//                              ^^^^^^^^^^^^^^^^^^^^^^^^^^
static std::string consumeToChar(cls::parser &p, const char *set)
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

static cls::init_spec_atom *parseInitAtom(cls::parser &p);

static cls::init_spec_atom *parseInitAtomPrim(cls::parser &p)
{
  auto loc = p.nextLoc();
  if (p.lookingAt(STRLIT)) {
    auto s = p.tokenStringLiteral();
    p.skip();
    return new cls::init_spec_file(loc, s);
  } else if (p.lookingAtFloat()) {
    return new cls::init_spec_float(loc, p.consumeFloat());
  } else if (p.lookingAtInt()) {
    return new cls::init_spec_int(loc, p.consumeInt());
  } else if (p.lookingAtIdent()) {
    // e.g. "X" or "g.x"
    auto s = p.tokenString();
    p.skip();
    if (p.lookingAt(DOT)) {
      while (p.consumeIf(DOT)) {
        s += '.';
        if (p.lookingAt(IDENT)) {
          p.fatal("syntax error in initializer expression field access");
        }
        s += p.tokenString();
      }
      return new cls::init_spec_symbol(loc, s);
    } else if (p.lookingAt(LPAREN) || p.consumeIf(LANGLE)) {
      if (s == "random") {
        int64_t seed = 0;
        if (p.consumeIf(LANGLE)) {
          seed = p.consumeInt("seed (int)");
          p.consume(RANGLE);
        }
        auto func = new cls::init_spec_rng_generator(loc, seed);
        if (p.consumeIf(LBRACK)) {
          func->e_lo = parseInitAtom(p);
          if (p.consumeIf(COMMA))
            func->e_hi = parseInitAtom(p);
          p.consume(RBRACK);
        }
        return func;
      } else if (s == "seq") {
        p.fatal("parseInitAtomPrim: finish adding functions");
      } else {
        p.fatal("undefined function");
      }
      return nullptr; // unreachable
    } else {
      // regular symbol
      //
      // TODO: support E and PI
      return new cls::init_spec_symbol(loc, s);
    }
  } else if (p.consumeIf(LBRACK)) {
    auto re = new cls::init_spec_record(loc);
    if (!p.lookingAt(RPAREN))
      re->children.push_back(parseInitAtom(p));
    while (!p.lookingAt(COMMA))
      re->children.push_back(parseInitAtom(p));
    p.consume(RBRACK);
    return re;
  } else if (p.lookingAt(LPAREN)) {
    cls::init_spec_atom *e = parseInitAtom(p);
    p.consume(RPAREN);
    return e;
  } else {
    p.fatal("syntax error in initializer expression");
    return nullptr;
  }
}
static cls::init_spec_atom *parseInitAtomMul(cls::parser &p)
{
  cls::init_spec_atom *e = parseInitAtomPrim(p);
  auto loc = p.nextLoc();
  while (p.lookingAt(MUL) || p.lookingAt(DIV)) {
    auto op = p.lookingAt(MUL) ?
      cls::init_spec_bin_expr::bin_op::E_MUL :
      cls::init_spec_bin_expr::bin_op::E_DIV;
    p.skip();
    e = new cls::init_spec_bin_expr(loc, op, e, parseInitAtomPrim(p));
    loc = p.nextLoc();
  }
  return e;
}
static cls::init_spec_atom *parseInitAtomAdd(cls::parser &p)
{
  cls::init_spec_atom *e = parseInitAtomMul(p);
  auto loc = p.nextLoc();
  while (p.lookingAt(ADD) || p.lookingAt(SUB)) {
    auto op = p.lookingAt(ADD) ?
      cls::init_spec_bin_expr::bin_op::E_ADD :
      cls::init_spec_bin_expr::bin_op::E_SUB;
    p.skip();
    e = new cls::init_spec_bin_expr(loc, op, e, parseInitAtomMul(p));
    loc = p.nextLoc();
  }
  return e;
}
static cls::init_spec_atom *parseInitAtomShift(cls::parser &p)
{
  cls::init_spec_atom *e = parseInitAtomAdd(p);
  auto loc = p.nextLoc();
  while (p.lookingAt(LSH) || p.lookingAt(RSH)) {
    auto op = p.lookingAt(LSH) ?
      cls::init_spec_bin_expr::bin_op::E_LSH :
      cls::init_spec_bin_expr::bin_op::E_RSH;
    p.skip();
    e = new cls::init_spec_bin_expr(loc, op, e, parseInitAtomAdd(p));
    loc = p.nextLoc();
  }
  return e;
}
static cls::init_spec_atom *parseInitAtomBitwiseAND(cls::parser &p)
{
  cls::init_spec_atom *e = parseInitAtomShift(p);
  auto loc = p.nextLoc();
  while (p.consumeIf(AMP)) {
    e = new cls::init_spec_bin_expr(
      loc,
      cls::init_spec_bin_expr::bin_op::E_AND,
      e,
      parseInitAtomShift(p));
    loc = p.nextLoc();
  }
  return e;
}
static cls::init_spec_atom *parseInitAtomBitwiseXOR(cls::parser &p)
{
  cls::init_spec_atom *e = parseInitAtomBitwiseAND(p);
  auto loc = p.nextLoc();
  while (p.consumeIf(CIRC)) {
    e = new cls::init_spec_bin_expr(
      loc,
      cls::init_spec_bin_expr::bin_op::E_XOR,
      e,
      parseInitAtomBitwiseAND(p));
    loc = p.nextLoc();
  }
  return e;
}
static cls::init_spec_atom *parseInitAtomBitwiseOR(cls::parser &p)
{
  cls::init_spec_atom *e = parseInitAtomBitwiseXOR(p);
  auto loc = p.nextLoc();
  while (p.consumeIf(PIPE)) {
    e = new cls::init_spec_bin_expr(
      loc,
      cls::init_spec_bin_expr::bin_op::E_OR,
      e,
      parseInitAtomBitwiseXOR(p));
    loc = p.nextLoc();
  }
  return e;
}
static cls::init_spec_atom *parseInitAtom(cls::parser &p)
{
  return parseInitAtomBitwiseOR(p);
}

static cls::init_spec *parseInit(cls::parser &p)
{
  auto l = p.nextLoc();
  cls::init_spec_atom *e = parseInitAtom(p);
  if (p.consumeIf(COLON)) {
    // memory initializer
    cls::init_spec_memory *m = new cls::init_spec_memory(l);
    m->root = e;
    if (p.consumeIf(LBRACK)) {
      cls::init_spec_atom *de = parseInitAtom(p);
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
      auto setTx = [&] (cls::init_spec_memory::transfer t) {
        if (m->transfer_properties != cls::init_spec_memory::transfer::TX_INVALID) {
          p.fatalAt(l, "memory transfer respecification");
        }
        m->transfer_properties = t;
      };

      switch (s[i]) {
      case 'r':
        m->access_properties = (cls::init_spec_memory::access)(
          m->access_properties |
          cls::init_spec_memory::access::INIT_SPEC_MEM_READ);
        break;
      case 'w':
        m->access_properties = (cls::init_spec_memory::access)(
          m->access_properties |
          cls::init_spec_memory::access::INIT_SPEC_MEM_WRITE);
        break;
      case 's': // SVM
        if (i < s.size() - 1) {
          i++;
          switch (s[i]) {
          case 'c':
          case 'f':
            if (m->transfer_properties != cls::init_spec_memory::transfer::TX_INVALID)
              p.fatalAt(l, "invalid svm memory attribute (must be sc or sf)");
            setTx(s[i] == 'c' ?
              cls::init_spec_memory::transfer::TX_SVM_COARSE :
              cls::init_spec_memory::transfer::TX_SVM_FINE);
            break;
          default:
            // p.fatalAt(l, "invalid svm memory attribute (must be sc or sf)");
            // assume coarse if only one char given
            setTx(cls::init_spec_memory::transfer::TX_SVM_COARSE);
          }
        }
        break;
      case 'm':
        setTx(cls::init_spec_memory::transfer::TX_MAP);
        break;
      case 'c':
        setTx(cls::init_spec_memory::transfer::TX_COPY);
        break;
      default:
        l.column += (uint32_t)i;
        l.offset += (uint32_t)i;
        p.fatalAt(l, "invalid memory attribute");
      }
    }
    if (m->transfer_properties == cls::init_spec_memory::transfer::TX_INVALID)
      m->transfer_properties = cls::init_spec_memory::transfer::TX_COPY; // default to copy
    return m;
  } else {
    // regular primitive
    return e;
  }
}

// let X=...
static void parseLetStatement(cls::parser &p, cls::script &s)
{
  auto let_loc = p.nextLoc();
  p.skip();      // let
  auto name = p.tokenString(); // X
  if (s.let_bindings.find(name) != s.let_bindings.end()) {
    p.fatal(name, ": redefinition of let binding");
  }
  p.skip();      // X
  p.consume(EQ); // =
  auto init = parseInit(p); // TODO: need to partial applications
  s.let_bindings[name] = init;
  s.statements.push_back(new cls::let_spec(let_loc, name, init));
}


// #1`path/foo.cl`kernel<128,16>(...)
static void parseDispatchStatement(cls::parser &p, cls::script &s)
{
  auto loc = p.nextLoc();
  auto d = new cls::dispatch_spec(loc);
  if (p.consumeIf(HASH)) {
    if (p.lookingAt(STRLIT)) {
      if (p.lookingAt(STRLIT)) {
        d->device.setSource(p.tokenStringLiteral());
      } else {
        d->device.setSource(p.tokenString());
      }
      p.skip();
    } else if (p.lookingAtInt()) {
      d->device.setSource((int)p.consumeInt("device index (integer)"));
    } else {
      p.fatal("invalid device specification");
    }
    p.consume(BACKTICK);
  } else {
    d->device.kind = cls::device_spec::BY_DEFAULT;
  }

  // #1`path/foo.cl`kernel<1024x1024,16x16>(...)
  //    ^^^^^^^^^^^
  // #GTX`"foo/spaces in path/k.cl"`kernel<...>(...)
  //      ^^^^^^^^^^^^^^^^^^^^^^^^^
  d->program.defined_at = p.nextLoc();
  if (p.lookingAt(STRLIT)) {
    d->program.program_path = p.tokenStringLiteral(); p.skip();
  } else {
    d->program.program_path = consumeToChar(p, "[`");
  }

  // #1`path/foo.cl[-DTYPE=int]`kernel<1024x1024,16x16>(...)
  //               ^^^^^^^^^^^^
  if (p.consumeIf(LBRACK)) {
    d->program.build_opts = consumeToChar(p, "]");
    p.consume(RBRACK);
  }
  p.consume(BACKTICK);

  // #1`path/foo.cl`kernel<1024x1024,16x16>(...)
  //                ^^^^^^
  if (p.lookingAt(IDENT)) {
    d->kernel.defined_at =
      d->kernel.defined_at = p.nextLoc();
    d->kernel.kernel_name = p.tokenString(); p.skip();
  }

  // #1`path/foo.cl`kernel<1024x1024>(...)
  //                      ^^^^^^^^^^^
  // #1`path/foo.cl`kernel<1024x1024,16x16>(...)
  //                      ^^^^^^^^^^^^^^^^^
  if (p.consumeIf(LANGLE)) {
    auto parseDim = [&](bool allow_null) {
      cls::dim_spec d(p.nextLoc());
      if (p.lookingAtIdent("nullptr") || p.lookingAtIdent("NULL")) {
        if (!allow_null)
          p.fatal(p.tokenString(), " not allowed here");
        p.skip();
      } else if (p.lookingAtInt()) {
        // 1024 x 768
        // 0x200 x 0x100
        d.dims.push_back((size_t)p.consumeInt("dimension (int)"));
        while (p.lookingAtIdent("x")) {
          p.skip();
          d.dims.push_back((size_t)p.consumeInt("dimension (int)"));
        }
      } else if (p.lookingAt(DIMENSION)) {
        // 1024x768
        auto s = p.tokenString();
        size_t s_off = 0;
        auto parseDimCoord = [&]() {
          if (s_off == s.size() || !isdigit(s[s_off])) {
            p.fatal("syntax error in dimension");
          }
          size_t val = 0;
          while (s_off < s.size() && isdigit(s[s_off])) {
            s_off++;
            val = 10*val + s[s_off] - '0';
          }
          return val;
        };
        d.dims.push_back(parseDimCoord());
        while (s_off < s.size() && s[s_off] == 'x') {
          s_off++;
          d.dims.push_back(parseDimCoord());
        }
        p.skip();
      } else {
        p.fatal("expected dimension");
      }
      return d;
    };
    d->global_size = parseDim(false);
    if (p.consumeIf(COMMA)) {
      d->local_size = parseDim(true);
    }
    p.consume(RANGLE);
  } // end dimension part <...>

  // #1`path/foo.cl`kernel<1024x1024>(0:rw,1:r,33)
  //                                 ^^^^^^^^^^^^^
  p.consume(LPAREN);
  while (!p.lookingAt(RPAREN)) {
    d->arguments.push_back(parseInit(p));
    if (!p.consumeIf(COMMA))
      break;
  }
  p.consume(RPAREN);

  s.statements.push_back(d);
}

static cls::init_spec_symbol *parseSymbol(cls::parser &p)
{
  auto loc = p.nextLoc();
  if (p.lookingAtIdent()) {
    auto ident = p.tokenString();
    return new cls::init_spec_symbol(loc, ident);
  } else {
    p.fatal("expected identifier");
    return nullptr;
  }
}

// template<typename T>
// static bool parseBuiltIn(Parser &p, cls::script &s) {
//  s.statements.emplace_back(T,);
// }

// barrier | diff(X,Y) | print(X) | save(sym,X)
static bool parseBuiltIn(cls::parser &p, cls::script &s)
{
  auto loc = p.nextLoc();
  if (p.lookingAtIdent("barrier")) {
      s.statements.push_back(new cls::barrier_spec(loc));
      p.skip();
      if (p.consumeIf(LPAREN)) // optional ()
        p.consume(RPAREN);
    return true;
  } else if (p.lookingAtIdent("diff")) {
    p.skip();
    p.consume(LPAREN);
    auto *ref = parseInit(p);
    p.consume(COMMA);
    cls::init_spec_symbol *sut = parseSymbol(p);
    s.statements.push_back(new cls::diff_spec(loc, ref, sut));
    p.consume(RPAREN);
    return true;
  } else if (p.lookingAtIdent("print")) {
    p.skip();
    p.consume(LPAREN);
    cls::init_spec_symbol *val = parseSymbol(p);
    s.statements.push_back(new cls::print_spec(loc, val));
    p.consume(RPAREN);
    return true;
  } else if (p.lookingAtIdent("save")) {
    p.skip();
    p.consume(LPAREN);
    if (!p.lookingAt(STRLIT))
      p.fatal("expected file name (string literal)");
    std::string file = p.tokenStringLiteral();
    cls::init_spec_symbol *val = parseSymbol(p);
    s.statements.push_back(new cls::save_spec(loc, file, val));
    p.consume(RPAREN);
    return true;
  } else {
    return false;
  }
}

static void parseStatementLine(cls::parser &p, cls::script &s)
{
  if (p.lookingAtIdent("let") && p.lookingAt(IDENT,1) && p.lookingAt(EQ,2)) {
    // let X = ...
    parseLetStatement(p, s);
  } else if (parseBuiltIn(p,s)) {
    ;
  } else {
    // #1`foo/bar.cl
    parseDispatchStatement(p, s);
  } // TODO: other statements
}

cls::script cls::ParseScript(
  const Opts &os,
  const std::string &input,
  const std::string &filename)
{
  cls::script s;
  s.source = &input;

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

  return s;
}