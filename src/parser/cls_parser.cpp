#define _USE_MATH_DEFINES
#include "cls_parser.hpp"
#include "parser.hpp"
#include "../system.hpp"

#include <cmath>
#include <iostream>
#include <sstream>

using namespace cls;

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

static init_spec_atom *parseInitAtom(parser &p);

static init_spec_atom *parseInitAtomPrim(parser &p)
{
  auto at = p.nextLoc();
  if (p.lookingAt(STRLIT)) {
    p.fatal("bare strings not allowed for files anymore (use bin('...') and txt('...'))");
    return nullptr;
  } else if (p.lookingAtFloat()) {
    return new init_spec_float(at, p.consumeFloat());
  } else if (p.lookingAtInt()) {
    return new init_spec_int(at, p.consumeIntegral<int64_t>());
  } else if (p.lookingAtIdent()) {
    // e.g. "X" or "g.x" or "pow(...)"
    auto s = p.tokenString();
    p.skip();
    if (p.lookingAt(DOT)) {
      // e.g. "g.x"
      while (p.consumeIf(DOT)) {
        s += '.';
        if (!p.lookingAt(IDENT))
          p.fatal("syntax error in initializer expression field access");
        s += p.tokenString();
        p.skip();
      }
      for (int biv = init_spec_builtin::BIV_FIRST;
        biv <= init_spec_builtin::BIV_LAST;
        biv++)
      {
        if (s == init_spec_builtin::syntax_for((init_spec_builtin::biv_kind)biv)) {
          at.extend_to(p.nextLoc());
          return new init_spec_builtin(at, (init_spec_builtin::biv_kind)biv);
        }
      }
      at.extend_to(p.nextLoc());
      return new init_spec_symbol(at, s);
    } else if (p.lookingAt(LPAREN) || p.lookingAt(LANGLE) ||
      s == "random" || s == "seq" || s == "file")
    {
      // foo<...  (e.g. random<12007>(...))
      // or
      // foo(...
      //
      // TODO: generalize function parsing to
      //    F<...>(....)
      // then match by template arguments
      if (s == "random") {
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
            auto *arg1 = parseInitAtom(p);
            if (p.consumeIf(COMMA)) {
              auto *arg2 = parseInitAtom(p);
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
      } else if (s == "file") {
        //
        // file('foo.bin'):r
        // file<bin>('foo.bin'):r
        //
        // file<text>('foo.txt'):r           // all tokens
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
      } else {
        ///////////////////////////////////////////////////
        // generic function
        std::vector<init_spec_atom *> args;
        p.consume(LPAREN);
        while (!p.lookingAt(RPAREN)) {
          args.push_back(parseInitAtom(p));
          if (!p.consumeIf(COMMA))
            break;
        }
        p.consume(RPAREN);

        ///////////////////////////////////////////////////
        // special functions (pseudo functions)
        if (s == "seq") {
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
          if (s == "fabs")
            p.fatalAt(at,"use \"abs\" for the absolute value");
          else if (s == "sqt")
            p.fatalAt(at,"use \"sqrt\" for the square root");

          const auto *op = init_spec_uex::lookup_op(s.c_str());
          if (!op) {
            if (init_spec_bex::lookup_op(s.c_str())) {
              p.fatalAt(at, "function requires two arguments");
            } else {
              p.fatalAt(at, "not a unary function");
            }
          }
          return new init_spec_uex(at,*op,args[0]);
        } else if (args.size() == 2) {
          const auto *op = init_spec_bex::lookup_op(s.c_str());
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
      if (s == "E")
        return new init_spec_float(at, M_E);
      else if (s == "PI")
        return new init_spec_float(at, M_PI);
      // some other symbol (may target a LET binding)
      return new init_spec_symbol(at, s);
    }
  } else if (p.consumeIf(LBRACE)) {
    // {...}
    auto re = new init_spec_record(at);
    if (!p.lookingAt(RBRACE)) {
      re->children.push_back(parseInitAtom(p));
      while (p.consumeIf(COMMA))
        re->children.push_back(parseInitAtom(p));
    }
    p.consume(RBRACE);
    re->defined_at.extend_to(p.nextLoc());
    return re;
  } else if (p.consumeIf(LPAREN)) {
    init_spec_atom *e = parseInitAtom(p);
    p.consume(RPAREN);
    return e;
  } else {
    p.fatal("syntax error in initializer expression");
    return nullptr;
  }
}
static init_spec_atom *parseInitAtomUnr(parser &p)
{
  if (p.lookingAt(SUB) || p.lookingAt(TILDE)) {
    auto loc = p.nextLoc();
    const auto &op =
      p.lookingAt(SUB) ?
        *init_spec_uex::lookup_op("-") :
        *init_spec_uex::lookup_op("~");
    p.skip();
    init_spec_atom *e = parseInitAtomUnr(p);
    return new init_spec_uex(loc, op, e);
  } else {
    return parseInitAtomPrim(p);
  }
}
static init_spec_atom *parseInitAtomMul(parser &p)
{
  init_spec_atom *e = parseInitAtomUnr(p);
  while (p.lookingAt(MUL) || p.lookingAt(DIV) || p.lookingAt(MOD)) {
    const auto &op =
      p.lookingAt(MUL) ? *init_spec_bex::lookup_op("*") :
      p.lookingAt(DIV) ? *init_spec_bex::lookup_op("/") :
      *init_spec_bex::lookup_op("%");
    p.skip();
    init_spec_atom *t = parseInitAtomUnr(p);
    e = new init_spec_bex(op, e, t);
  }
  return e;
}
static init_spec_atom *parseInitAtomAdd(parser &p)
{
  init_spec_atom *e = parseInitAtomMul(p);
  while (p.lookingAt(ADD) || p.lookingAt(SUB)) {
    const auto &op = p.lookingAt(ADD) ?
      *init_spec_bex::lookup_op("+") :
      *init_spec_bex::lookup_op("-");
    p.skip();
    init_spec_atom *t = parseInitAtomMul(p);
    e = new init_spec_bex(op, e, t);
  }
  return e;
}
static init_spec_atom *parseInitAtomShift(parser &p)
{
  init_spec_atom *e = parseInitAtomAdd(p);
  while (p.lookingAt(LSH) || p.lookingAt(RSH)) {
    const auto &op = p.lookingAt(LSH) ?
      *init_spec_bex::lookup_op("<<") :
      *init_spec_bex::lookup_op(">>");
    p.skip();
    init_spec_atom *t = parseInitAtomAdd(p);
    e = new init_spec_bex(op, e, t);
  }
  return e;
}
static init_spec_atom *parseInitAtomBitwiseAND(parser &p)
{
  init_spec_atom *e = parseInitAtomShift(p);
  while (p.consumeIf(AMP)) {
    init_spec_atom *t = parseInitAtomShift(p);
    e = new init_spec_bex(*init_spec_bex::lookup_op("&"), e, t);
  }
  return e;
}
static init_spec_atom *parseInitAtomBitwiseXOR(parser &p)
{
  init_spec_atom *e = parseInitAtomBitwiseAND(p);
  while (p.consumeIf(CIRC)) {
    init_spec_atom *t = parseInitAtomBitwiseAND(p);
    e = new init_spec_bex(*init_spec_bex::lookup_op("^"), e, t);
  }
  return e;
}
static init_spec_atom *parseInitAtomBitwiseOR(parser &p)
{
  init_spec_atom *e = parseInitAtomBitwiseXOR(p);
  while (p.consumeIf(PIPE)) {
    init_spec_atom *t = parseInitAtomBitwiseXOR(p);
    e = new init_spec_bex(*init_spec_bex::lookup_op("|"), e, t);
  }
  return e;
}
static init_spec_atom *parseInitAtom(parser &p)
{
  return parseInitAtomBitwiseOR(p);
}

static init_spec *parseInit(parser &p)
{
  auto l = p.nextLoc();
  init_spec_atom *e = parseInitAtom(p);

  if (p.consumeIf(COLON)) {
    // memory initializer
    init_spec_mem *m = new init_spec_mem(l);
    m->root = e;
    if (p.consumeIf(LBRACK)) {
      init_spec_atom *de = parseInitAtom(p);
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
      default:
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
  enum spec::spec_kind kind,
  const char *what)
{
  auto loc = p.nextLoc();
  std::string name = p.consumeIdent();
  auto itr = s.let_bindings.find(name);
  if (itr == s.let_bindings.end()) {
    p.fatalAt(loc,what," not defined");
  }
  let_spec *ls = itr->second;
  spec *rs = ls->value;
  if (rs->kind != kind) {
    std::stringstream ss;
    ss << "identifier does not reference a " << what;
    ss << " (defined on line " << rs->defined_at.line << ")";
    p.fatalAt(loc,ss.str());
  }
  return refable<T>(loc,name,(T *)rs);
}

static refable<init_spec_mem> dereferenceLetMem(
  parser &p,
  script &s)
{
  refable<init_spec> rf =
    dereferenceLet<init_spec>(p, s, spec::INIT_SPEC, "memory object");
  if ((*rf).kind != init_spec::IS_MEM) {
    p.fatalAt(rf.defined_at,"identifier does not reference a memory object");
  }
  return refable<init_spec_mem>(
    rf.defined_at,
    rf.identifier,
    (init_spec_mem *)rf.value);
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
    auto parseDim = [&] (bool allow_null, loc &loc_ref) {
      ndr d;
      loc_ref = p.nextLoc();
      std::vector<size_t> ds;
      if (p.lookingAtIdentEq("nullptr") || p.lookingAtIdentEq("NULL")) {
        if (!allow_null)
          p.fatal(p.tokenString(), " not allowed here");
        p.skip();
      } else if (p.lookingAtInt()) {
        // 1024 x 768
        // 0x200 x 0x100
        ds.push_back((size_t)p.consumeIntegral<uint64_t>("dimension (int)"));
        while (p.lookingAtIdentEq("x")) {
          p.skip();
          ds.push_back((size_t)p.consumeIntegral<uint64_t>("dimension (int)"));
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
            val = 10*val + s[s_off++] - '0';
          }
          return val;
        };
        ds.push_back(parseDimCoord());
        while (s_off < s.size() && s[s_off] == 'x') {
          s_off++;
          ds.push_back(parseDimCoord());
        }
        p.skip();
      } else {
        p.fatal("expected dimension");
      }

      loc_ref.extend_to(p.nextLoc());
      if (ds.size() == 0) {
        d = ndr();
      } else if (ds.size() == 1) {
        d = ndr(ds[0]);
      } else if (ds.size() == 2) {
        d = ndr(ds[0],ds[1]);
      } else if (ds.size() == 3) {
        d = ndr(ds[0],ds[1],ds[2]);
      } else {
        p.fatal("dimension is too large");
      }
      return d;
    }; // parseDim
    ds.global_size = parseDim(false, ds.global_size_loc);
    if (p.consumeIf(COMMA)) {
      ds.local_size = parseDim(true, ds.local_size_loc);
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
    init_spec *is = parseInit(p);
    if (is->kind == init_spec::IS_SYM) {
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
      init_spec *i = parseInit(p);
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
        if (ls->value->kind != spec::INIT_SPEC) {
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
    p.consume(BACKTICK);
    dev.defined_at.extend_to(p.nextLoc());
  } else {
    dev.kind = device_spec::BY_DEFAULT;
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
  init_spec *is = parseInit(p);
  if (is->kind == init_spec::IS_SYM) {
    // make a reference argument
    auto itr = s.let_bindings.find(((const init_spec_symbol *)is)->identifier);
    if (itr == s.let_bindings.end()) {
      p.fatalAt(is->defined_at,"unbound identifier");
    } else if (itr->second->value->kind != spec::INIT_SPEC ||
      ((init_spec *)itr->second->value)->kind != init_spec::IS_MEM)
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

// template<typename T>
// static bool parseBuiltIn(Parser &p, script &s) {
//  s.statements.emplace_back(T,);
// }

// barrier | diff(X,Y) | print(X) | save(sym,X)
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
      elem_type = lookupPrimtiveType(p.consumeIdent("type name"));
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
        elem_type = lookupPrimtiveType(p.consumeIdent("type name"));
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
    value = parseInit(p);
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