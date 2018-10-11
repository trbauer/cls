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
  auto loc = p.nextLoc();
  if (p.lookingAt(STRLIT)) {
    auto s = p.tokenStringLiteral();
    p.skip();
    return new init_spec_file(loc, s);
  } else if (p.lookingAtFloat()) {
    return new init_spec_float(loc, p.consumeFloat());
  } else if (p.lookingAtInt()) {
    return new init_spec_int(loc, p.consumeInt());
  } else if (p.lookingAtIdent()) {
    // e.g. "X" or "g.x" or "pow(...)"
    auto s = p.tokenString();
    p.skip();
    if (p.lookingAt(DOT)) {
      // e.g. "g.x"
      while (p.consumeIf(DOT)) {
        s += '.';
        if (p.lookingAt(IDENT)) {
          p.fatal("syntax error in initializer expression field access");
        }
        s += p.tokenString();
      }
      for (int biv = init_spec_builtin::BIV_FIRST;
        biv <= init_spec_builtin::BIV_LAST;
        biv++)
      {
        if (s == init_spec_builtin::syntax_for((init_spec_builtin::biv_kind)biv)) {
          return new init_spec_builtin(loc, (init_spec_builtin::biv_kind)biv);
        }
      }
      loc.extend_to(p.nextLoc());
      return new init_spec_symbol(loc, s);
    } else if (p.lookingAt(LPAREN) || p.lookingAt(LANGLE)) {
      // foo<...  (e.g. random<12007>(...))
      // or
      // foo(...
      //
      // TODO: generalize function parsing to
      //    F<...>(....)
      // then match by template arguments
      if (s == "random") {
        int64_t seed = 0;
        if (p.consumeIf(LANGLE)) {
          seed = p.consumeInt("seed (int)");
          p.consume(RANGLE);
        }
        auto func = new init_spec_rng(loc, seed);
        if (p.consumeIf(LBRACK)) {
          func->e_lo = parseInitAtom(p);
          if (p.consumeIf(COMMA))
            func->e_hi = parseInitAtom(p);
          p.consume(RBRACK);
        }
        func->defined_at.extend_to(p.nextLoc());
        return func;
      } else {
        std::vector<init_spec_atom *> args;
        p.consume(LPAREN);
        while (!p.lookingAt(RPAREN)) {
          args.push_back(parseInitAtom(p));
          if (!p.consumeIf(COMMA))
            break;
        }
        p.consume(RPAREN);

        auto makeBinary = [&](init_spec_bin_expr::bin_op op) {
          if (args.size() != 2) {
            p.fatalAt(loc, "binary function requires two arguments");
          }
          auto *isbe = new init_spec_bin_expr(op,args[0],args[1]);
          isbe->defined_at = loc; // reset start loc to beg. of the primary
          isbe->defined_at.extend_to(p.nextLoc());
          return isbe;
        };
        auto makeUnary = [&](init_spec_unr_expr::unr_op op) {
          if (args.size() != 1) {
            p.fatalAt(loc, "unary function requires one argument");
          }
          return new init_spec_unr_expr(loc,op,args[0]);
        };
        // binary functions
        if (s == "min")
          return makeBinary(init_spec_bin_expr::E_MIN);
        else if (s == "max")
          return makeBinary(init_spec_bin_expr::E_MAX);
        else if (s == "pow")
          return makeBinary(init_spec_bin_expr::E_POW);
        else if (s == "lcm")
          return makeBinary(init_spec_bin_expr::E_LCM);
        else if (s == "gcd")
          return makeBinary(init_spec_bin_expr::E_GCD);
        // unary functions
        else if (s == "abs")
          return makeUnary(init_spec_unr_expr::E_ABS);
        else if (s == "fabs")
          p.fatalAt(loc,"use \"abs\" for the absolute value");
        else if (s == "sqrt")
          p.fatalAt(loc,"use \"sqt\" for the square root");
        else if (s == "sqt")
          return makeUnary(init_spec_unr_expr::E_SQT);
        else if (s == "exp")
          return makeUnary(init_spec_unr_expr::E_EXP);
        else if (s == "log")
          return makeUnary(init_spec_unr_expr::E_LOG);
        else if (s == "log2")
          return makeUnary(init_spec_unr_expr::E_LOG2);
        else if (s == "log10")
          return makeUnary(init_spec_unr_expr::E_LOG10);
        else if (s == "sin")
          return makeUnary(init_spec_unr_expr::E_SIN);
        else if (s == "cos")
          return makeUnary(init_spec_unr_expr::E_COS);
        else if (s == "tan")
          return makeUnary(init_spec_unr_expr::E_TAN);
        else if (s == "atan")
          return makeUnary(init_spec_unr_expr::E_ATAN);
        ///////////////////////////////////////////////////
        // other expressions (pseudo functions)
        if (s == "seq") {
          p.fatal("parseInitAtomPrim: implement SEQ");
        }
        // fallback
        p.fatal("undefined function");
        return nullptr; // unreachable
      } // end else not random
    } else {
      if (s == "E")
        return new init_spec_float(loc, M_E);
      else if (s == "PI")
        return new init_spec_float(loc, M_PI);
      // regular symbol
      //
      // TODO: support E and PI
      return new init_spec_symbol(loc, s);
    }
  } else if (p.consumeIf(LBRACE)) {
    // {...,...,...,...}
    auto re = new init_spec_record(loc);
    if (!p.lookingAt(RBRACE)) {
      re->children.push_back(parseInitAtom(p));
      while (p.consumeIf(COMMA))
        re->children.push_back(parseInitAtom(p));
    }
    p.consume(RBRACE);
    re->defined_at.extend_to(p.nextLoc());
    return re;
  } else if (p.lookingAt(LPAREN)) {
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
    auto op =
      p.lookingAt(SUB) ?
        init_spec_unr_expr::E_NEG : init_spec_unr_expr::E_COMPL;
    p.skip();
    init_spec_atom *e = parseInitAtomUnr(p);
    return new init_spec_unr_expr(loc, op, e);
  } else {
    return parseInitAtomPrim(p);
  }
}
static init_spec_atom *parseInitAtomMul(parser &p)
{
  init_spec_atom *e = parseInitAtomUnr(p);
  while (p.lookingAt(MUL) || p.lookingAt(DIV)) {
    auto op = p.lookingAt(MUL) ?
      init_spec_bin_expr::bin_op::E_MUL :
      init_spec_bin_expr::bin_op::E_DIV;
    p.skip();
    init_spec_atom *t = parseInitAtomUnr(p);
    e = new init_spec_bin_expr(op, e, t);
  }
  return e;
}
static init_spec_atom *parseInitAtomAdd(parser &p)
{
  init_spec_atom *e = parseInitAtomMul(p);
  while (p.lookingAt(ADD) || p.lookingAt(SUB)) {
    auto op = p.lookingAt(ADD) ?
      init_spec_bin_expr::bin_op::E_ADD :
      init_spec_bin_expr::bin_op::E_SUB;
    p.skip();
    init_spec_atom *t = parseInitAtomMul(p);
    e = new init_spec_bin_expr(op, e, t);
  }
  return e;
}
static init_spec_atom *parseInitAtomShift(parser &p)
{
  init_spec_atom *e = parseInitAtomAdd(p);
  while (p.lookingAt(LSH) || p.lookingAt(RSH)) {
    auto op = p.lookingAt(LSH) ?
      init_spec_bin_expr::bin_op::E_LSH :
      init_spec_bin_expr::bin_op::E_RSH;
    p.skip();
    init_spec_atom *t = parseInitAtomAdd(p);
    e = new init_spec_bin_expr(op, e, t);
  }
  return e;
}
static init_spec_atom *parseInitAtomBitwiseAND(parser &p)
{
  init_spec_atom *e = parseInitAtomShift(p);
  while (p.consumeIf(AMP)) {
    init_spec_atom *t = parseInitAtomShift(p);
    e = new init_spec_bin_expr(init_spec_bin_expr::bin_op::E_AND, e, t);
  }
  return e;
}
static init_spec_atom *parseInitAtomBitwiseXOR(parser &p)
{
  init_spec_atom *e = parseInitAtomBitwiseAND(p);
  while (p.consumeIf(CIRC)) {
    init_spec_atom *t = parseInitAtomBitwiseAND(p);
    e = new init_spec_bin_expr(init_spec_bin_expr::bin_op::E_XOR, e, t);
  }
  return e;
}
static init_spec_atom *parseInitAtomBitwiseOR(parser &p)
{
  init_spec_atom *e = parseInitAtomBitwiseXOR(p);
  while (p.consumeIf(PIPE)) {
    init_spec_atom *t = parseInitAtomBitwiseXOR(p);
    e = new init_spec_bin_expr(init_spec_bin_expr::bin_op::E_OR, e, t);
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
    init_spec_memory *m = new init_spec_memory(l);
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
      auto setTx = [&] (init_spec_memory::transfer t) {
        if (m->transfer_properties != init_spec_memory::transfer::TX_INVALID) {
          p.fatalAt(l, "memory transfer respecification");
        }
        m->transfer_properties = t;
      };

      switch (s[i]) {
      case 'r':
        m->access_properties = (init_spec_memory::access)(
          m->access_properties |
          init_spec_memory::access::INIT_SPEC_MEM_READ);
        break;
      case 'w':
        m->access_properties = (init_spec_memory::access)(
          m->access_properties |
          init_spec_memory::access::INIT_SPEC_MEM_WRITE);
        break;
      case 's': // SVM
        if (i < s.size() - 1) {
          i++;
          switch (s[i]) {
          case 'c':
          case 'f':
            if (m->transfer_properties != init_spec_memory::transfer::TX_INVALID)
              p.fatalAt(l, "invalid svm memory attribute (must be :..sc.. or :..sf..)");
            setTx(s[i] == 'c' ?
              init_spec_memory::transfer::TX_SVM_COARSE :
              init_spec_memory::transfer::TX_SVM_FINE);
            break;
          default:
            // p.fatalAt(l, "invalid svm memory attribute (must be sc or sf)");
            // assume coarse if only one char given
            setTx(init_spec_memory::transfer::TX_SVM_COARSE);
          }
        }
        break;
      case 'm':
        setTx(init_spec_memory::transfer::TX_MAP);
        break;
      case 'c':
        setTx(init_spec_memory::transfer::TX_COPY);
        break;
      // SPECIFY: do we consider deprecating these after stable development
      //          they are certainly nice for debugging
      case 'P':
        m->print_pre = true;
        break;
      case 'p':
        m->print_post = true;
        break;
      default:
        l.column += (uint32_t)i;
        l.offset += (uint32_t)i;
        p.fatalAt(l, "invalid memory attribute");
      }
    }
    if (m->transfer_properties == init_spec_memory::transfer::TX_INVALID)
      m->transfer_properties = init_spec_memory::transfer::TX_COPY; // default to copy
    m->defined_at.extend_to(p.nextLoc());
    return m;
  } else {
    // regular primitive
    return e;
  }
}


template <typename T>
static refable<T*> dereferenceLet(
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
  return refable<T*>((T *)rs);
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
    auto parseDim = [&] (bool allow_null) {
      dim d;
      d.defined_at = p.nextLoc();
      if (p.lookingAtIdentEq("nullptr") || p.lookingAtIdentEq("NULL")) {
        if (!allow_null)
          p.fatal(p.tokenString(), " not allowed here");
        p.skip();
      } else if (p.lookingAtInt()) {
        // 1024 x 768
        // 0x200 x 0x100
        d.dims.push_back((size_t)p.consumeInt("dimension (int)"));
        while (p.lookingAtIdentEq("x")) {
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
            val = 10*val + s[s_off++] - '0';
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
    ds.global_size = parseDim(false);
    if (p.consumeIf(COMMA)) {
      ds.local_size = parseDim(true);
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
      dev.setSource((int)p.consumeInt("device index (integer)"));
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
//                                                     ^ YES!
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
        p.lookingAt(LANGLE,i)) // malformed dispatch   foo.cl<1024>(...)
      {
        return true;
      } else if (p.lookingAt(NEWLINE,i) || // malformed statement
        p.lookingAt(SEMI,i) || // malformed statement
        p.lookingAt(EQ,i)) // malformed let possibly
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
      refable<program_spec *> ps =
        dereferenceLet<program_spec>(p,s,spec::PROGRAM_SPEC,"program");
      p.consume(BACKTICK);
      std::string kernel_name = p.consumeIdent("kernel name");
      ds->kernel = refable<kernel_spec*>(new kernel_spec(ps));
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
    return true;
  } else if (p.lookingAtIdentEq("diff")) {
    p.skip();
    p.consume(LPAREN);
    auto *ref = parseInit(p);
    p.consume(COMMA);
    init_spec_symbol *sut = parseSymbol(p);
    s.statement_list.statements.push_back(new diff_spec(loc, ref, sut));
    p.consume(RPAREN);
    return true;
  } else if (p.lookingAtIdentEq("print")) {
    p.skip();
    p.consume(LPAREN);
    init_spec_symbol *val = parseSymbol(p);
    s.statement_list.statements.push_back(new print_spec(loc, val));
    p.consume(RPAREN);
    return true;
  } else if (p.lookingAtIdentEq("save")) {
    p.skip();
    p.consume(LPAREN);
    if (!p.lookingAt(STRLIT))
      p.fatal("expected file name (string literal)");
    std::string file = p.tokenStringLiteral();
    init_spec_symbol *val = parseSymbol(p);
    s.statement_list.statements.push_back(new save_spec(loc, file, val));
    p.consume(RPAREN);
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
  if (p.lookingAtSeq(IDENT,LANGLE)) {
    // let D = K<1024>(....) where ...
    dispatch_spec *ds = new dispatch_spec(value_loc);
    ds->kernel = dereferenceLet<kernel_spec>(p,s,spec::KERNEL_SPEC,"kernel");
    parseDispatchStatementDimensions(p,s,*ds);
    parseDispatchStatementArguments(p,s,*ds);
    parseDispatchStatementWhereClause(p,s,*ds,ls);
    ds->defined_at.extend_to(p.nextLoc());
    value = ds;
  } else if (p.lookingAtSeq(IDENT,BACKTICK,IDENT,LANGLE)) {
    // let D = P`kernel<...>(...) where ...
    dispatch_spec *ds = new dispatch_spec(value_loc);
    refable<program_spec *> rps =
      dereferenceLet<program_spec>(p,s,spec::PROGRAM_SPEC,"programs");
    p.consume(BACKTICK);
    ds->kernel = new kernel_spec(rps.value);
    parseDispatchStatementDimensions(p,s,*ds);
    parseDispatchStatementArguments(p,s,*ds);
    parseDispatchStatementWhereClause(p,s,*ds,ls);
    ds->defined_at.extend_to(p.nextLoc());
    value = ds;
  } else if (lookingAtImmediateDispatchStatement(p)) {
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