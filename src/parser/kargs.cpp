#include "kargs.hpp"
#include "parser.hpp"
#include "../cl_headers.hpp"
#include "../devices.hpp"
#include "../system.hpp"
#include "../text.hpp"

using namespace cls::k;
using namespace cls;

struct karg_parser : cls::parser
{
  prog  &p;

  karg_parser(std::string inp, prog  &_p)
    : cls::parser(inp)
    , p(_p)
  {
  }
  // TODO: how do we chain this
  // want diganostic to be properly chained
  //
  // 1.2: failed to parse kernel arguments
  // #1`foo.cl[-opts]`kernel<...>(...)
  //                  ^^^^^^
  //
  // in [CPP(foo.cl,-D... )] => cpp-foo.cl
  // kernel void foo(...........)
  //                 ^^^^
  void parse() {

  }
};

static cls::k::prog parseProgramInfoText(
  const cls::fatal_handler *fh, cls::loc at,
  const cls::program_source &src)
{
  std::stringstream ss;
  size_t off = src.build_opts.find("-D",0);
  while (off < src.build_opts.size()) {
    if (ss.gcount() != 0) // is this right?
      ss << " ";

    // consume the -D option
    //
    // -Dfoo
    // -Dfoo=bar
    // -D foo
    // -D foo=bar
    size_t d_start = off;
    off += 2;
    while (off < src.build_opts.size() && ::isspace(src.build_opts[off])) {
      // deals with separate tokens: e.g. "-D" "foo=bar"
      ss << src.build_opts[off++];
    }
    while (off < src.build_opts.size() && !::isspace(src.build_opts[off])) {
      ss << src.build_opts[off++];
    }

    // next iteration
    off = src.build_opts.find("-D",off);
  }
  std::string cpp_inp = text::load_c_preprocessed(src.path,ss.str());
  // suffix the build options so that line numbers all map correctly
  // (when we decide to support #line directives)
  cpp_inp +=
    "\n\n"
    "// CPP OPTIONS: " + ss.str();

  prog p;
  karg_parser kp(cpp_inp, p);
  try {
    kp.parse();
  } catch (const diagnostic &d) {
    std::stringstream ss;
    d.str(ss);
    fh->fatalAt(at,ss.str());
  }
  return p;
}

static cls::k::prog parseProgramInfoBinary(
  const cls::fatal_handler *fh, cls::loc at,
  const std::string &path)
{
  auto bits = sys::read_file_binary(path);
  // TODO: parse PTX or GEN binary
  fh->fatalAt(at,"parseProgramInfoBinary");
  return prog();
}

cls::k::prog cls::k::parseProgramInfo(
  const cls::fatal_handler *fh, cls::loc at,
  const cls::program_source &src)
{
  if (src.is_binary) {
    return parseProgramInfoBinary(fh, at, src.path);
  } else {
    return parseProgramInfoText(fh, at, src);
  }
}