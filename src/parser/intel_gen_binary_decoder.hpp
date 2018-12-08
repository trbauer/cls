#ifndef INTEL_GEN_BINARY_DECODER_HPP
#define INTEL_GEN_BINARY_DECODER_HPP

#include "kargs.hpp"
#include "../cls_opts.hpp"
#include "../fatal.hpp"

#include <string>

namespace cls
{
  cls::k::program_info *parseProgramInfoBinaryGEN(
    const opts &os,
    const fatal_handler *fh, loc at,
    const std::string &path);
}

#endif