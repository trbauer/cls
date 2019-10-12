#ifndef INTEL_GEN_BINARY_DECODER_HPP
#define INTEL_GEN_BINARY_DECODER_HPP

#include "kargs.hpp"
#include "../cls_opts.hpp"
#include "../fatal.hpp"

#include <cstdint>
#include <string>

namespace cls
{
  constexpr uint32_t ELF_MAGIC = 0x464C457F; // "\7FELF"

  cls::k::program_info *parseProgramInfoBinaryGEN(
    const opts &os,
    fatal_handler *fh, loc at,
    const std::string &path);
}

#endif