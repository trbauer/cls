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

  cls::k::program_info *parse_program_info_binary_gen(
    const opts &os,
    diagnostics &ds, loc at,
    const std::string &path);
}

#endif