#ifndef INTEL_SPIRV_DECODER_HPP
#define INTEL_SPIRV_DECODER_HPP

#include "kargs.hpp"
#include "../cls_opts.hpp"
#include "../fatal.hpp"

#include <cstdint>
#include <string>

namespace cls
{
  constexpr uint32_t SPIRV_MAGIC = 0x07230203;

  cls::k::program_info *parse_program_info_binary_spirv(
    const opts &os,
    diagnostics &ds, loc at,
    const std::string &path);
}

#endif