#include "intel_gen_binary_decoder.hpp"
#include "../system.hpp"


struct igb_decoder : decoder {
  const uint32_t INTEL_GEN_DEVICE_BINARY_SECTION_CODE = 0xFF0000007;
  const uint32_t INTEL_GEN_DEVICE_BINARY_MAGIC = 0x43544E49; // "CTNI"
  const uint32_t ELF_MAGIC = 0x7F454C46; // "\7FELF"

  program_info &pi;

  igb_decoder(loc at, const uint8_t *bits, size_t bits_length, program_info &_pi)
    : decoder(at, bits, bits_length), pi(_pi) { }


  void run() {
  // CTNI bits are usually contained within the ELF as a section,
  // but we can handle them directly
    if (peek<uint32_t>() != ELF_MAGIC) {
      decElf();
    } else  if (peek<uint32_t>() != INTEL_GEN_DEVICE_BINARY_MAGIC) {
      decCtni();
    }
  }

  void decElf() {
    if (peek<uint32_t>() != ELF_MAGIC) {
      fatalAt(at, "expected ELF file input");
    }

    seek(0x28);
    auto e_shoff = decode<uint64_t>();

    seek(0x3A);
    auto e_shentsize = decode<uint16_t>(); // size of section header
    auto e_shnum     = decode<uint16_t>(); // section header count
    auto e_shstrndx  = decode<uint16_t>(); // section with section header name

    seek((size_t)e_shoff);
    for (size_t i = 0; i < (size_t)e_shnum; i++) {
      seek((size_t)e_shoff + i*e_shentsize);

      auto sh_name = decode<uint32_t>();
      auto sh_type = decode<uint32_t>();
      if (sh_type == INTEL_GEN_DEVICE_BINARY_SECTION_CODE) {
        skip(2*sizeof(uint64_t)); // sh_flags; sh_addr
        auto sh_offset = (size_t)decode<uint64_t>();
        auto sh_size = (size_t)decode<uint64_t>();
        // bounds check
        seek(sh_offset);
        skip(sh_size);
        // parse the CTNI
        igb_decoder ctni(at, bits + sh_offset, sh_size, pi);
        ctni.decCtni();
        return;
      }
    }
    fatalAt(at, "unable to find Intel Device Binary in ELF container");
  }

  // https://github.com/intel/intel-graphics-compiler/tree/master
  //   .../IGC/AdaptorOCL/ocl_igc_shared/executable_format
  //
  // C.f. SKernelBinaryHeaderCommon
  void decCtni() {
    if (peek<uint32_t>() != INTEL_GEN_DEVICE_BINARY_MAGIC) {
      fatalAt(at, "expected Intel GEN Device Binary magic");
    }
    skip(sizeof(uint32_t));
    auto version = decode<uint32_t>();
    (void)version;

    auto device = decode<uint32_t>();
    (void)device;
    // TODO: we could sanity check the device if we really cared
    //
    auto ptr_size = decode<uint32_t>();
    if (ptr_size != sizeof(void *)) {
      fatalAt(at, "GPUPointerSizeInBytes is not ",
        8*sizeof(void*), "b is this a ",8*ptr_size,"b binary");
    }
    //
    auto num_kernels = decode<uint32_t>();
    (void)num_kernels;
    //
    auto stepping_id = decode<uint32_t>();
    (void)stepping_id;
    //
    auto program_patch_list_size = decode<uint32_t>();
    skip(program_patch_list_size);

    // SKernelBinaryHeader
    for (uint32_t k_ix = 0; k_ix < num_kernels; k_ix++) {
      auto checksum = decode<uint32_t>(); // paranoid nitwits make life hard
      (void)checksum;
      //
      auto shader_hash_code = decode<uint64_t>();
      (void)shader_hash_code;
      //
      // strings sizes are aligned up to DW size
      auto alignUp4 = [&](uint32_t x) {return ((x + 3)/4;};
      auto kernel_name_size = alignUp4(decode<uint32_t>());
      //
      auto kernel_patch_list_size = decode<uint32_t>();
      //
      auto kernel_text_size = decode<uint32_t>();
      auto kernel_general_state_heap_size = decode<uint32_t>();
      auto kernel_dynamic_state_heap_size = decode<uint32_t>();
      auto kernel_surface_state_heap_size = decode<uint32_t>();
      auto kernel_text_unpadded_size = decode<uint32_t>();
      //
      char *kernel_name = (char *)alloca(kernel_name_size + 1);
      decode(kernel_name, kernel_name_size);
      // I think they pad, but just in case, we'll do it too
      kernel_name[kernel_name_size - 1] = 0;
      //
      // TODO: patches are here
      size_t patch_bytes_read = 0;
      while (patch_bytes_read < kernel_patch_list_size) {
        auto tag = decode<uint32_t>();
        auto size = decode<uint32_t>();

      }
      //
      skip(
        kernel_general_state_heap_size +
        kernel_dynamic_state_heap_size +
        kernel_surface_state_heap_size +
        kernel_text_size);
      // now we're at the patches
    }
  }
}

program_info cls::parseProgramInfoBinaryGEN(
  const opts &os,
  const fatal_handler *fh, loc at,
  const std::string &path)
{
  program_info pi;
  auto bits = sys::read_file_binary(path);
  igb_decoder d(at, bits.data(), bits.size(), pi);
  d.run();
  return pi;
}