#include "decoder.hpp"
#include "kargs.hpp"
#include "intel_gen_binary_decoder.hpp"
#include "../system.hpp"

using namespace cls;
using namespace cls::k;


struct igb_decoder : decoder {
  const uint32_t INTEL_GEN_DEVICE_BINARY_SH_TYPE = 0xFF000005;
  const uint32_t INTEL_GEN_DEVICE_BINARY_MAGIC = 0x494E5443; // "CTNI"
//  const uint32_t INTEL_GEN_DEVICE_BINARY_MAGIC = 0x43544E49; // "CTNI"
  const uint32_t ELF_MAGIC = 0x464C457F; // "\7FELF"

  program_info &pi;

  igb_decoder(
    const fatal_handler &fh,
    loc at, const uint8_t *bits, size_t bits_length,
    program_info &_pi)
    : decoder(fh, at, bits, bits_length), pi(_pi) { }


  void run() {
  // CTNI bits are usually contained within the ELF as a section,
  // but we can handle them directly
    if (peek<uint32_t>() == ELF_MAGIC) {
      decElf();
    } else if (peek<uint32_t>() == INTEL_GEN_DEVICE_BINARY_MAGIC) {
      decCtni();
    }
  }

  void decElf() {
    if (peek<uint32_t>() != ELF_MAGIC) {
      fatalHere("expected ELF file input");
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
      if (sh_type == INTEL_GEN_DEVICE_BINARY_SH_TYPE) {
        skip(2*sizeof(uint64_t)); // sh_flags; sh_addr
        auto sh_offset = (size_t)decode<uint64_t>();
        auto sh_size = (size_t)decode<uint64_t>();
        // bounds check to ensure the section is complete
        seek(sh_offset);
        skip(sh_size);
        //
        // parse the CTNI
        // construct a subview of the section
        // retain the offset in terms of the ELF file though
        igb_decoder ctni(fh, at, bits, off + sh_size, pi);
        ctni.seek(sh_offset);
        ctni.decCtni();
        return;
      }
    }
    fatal("unable to find Intel Device Binary in ELF container");
  }

  // strings lengths may or may not be specified as multiples of 4 length, but
  // they are always stored that way
  size_t decodeStringLength() {
    auto n = decode<uint32_t>();
    return (n + 3) - ((n + 3)  % 4);
  }
  // later pass the value from decodeStringLength() to this
  std::string decodeString(size_t len) {
    char *str = (char *)alloca(len + 1);
    decodeInto(str, len);
    str[len - 1] = 0; // I think they pad, but just in case, we'll do it too
    return std::string(str);
  }

  void decodeKernelArgument(kernel_info &ki, size_t ptr_size) {
    auto arg_ix = (int)decode<uint32_t>();
    if (ki.args.size() <= (size_t)arg_ix) {
      ki.args.resize(arg_ix + 1);
    }
    arg_info &ai = ki.args[arg_ix];

    // Why in God's name do they encode well-defined OpenCL binary
    // values all as strings, I don't know.  It frustrates me.
    //
    auto addr_qual_len = decodeStringLength();
    auto acc_qual_len = decodeStringLength();
    auto arg_name_len = decodeStringLength();
    auto type_name_len = decodeStringLength();
    auto type_qual_len = decodeStringLength();
    //
    auto addr_qual = decodeString(addr_qual_len);
    if (addr_qual == "__global" || addr_qual == "global") {
      ai.addr_qual = CL_KERNEL_ARG_ADDRESS_GLOBAL;
    } else if (addr_qual == "__constant" || addr_qual == "constant") {
      ai.addr_qual = CL_KERNEL_ARG_ADDRESS_CONSTANT;
    } else if (addr_qual == "__local" || addr_qual == "local") {
      ai.addr_qual = CL_KERNEL_ARG_ADDRESS_LOCAL;
    } else if (addr_qual == "__private" || addr_qual == "private" ||
      addr_qual == "NONE")
    {
      ai.addr_qual = CL_KERNEL_ARG_ADDRESS_PRIVATE;
    } else {
      fatalHere(
        "in kernel ",ki.name," for arg ",arg_ix,
        ": invalid address qualifier encoded: ", addr_qual);
    }
    auto acc_qual = decodeString(acc_qual_len);
    if (acc_qual == "NONE") {
      ki.args[arg_ix].accs_qual = CL_KERNEL_ARG_ACCESS_NONE;
    } else if (acc_qual == "read_only" || acc_qual == "__read_only") {
      ki.args[arg_ix].accs_qual = CL_KERNEL_ARG_ACCESS_READ_ONLY;
    } else if (acc_qual == "write_only" || acc_qual == "__write_only") {
      ki.args[arg_ix].accs_qual = CL_KERNEL_ARG_ACCESS_WRITE_ONLY;
    } else if (acc_qual == "read_write" || acc_qual == "__read_write") {
      ki.args[arg_ix].accs_qual = CL_KERNEL_ARG_ACCESS_READ_WRITE;
    } else if (acc_qual == "" || acc_qual == "") {
      ki.args[arg_ix].accs_qual = CL_KERNEL_ARG_ACCESS_NONE;
    } else {
      fatalHere(
        "in kernel ",ki.name," for arg ",arg_ix,
        ": invalid access qualifier encoded: ", acc_qual);
    }

    ki.args[arg_ix].name = decodeString(arg_name_len);

    // they store pointers as in the following example "uint*;8"
    auto type_name = decodeString(type_name_len);
    size_t semi = type_name.find(';');
    size_t star = type_name.find('*');
    auto base_type = type_name.substr(0,std::min(semi,star));
    const type *t = lookupBuiltinType(base_type, ptr_size);
    if (!t) {
      fatalHere(
        "in kernel ",ki.name," for arg ",arg_ix,
        ": invalid type name: ", type_name);
    }
    if (star != std::string::npos) {
      // something like "uint*;8";
      // but handle harder stuff too like "uint***;8"
      for (size_t i = star; i < std::min(semi,type_name.size()); i++) {
        if (type_name[i] == '*') {
          t = &pi.pointerTo(*t, ptr_size);
        }
      }
    }
    ki.args[arg_ix].type = *t;
    //
    ki.args[arg_ix].type_qual = CL_KERNEL_ARG_TYPE_NONE;
    auto type_qual = decodeString(type_qual_len);
    if (type_qual != "NONE") {
      bool parsed_something = false;
      if (type_qual.find("const") != std::string::npos) {
        ki.args[arg_ix].type_qual |= CL_KERNEL_ARG_TYPE_CONST;
        parsed_something = true;
      }
      if (type_qual.find("restrict") != std::string::npos) {
        ki.args[arg_ix].type_qual |= CL_KERNEL_ARG_TYPE_RESTRICT;
        parsed_something = true;
      }
      if (type_qual.find("volatile") != std::string::npos) {
        ki.args[arg_ix].type_qual |= CL_KERNEL_ARG_TYPE_VOLATILE;
        parsed_something = true;
      }
      if (type_qual.find("pipe") != std::string::npos) {
        ki.args[arg_ix].type_qual |= CL_KERNEL_ARG_TYPE_PIPE;
        parsed_something = true;
      }
      if (!parsed_something) {
        fatalHere(
          "in kernel ",ki.name," for arg ",arg_ix,
          ": invalid type qualifier encoded: ", type_qual);
      }
    }
  }

  // https://github.com/intel/intel-graphics-compiler/tree/master
  //   .../IGC/AdaptorOCL/ocl_igc_shared/executable_format
  //
  // C.f. SKernelBinaryHeaderCommon
  void decCtni() {
    if (peek<uint32_t>() != INTEL_GEN_DEVICE_BINARY_MAGIC) {
      fatalHere("expected Intel GEN Device Binary magic");
    }
    skip(sizeof(uint32_t));
    auto version = decode<uint32_t>();
    (void)version;

    auto device = decode<uint32_t>();
    (void)device;
    // TODO: we could sanity check the device if we had that handy
    // failure to do that will just lead to CL_INVALID_BINARY though,
    // so better diagnostics are the only gain
    //
    auto ptr_size = peek<uint32_t>();
    if (ptr_size != sizeof(void *)) {
      fatalHere("GPUPointerSizeInBytes is not ",
        8*sizeof(void*), "b is this a ",8*ptr_size,"b binary");
    }
    skip(sizeof(ptr_size));
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
      pi.kernels.emplace_back();
      kernel_info &ki = pi.kernels.back();

      auto checksum = decode<uint32_t>(); // paranoid nitwits make life hard
      (void)checksum;
      //
      auto shader_hash_code = decode<uint64_t>();
      (void)shader_hash_code;
      //
      // strings sizes are aligned up to DW size
      auto kernel_name_size = decodeStringLength();
      //
      auto kernel_patch_list_size = decode<uint32_t>();
      //
      auto kernel_text_size = decode<uint32_t>();
      auto kernel_general_state_heap_size = decode<uint32_t>();
      auto kernel_dynamic_state_heap_size = decode<uint32_t>();
      auto kernel_surface_state_heap_size = decode<uint32_t>();
      auto kernel_text_unpadded_size = decode<uint32_t>();
      //
      ki.name = decodeString(kernel_name_size);
      //
      skip(
        kernel_general_state_heap_size +
        kernel_dynamic_state_heap_size +
        kernel_surface_state_heap_size +
        kernel_text_size);
      //
      // patches are here even though that's logically out of order;
      // just another wart from lazy programmers
      //
      size_t patch_bytes_read = 0;
      while (patch_bytes_read < kernel_patch_list_size) {
        auto patch_tag = decode<uint32_t>();
        auto patch_size = decode<uint32_t>();
        const uint32_t PATCH_TOKEN_EXECUTION_ENVIRONMENT = 0x17;
        const uint32_t PATCH_TOKEN_KERNEL_ARGUMENT_INFO = 0x1A;

        if (patch_tag == PATCH_TOKEN_EXECUTION_ENVIRONMENT) {
          ki.reqd_word_group_size[0] = decode<uint32_t>();
          ki.reqd_word_group_size[1] = decode<uint32_t>();
          ki.reqd_word_group_size[2] = decode<uint32_t>();
          skip(patch_size - 8 - (4 + 4 + 4));
        } else if(patch_tag == PATCH_TOKEN_KERNEL_ARGUMENT_INFO) {
          decodeKernelArgument(ki, ptr_size);
        } else {
          skip(patch_size - 8);
        }
        //
        patch_bytes_read += patch_size;
      } //
    }
  } //
}; // struct igb_decoder

program_info *cls::parseProgramInfoBinaryGEN(
  const opts &os,
  const fatal_handler *fh, loc at,
  const std::string &path)
{
  program_info *pi = new program_info();
  auto bits = sys::read_file_binary(path);
  igb_decoder d(*fh, at, bits.data(), bits.size(), *pi);
  d.run();
  return pi;
}