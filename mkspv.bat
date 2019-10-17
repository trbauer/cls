@REM ocloc.bat calls ocloc.exe which comes from the Intel NEO Compute Runtime
@REM C.f. https://github.com/intel/compute-runtime
call ocloc.bat -file tests/spirv.cl -device skl
@del spirv_Gen9core.bin
@del spirv_Gen9core.gen
@move /Y spirv_Gen9core.spv tests/spirv.spv
@spirv-dis --offsets --raw-id tests/spirv.spv > tests/spirv.spvtxt

call ocloc.bat -file tests/add.cl -device skl -options "-DT=int"
@del add_Gen9core.bin
@del add_Gen9core.gen
@move /Y add_Gen9core.spv tests/add_int.spv
@spirv-dis --offsets tests/add_int.spv > tests/add_int.spvtxt

call ocloc.bat -file tests/add.cl -device skl -options "-DT=float2"
@del add_Gen9core.bin
@del add_Gen9core.gen
@move /Y add_Gen9core.spv tests/add_float2.spv
@spirv-dis --offsets tests/add_float2.spv > tests/add_float2.spvtxt

call ocloc.bat -file tests/particles.cl -device skl
@del particles_Gen9core.bin
@del particles_Gen9core.gen
@move /Y particles_Gen9core.spv tests/particles.spv
@spirv-dis --offsets tests/particles.spv > tests/particles.spvtxt
