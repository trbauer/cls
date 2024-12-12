cmake_minimum_required(VERSION 3.23)

project(cls)

###############################################################################
# TODO: valid build arguments with default values
# https://stackoverflow.com/questions/6787371/how-do-i-specify-build-options-for-cmake-based-projects
# option(INCLUDE_LodePNG "includes png support via LodePNG" True)
#
###############################################################################

################################################################################
# Build the version string based on repository information.
# If that is absent (e.g. you don't  have git installed or git fails
# for some other reason, then we generate an invalid string so the user
# can at least build the system.
#
# This defines ${CLS_VERSION_STRING} as something like
#   "1.69-3610b3a" or "1.69-3610b3a+" (suffixed with +)
#
# * The initial "1" is an arbitrary major version which gets incremented during
#   major breaking changes (e.g. radical syntax changes)
#
# * The next ".69" is a revision index, which is the count of commits
#   in the current branch.
#
# * The suffix "3610b3a" is the git short hash.
#
# * The final optional "*" indicates that there there are outstanding changes
#   to the repository.
#
set (CLS_MAJOR_VERSION "1")
execute_process(
    COMMAND git rev-parse --short HEAD
    RESULT_VARIABLE CLS_GIT_SUCCEEDED
    OUTPUT_VARIABLE CLS_GIT_SHORT_HASH
    OUTPUT_STRIP_TRAILING_WHITESPACE
    WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
)
if (CLS_GIT_SUCCEEDED EQUAL 0)
  execute_process(
      COMMAND git diff-index --quiet HEAD
      RESULT_VARIABLE CLS_GIT_IS_DIRTY
      OUTPUT_STRIP_TRAILING_WHITESPACE
      WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
  )
  message("  git diff-index --quiet HEAD ==> ${CLS_GIT_IS_DIRTY}")
  if (${CLS_GIT_IS_DIRTY})
      execute_process(
          COMMAND git diff-index HEAD
          OUTPUT_STRIP_TRAILING_WHITESPACE
          OUTPUT_VARIABLE CLS_WHICH_GIT_FILES_DIRTY
          WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
      )
      message("  these files are different ==>")
      message("  ${CLS_WHICH_GIT_FILES_DIRTY}")
  endif()
  # git rev-list --count --all HEAD gives all branches
  execute_process(
      COMMAND git rev-list --count HEAD
      OUTPUT_VARIABLE CLS_GIT_REV_INDEX
      OUTPUT_STRIP_TRAILING_WHITESPACE
      WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
  )
  if (${CLS_GIT_IS_DIRTY})
    set(CLS_GIT_IS_DIRTY_SUFFIX "")
  else ()
    set(CLS_GIT_IS_DIRTY_SUFFIX "+")
  endif()
  set(CLS_VERSION_STRING "${CLS_MAJOR_VERSION}.${CLS_GIT_REV_INDEX}-${CLS_GIT_SHORT_HASH}${CLS_GIT_IS_DIRTY_SUFFIX}")
else()
  message("== GIT not found on the system (version will be unknown)")
  set(CLS_VERSION_STRING "${CLS_MAJOR_VERSION}.unknown")
endif()

message("== CLS_VERSION_STRING = ${CLS_VERSION_STRING}")
#####################################################


if(CMAKE_SIZEOF_VOID_P EQUAL 4)
  set(TARGET_MODIFIER "32")
  set(TARGET_MODIFIER_PATH "x86")
elseif(CMAKE_SIZEOF_VOID_P EQUAL 8)
  set(TARGET_MODIFIER "64")
  set(TARGET_MODIFIER_PATH "x64")
else()
  message(FATAL_ERROR "unexpected platform")
endif()

# On Windows MSVC enable both 32 and 64 as separate targets.
# Everyone else omits the suffix.
if (MSVC)
    set(CLS_EXE "cls${TARGET_MODIFIER}")
else()
    set(CLS_EXE "cls")
endif()

add_definitions(-DCLS_VERSION_STRING="${CLS_VERSION_STRING}")
add_definitions(-DCLS_EXE="${CLS_EXE}")

if(CMAKE_SIZEOF_VOID_P EQUAL 4)
  add_definitions(-DCLS_HOST_POINTER_SIZE=32)
elseif(CMAKE_SIZEOF_VOID_P EQUAL 8)
  add_definitions(-DCLS_HOST_POINTER_SIZE=64)
endif()



###############################################################################
# include LodePNG if it's present
# place deps/lodepng/loadpng.{h,cpp}
set(LODE_PNG_SRC )
if (EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/deps/lodepng/lodepng.cpp" AND
    EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/deps/lodepng/lodepng.h")
  message("== Including LodePNG for PNG support from deps/lodepng")
  set(USE_LODE_PNG 1)
  add_definitions("-DUSE_LODE_PNG")
  set(LODE_PNG_SRC "deps/lodepng/lodepng.h" "deps/lodepng/lodepng.cpp")
else()
  set(USE_LODE_PNG 0)
  message("== OMITTING LodePNG for PNG support")
  message("==   fetch https://lodev.org/lodepng/ and place in deps/lodepng/lodepng.{h,cpp})")
endif()

###############################################################################
# cls##.exe
###############################################################################
# we split the above up so we can setup the source groups
file(GLOB CLS_ROOT
  "src/*.hpp" "src/*.cpp"
)
file(GLOB CLS_IR
  "src/ir/*.hpp" "src/ir/*.cpp"
)
file(GLOB CLS_PARSE
  "src/parser/*.hpp" "src/parser/*.cpp" "src/parser/cls.flex"
)
file(GLOB CLS_PROC
  "src/processors/*.hpp" "src/processors/*.cpp"
)
file(GLOB MDAPI_SRC
  "deps/mdapi/*.h" "deps/mdapi/*.hpp" "deps/mdapi/*.cpp"
)

source_group("src" FILES ${CLS_ROOT})
source_group("src\\ir" FILES ${CLS_IR})
source_group("src\\parser" FILES ${CLS_PARSE})
source_group("src\\interp" FILES ${CLS_PROC})
if (${USE_LODE_PNG})
  source_group("src\\deps\\lodepng" FILES ${LODE_PNG_SRC})
endif()
source_group("src\\deps\\mdapi" FILES ${MDAPI_SRC})

set(CLS_SOURCES
  ${CLS_ROOT}
  ${CLS_IR}
  ${CLS_PARSE}
  ${CLS_PROC}
  ${LODE_PNG_SRC}
  ${MDAPI_SRC})

add_executable("${CLS_EXE}" ${CLS_SOURCES})

###############################################################################
# enable parallel build
if (MSVC)
  target_compile_options("${CLS_EXE}" PRIVATE "/MP")
#  target_compile_options("${CLS_EXE}" PRIVATE "/MP" "/w4")
#  add_compile_options("/w4")
endif()

if (UNIX)
  set(CMAKE_CXX_FLAGS ${COMMON_C_FLAGS})
  set(CMAKE_CXX_FLAGS_RELEASE "-O3")
  set(CMAKE_CXX_FLAGS_DEBUG   "-D_DEBUG -D__DEBUG -O0 -g")

  set(CMAKE_C_FLAGS ${COMMON_C_FLAGS})
  set(CMAKE_C_FLAGS_RELEASE "-O3")
  set(CMAKE_C_FLAGS_DEBUG   "-D_DEBUG -D__DEBUG -O0 -g")
endif()
# disable nagging warning on MSVC 2017 since I don't own that source
if (${USE_LODE_PNG} AND MSVC)
  set_source_files_properties(
    SOURCE "deps/lodepng/lodepng.cpp"
    COMPILE_FLAGS "/wd4267")
endif()

###############################################################################
# Include OpenCL headers and stub libraries.
# Note, I'm using the latest Intel OpenCL SDK.
#
# TODO: need to ensure it's a 2.0 version when we start using SVM
# NOTE: find_patckage above isn't working because find_package(OpenCL ...)
# doesn't find a OpenCL 2.0 version of the library.
# (c.f. $CMAKE_ROOT/share/cmake-3.11/Modules/FindOpenCL.cmake)
# This could be because Intel moved their SDK paths around recently
# (I did have env $INTELOCLSDKROOT set).
# For now we just set the paths explicitly.
#
# Maybe it makes sense to not use find_package(...) because this tool has
# such sensitive needs (e.g. needs binding points for SVM and KHR stubs).
# Some SDKs do not have new enough libraries and miss some of these.
#
# Using headers from my own version of deps/CL/* doesn't work
# (or I can't figure it out)
# Cmake or the compiler sniffs out headers from SDKs or the system and causes
# cross-version chaos.  I tried prepending inlcudes without success.
#
#
# FIXME: I would love a better solution to this.
#
if (WIN32)
  if (NOT EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/deps/oclsdk")
    message(FATAL_ERROR
       "deps/oclsdk/* not found; download Intel OpenCL SDK and copy in inlcude and libs under this")
  endif()
  set(OpenCL_INCLUDE_DIRS "${CMAKE_CURRENT_SOURCE_DIR}/deps/oclsdk/include")
  if (NOT EXISTS "${OpenCL_INCLUDE_DIRS}")
    message(FATAL_ERROR
        "${OpenCL_INCLUDE_DIRS}: OpenCL include directory; setup deps/oclsdk/include")
  endif()
#  if(CMAKE_SIZEOF_VOID_P EQUAL 4)
#    set(OpenCL_LIBRARIES "${CMAKE_CURRENT_SOURCE_DIR}/deps/oclsdk/lib/x86/OpenCL.lib")
#  else()
#    set(OpenCL_LIBRARIES "${CMAKE_CURRENT_SOURCE_DIR}/deps/oclsdk/lib/x64/OpenCL.lib")
#  endif()
#  if (NOT EXISTS "${OpenCL_LIBRARIES}")
#    message(FATAL_ERROR
#        "${OpenCL_LIBRARIES}: OpenCL library not found; setup deps/oclsdk")
#  endif()
else()
  find_package(OpenCL REQUIRED)
endif()

target_include_directories("${CLS_EXE}" PUBLIC ${OpenCL_INCLUDE_DIRS})
if (WIN32)
  target_link_libraries("${CLS_EXE}" SetupAPI Shlwapi)
#  target_link_libraries("${CLS_EXE}" "${OpenCL_LIBRARIES}" SetupAPI Shlwapi)
else()
  target_link_libraries("${CLS_EXE}" ${OpenCL_LIBRARIES} "dl" "pthread")
endif()

# target_compile_features(${CLS_EXE} PRIVATE cxx_std_17)
target_compile_features(${CLS_EXE} PRIVATE cxx_std_20)

set_target_properties("${CLS_EXE}" PROPERTIES
  CXX_STANDARD 20
  OUTPUT_NAME  "${CLS_EXE}"
)




###############################################################################
###############################################################################
###############################################################################

set(CL2BIN_EXE "cl2bin")

set(CL2BIN_SOURCES
    "src/cl2bin/cl2bin.cpp"
    "src/cl_headers.hpp"
    "src/cl_exts.hpp"
    "src/cls_opts.hpp"
    "src/cl_lib.cpp" "src/cl_lib.hpp"
    "src/devices.hpp" "src/devices.cpp"
    "src/fatal.hpp" "src/fatal.cpp"
    "src/system.hpp" "src/system.cpp"
    "src/text.hpp" "src/text.cpp"
    )

add_executable("${CL2BIN_EXE}" ${CL2BIN_SOURCES})

source_group("src\\cl2bin" FILES "src/cl2bin/cl2bin.cpp")

target_compile_features(${CL2BIN_EXE} PRIVATE cxx_std_20)
target_include_directories("${CL2BIN_EXE}" PUBLIC ${OpenCL_INCLUDE_DIRS} "src")


if (MSVC)
  target_compile_options("${CL2BIN_EXE}" PRIVATE "/MP")
#  target_compile_options("${CL2BIN_EXE}" PRIVATE "/MP" "/w4")
endif()

set_target_properties("${CL2BIN_EXE}" PROPERTIES
  CXX_STANDARD 20
  OUTPUT_NAME  "${CL2BIN_EXE}"
)
