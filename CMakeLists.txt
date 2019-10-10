cmake_minimum_required(VERSION 3.0)

project(cls)

###############################################################################
# TODO: valid build arguments with default values
# https://stackoverflow.com/questions/6787371/how-do-i-specify-build-options-for-cmake-based-projects
# option(INCLUDE_LodePNG "includes png support via LodePNG" True)
#
#####################################################
set(VERSION_STRING "0.9.6")

if(CMAKE_SIZEOF_VOID_P EQUAL 4)
  set(TARGET_MODIFIER "32")
  set(TARGET_MODIFIER_PATH "x86")
elseif(CMAKE_SIZEOF_VOID_P EQUAL 8)
  set(TARGET_MODIFIER "64")
  set(TARGET_MODIFIER_PATH "x64")
else()
  message(FATAL_ERROR "unexpected platform")
endif()

# NOTE: use MSVC (bare non-variable)   if (MSVC)
# if(${CMAKE_GENERATOR} MATCHES "Visual Studio")
#  set(MSVC_GENERATOR 1)
# endif()

add_definitions(-DVERSION_STRING="${VERSION_STRING}")

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
  message("== Including LodePNG for PNG support in deps/lodepng")
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
# file(GLOB CLS_SOURCES
#  "src/*.hpp" "src/*.cpp"
#  "src/ir/*.hpp" "src/ir/*.cpp"
#  "src/parser/*.hpp" "src/parser/*.cpp" "src/parser/cls.flex"
#  "src/processors/*.hpp" "src/processors/*.cpp"
#	)
#
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

source_group("Source" FILES ${CLS_ROOT})
source_group("Source\\IR" FILES ${CLS_IR})
source_group("Source\\Parser" FILES ${CLS_PARSE})
source_group("Source\\Interpreters" FILES ${CLS_PROC})
if (${USE_LODE_PNG})
  source_group("Source\\LodePNG" FILES ${LODE_PNG_SRC})
endif()


# set(CLS_SOURCES ${CLS_ROOT} ${CLS_IR} ${CLS_PARSE} ${CLS_PROC} .editorconfig)
set(CLS_SOURCES ${CLS_ROOT} ${CLS_IR} ${CLS_PARSE} ${CLS_PROC} ${LODE_PNG_SRC})

add_executable("cls${TARGET_MODIFIER}"
  ${CLS_SOURCES}
  )

###############################################################################
# enable parallel build
if (MSVC)
  target_compile_options("cls${TARGET_MODIFIER}" PRIVATE "/MP")
  add_compile_options("/w4")
endif()
# disable nagging warning on MSVC 2017 since I don't own that source
if (${USE_LODE_PNG} AND MSVC)
  set_source_files_properties(SOURCE "deps/lodepng/lodepng.cpp" COMPILE_FLAGS "/wd4267")
endif()

###############################################################################
# Include OpenCL headers and stub libraries.
# Note, I'm using the latest Intel OpenCL SDK.
#
# TODO: need to ensure it's a 2.0 version when we start using SVM
# find_package(OpenCL REQUIRED)
# include_directories(. ${OpenCL_INCLUDE_DIRS})
# target_link_libraries("cls${TARGET_MODIFIER}" ${OpenCL_LIBRARIES})
#
# NOTE: the above aren't working because find_package(OpenCL ...) does
# find a OpenCL 2.0 version of the library
# (c.f. $CMAKE_ROOT/share/cmake-3.11/Modules/FindOpenCL.cmake)
# This could be because Intel moved their SDK paths around recently
# (I did have env $INTELOCLSDKROOT set).
# For now we just set the paths explicitly.
#
# Maybe it makes sense to not use find_package(...) because this tool has
# such sensitive needs (e.g. needs binding points for SVM and KHR stubs).
# Some SDKs do not have new enough libraries and miss some of these.
#
set(OpenCL_INCLUDE_DIRS "C:\\Intel\\OpenCL\\sdk\\include")
if(CMAKE_SIZEOF_VOID_P EQUAL 4)
set(OpenCL_LIBRARY_STUB "C:\\Intel\\OpenCL\\sdk\\lib\\x86\\OpenCL.lib")
else()
set(OpenCL_LIBRARY_STUB "C:\\Intel\\OpenCL\\sdk\\lib\\x64\\OpenCL.lib")
endif()

# Using headers from ./CL is not working out
# (cmake or the compiler sniffs out headers from SDKs and causes
# cross-version chaos)
# include_directories(.)

include_directories(${OpenCL_INCLUDE_DIRS})

target_link_libraries("cls${TARGET_MODIFIER}" ${OpenCL_LIBRARY_STUB})

set_target_properties("cls${TARGET_MODIFIER}" PROPERTIES
  CXX_STANDARD 17
  OUTPUT_NAME  "cls${TARGET_MODIFIER}"
)
