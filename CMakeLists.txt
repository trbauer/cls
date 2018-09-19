cmake_minimum_required(VERSION 3.0)

project(cls)

set(VERSION_STRING "0.9.0")

if(CMAKE_SIZEOF_VOID_P EQUAL 4)
  set(TARGET_MODIFIER "32")
  set(TARGET_MODIFIER_PATH "x86")
elseif(CMAKE_SIZEOF_VOID_P EQUAL 8)
  set(TARGET_MODIFIER "64")
  set(TARGET_MODIFIER_PATH "x64")
else()
  message(FATAL_ERROR "unexpected platform")
endif()

add_definitions(-DVERSION_STRING="${VERSION_STRING}")
if(CMAKE_SIZEOF_VOID_P EQUAL 4)
add_definitions(-DCLS_HOST_POINTER_SIZE=32)
elseif(CMAKE_SIZEOF_VOID_P EQUAL 8)
add_definitions(-DCLS_HOST_POINTER_SIZE=64)
endif()

##############################
# cls##.exe
##############################
file(GLOB CLS_SOURCES
  "src/*.hpp" "src/*.cpp"
  "src/ir/*.hpp" "src/ir/*.cpp"
  "src/parser/*.hpp" "src/parser/*.cpp" "src/parser/cls.flex"
  "src/processors/*.hpp" "src/processors/*.cpp"
	)

add_executable("cls${TARGET_MODIFIER}"
  ${CLS_SOURCES}
  )
#   cls.flex

find_package(OpenCL REQUIRED)
include_directories(. ${OpenCL_INCLUDE_DIRS})
# include_directories(.)
target_link_libraries("cls${TARGET_MODIFIER}" ${OpenCL_LIBRARIES})

set_target_properties("cls${TARGET_MODIFIER}" PROPERTIES
  CXX_STANDARD 17
  OUTPUT_NAME  "cls${TARGET_MODIFIER}"
)
