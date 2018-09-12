cmake_minimum_required(VERSION 3.0)

project(cls)

set(VERSION_STRING "1.0.0")

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


##############################
# cls##.exe
##############################
file(GLOB CLS_SOURCES "src/*.hpp" "src/*.cpp")

add_executable("cls${TARGET_MODIFIER}"
  ${CLS_SOURCES}
  )
#   cls.flex

find_package(OpenCL REQUIRED)
include_directories(${OpenCL_INCLUDE_DIRS})
target_link_libraries("cls${TARGET_MODIFIER}" ${OpenCL_LIBRARIES})

set_target_properties("cls${TARGET_MODIFIER}" PROPERTIES
   OUTPUT_NAME  "cls${TARGET_MODIFIER}"
)
