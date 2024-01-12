#!/bin/sh

export CC=/usr/bin/clang
export CXX=/usr/bin/clang++

mkdir -p builds/clang/Debug
cd builds/clang/Debug
cmake -G "Unix Makefiles" ../../.. "-DCMAKE_BUILD_TYPE=DEBUG"
cd ../../..

mkdir -p builds/clang/Release
cd builds/clang/Release
cmake -G "Unix Makefiles" ../../.. "-DCMAKE_BUILD_TYPE=RELEASE"
cd ../../..
