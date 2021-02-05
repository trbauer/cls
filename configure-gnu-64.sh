#!/bin/sh

mkdir -p builds/gnu-64/Debug
cd builds/gnu-64/Debug
cmake -G "Unix Makefiles" ../../.. "-DCMAKE_BUILD_TYPE=DEBUG"
cd ../../..

mkdir -p builds/gnu-64/Release
cd builds/gnu-64/Release
cmake -G "Unix Makefiles" ../../.. "-DCMAKE_BUILD_TYPE=RELEASE"
cd ../../..
