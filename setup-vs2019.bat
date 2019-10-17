mkdir builds\vs2019-64
cd builds\vs2019-64
cmake -G "Visual Studio 16 2019" -A x64 ..\..
cd ..\..

mkdir builds\vs2019-32
cd builds\vs2019-32
cmake -G "Visual Studio 16 2019" ..\..
cd ..\..
