mkdir builds\vs2017-64
cd builds\vs2017-64
cmake -G "Visual Studio 15 2017 Win64" ..\..
cd ..\..
mkdir builds\vs2017-32
cd builds\vs2017-32
cmake -G "Visual Studio 15 2017" ..\..
cd ..\..