mkdir builds\vs2019-64
cd builds\vs2019-64
cmake -G "Visual Studio 16 2019" -A x64 ..\..
cd ..\..

@REM This is how to configure 32b version
@REM mkdir builds\vs2019-32
@REM cd builds\vs2019-32
@REM cmake -G "Visual Studio 16 2019" -A Win32 ..\..
@REM cd ..\..
