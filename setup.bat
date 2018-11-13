mkdir builds\vs2017-64
cd builds\vs2017-64
cmake -G "Visual Studio 15 2017 Win64" ..\..
cd ..\..
@echo ==============================================================
@echo ==^> ADD .editorconfig to VS Solution Node ^(or SHIFT+ALT+A^)