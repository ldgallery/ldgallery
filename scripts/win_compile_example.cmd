@echo off
SET rebuild=--rebuild
CHOICE /M "Rebuild all ('--rebuild' argument)?"
IF ERRORLEVEL 2 SET rebuild=
echo.
cd ..\compiler\

@echo on
stack exec ldgallery -- %rebuild% --clean-output -i=../example/src/ -o=../example/out/
@pause
