@echo off
SET rebuild=--rebuild
CHOICE /M "Rebuild all ('--rebuild' argument)?"
IF ERRORLEVEL 2 SET rebuild=
echo.

@echo on
stack exec ldgallery-compiler-exe -- %rebuild% --clean-output -i=../example/ -o=../example/out/
@pause
