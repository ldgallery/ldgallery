@echo off
SET rebuild=--rebuild
CHOICE /M "Rebuild all?"
IF ERRORLEVEL 2 SET rebuild=
echo.
echo stack exec ldgallery-compiler-exe -- %rebuild% -i=../example/ -o=../example/out/
echo.
stack exec ldgallery-compiler-exe -- %rebuild% -i=../example/ -o=../example/out/
echo.
pause
