@echo off
cd ..\compiler\
stack setup
stack build --flag ldgallery-compiler:portable --copy-bins --local-bin-path dist
pause
