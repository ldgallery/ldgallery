@echo off
set NODE_VERSION=12.22.2
cd ..\viewer\

chcp 65001
echo.
echo === nvm install ===
nvm install %NODE_VERSION%
nvm list | find /i "%NODE_VERSION% (Currently"
if errorlevel 1 (
  echo.
  echo === nvm use %NODE_VERSION% ===
  nvm use %NODE_VERSION%

  ping localhost -n 3 >NUL
  cmd /c npm install -g yarn
)
echo.
echo === yarn deps ===
cmd /c rmdir /s /q .\node_modules\
cmd /c yarn
echo.
