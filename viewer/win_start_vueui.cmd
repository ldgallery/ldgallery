@echo off
set NODE_VERSION=12.14.0
set VUECLI_VERSION=4.1.1

chcp 65001
echo.
echo === nvm install ===
nvm install %NODE_VERSION%
nvm list | find /i "%NODE_VERSION% (Currently"
if errorlevel 1 (
  echo.
  echo === nvm use %NODE_VERSION% ===
  nvm use %NODE_VERSION%

  echo.
  echo === npm install @vue/cli@%VUECLI_VERSION% ===
  ping localhost -n 3 >NUL
  cmd /c npm install -g @vue/cli@%VUECLI_VERSION%
)
echo.
echo === npm install ===
cmd /c npm install
echo.
echo === npm prune ===
cmd /c npm prune
echo.
echo === npm dedupe ===
cmd /c npm dedupe
echo.
echo === vue ui ===
title Vue UI - server
vue ui
