@echo off

REM Copy this file to your picture directory to quickly generate the YAML templates
REM ===============================================================================

for %%f in (*.jpg,*.png,*.gif) do (
  if exist "%%f.yaml" goto CONTINUE
    @echo %%f
    (
    echo title: %%f
    echo.
    echo datetime:
    echo.
    echo description:
    echo.
    echo tags:
    echo   - not_tagged
    ) >> "%%f.yaml"
  :CONTINUE
  rem
)
