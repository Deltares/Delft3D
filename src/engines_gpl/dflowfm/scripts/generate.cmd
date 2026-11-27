@echo off
setlocal EnableDelayedExpansion
pushd "%~dp0..\packages\dflowfm_lib\include" || exit /b 1
set "bmi_fortran_args="
for /f "usebackq delims=" %%F in ("..\..\..\scripts\fortranfiles.txt") do (
    set bmi_fortran_args=!bmi_fortran_args! "%%F"
)
python ..\..\..\scripts\template\generate.py --fortranfiles !bmi_fortran_args! --template-dir=..\..\dflowfm_lib\templates --verbose
set "result=%ERRORLEVEL%"
popd
exit /b %result%
