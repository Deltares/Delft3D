@echo off
rem Dump the full inherited environment for debugging before any of it is modified.
echo ===== Environment before set-env-vs2022.cmd =====
set
echo =================================================

rem Recreate the 8.3 short names for the space-containing "Program Files" folders.
rem PETSc's Windows configure (run from Cygwin) uses `cygpath -ms` to obtain a
rem space-free path to the Intel oneAPI MPI/MKL libraries. Windows containers do
rem not persist 8.3 metadata changes made to base-image folders at build time, so
rem the short names must be (re)created at container startup instead.
fsutil 8dot3name set C: 0 >nul 2>&1
fsutil file setshortname "C:\Program Files" PROGRA~1 >nul 2>&1
fsutil file setshortname "C:\Program Files (x86)" PROGRA~2 >nul 2>&1

rem TeamCity sets the PATH and TMP environment variables in env. build parameters,
rem and these are also forwarded into the container. The PATH variable only makes sense
rem on the host, so reset it to its registry value before continuing.
rem The TEMP/TMP/TMPDIR variables point to a mounted folder where executables cannot be run
rem sometimes, which breaks the build process of PETSc for example when it runs some test
rem executables there.
if not exist C:\build-temp mkdir C:\build-temp
set TEMP=C:\build-temp
set TMP=C:\build-temp
set TMPDIR=C:\build-temp

for /f "tokens=2,*" %%A in ('reg query "HKLM\SYSTEM\CurrentControlSet\Control\Session Manager\Environment" /v Path 2^>nul') do set "PATH=%%B"

call "C:\\Program Files (x86)\\Intel\\oneAPI\\setvars.bat" --force
call "C:\\Program Files\\Microsoft Visual Studio\\17\\Community\\Common7\\Tools\\VsDevCmd.bat" -arch=amd64 -host_arch=amd64
