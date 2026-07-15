@echo off
rem Recreate the 8.3 short names for the space-containing "Program Files" folders.
rem PETSc's Windows configure (run from Cygwin) uses `cygpath -ms` to obtain a
rem space-free path to the Intel oneAPI MPI/MKL libraries. Windows containers do
rem not persist 8.3 metadata changes made to base-image folders at build time, so
rem the short names must be (re)created at container startup instead.
fsutil 8dot3name set C: 0 >nul 2>&1
fsutil file setshortname "C:\Program Files" PROGRA~1 >nul 2>&1
fsutil file setshortname "C:\Program Files (x86)" PROGRA~2 >nul 2>&1

rem TeamCity forwards the build configuration's environment variables into the
rem container. Among the forwarded ones are TEMP/TMP/TMPDIR, which point to a host
rem folder that is bind-mounted into the container. Executables sometimes cannot be
rem run from that mount, which breaks builds that compile and run test executables
rem there (e.g. PETSc's ./configure). Redirect them to a container-local directory.
if not exist C:\build-temp mkdir C:\build-temp
set TEMP=C:\build-temp
set TMP=C:\build-temp
set TMPDIR=C:\build-temp

call "C:\\Program Files (x86)\\Intel\\oneAPI\\setvars.bat" --force
