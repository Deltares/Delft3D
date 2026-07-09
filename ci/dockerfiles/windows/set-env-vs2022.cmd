@echo off
rem Recreate the 8.3 short names for the space-containing "Program Files" folders.
rem PETSc's Windows configure (run from Cygwin) uses `cygpath -ms` to obtain a
rem space-free path to the Intel oneAPI MPI/MKL libraries. Windows containers do
rem not persist 8.3 metadata changes made to base-image folders at build time, so
rem the short names must be (re)created at container startup instead.
fsutil 8dot3name set C: 0 >nul 2>&1
fsutil file setshortname "C:\Program Files" PROGRA~1 >nul 2>&1
fsutil file setshortname "C:\Program Files (x86)" PROGRA~2 >nul 2>&1

call "C:\\Program Files (x86)\\Intel\\oneAPI\\setvars.bat" --force
call "C:\\Program Files\\Microsoft Visual Studio\\17\\Community\\Common7\\Tools\\VsDevCmd.bat" -arch=amd64 -host_arch=amd64
