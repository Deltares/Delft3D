@echo off
rem ##########################
rem ### user defined input ###
rem ##########################

set model=eutrof1a
set compiler_path=c:\MinGW\bin

if exist "%compiler_path%"\gfortran.exe (

rem ############################################################
rem ### compile FORTRAN90 code into dll to be read by DELWAQ ###
rem ############################################################

copy ..\packages\hydduflow\errsys.f90 errsys.f90
copy ..\packages\hydduflow\hydduflow.f90 hydduflow.f90
"%compiler_path%"\gfortran -c %model%.f90    -ffree-line-length-none -fno-underscoring
"%compiler_path%"\gfortran -c errsys.f90     -ffree-line-length-none -fno-underscoring
"%compiler_path%"\gfortran -c hydduflow.f90  -ffree-line-length-none -fno-underscoring
"%compiler_path%"\gfortran -shared -o %model%.dll %model%.o errsys.o hydduflow.o -static 

pause
) else (
echo %compiler_path%\gfortran.exe does not exist.
echo - Did you install MinGW? Get it from http://www.mingw.org.
echo - When you installed MinGW, but not in %compiler_path%, change the path in this batchfile
pause
)
