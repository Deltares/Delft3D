@echo off
rem MSVC60OPTS.BAT
rem
rem    Compile and link options used for building MEX-files
rem    using the Microsoft Visual C++ compiler version 6.0 
rem
rem    $Revision: 1.11 $  $Date: 2001/11/06 16:53:46 $
rem
rem ********************************************************************
rem General parameters
rem ********************************************************************
set UNIDATA_INC=h:\src\netcdf-3.6.1\src\libsrc
set UNIDATA_LIB=h:\src\netcdf-3.6.1\src\win32\NET\Debug
set UNIDATA_LIBS=h:\src\netcdf-3.6.1\src\win32\NET\Debug\netcdf.lib

set MATLAB=%MATLAB%
set MSVCDir=C:\Program Files\Microsoft Visual Studio\VC98
set MSDevDir=%MSVCDir%\..\Common\msdev98
set PATH=%MSVCDir%\BIN;%MSDevDir%\bin;%PATH%
set INCLUDE=%UNIDATA_INC%;%MSVCDir%\INCLUDE;%MSVCDir%\MFC\INCLUDE;%MSVCDir%\ATL\INCLUDE;%INCLUDE%
set LIB=%MSVCDir%\LIB;%MSVCDir%\MFC\LIB;%LIB%

rem ********************************************************************
rem Compiler parameters
rem ********************************************************************
set COMPILER=cl
set COMPFLAGS=-c -Zp8 -G5 -W3 -DMATLAB_MEX_FILE -nologo -DDLL_NETCDF
set OPTIMFLAGS=/MD -O2 -Oy- -DNDEBUG
set DEBUGFLAGS=/MDd -Zi -Fd"%OUTDIR%%MEX_NAME%.pdb"
set NAME_OBJECT=/Fo

rem ********************************************************************
rem Linker parameters
rem ********************************************************************
set LIBLOC=%MATLAB%\extern\lib\win32\microsoft\msvc60
set LINKER=link
set LINKFLAGS=/dll /export:%ENTRYPOINT% /MAP /LIBPATH:"%LIBLOC%" libmx.lib libmex.lib libmatlb.lib libmat.lib /implib:%LIB_NAME%.x /LIBPATH:"%UNIDATA_LIB%" netcdf.lib
set LINKOPTIMFLAGS=
set LINKDEBUGFLAGS=/debug
set LINK_FILE=
set LINK_LIB=
set NAME_OUTPUT=/out:"%OUTDIR%%MEX_NAME%.dll"
set RSP_FILE_INDICATOR=@

rem ********************************************************************
rem Resource compiler parameters
rem ********************************************************************
set RC_COMPILER=rc /fo "%OUTDIR%mexversion.res"
set RC_LINKER=

set POSTLINK_CMDS=del "%OUTDIR%%MEX_NAME%.map"
set POSTLINK_CMDS1=del %LIB_NAME%.x
