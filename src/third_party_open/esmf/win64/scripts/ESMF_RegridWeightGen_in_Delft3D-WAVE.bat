@echo off

if exist esmf_bat.log del esmf_bat.log
echo screen output of ESMF_RegridWeightGen_in_Delft3D-WAVE.bat is written to this file >esmf_bat.log
echo and will be overwritten everytime that ESMF_RegridWeightGen_in_Delft3D-WAVE.bat is executed >>esmf_bat.log
echo >>esmf_bat.log

setlocal enabledelayedexpansion

    rem Get the location of this script and ESMF_RegridWeightGen.exe
set workdir=%CD%
set scriptdir=%~dp0
set exedir=%scriptdir%\..\bin
set regridexec="%exedir%\ESMF_RegridWeightGen.exe"

echo Executing batchscript "ESMF_RegridWeightGen_in_Delft3D-WAVE.bat" for Delft3D-WAVE >>esmf_bat.log
echo This script is located in directory %scriptdir% >>esmf_bat.log
echo Using %regridexec% >>esmf_bat.log

    rem Arguments
if [%3] EQU [] (
    echo "ERROR: script called without the correct number of arguments" >>esmf_bat.log
    goto usage
)
set srcfile=%1
set destfile=%2
set wfile=%3
set addflags=--method bilinear
if [%4] EQU [CARTESIAN] (
    set addflags=%addflags% --src_loc corner --dst_loc corner
)

set defaultflags=--ignore_unmapped
set arguments=%defaultflags% %addflags% --source %srcfile% --destination %destfile% --weight %wfile%

set PATH=%exedir%;%PATH%

    rem Remove output file
if exist %wfile% del %wfile%
if exist PET0.RegridWeightGen.Log del PET0.RegridWeightGen.Log
    rem Check whether needed files exist
if not exist %srcfile% goto error1
if not exist %destfile% goto error2
if not exist %regridexec% goto errorexec

   rem RUN
echo Calling ESMF_RegridWeightGen with arguments: %arguments% >>esmf_bat.log
%regridexec% %arguments% >>esmf_bat.log

goto end



:usage
echo "Usage:" >>esmf_bat.log
echo "ESMF_RegridWeightGen_in_Delft3D-WAVE.bat <sourcefile> <destfile> <weightfile> [addflags]" >>esmf_bat.log
echo "   <sourcefile>: (input)    name of source file      (NetCDF)" >>esmf_bat.log
echo "   <destfile>  : (input)    name of destination file (NetCDF)" >>esmf_bat.log
echo "   <weightfile>: (output)   name of weight file      (NetCDF)" >>esmf_bat.log
echo "   [addflags]  : (optional) additional flags. Possible values: CARTESIAN" >>esmf_bat.log
goto end

:error1
echo >>esmf_bat.log
echo     ************************************************************** >>esmf_bat.log
echo     ERROR: Source file does not exist: %srcfile% >>esmf_bat.log
echo     ************************************************************** >>esmf_bat.log
rem pause
goto end

:error2
echo >>esmf_bat.log
echo     ************************************************************** >>esmf_bat.log
echo     ERROR: Destination file does not exist: %destfile% >>esmf_bat.log
echo     ************************************************************** >>esmf_bat.log
rem pause
goto end

:errorexec
echo >>esmf_bat.log
echo     ************************************************************** >>esmf_bat.log
echo     ERROR: Executable does not exist: %regridexec% >>esmf_bat.log
echo     ************************************************************** >>esmf_bat.log
rem pause
goto end



:end

    rem To prevent the DOS box from disappearing immediately: remove the rem on the following line
rem pause
