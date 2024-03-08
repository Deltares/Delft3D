@ echo off
title run_delwaq
    rem
    rem This script runs Delwaq on Windows
    rem Adapt and use it for your own purpose
    rem
setlocal enabledelayedexpansion

    rem
    rem Set the config file
    rem
set argfile= 
if [%1] EQU [] (
    goto usage
) else (
    if [%1] EQU [--help] (
        goto usage
    ) else (
        set argfile=%1
    )
)
echo Configfile:%argfile%
if not exist %argfile% (
    if not exist %argfile%.inp (
        echo ERROR: configfile "%argfile%" does not exist
        goto usage
    )
)

set workdir=%CD%
set argfile=%workdir%\%argfile%
echo Working directory: %workdir%
    rem
    rem Set the directories containing the binaries
    rem
set D3D_HOME=%~dp0..
set waqdir=%D3D_HOME%\bin
set sharedir=%D3D_HOME%\share\delft3d
set libdir=%D3D_HOME%\lib
set PATH=%sharedir%;%libdir%;%waqdir%
rem Remove "\dwaq\scripts\..\..\.." from D3D_HOME

rem
rem process other arguments
rem
set userprocfile=none
set eco=false
set userspefile=none
set only2=false
set switches=

shift
:loop
if [%1] EQU [] (
   goto continue
)

if [%1] EQU [-p] (
   set userprocfile=%2
   shift
   ) else if [%1] EQU [-eco] (
      set eco=true
      if not [%2] EQU [] (
         set userspefile=%2
         shift
   ) ) else if [%1] EQU [-only2] (
      echo set only2=true
      set only2=true
   ) else (
   rem always copy all additional arguments to delwaq
   set switches=%switches% %1
   )
shift
goto loop
:continue

if [%userprocfile%] EQU [none] (
    set procfile=%sharedir%\proc_def
    ) else (
       set procfile=%userprocfile%
    )

if [%eco%] EQU [true] (
    if [%userspefile%] EQU [none] (
       set spefile=%sharedir%\bloom.spe
       ) else (
          set spefile=%userspefile%
       )
    )
if [%eco%] EQU [true] (
    set switches=%switches% -eco %spefile%
    )

    rem
    rem No adaptions needed below
    rem

if [%only2%] EQU [true] goto delwaq2
    rem
    rem Run delwaq 1
    rem
echo executing: "%waqdir%\delwaq1.exe" "%argfile%" -p "%procfile%" %switches% 
"%waqdir%\delwaq1.exe" "%argfile%" -p "%procfile%" %switches%

if %ERRORLEVEL% neq 0 (
    echo.
    echo Delwaq1 did not run correctly, ending calculation
    goto end
)
echo. 
echo Delwaq1 did run without errors.
echo.

:delwaq2
    rem Run delwaq 2
    rem
echo executing: "%waqdir%\delwaq2.exe" "%argfile%" %switches% 
"%waqdir%\delwaq2.exe" "%argfile%" %switches% 

if %ERRORLEVEL% neq 0 (
    echo.
    echo Delwaq2 did not run correctly
    goto end
)
echo. 
echo Delwaq2 did run without errors.


goto end

:usage
echo Usage:
echo run_delwaq.bat [--help] delwaq.inp [-p proc_def] [-eco [bloom.spe]] [...]
echo     --help             : (Optional) show this usage
echo     delwaq.inp         : (Mandatory) Delwaq input file
echo     -p proc_def        : use an alternative process library file instead of $D3D_HOME/share/delft3d/proc_def
echo     -np                : do not use any Delwaq processes (all substances will be seen as tracers)
echo     -eco [bloom.spe]   : use BLOOM, optionally using an alternative algea database for the default
echo                          $D3D_HOME/share/delft3d/bloom.spe
echo     ...                : any other options are passed through to Delwaq to process
:end
    rem To prevent the DOS box from disappearing immediately: remove the rem on the following line
rem pause
