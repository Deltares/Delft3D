@ echo off
title run_delwaq
    rem
    rem This script runs Delwaq on Windows
    rem Adapt and use it for your own purpose
    rem
setlocal enabledelayedexpansion

rem show usage?
if [%1] EQU []        goto usage
if [%1] EQU [-h]      goto usage
if [%1] EQU [--help]  goto usage
if [%1] EQU [--usage] goto usage

rem set the directories containing the binaries, proc_def and bloom.spe and set PATH
set bindir=%~dp0
set libdir=%bindir%..\lib
set sharedir=%bindir%..\share\delft3d
set PATH=%libdir%;%bindir%;%PATH%

rem set some defaults
set userprocfile=none
set eco=false
set noprocesses=false
set userspefile=none
set switches=

rem process the arguments (name of the input file is always the first argument)
set inputfile=%1
:loop
shift
set argument=%1
if [%argument%] EQU [] goto endloop
set value=%2
if not [%value%] EQU [] (set switch=%value:~0,1%) else (set switch="")
if [%argument%] EQU [-p] (
    set userprocfile=%value%
    shift
) else if [%argument%] EQU [-eco] (
    set eco=true
    if [%value%] EQU [] goto loop 
    if [%switch%] EQU [-] goto loop
    set userspefile=%value%
    shift
) else if [%argument%] EQU [-np] (
    set noprocesses=true
) else (
    rem always copy all additional arguments to delwaq
    set switches=%switches% %argument%)
)
goto :loop
:endloop

rem provide default proc_def and bloom.spe when not provided by the user
if [%userprocfile%] EQU [none] (set procfile=%sharedir%\proc_def) else (set procfile=%userprocfile%)
if [%noprocesses%] EQU [false] (set switches=%switches%  -p %procfile%) else (set switches=%switches% -np)
if [%userspefile%] EQU [none] (set spefile=%sharedir%\bloom.spe) else (set spefile=%userspefile%)
if [%eco%] EQU [true] set switches=%switches% -eco %spefile% 

rem go to input file directory, run delwaq, and return
set currentdir=%CD%
for %%A in ("%inputfile%") do (
    set argName=%%~nxA
    set argPath=%%~dpA
)
cd /d "%argPath%"
echo executing in this window: "%bindir%delwaq.exe" %argName%  %switches%
"%bindir%delwaq.exe" %argName% -p %procfile% %switches%
cd /d "%currentdir%"
goto end

:usage
echo Purpose: Sets PATH and runs delwaq on Windows, with the location of the default proc_def and bloom.spe file when not provided by the user.
echo.
echo Usage:
echo run_delwaq.bat ^<inp-file^> [-p ^<proc_def^>] [-eco [^<spe-file^>]] [...]
echo     --help             : (Optional) show this usage
echo     ^<inp-file^>         : Name of the Delwaq input file (mandatory) 
echo     -p ^<proc_def^>      : use an alternative process library file instead of $D3D_HOME/share/delft3d/proc_def
echo     -openbp ^<dll-file^> : provide a dll with extra subroutines for user defined processes
echo     -np                : do not use any Delwaq processes (all substances will be seen as tracers)
echo     -eco [^<spe-file^>]  : use BLOOM, optionally using an alternative algea database for the default
echo                          $D3D_HOME/share/delft3d/bloom.spe
echo     ...                : any other options are passed through to Delwaq
:end

rem To prevent the DOS box from disappearing immediately: remove the rem on the following line
rem pause
