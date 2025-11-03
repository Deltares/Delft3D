@ echo off

rem Usage:
rem     Either:
rem         Call this script with one argument being the path to a Dimrset-bin folder containing a matching run script
rem     Or:
rem         Build the source code
rem         In this script: Set dimrset_bin to point to the appropriate "install-folder\bin"
rem         Execute this script
rem 

if "%~1" == "" (
    set dimrset_bin="..\..\..\..\install_all\bin"
) else (
    set dimrset_bin=%1
)


del /f COSUMO\FF2NF\FF2NF*.* >del.log 2>&1
del /f del.log


rem Remove quotes surrounding dimrset_bin, add the appropriate run script, re-add quotes
call "%dimrset_bin:"=%\run_dflow2d3d.bat"


rem pause
