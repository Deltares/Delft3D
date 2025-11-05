@ echo off
title run all examples
setlocal enabledelayedexpansion

set dimrset_bin="c:\checkouts\github\Delft3D\install_all\bin"


echo Search subfolders for "run.bat" scripts ...
for /R /D %%s in (.\*) do (
    cd %%s
    if exist run.bat (
        echo(
        echo(
        echo(
        echo ===================================
        echo %%s\run.bat %dimrset_bin%
        call run.bat %dimrset_bin%
    )
)

rem TODO: Search subfolders for run_parallel.bat scripts


echo ...finished
pause
