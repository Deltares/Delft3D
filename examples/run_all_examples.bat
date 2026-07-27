@ echo off
title run all examples
setlocal enabledelayedexpansion
set root=%CD%

rem Call all run scripts with a path to a Dimrset-bin folder
set dimrset_bin="%CD%\..\install_all\bin"

echo ===================================
echo === SEQUENTIAL COMPUTATIONS
echo ===================================
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

cd %root%

echo ===================================
echo === PARALLEL COMPUTATIONS
echo ===================================
echo Search subfolders for "run_parallel.bat" scripts ...
for /R /D %%s in (.\*) do (
    cd %%s
    if exist run_parallel.bat (
        echo(
        echo(
        echo(
        echo ===================================
        echo %%s\run_parallel.bat %dimrset_bin%
        call run_parallel.bat %dimrset_bin%
    )
)


echo ...finished
pause
