@ echo off
title run_dimr_parallel
    rem When using Intel MPI for parallel execution:
    rem 
    rem For LOCAL parallel execution (most common case):
    rem No special setup is required. The -localonly flag allows MPI to run
    rem without the hydra service.
    rem 
    rem For REMOTE/CLUSTER parallel execution:
    rem Modern Intel MPI integrates with cluster resource managers and cloud platforms.
    rem 
    rem For HPC clusters:
    rem   Use your cluster's job scheduler (Slurm, PBS, etc.) - no additional setup needed
    rem   Example: sbatch, qsub, or similar cluster commands
    rem
    rem For cloud environments:
    rem   Intel MPI has built-in support for AWS, Azure, and Google Cloud
    rem
    rem For ad-hoc multi-node execution:
    rem   Use -hosts option: mpiexec -hosts host1,host2,host3 -n 6 your_program
    rem   Or use host file: mpiexec -f hostfile -n 6 your_program
    rem   Requires SSH access between nodes (no hydra service installation needed)

    rem
    rem This script runs dimr in parallel mode on Windows
    rem Adapt and use it for your own purpose
    rem
    rem Usage example:
    rem Execute in the working directory:
    rem path\to\delft3d\installation\x64\bin\run_dimr_parallel.bat
    rem More examples: check run scripts in https://github.com/Deltares/Delft3D/tree/main/examples/*

setlocal enabledelayedexpansion
set debuglevel=-1

    rem
    rem Read arguments

    rem No arguments:
if [%1] EQU [] (
    set numpar=%NUMBER_OF_PROCESSORS%
    set argfile=dimr_config.xml
    goto readyreading
)

    rem --help:
if [%1] EQU [--help] ( goto usage )

if [%1] EQU [-c] (
    set numpar=%2
)

if [%3] EQU [-m] (
    set argfile=%4
    goto readyreading
)

    rem number of partitions:
set numpar=%1

    rem debuglevel and or configfile
if [%2] EQU [-d] (
    set debuglevel=%3
    if [%4] EQU [] (
        set argfile=dimr_config.xml
        goto readyreading
    ) else (
        set argfile=%4
        goto readyreading
    )
) else (
    set argfile=%2
)
if [%3] EQU [-d] (
    set debuglevel=%4
    goto readyreading
)

:readyreading

    rem Check configfile
echo Configfile:%argfile%
if not exist %argfile% (
    echo ERROR: configfile "%argfile%" does not exist
    goto usage
)

    rem Check debuglevel, translate into argument for dimr
if  %debuglevel% EQU -1 (
    set debugarg=
) else (
    set debugarg=-d !debuglevel!
)

    rem Sets the number of threads if it is not defined
if defined OMP_NUM_THREADS (
echo OMP_NUM_THREADS is already defined
) else (
   rem Getting and setting the number of physical cores
   for /F "tokens=*" %%C in ('powershell -Command "Get-CimInstance -ClassName Win32_Processor | Select-Object -ExpandProperty NumberOfCores | Measure-Object -Sum | Select-Object -ExpandProperty Sum"') do set NumberOfPhysicalCores=%%C
   set /A OMP_NUM_THREADS=!NumberOfPhysicalCores! - 2
   if /I OMP_NUM_THREADS LEQ 2 ( set OMP_NUM_THREADS=2 )
)

echo number of partitions: %numpar%

set workdir=%CD%
echo Working directory: %workdir%
    rem
    rem Set the directories containing the binaries
    rem

set D3D_HOME=%~dp0..
echo D3D_HOME         : %D3D_HOME%
set exedir=%D3D_HOME%\bin
set sharedir=%D3D_HOME%\share
set libdir=%D3D_HOME%\lib
set proc_def_dir=%sharedir%\delft3d

    rem
    rem No adaptions needed below
    rem

    rem Run
set PATH=%sharedir%;%libdir%;%exedir%
if exist %exedir%\vars.bat (
    echo executing: "%exedir%\vars.bat"
        call "%exedir%\vars.bat"
) else (
    echo "WARNING: File not found: %exedir%\vars.bat"
    echo "         Problems may occur when using IntelMPI"
)
echo executing: "%exedir%\mpiexec.exe" -n %numpar% -localonly "%exedir%\dimr.exe" %debugarg% %argfile%
                "%exedir%\mpiexec.exe" -n %numpar% -localonly "%exedir%\dimr.exe" %debugarg% %argfile%

goto end

:usage
echo Usage:
echo run_dimr_parallel.bat [--help] [n] [-d debuglevel] [dimr_config.xml]
echo     --help         : (Optional) show this usage
echo     n              : (Optional) integer, number of partitions. Must match with the prepared D-Flow FM calculation.
echo                      Default value: NUMBER_OF_PROCESSORS
echo     -d debuglevel  : (Optional) debuglevel=0:ALL, 6:SILENT
echo     dimr_config.xml: (Optional) default: dimr_config.xml

:end
    rem To prevent the DOS box from disappearing immediately: remove the rem on the following line
rem pause
