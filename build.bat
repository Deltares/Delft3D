@ echo off

setlocal enabledelayedexpansion
rem Default arguments
set config=all
set build=
set vs=
set coverage=
set build_type=Debug
set keep_build=
set compiler=ifort

rem Non-argument variables
set generator=
set cmake=cmake
set oneapi=

rem Argument variables
set -help=
set -config=
set -build=
set -vs=
set -coverage=
set -build_type=
set -keep_build=
set -compiler=

rem Jump to the directory where this build.bat script is
cd %~dp0
set root=%CD%

call :get_arguments %*
if !ERRORLEVEL! NEQ 0 exit /B %~1
call :get_environment_vars
if !ERRORLEVEL! NEQ 0 exit /B %~1
call :set_generator
if !ERRORLEVEL! NEQ 0 exit /B %~1
call :check_cmake_installation
if !ERRORLEVEL! NEQ 0 exit /B %~1

echo.
echo     oneapi      : !oneapi!
echo     compiler    : !compiler!
echo     vs          : !vs!
echo     config      : !config!
echo     generator   : !generator!
echo     build_type  : !build_type!
echo     build       : !build!
echo     keep_build  : !keep_build!
echo     coverage    : !coverage!

call :checks
if !ERRORLEVEL! NEQ 0 exit /B %~1

rem Only set the enviroment if not run from a developer command prompt
if "%VCINSTALLDIR%" == "" (
    call :set_vs_environment
    if !ERRORLEVEL! NEQ 0 exit /B %~1
)

call :do_cmake
if !ERRORLEVEL! NEQ 0 exit /B %~1

if !coverage! EQU 1 call :insert_coverage !config!
if !ERRORLEVEL! NEQ 0 exit /B %~1

call :build
if !ERRORLEVEL! NEQ 0 exit /B %~1

call :install
if !ERRORLEVEL! NEQ 0 exit /B %~1


echo.
echo Generated Visual Studio solution file: %root%\build_%config%\%config%.sln
echo Finished
goto :end

rem ===
rem === PROCEDURES
rem ===

rem =================================
rem === Command line arguments    ===
rem =================================
:get_arguments
    echo.
    echo Get command line arguments...

    rem Read arguments
    set "options=-config:!config! -help:!help! -vs:!vs! -compiler:!compiler! -coverage:!coverage! -build:!build! -build_type:!build_type! -keep_build:!keep_build!"
    rem see: https://stackoverflow.com/questions/3973824/windows-bat-file-optional-argument-parsing answer 2.
    for %%O in (%options%) do for /f "tokens=1,* delims=:" %%A in ("%%O") do set "%%A=%%~B"
    :loop
    if not "%~1" == "" (
      set "test=!options:*%~1:=! "
      if "!test!" == "!options! " (
          echo Error: Invalid option %~1
          goto :argument_error
      ) else if "!test:~0,1!" == " " (
          set "%~1=1"
      ) else (
          set "%~1=%~2"
          shift /1
      )
      shift /1
      goto :loop
    )

    if !-help! == 1 (
        goto :usage
    )

    set configs="all delft3d4 delft3dfm dflowfm dflowfm_interacter dimr drr dwaq dwaves flow2d3d swan tests tools tools_gpl"
    set "modified=!configs:%-config%=!"
    if !modified!==!configs! (
        echo ERROR: Configuration !-config! not recognized
        goto :argument_error
    )

    set config=!-config!

    set compilers="ifort ifx"
    set "modified=!compilers:%-compiler%=!"
    if !modified!==!compilers! (
        echo ERROR: Compiler !-compiler! not recognized
        goto :argument_error
    )

    set compiler=!-compiler!

    if !-coverage! == 1 (
        set coverage=1
    ) else (
        set coverage=0
    )

    if !-build! == 1 (
        set build=1
    ) else (
        set build=0
    )

    if !-keep_build! == 1 (
        set keep_build=1
    ) else (
        set keep_build=0
    )

    set build_types="Debug Release RelWithDebInfo"
    set "modified=!build_types:%-build_type%=!"
    if !modified!==%build_types% (
        echo ERROR: Build type !-build_type! not recognized
        goto :argument_error
    )
    set "build_type=!-build_type!"
    goto :eof

rem =======================
rem === ERROR IN ARG  =====
rem =======================
:argument_error
    echo.
    echo Error in command line arguments.
    goto :usage

rem =================================
rem === Get environment variables ===
rem =================================
:get_environment_vars
    echo.
    echo Attempting to find latest versions of Intel OneAPI and Visual Studio based on environment variables...

    if NOT "%IFORT_COMPILER16%" == "" (
        set oneapi=16
        echo Found: Intel Fortran 2016
    )
    if NOT "%IFORT_COMPILER18%" == "" (
        set oneapi=18
        echo Found: Intel Fortran 2018
    )
    if NOT "%IFORT_COMPILER19%" == "" (
        set oneapi=19
        echo Found: Intel Fortran 2019
    )
    if NOT "%IFORT_COMPILER21%" == "" (
        set oneapi=21
        echo Found: Intel Fortran 2021
    )
    if NOT "%IFORT_COMPILER23%" == "" (
        set oneapi=23
        echo Found: Intel Fortran 2023
    )
    if NOT "%IFORT_COMPILER24%" == "" (
        set oneapi=24
        echo Found: Intel Fortran 2024
    )

    if "!oneapi!" == "" (
        echo Warning: Could not find Intel OneAPI version in environment.
    )

    if "!oneapi!" == "" (
        echo Error: oneapi not set. Please ensure that Intel OneAPI is installed and run build.bat from a prompt with the right environment set.
        set ERRORLEVEL=1
        goto :end
    )

    set "vs2017_found="
    if NOT "%VS2017INSTALLDIR%" == "" (
        set "vs2017_found=true"
    )
    if "%VisualStudioVersion%" == "15.0" (
        set "vs2017_found=true"
    )
    if "%vs2017_found%" == "true" (
        set vs=2017
        echo Found: VisualStudio 15 2017
    )

    set "vs2019_found="
    if NOT "%VS2019INSTALLDIR%" == "" (
        set "vs2019_found=true"
    )
    if "%VisualStudioVersion%" == "16.0" (
        set "vs2019_found=true"
    )
    if "%vs2019_found%" == "true" (
        set vs=2019
        echo Found: VisualStudio 16 2019
    )

    set "vs2022_found="
    if NOT "%VS2022INSTALLDIR%" == "" (
        set "vs2022_found=true"
    )
    if "%VisualStudioVersion%" == "17.0" (
        set "vs2022_found=true"
    )
    if "%vs2022_found%" == "true" (
        set vs=2022
        echo Found: VisualStudio 17 2022
    )

    if "!vs!" == "" (
        echo Warning: Could not find Visual Studio version in environment.
    )

    if NOT "!-vs!" == "" (
        echo Overriding automatically found VS version !vs! with argument !-vs!
        set vs=!-vs!
    ) else if "!vs!" == "" (
        echo Warning: Visual Studio not found nor provided by -vs. Please ensure that Visual Studio is installed and run build.bat from a prompt with the right environment set.
        echo Continuing without specifying the generator, using the CMake default.
    )
    goto :eof

rem ================================
rem === Check CMake installation ===
rem ================================
:check_cmake_installation
    echo.
    echo Checking whether CMake is installed...
    set count=1
    for /f "tokens=* usebackq" %%f in (`!cmake! --version`) do (
      if !count! LEQ 1 (
          set var!count!=%%f
          set /a count=!count!+1
      )
    )
    if "!var1:~0,13!" == "cmake version" (
        echo !cmake! version: !var1:~13,20!
    ) else (

        echo !cmake! not found, trying with default path...
        set cmake="c:/Program Files/CMake/bin/cmake"
        set count=1
        for /f "tokens=* usebackq" %%f in (`!cmake! --version`) do (
          if !count! LEQ 1 (
              set var!count!=%%f
              set /a count=!count!+1
          )
        )
        if "!var1:~0,13!" == "cmake version" (
            echo !cmake! version: !var1:~13,20!
        ) else (
            echo ERROR: CMake not found.
            echo        Download page: https://cmake.org/download/
            echo        Be sure that the cmake directory is added to environment parameter PATH
            goto :end
        )
    )
    goto :eof

rem =======================
rem === Set generator  ====
rem =======================
:set_generator
    set generator=
    if "!vs!" == "2017" (
        set generator="Visual Studio 15 2017"
    )
    if "!vs!" == "2019" (
        set generator="Visual Studio 16 2019"
    )
    if "!vs!" == "2022" (
        set generator="Visual Studio 17 2022"
    )
    set cmake_generator_arg=
    if not "!generator!" == "" (
        set "cmake_generator_arg=-G !generator!"
    )
    goto :eof

rem =======================
rem === Checks ============
rem =======================
:checks
    if "!config!" == "" (
        echo ERROR: config is empty.
        set ERRORLEVEL=1
        goto :end
    )
    goto :eof

rem =======================
rem === Set VS enviroment =
rem =======================
:set_vs_environment
    rem # Attempt to execute vcvarsall.bat if not run from a developer command prompt
    if %build% EQU 0 goto :eof
    if !ERRORLEVEL! NEQ 0 goto :eof

    echo.

    if "!VS%vs%INSTALLDIR!" == "" (
        echo Cannot set Visual Studio enviroment variables, please run build.bat from a Visual Studio Developer Command Prompt.
        set ERRORLEVEL=1
        goto :end
    )
    echo Calling vcvarsall.bat for VisualStudio %vs%...
    call "!VS%vs%INSTALLDIR!\VC\Auxiliary\Build\vcvarsall.bat" amd64

    rem # Execution of vcvarsall results in a jump to the C-drive. Jump back to the script directory
    cd /d "%root%\"
    if !ERRORLEVEL! NEQ 0 call :errorMessage
    goto :eof

rem =======================
rem === CMake configure ===
rem =======================
:do_cmake
    if !ERRORLEVEL! NEQ 0 goto :eof
    echo.
    call :create_cmake_dir build_!config!
    echo Running CMake for !config!...
    !cmake! -S .\src\cmake -B build_!config! !cmake_generator_arg! -T fortran=!compiler! -A x64 -D CONFIGURATION_TYPE:STRING="!config!" -D CMAKE_INSTALL_PREFIX=.\install_!config!\ 1>build_!config!\cmake_!config!.log 2>&1
    if !ERRORLEVEL! NEQ 0 call :errorMessage
    goto :eof

rem =======================
rem === Insert coverage ===
rem =======================
:insert_coverage
    rem Insert options to implement the build objects with hooks for the code-coverage tool.
    rem This code is running from within build_%~1
    python %root%\src\scripts_lgpl\win64\testcoverage\addcovoptions.py %~1.sln

rem =======================
rem === Build =============
rem =======================
:build
    if %build% EQU 0 goto :eof
    if !ERRORLEVEL! NEQ 0 goto :eof
    echo.
    echo Building !config!...
    !cmake! --build build_!config! --config !build_type! 1>build_!config!\build_!config!.log 2>&1
    if !ERRORLEVEL! NEQ 0 call :errorMessage
    goto :eof

rem =======================
rem === Create CMake dir ==
rem =======================
:create_cmake_dir
    cd /d %root%
    if %keep_build% == 0 (
        echo Cleaning directories %root%\build_%config% and %root%\install_%config%...
        if exist "%root%\build_%config%\" rmdir /s/q "%root%\build_%config%\" > del.log 2>&1
        if exist "%root%\install_%config%\" rmdir /s/q "%root%\install_%config%\" > del.log 2>&1
    )
    if NOT exist "%root%\build_%config%\" mkdir "%root%\build_%config%\" > del.log 2>&1
    if exist "del.log" del /f/q del.log
    goto :eof

rem =======================
rem === Install ===========
rem =======================
:install
    if %build% EQU 0      goto :eof
    if !ERRORLEVEL! NEQ 0 goto :eof

    echo.
    echo Installing !config!...
    !cmake! --install build_%config% --config !build_type! 1>build_!config!\install_!config!.log 2>&1
    if !ERRORLEVEL! NEQ 0 call :errorMessage
    goto :eof

rem =======================
rem === Usage =============
rem =======================
:usage
    echo.
    echo Usage:
    echo build.bat [OPTIONS]
    echo     The following actions will be executed:
    echo     - Create directory 'build_^<CONFIG^>', where ^<CONFIG^> can be specified by the -config option.
    echo       Delete it first when it already exists, unless -keep_build is specified
    echo     - Execute 'CMake ^<CONFIG^>' to create file '^<CONFIG^>.sln' inside 'build_^<CONFIG^>'
    echo.
    echo [OPTIONS]: space separated list of options, sometimes followed by a value, in any order
    echo.
    echo -config ^<CONFIG^>:
    echo   all     (default) : D-Flow FM   , D-WAQ, D-Waves, DIMR
    echo   delft3d4          : Delft3D-FLOW, D-WAQ, D-Waves
    echo   delft3dfm         : D-Flow FM   , D-WAQ, D-Waves, DIMR
    echo   dflowfm           : D-Flow FM
    echo   dflowfm_interacter: D-Flow FM with Interacter
    echo   dimr              : DIMR
    echo   drr               : D-RR
    echo   dwaq              : D-WAQ
    echo   dwaves            : D-Waves
    echo   flow2d3d          : Delft3D-FLOW
    echo   swan              : SWAN
    echo   tests
    echo   tools
    echo   tools_gpl
    echo.
    echo -help: Show this help page.                                                           Example: -help
    echo -coverage: Instrument object files for code-coverage tool (codecov).                  Example: -coverage
    echo -build: Run build and install steps after running cmake.                              Example: -build
    echo -vs: desired visual studio version.                                                   Example: -vs 2019
    echo -compiler: desired Intel compiler, either ifort (default) or ifx.                     Example: -compiler ifx
    echo -build_type: build optimization level.                                                Example: -build_type Release
rem extra four spaces required for aligning Example, compensating for ^ characters:
    echo -keep_build: do not delete the 'build_^<CONFIG^>' and 'install_^<CONFIG^>' folders.       Example: -keep_build
    echo.
    echo More info  : https://oss.deltares.nl/web/delft3d/source-code
    echo About CMake: https://git.deltares.nl/oss/delft3d/-/tree/main/src/cmake/doc/README
    echo.
    set ERRORLEVEL=1
    goto :end

rem =======================
rem === Error message =====
rem =======================
:errorMessage
    echo.
    echo.
    echo.
    echo ERROR: Please check previous error messages and the CMake output in build_%config%\cmake_%config%.log.
    goto :eof

rem =======================
rem === End tag ===========
rem =======================
:end
    rem # To prevent the DOS box from disappearing immediately: remove the rem on the following line
    rem pause
    if !ERRORLEVEL! NEQ 0 (
        exit /B %~1
    ) else (
        exit /B
    )

rem =======================
rem === EOF tag ===========
rem =======================
:eof
