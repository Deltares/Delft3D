@ echo off

setlocal enabledelayedexpansion
set prepareonly=0
set config=all
set generator=
set vs=
set -vs=0
set ifort=
set -ifort=0
set coverage=0
set cmake=cmake

rem # Jump to the directory where this build.bat script is
cd %~dp0
set root=%CD%

call :GetArguments %*
if !ERRORLEVEL! NEQ 0 exit /B %~1
call :GetEnvironmentVars
if !ERRORLEVEL! NEQ 0 exit /B %~1
call :SetGenerator
if !ERRORLEVEL! NEQ 0 exit /B %~1
call :CheckCMakeInstallation
if !ERRORLEVEL! NEQ 0 exit /B %~1


echo.
echo     config      : !config!
echo     generator   : !generator!
echo     ifort       : !ifort!
echo     prepareonly : !prepareonly!
echo     coverage    : !coverage!
echo     vs          : !vs!

call :Checks
if !ERRORLEVEL! NEQ 0 exit /B %~1

rem Only set the enviroment if not run from a developer command prompt
if "%VCINSTALLDIR%" == "" (
    call :vcvarsall
    if !ERRORLEVEL! NEQ 0 exit /B %~1
)

call :DoCMake !config!
if !ERRORLEVEL! NEQ 0 exit /B %~1

if "!config!" == "dflowfm_interacter" call :set_dflowfm_interacter_link_flag
if !ERRORLEVEL! NEQ 0 exit /B %~1

if !coverage! EQU 1 call :insertCoverage !config!
if !ERRORLEVEL! NEQ 0 exit /B %~1

call :Build !config!
if !ERRORLEVEL! NEQ 0 exit /B %~1

call :installall
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
:GetArguments
    echo.
    echo Get command line arguments ...

    if [%1] EQU [] (
        rem No arguments: continue with defaults
        goto :eof
    )
    if "%1" == "--help" (
        goto :usage
    )
    rem First argument is the config
    set "config=%1"
    shift /1

    set configs="all delft3d4 delft3dfm dflowfm dflowfm_interacter dimr drr dwaq dwaves flow2d3d swan tests tools tools_gpl"
    set "modified=!configs:%config%=!"
    if !modified!==%configs% (
        echo ERROR: Configuration !config! not recognized
        goto :argument_error
    )

    rem Read other arguments
    set "options=-vs:0 -ifort:0 -coverage: -prepareonly:"
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
    if !-coverage! == 1 (
    set coverage=1
    )
    if !-prepareonly! == 1 (
    set prepareonly=1
    )
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
:GetEnvironmentVars
    echo.
    echo Attempting to find latest versions of ifort and Visual Studio based on environment variables ...

    if NOT "%IFORT_COMPILER16%" == "" (
        set ifort=16
        echo Found: Intel Fortran 2016
    )
    if NOT "%IFORT_COMPILER18%" == "" (
        set ifort=18
        echo Found: Intel Fortran 2018
    )
    if NOT "%IFORT_COMPILER19%" == "" (
        set ifort=19
        echo Found: Intel Fortran 2019
    )
    if NOT "%IFORT_COMPILER21%" == "" (
        set ifort=21
        echo Found: Intel Fortran 2021
    )
    if NOT "%IFORT_COMPILER23%" == "" (
        set ifort=23
        echo Found: Intel Fortran 2023
    )
    if NOT "%IFORT_COMPILER24%" == "" (
        set ifort=24
        echo Found: Intel Fortran 2024
    )

    if NOT !-ifort! == 0 (
        echo Overriding automatically found ifort version !ifort! with argument !-ifort!
        set ifort=!-ifort!
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

    if NOT !-vs! == 0 (
        echo Overriding automatically found VS version !vs! with argument !-vs!
        set vs=!-vs!
    )
    goto :eof

rem ================================
rem === Check CMake installation ===
rem ================================
:CheckCMakeInstallation
    echo.
    echo Checking whether CMake is installed ...
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

        echo !cmake! not found, trying with default path ...
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
rem === SetGenerator   ====
rem =======================
:SetGenerator
    if "!vs!" == "2017" (
        set generator="Visual Studio 15 2017"
    )
    if "!vs!" == "2019" (
        set generator="Visual Studio 16 2019"
    )
    if "!vs!" == "2022" (
        set generator="Visual Studio 17 2022"
    )
    goto :eof

rem =======================
rem === Checks         ====
rem =======================
:Checks
    if "!config!" == "" (
        echo ERROR: config is empty.
        set ERRORLEVEL=1
        goto :end
    )
    if "!generator!" == "" (
        echo ERROR: generator is empty.
        echo        Possible causes:
        echo            In prepare_sln.py:
        echo                Chosen Visual Studio version is not installed
        set ERRORLEVEL=1
        goto :end
    )
    goto :eof

rem =================
rem === vcvarsall ===
rem =================
:vcvarsall
    rem # Attempt to execute vcvarsall.bat if not run from a developer command prompt
    if %prepareonly% EQU 1 goto :eof
    if !ERRORLEVEL! NEQ 0 goto :eof

    echo.

    if "!VS%vs%INSTALLDIR!" == "" (
        echo Cannot set Visual Studio enviroment variables, please run build.bat from a Visual Studio developer command prompt.
        set ERRORLEVEL=1
        goto :end
    )
    echo Calling vcvarsall.bat for VisualStudio %vs% ...
    call "!VS%vs%INSTALLDIR!\VC\Auxiliary\Build\vcvarsall.bat" amd64

    rem # Execution of vcvarsall results in a jump to the C-drive. Jump back to the script directory
    cd /d "%root%\"
    if !ERRORLEVEL! NEQ 0 call :errorMessage
    goto :eof

rem =======================
rem === DoCMake        ====
rem =======================
:DoCMake
    if !ERRORLEVEL! NEQ 0 goto :eof
    echo.
    call :createCMakeDir build_%~1
    echo Running CMake for %~1 ...
    cd /d "%root%\build_%~1\"
    !cmake! ..\src\cmake -G %generator% -A x64 -B "." -D CONFIGURATION_TYPE="%~1" 1>cmake_%~1.log 2>&1
    if !ERRORLEVEL! NEQ 0 call :errorMessage
    goto :eof



rem =========================================
rem === set_dflowfm_interacter_link_flag ====
rem =========================================
:set_dflowfm_interacter_link_flag
    rem # Ugly workaround to change "LinkLibraryDependencies=false" into "LinkLibraryDependencies=true"
    %root%\src\third_party_open\commandline\bin\win32\sed.exe -e "s/LinkLibraryDependencies=\"false\"/LinkLibraryDependencies=\"true\"/g" "%root%\build_dflowfm_interacter\dflowfm_cli_exe\dflowfm-cli.vfproj" >"%root%\build_dflowfm_interacter\dflowfm_cli_exe\dflowfm-cli_new.vfproj"
    del "%root%\build_dflowfm_interacter\dflowfm_cli_exe\dflowfm-cli.vfproj"  > del.log 2>&1
    del /f/q del.log
    rename %root%\build_dflowfm_interacter\dflowfm_cli_exe\dflowfm-cli_new.vfproj dflowfm-cli.vfproj
    goto :eof



rem =======================
rem === insertCoverage ====
rem =======================
:insertCoverage
    rem Insert options to implement the build objects with hooks for the code-coverage tool.
    rem This code is running from within build_%~1
    python %root%\src\scripts_lgpl\win64\testcoverage\addcovoptions.py %~1.sln


rem =======================
rem === Build          ====
rem =======================
:Build
    if %prepareonly% EQU 1 goto :eof
    if !ERRORLEVEL! NEQ 0 goto :eof
    echo.
    echo Building %~1 ...
    cd /d "%root%\build_%~1\"
    call :VSbuild %~1
    cd /d "%root%\"
    goto :eof



rem =======================
rem === createCMakeDir ====
rem =======================
:createCMakeDir
    echo Creating directory %root%\%~1 ...
    cd /d %root%
    if exist "%root%\%~1\" rmdir /s/q "%root%\%~1\" > del.log 2>&1
    mkdir    "%root%\%~1\"                          > del.log 2>&1
    del /f/q del.log
    goto :eof



rem =======================
rem === VSbuild        ====
rem =======================
:VSbuild
    echo.
    echo Building with VisualStudio: %~1 ...
    set currentWorkDir=%CD%
    devenv.com %~1.sln /Rebuild "Release|x64" 1>%currentWorkDir%\build_%~1.log 2>&1
    if !ERRORLEVEL! NEQ 0 call :errorMessage

    rem # In build.log, replace "error" by TeamCity messages
    %root%\src\third_party_open\commandline\bin\win32\sed.exe -e "/[Ee]rror[\:\ ]/s/^/\#\#teamcity\[buildStatus status\=\'FAILURE\' text\=\' /g;/buildStatus/s/$/\'\]/g" %currentWorkDir%\build_%~1.log
    goto :eof

rem =======================
rem === InstallAll     ====
rem =======================
:installall
    if %prepareonly% EQU 1                goto :eof
    if !ERRORLEVEL! NEQ 0                 goto :eof

    if "!config!" == "all" (
        echo.
        echo Installing in build_all ...
        xcopy %root%\build_all\x64\Release\dimr                     %root%\build_all\x64\dimr\            /E /C /Y /Q > del.log 2>&1

        xcopy %root%\build_all\x64\Release\dflowfm\bin\dflowfm*     %root%\build_all\x64\dflowfm\bin\     /E /C /Y /Q > del.log 2>&1
        xcopy %root%\build_all\x64\Release\dflowfm\bin\dfm*         %root%\build_all\x64\dflowfm\bin\     /E /C /Y /Q > del.log 2>&1
        xcopy %root%\build_all\x64\Release\dflowfm\bin\cosumo_bmi*  %root%\build_all\x64\dflowfm\bin\     /E /C /Y /Q > del.log 2>&1
        xcopy %root%\build_all\x64\Release\dflowfm\default          %root%\build_all\x64\dflowfm\default\ /E /C /Y /Q > del.log 2>&1
        xcopy %root%\build_all\x64\Release\dflowfm\scripts          %root%\build_all\x64\dflowfm\scripts\ /E /C /Y /Q > del.log 2>&1

        xcopy %root%\build_all\x64\Release\dwaq\bin\delwaq.dll      %root%\build_all\x64\dwaq\bin\        /E /C /Y /Q > del.log 2>&1
        xcopy %root%\build_all\x64\Release\dwaq\bin\delwaq*.exe     %root%\build_all\x64\dwaq\bin\        /E /C /Y /Q > del.log 2>&1
        xcopy %root%\build_all\x64\Release\dwaq\bin\agrhyd.exe      %root%\build_all\x64\dwaq\bin\        /E /C /Y /Q > del.log 2>&1
        xcopy %root%\build_all\x64\Release\dwaq\bin\ddcouple.exe    %root%\build_all\x64\dwaq\bin\        /E /C /Y /Q > del.log 2>&1
        xcopy %root%\build_all\x64\Release\dwaq\bin\maptonetcdf.exe      %root%\build_all\x64\dwaq\bin\        /E /C /Y /Q > del.log 2>&1
        xcopy %root%\build_all\x64\Release\dwaq\bin\waqmerge.exe    %root%\build_all\x64\dwaq\bin\        /E /C /Y /Q > del.log 2>&1
        xcopy %root%\build_all\x64\Release\dwaq\bin\waqpb_export.exe    %root%\build_all\x64\dwaq\bin\        /E /C /Y /Q > del.log 2>&1
        xcopy %root%\build_all\x64\Release\dwaq\bin\waqpb_import.exe    %root%\build_all\x64\dwaq\bin\        /E /C /Y /Q > del.log 2>&1
        xcopy %root%\build_all\x64\Release\dwaq\bin\waq_plugin*.dll %root%\build_all\x64\dwaq\bin\        /E /C /Y /Q > del.log 2>&1
        xcopy %root%\build_all\x64\Release\dwaq\resources             %root%\build_all\x64\dwaq\resources\    /E /C /Y /Q > del.log 2>&1
        xcopy %root%\build_all\x64\Release\dwaq\installation_default    %root%\build_all\x64\dwaq\installation_default\    /E /C /Y /Q > del.log 2>&1
        xcopy %root%\build_all\x64\Release\dwaq\scripts             %root%\build_all\x64\dwaq\scripts\    /E /C /Y /Q > del.log 2>&1

        xcopy %root%\build_all\x64\Release\dpart\bin\delpar.exe     %root%\build_all\x64\dpart\bin\        /E /C /Y /Q > del.log 2>&1
        xcopy %root%\build_all\x64\Release\dpart\scripts            %root%\build_all\x64\dpart\scripts\    /E /C /Y /Q > del.log 2>&1

        xcopy %root%\build_all\x64\Release\dwaves\bin\wave.dll      %root%\build_all\x64\dwaves\bin\      /E /C /Y /Q > del.log 2>&1
        xcopy %root%\build_all\x64\Release\dwaves\bin\wave.exe      %root%\build_all\x64\dwaves\bin\      /E /C /Y /Q > del.log 2>&1
        xcopy %root%\build_all\x64\Release\dwaves\default           %root%\build_all\x64\dwaves\default\  /E /C /Y /Q > del.log 2>&1
        xcopy %root%\build_all\x64\Release\dwaves\scripts           %root%\build_all\x64\dwaves\scripts\  /E /C /Y /Q > del.log 2>&1

        xcopy %root%\build_all\x64\Release\swan\bin\swan_mpi.exe    %root%\build_all\x64\swan\bin\        /E /C /Y /Q > del.log 2>&1
        xcopy %root%\build_all\x64\Release\swan\bin\swan_omp.exe    %root%\build_all\x64\swan\bin\        /E /C /Y /Q > del.log 2>&1
        xcopy %root%\build_all\x64\Release\swan\scripts             %root%\build_all\x64\swan\scripts\    /E /C /Y /Q > del.log 2>&1

        xcopy %root%\build_all\x64\Release\esmf\bin                 %root%\build_all\x64\esmf\bin\        /E /C /Y /Q > del.log 2>&1
        xcopy %root%\build_all\x64\Release\esmf\scripts             %root%\build_all\x64\esmf\scripts\    /E /C /Y /Q > del.log 2>&1

        xcopy %root%\build_all\x64\Release\dmor                     %root%\build_all\x64\dmor\            /E /C /Y /Q > del.log 2>&1

        xcopy %root%\build_all\x64\Release\share\bin                %root%\build_all\x64\share\bin\       /E /C /Y /Q > del.log 2>&1
        rmdir /s /q %root%\build_all\x64\Release > del.log 2>&1
        del /f/q del.log
    )
    if "!config!" == "delft3d4" (
        echo.
        echo Installing in build_delft3d4 ...
        xcopy %root%\build_delft3d4\x64\Release\dflow2d3d                %root%\build_delft3d4\x64\dflow2d3d\       /E /C /Y /Q > del.log 2>&1
        xcopy %root%\build_delft3d4\x64\Release\dimr                     %root%\build_delft3d4\x64\dimr\            /E /C /Y /Q > del.log 2>&1
        xcopy %root%\build_delft3d4\x64\Release\dpart                    %root%\build_delft3d4\x64\dpart\           /E /C /Y /Q > del.log 2>&1
        xcopy %root%\build_delft3d4\x64\Release\dwaq                     %root%\build_delft3d4\x64\dwaq\            /E /C /Y /Q > del.log 2>&1
        xcopy %root%\build_delft3d4\x64\Release\dwaves                   %root%\build_delft3d4\x64\dwaves\          /E /C /Y /Q > del.log 2>&1
        xcopy %root%\build_delft3d4\x64\Release\share                    %root%\build_delft3d4\x64\share\           /E /C /Y /Q > del.log 2>&1
        xcopy %root%\build_delft3d4\x64\Release\swan                     %root%\build_delft3d4\x64\swan\            /E /C /Y /Q > del.log 2>&1
        xcopy %root%\build_delft3d4\x64\Release\d_hydro\bin\*.exe        %root%\build_delft3d4\x64\dflow2d3d\bin\   /E /C /Y /Q > del.log 2>&1
        xcopy %root%\build_delft3d4\x64\Release\dmor                     %root%\build_delft3d4\x64\dmor\            /E /C /Y /Q > del.log 2>&1

        rmdir /s /q %root%\build_delft3d4\x64\Release > del.log 2>&1
        del /f/q del.log
    )
    goto :eof

rem =======================
rem === USAGE        ======
rem =======================
:usage
    echo.
    echo.
    echo.
    echo Usage:
    echo "build.bat"
    echo "build.bat <CONFIG> [OPTIONS]"
    echo "    The following actions will be executed:"
    echo "    - Create directory 'build_<CONFIG>'"
    echo "      Delete it first when it already exists"
    echo "    - Execute 'CMake <CONFIG>' to create file '<CONFIG>.sln' inside 'build_<CONFIG>'"
    echo "    - Execute 'devenv.com <CONFIG>.sln /Build'"
    echo "    - Only when <CONFIG>=all: Combine all binaries in 'build_<CONFIG>\x64'"
    echo.
    echo "<CONFIG>:"
    echo "- all     (default) : D-Flow FM   , D-WAQ, D-Waves, DIMR"
    echo "- delft3d4          : Delft3D-FLOW, D-WAQ, D-Waves"
    echo "- delft3dfm         : D-Flow FM   , D-WAQ, D-Waves, DIMR"
    echo "- dflowfm           : D-Flow FM"
    echo "- dflowfm_interacter: D-Flow FM with Interacter"
    echo "- dimr              : DIMR"
    echo "- drr               : D-RR"
    echo "- dwaq              : D-WAQ"
    echo "- dwaves            : D-Waves"
    echo "- flow2d3d          : Delft3D-FLOW"
    echo "- swan              : SWAN"
    echo "- tests"
    echo "- tools"
    echo "- tools_gpl"
    echo.
    echo "[OPTIONS]: usage [OPTION], sometimes followed by a value, space separated, in any order"
    echo "-coverage: Instrument object files for code-coverage tool (codecov) Example: -coverage"
    echo "-prepareonly: Only prepare solution, do not build the code.         Example: -prepareonly"
    echo "-vs: desired visual studio version.                                 Example: -vs 2019
    echo "-ifort: desired intel fortran compiler version.                     Example: -ifort 21
    echo.
    echo "More info  : https://oss.deltares.nl/web/delft3d/source-code"
    echo "About CMake: https://git.deltares.nl/oss/delft3d/-/tree/main/src/cmake/doc/README"
    echo.
    set ERRORLEVEL=1
    goto :end

rem =======================
rem === errorMessage ======
rem =======================
:errorMessage
    echo.
    echo.
    echo.
    echo ERROR: Please check the log files in the build_%config% directory.
    goto :eof


rem =======================
rem === END TAG ===========
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
rem === EOF TAG ===========
rem =======================
:eof
