@ echo off

setlocal enabledelayedexpansion
set prepareonly=1
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
echo.

call :Checks
if !ERRORLEVEL! NEQ 0 exit /B %~1

call :vcvarsall
if !ERRORLEVEL! NEQ 0 exit /B %~1

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
echo Visual Studio sln-files:
echo all       : %root%\build_all\all.sln
echo D-Flow FM : %root%\build_dflowfm\dflowfm.sln
echo DIMR      : %root%\build_dimr\dimr.sln
echo DWAQ      : %root%\build_dwaq\dwaq.sln
echo D-Waves   : %root%\build_dwaves\dwaves.sln
echo Tests     : %root%\build_tests\tests.sln
echo Delft3D4  : %root%\build_delft3d4\delft3d4.sln
echo.
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
    echo "Get command line arguments ..."

    if [%1] EQU [] (
        rem No arguments: continue with defaults
        goto :endproc
    )
    if "%1" == "--help" (
        goto usage
    )
    rem First argument is the config
    set "config=%1"
    shift /1

    set configs="all delft3d4 delft3dfm dflowfm dflowfm_interacter dimr drr dwaq dwaves flow2d3d swan tests tools tools_gpl"
    set "modified=!configs:%config%=!"
    if !modified!==%configs% (
        echo ERROR: Configuration !config! not recognized
    )

    rem Read other arguments
    set "options=-vs:0 -ifort:0 -coverage: -prepareonly:"
    rem see: https://stackoverflow.com/questions/3973824/windows-bat-file-optional-argument-parsing answer 2.
    for %%O in (%options%) do for /f "tokens=1,* delims=:" %%A in ("%%O") do set "%%A=%%~B"
    :loop
    if not "%~1"=="" (
      set "test=!options:*%~1:=! "
      if "!test!"=="!options! " (
          echo Error: Invalid option %~1
      ) else if "!test:~0,1!"==" " (
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
    goto :endproc




rem =================================
rem === Get environment variables ===
rem =================================
:GetEnvironmentVars
    echo.
    echo "Get environment variables ..."
    rem # Check combinations!
    rem # VISUAL STUDIO: First try the official version (2017), then newer (2019) then older (2015), stop on success
    rem # IFORT: Order (overwriting when already found) in VS17: IFORT21, IFORT19, IFORT18(=official combination)
    rem #                                               in VS19: IFORT21, IFORT19
    rem #                                               in VS15: IFORT21, IFORT19, IFORT18, IFORT16(=previous official combination)
    rem # On TeamCity VS2019 is installed without IFORT



    if NOT "%VS2022INSTALLDIR%" == "" (
        set vs=2022
        echo Found: VisualStudio 17 2022
        if NOT "%IFORT_COMPILER21%" == "" (
            set ifort=23
            echo Found: Intel Fortran 2023
        )
        goto :endfun
    )


    if NOT "%VS2017INSTALLDIR%" == "" (
        set vs=2017
        echo Found: VisualStudio 15 2017
        if NOT "%IFORT_COMPILER21%" == "" (
            set ifort=21
            echo Found: Intel Fortran 2021
        )
        if NOT "%IFORT_COMPILER19%" == "" (
            set ifort=19
            echo Found: Intel Fortran 2019
        )
        if NOT "%IFORT_COMPILER18%" == "" (
            set ifort=18
            echo Found: Intel Fortran 2018
        )
        goto :endfun
    )
    if NOT "%VS2019INSTALLDIR%" == "" (
        set vs=2019
        echo Found: VisualStudio 16 2019
        if NOT "%IFORT_COMPILER21%" == "" (
            set ifort=21
            echo Found: Intel Fortran 2021
        )
        if NOT "%IFORT_COMPILER19%" == "" (
            set ifort=19
            echo Found: Intel Fortran 2019
        )
        goto :endfun
    )
    if NOT "%VS2015INSTALLDIR%" == "" (
        set vs=2015
        echo Found: VisualStudio 14 2015
        if NOT "%IFORT_COMPILER21%" == "" (
            set ifort=21
            echo Found: Intel Fortran 2021
        )
        if NOT "%IFORT_COMPILER19%" == "" (
            set ifort=19
            echo Found: Intel Fortran 2019
        )
        if NOT "%IFORT_COMPILER18%" == "" (
            set ifort=18
            echo Found: Intel Fortran 2018
        )
        if NOT "%IFORT_COMPILER16%" == "" (
            set ifort=16
            echo Found: Intel Fortran 2016
        )
        goto :endfun
    )
    :endfun
    if NOT !-vs! == 0 (
        set vs=!-vs!
        echo overriding vs with !-vs!
    )

    if NOT !-ifort! == 0 (
        set ifort=!-ifort!
        echo overriding ifort with !-ifort!
    )
    goto :endproc


rem ================================
rem === Check CMake installation ===
rem ================================
:CheckCMakeInstallation
    echo.
    echo "Checking whether CMake is installed ..."
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
    goto :endproc



rem =======================
rem === SetGenerator   ====
rem =======================
:SetGenerator
    if "!vs!" == "2015" (
        set generator="Visual Studio 14 2015"
    )
    if "!vs!" == "2017" (
        set generator="Visual Studio 15 2017"
    )
    if "!vs!" == "2019" (
        set generator="Visual Studio 16 2019"
    )
    if "!vs!" == "2022" (
        set generator="Visual Studio 17 2022"
    )
    goto :endproc



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
    goto :endproc



rem =================
rem === vcvarsall ===
rem =================
:vcvarsall
    rem # Execute vcvarsall.bat in case of compilation
    if %prepareonly% EQU 1 goto :endproc
    if !ERRORLEVEL! NEQ 0 goto :endproc

    echo.
    if !generator! == "Visual Studio 14 2015" (
        echo "Calling vcvarsall.bat for VisualStudio 2015 ..."
        call "%VS140COMNTOOLS%..\..\VC\vcvarsall.bat" amd64
    )
    if !generator! == "Visual Studio 15 2017" (
        echo "Calling vcvarsall.bat for VisualStudio 2017 ..."
        call "%VS2017INSTALLDIR%\VC\Auxiliary\Build\vcvarsall.bat" amd64
    )
    if !generator! == "Visual Studio 16 2019" (
        echo "Calling vcvarsall.bat for VisualStudio 2019 ..."
        call "%VS2019INSTALLDIR%\VC\Auxiliary\Build\vcvarsall.bat" amd64
    )
    if !generator! == "Visual Studio 17 2022" (
        echo "Calling vcvarsall.bat for VisualStudio 2022 ..."
        call "%VS2022INSTALLDIR%\VC\Auxiliary\Build\vcvarsall.bat" amd64
    )

    rem # Execution of vcvarsall results in a jump to the C-drive. Jump back to the script directory
    cd /d "%root%\"
    if !ERRORLEVEL! NEQ 0 call :errorMessage
    goto :endproc



rem =======================
rem === DoCMake        ====
rem =======================
:DoCMake
    if !ERRORLEVEL! NEQ 0 goto :endproc
    echo.
    call :createCMakeDir build_%~1
    echo "Running CMake for %~1 ..."
    cd /d "%root%\build_%~1\"
    !cmake! ..\src\cmake -G %generator% -A x64 -B "." -D CONFIGURATION_TYPE="%~1" 1>cmake_%~1.log 2>&1
    if !ERRORLEVEL! NEQ 0 call :errorMessage
    goto :endproc



rem =========================================
rem === set_dflowfm_interacter_link_flag ====
rem =========================================
:set_dflowfm_interacter_link_flag
    rem # Ugly workaround to change "LinkLibraryDependencies=false" into "LinkLibraryDependencies=true"
    %root%\src\third_party_open\commandline\bin\win32\sed.exe -e "s/LinkLibraryDependencies=\"false\"/LinkLibraryDependencies=\"true\"/g" "%root%\build_dflowfm_interacter\dflowfm_cli_exe\dflowfm-cli.vfproj" >"%root%\build_dflowfm_interacter\dflowfm_cli_exe\dflowfm-cli_new.vfproj"
    del "%root%\build_dflowfm_interacter\dflowfm_cli_exe\dflowfm-cli.vfproj"  > del.log 2>&1
    del /f/q del.log
    rename %root%\build_dflowfm_interacter\dflowfm_cli_exe\dflowfm-cli_new.vfproj dflowfm-cli.vfproj
    goto :endproc



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
    if %prepareonly% EQU 1 goto :endproc
    if !ERRORLEVEL! NEQ 0 goto :endproc
    echo.
    echo "Building %~1 ..."
    cd /d "%root%\build_%~1\"
    call :VSbuild %~1
    cd /d "%root%\"
    goto :endproc



rem =======================
rem === createCMakeDir ====
rem =======================
:createCMakeDir
    echo.
    echo "Creating directory %root%\%~1 ..."
    cd /d %root%
    if exist "%root%\%~1\" rmdir /s/q "%root%\%~1\" > del.log 2>&1
    mkdir    "%root%\%~1\"                          > del.log 2>&1
    del /f/q del.log
    goto :endproc



rem =======================
rem === VSbuild        ====
rem =======================
:VSbuild
    echo.
    echo "Building with VisualStudio: %~1 ..."
    set currentWorkDir=%CD%
    devenv.com %~1.sln /Rebuild "Release|x64" 1>%currentWorkDir%\build_%~1.log 2>&1
    if !ERRORLEVEL! NEQ 0 call :errorMessage

    rem # In build.log, replace "error" by TeamCity messages
    %root%\src\third_party_open\commandline\bin\win32\sed.exe -e "/[Ee]rror[\:\ ]/s/^/\#\#teamcity\[buildStatus status\=\'FAILURE\' text\=\' /g;/buildStatus/s/$/\'\]/g" %currentWorkDir%\build_%~1.log
    goto :endproc



rem =======================
rem === InstallAll     ====
rem =======================
:installall
    if %prepareonly% EQU 1                goto :endproc
    if !ERRORLEVEL! NEQ 0                 goto :endproc

    if "!config!" == "all" (
        echo.
        echo "Installing in build_all ..."
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
        echo "Installing in build_delft3d4 ..."
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
    goto :endproc



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
    echo ERROR: Check log files in build_* directories
    echo        Is the correct Fortran compiler installed, available and selected?
    echo        Common problem: NetExtender needs to be switched on to reach the license server.
    goto :endproc


rem =======================
rem === END TAG      ======
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
rem === ENDPROC TAG  ======
rem =======================
:endproc
   rem # No exit  here, otherwise the script exits directly at the first missing artefact
   rem # No pause here, otherwise a pause will appear after each procedure execution
