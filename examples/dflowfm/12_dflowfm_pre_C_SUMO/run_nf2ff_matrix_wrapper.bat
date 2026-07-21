@echo off
setlocal EnableExtensions EnableDelayedExpansion

set "ROOT=%~dp0"
pushd "%ROOT%"

set "RUNBAT=%ROOT%run.bat"
set "NFROOT=%ROOT%cosumo\NF2FF"
set "FMROOT=%ROOT%fm"
set "RESULTS=%ROOT%results"
set "LOGDIR=%RESULTS%\logs"
set "SUMMARY=%RESULTS%\summary.csv"
set "MATRIX=%RESULTS%\matrix.md"

set "CASES=i0si2so2 i0si2so1 i1si2so2 i10si2so1"
set "REQUIRED_FILES=NF2FF__FlowFM_SubMod001_0.000.xml NF2FF__FlowFM_SubMod001_120.000.xml NF2FF__FlowFM_SubMod001_240.000.xml NF2FF__preC-SUMO_SubMod001_0.000.xml NF2FF__preC-SUMO_SubMod001_120.000.xml NF2FF__preC-SUMO_SubMod001_240.000.xml"

if not exist "%RUNBAT%" (
    echo ERROR: Missing run.bat at %RUNBAT%
    popd
    exit /b 2
)

if not exist "%RESULTS%" mkdir "%RESULTS%"
if not exist "%LOGDIR%" mkdir "%LOGDIR%"

> "%SUMMARY%" echo case,mode,start_iso,end_iso,duration_sec,exit_code,artifact_path,notes
>> "%SUMMARY%" echo i1si42so1,skipped,,,,,,manual run already completed

echo === PREFLIGHT ===
call :preflight
if errorlevel 1 (
    echo ERROR: Preflight failed. See %SUMMARY%
    popd
    exit /b 3
)

set "OVERALL_RC=0"
call :run_mode precice 1
if errorlevel 1 set "OVERALL_RC=1"

call :run_mode dimr 0
if errorlevel 1 set "OVERALL_RC=1"

call :write_matrix

echo === DONE ===
echo Summary: %SUMMARY%
echo Matrix : %MATRIX%
popd
exit /b %OVERALL_RC%

:preflight
set "MISSING_ANY=0"
for %%C in (%CASES%) do (
    set "CASE_MISSING=0"
    for %%F in (%REQUIRED_FILES%) do (
        if not exist "%NFROOT%\%%C\%%F" (
            set "CASE_MISSING=1"
            set "MISSING_ANY=1"
            echo MISSING: %NFROOT%\%%C\%%F
        )
    )

    if "!CASE_MISSING!"=="0" (
        >> "%SUMMARY%" echo %%C,preflight,,,,0,,ok
    ) else (
        >> "%SUMMARY%" echo %%C,preflight,,,,1,,missing required NF2FF input files
    )
)

if "%MISSING_ANY%"=="1" exit /b 1
exit /b 0

:run_mode
set "MODE=%~1"
set "MODE_FLAG=%~2"
set "BATCH_FAILED=0"

echo === MODE %MODE% ===
for %%C in (%CASES%) do (
    echo --- CASE %%C / %MODE% ---

    call :sync_case %%C
    if errorlevel 1 (
        echo ERROR: Case sync failed for %%C ^(%MODE%^)
        >> "%SUMMARY%" echo %%C,%MODE%,,,,2,,sync failed
        set "BATCH_FAILED=1"
    ) else (
        set "CASE_OUT=%RESULTS%\%%C\%MODE%"
        if exist "!CASE_OUT!" rmdir /s /q "!CASE_OUT!"
        mkdir "!CASE_OUT!"

        call :cleanup_processes

        set "USE_PRECICE_OVERRIDE=%MODE_FLAG%"
        set "NO_PAUSE=1"
        set "START_FM_OVERRIDE=1"
        set "START_PRECSUMO_OVERRIDE=1"

        for /f %%T in ('powershell -NoProfile -Command "[DateTimeOffset]::Now.ToUnixTimeSeconds()"') do set "START_EPOCH=%%T"
        for /f %%S in ('powershell -NoProfile -Command "Get-Date -Format \"yyyy-MM-ddTHH:mm:ss\""') do set "START_ISO=%%S"

        set "RUN_LOG=%LOGDIR%\%%C_%MODE%.log"
        call "%RUNBAT%" > "!RUN_LOG!" 2>&1
        set "RC=!ERRORLEVEL!"

        for /f %%T in ('powershell -NoProfile -Command "[DateTimeOffset]::Now.ToUnixTimeSeconds()"') do set "END_EPOCH=%%T"
        for /f %%S in ('powershell -NoProfile -Command "Get-Date -Format \"yyyy-MM-ddTHH:mm:ss\""') do set "END_ISO=%%S"

        set /a "DURATION_SEC=!END_EPOCH!-!START_EPOCH!"

        if exist "%FMROOT%\DFM_OUTPUT_FlowFM" (
            robocopy "%FMROOT%\DFM_OUTPUT_FlowFM" "!CASE_OUT!\DFM_OUTPUT_FlowFM" /E /R:1 /W:1 /NFL /NDL /NJH /NJS /NC /NS >nul
        )
        copy /y "%FMROOT%\*.mdu" "!CASE_OUT!\" >nul 2>nul
        copy /y "%FMROOT%\*.dia" "!CASE_OUT!\" >nul 2>nul
        copy /y "%FMROOT%\*.nc" "!CASE_OUT!\" >nul 2>nul

        if not "!RC!"=="0" set "BATCH_FAILED=1"
        >> "%SUMMARY%" echo %%C,%MODE%,!START_ISO!,!END_ISO!,!DURATION_SEC!,!RC!,"!CASE_OUT!","log=!RUN_LOG!"

        call :cleanup_processes
    )
)

if "%BATCH_FAILED%"=="1" exit /b 1
exit /b 0

:sync_case
set "CASE_NAME=%~1"
for %%F in (%REQUIRED_FILES%) do (
    copy /y "%NFROOT%\%CASE_NAME%\%%F" "%NFROOT%\%%F" >nul
    if errorlevel 1 exit /b 1
)

if exist "%NFROOT%\NF2FF__FLowFM_SubMod001_240.000.xml" del /f /q "%NFROOT%\NF2FF__FLowFM_SubMod001_240.000.xml"
exit /b 0

:cleanup_processes
taskkill /f /im preC-SUMO.exe >nul 2>nul
taskkill /f /im dflowfm.exe >nul 2>nul
taskkill /f /im dflowfm-cli.exe >nul 2>nul
taskkill /f /im dimr.exe >nul 2>nul
exit /b 0

:write_matrix
> "%MATRIX%" echo ^| case ^| precice rc ^| precice sec ^| dimr rc ^| dimr sec ^|
>> "%MATRIX%" echo ^|---^|---:^|---:^|---:^|---:^|

for %%C in (%CASES%) do (
    set "P_RC="
    set "P_SEC="
    set "D_RC="
    set "D_SEC="

    for /f "tokens=1-8 delims=," %%a in ('findstr /b /c:"%%C,precice," "%SUMMARY%"') do (
        set "P_SEC=%%e"
        set "P_RC=%%f"
    )

    for /f "tokens=1-8 delims=," %%a in ('findstr /b /c:"%%C,dimr," "%SUMMARY%"') do (
        set "D_SEC=%%e"
        set "D_RC=%%f"
    )

    >> "%MATRIX%" echo ^| %%C ^| !P_RC! ^| !P_SEC! ^| !D_RC! ^| !D_SEC! ^|
)

>> "%MATRIX%" echo.
>> "%MATRIX%" echo Skipped: i1si42so1 ^(manual run already completed^).
exit /b 0
