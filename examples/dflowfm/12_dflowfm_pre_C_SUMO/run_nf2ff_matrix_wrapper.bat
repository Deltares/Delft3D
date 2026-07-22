:: Run this script at your own risk.
:: Requires valid NF2FF XML inputs in cosumo\NF2FF\<case>.
::
:: Purpose:
::   - Run one or more NF2FF matrix cases.
::   - Run each selected case in precice, dimr, or both modes.
::   - Collect case outputs under results\<case>\<mode>[ _timestamp ].
::
:: Usage:
::   run_nf2ff_matrix_wrapper.bat [case_list] [mode]
::
:: Arguments:
::   [case_list] Optional. One case or comma-separated cases.
::              Allowed: i0si2so2 i0si2so1 i1si2so2 i10si2so1 i1si42so1
::              Example: i0si2so2,i1si2so2
::              If omitted: all allowed cases are run.
::   [mode] Optional. precice | dimr | both
::          If omitted: uses MATRIX_RUN_MODES, otherwise defaults to "precice dimr".
::
:: Expected outputs:
::   - Always: per-run logs in results\logs and per-case artifacts in results\<case>\<mode>*.
::   - Multi-case runs: summary CSV and matrix Markdown are generated.
::   - Single-case runs: summary CSV and matrix Markdown are skipped.
::   - Matrix file contains header comments with invocation arguments, selected cases, and selected modes.
::
:: Cleanup behavior:
::   - If old artifacts exist, confirmation is requested before deletion.
::   - If not deleted, timestamped output names are used to avoid overwriting.
::   - Set AUTO_CONFIRM_DELETE=1 for non-interactive cleanup confirmation.
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

set "ALL_CASES=i0si2so2 i0si2so1 i1si2so2 i10si2so1 i1si42so1"
set "CASES=%ALL_CASES%"
set "RUN_MODES=precice dimr"
if defined MATRIX_RUN_MODES set "RUN_MODES=%MATRIX_RUN_MODES%"
set "REQUIRED_FILES=NF2FF__FlowFM_SubMod001_0.000.xml NF2FF__FlowFM_SubMod001_120.000.xml NF2FF__FlowFM_SubMod001_240.000.xml NF2FF__preC-SUMO_SubMod001_0.000.xml NF2FF__preC-SUMO_SubMod001_120.000.xml NF2FF__preC-SUMO_SubMod001_240.000.xml"
set "WRITE_AGGREGATES=1"
set "ALLOW_DELETE_OLD=0"
set "RAW_ARGS=%*"
if not defined RAW_ARGS set "RAW_ARGS=(none)"

set "ARG_COUNT=0"
for %%A in (%*) do (
    set /a ARG_COUNT+=1
    set "ARG_!ARG_COUNT!=%%~A"
)

if %ARG_COUNT% GTR 0 (
    set "MODE_INPUT="
    set "CASE_ARG_COUNT=%ARG_COUNT%"
    set "LAST_ARG=!ARG_%ARG_COUNT%!"

    call :is_mode "!LAST_ARG!"
    if not errorlevel 1 (
        set "MODE_INPUT=!LAST_ARG!"
        set /a CASE_ARG_COUNT=ARG_COUNT-1
    )

    if !CASE_ARG_COUNT! GTR 0 (
        set "CASES="
        for /l %%I in (1,1,!CASE_ARG_COUNT!) do (
            set "CASE_TOKEN=!ARG_%%I!"
            set "CASE_TOKEN_SPLIT=!CASE_TOKEN:,= !"
            for %%K in (!CASE_TOKEN_SPLIT!) do (
                call :validate_case "%%~K"
                if errorlevel 1 (
                    echo ERROR: Unknown case "%%~K"
                    echo Allowed cases: %ALL_CASES%
                    popd
                    exit /b 4
                )
                if defined CASES (
                    set "CASES=!CASES! %%~K"
                ) else (
                    set "CASES=%%~K"
                )
            )
        )
    )

    if defined MODE_INPUT (
        if /i "!MODE_INPUT!"=="both" (
            set "RUN_MODES=precice dimr"
        ) else if /i "!MODE_INPUT!"=="precice" (
            set "RUN_MODES=precice"
        ) else if /i "!MODE_INPUT!"=="dimr" (
            set "RUN_MODES=dimr"
        ) else (
            echo ERROR: Unknown mode "!MODE_INPUT!"
            echo Allowed modes: precice dimr both
            popd
            exit /b 5
        )
    )
)

set "CASE_COUNT=0"
for %%C in (%CASES%) do set /a CASE_COUNT+=1
if %CASE_COUNT% LEQ 1 set "WRITE_AGGREGATES=0"

for /f %%S in ('powershell -NoProfile -Command "Get-Date -Format \"yyyyMMdd_HHmmss\""') do set "RUN_STAMP=%%S"

if not exist "%RUNBAT%" (
    echo ERROR: Missing run.bat at %RUNBAT%
    popd
    exit /b 2
)

if not exist "%RESULTS%" mkdir "%RESULTS%"
call :confirm_cleanup
if errorlevel 1 (
    popd
    exit /b 6
)

if not exist "%LOGDIR%" mkdir "%LOGDIR%"

if "%WRITE_AGGREGATES%"=="1" (
    if "%ALLOW_DELETE_OLD%"=="0" (
        if exist "%SUMMARY%" set "SUMMARY=%RESULTS%\summary_!RUN_STAMP!.csv"
        if exist "%MATRIX%" set "MATRIX=%RESULTS%\matrix_!RUN_STAMP!.md"
    )
    > "%SUMMARY%" echo case,mode,start_iso,end_iso,duration_sec,exit_code,artifact_path,notes
) else (
    echo INFO: Single-case run detected. summary.csv and matrix.md will not be generated.
)

echo === PREFLIGHT ===
call :preflight
if errorlevel 1 (
    if "%WRITE_AGGREGATES%"=="1" (
        echo ERROR: Preflight failed. See %SUMMARY%
    ) else (
        echo ERROR: Preflight failed.
    )
    popd
    exit /b 3
)

set "OVERALL_RC=0"
for %%M in (%RUN_MODES%) do (
    if /i "%%M"=="precice" (
        call :run_mode precice 1
        if errorlevel 1 set "OVERALL_RC=1"
    ) else if /i "%%M"=="dimr" (
        call :run_mode dimr 0
        if errorlevel 1 set "OVERALL_RC=1"
    ) else (
        echo WARNING: Ignoring unknown mode %%M
    )
)

if "%WRITE_AGGREGATES%"=="1" call :write_matrix

echo === DONE ===
if "%WRITE_AGGREGATES%"=="1" (
    echo Summary: %SUMMARY%
    echo Matrix : %MATRIX%
) else (
    echo Aggregate summary and matrix skipped for single-case run.
)
popd
exit /b %OVERALL_RC%

:validate_case
set "IS_VALID_CASE=0"
for %%X in (%ALL_CASES%) do (
    if /i "%%X"=="%~1" set "IS_VALID_CASE=1"
)
if "%IS_VALID_CASE%"=="1" exit /b 0
exit /b 1

:is_mode
if /i "%~1"=="precice" exit /b 0
if /i "%~1"=="dimr" exit /b 0
if /i "%~1"=="both" exit /b 0
exit /b 1

:confirm_cleanup
set "HAS_OLD_ARTIFACTS=0"
if exist "%SUMMARY%" set "HAS_OLD_ARTIFACTS=1"
if exist "%MATRIX%" set "HAS_OLD_ARTIFACTS=1"
if exist "%LOGDIR%" set "HAS_OLD_ARTIFACTS=1"

for %%C in (%CASES%) do (
    for %%M in (%RUN_MODES%) do (
        if exist "%RESULTS%\%%C\%%M" set "HAS_OLD_ARTIFACTS=1"
    )
)

if "%HAS_OLD_ARTIFACTS%"=="0" exit /b 0

if /i "%AUTO_CONFIRM_DELETE%"=="1" (
    set "DELETE_REPLY=Y"
) else (
    echo.
    echo Existing result artifacts were found, including prior summary/matrix/logs and case folders.
    set /p "DELETE_REPLY=Delete old artifacts before this run? [y/N]: "
)

if /i "%DELETE_REPLY%"=="y" (
    set "ALLOW_DELETE_OLD=1"
    if exist "%SUMMARY%" del /f /q "%SUMMARY%"
    if exist "%MATRIX%" del /f /q "%MATRIX%"
    if exist "%LOGDIR%" rmdir /s /q "%LOGDIR%"
    for %%C in (%CASES%) do (
        for %%M in (%RUN_MODES%) do (
            if exist "%RESULTS%\%%C\%%M" rmdir /s /q "%RESULTS%\%%C\%%M"
        )
    )
) else (
    set "ALLOW_DELETE_OLD=0"
    echo Keeping existing artifacts. New outputs will be written to timestamped folders when needed.
)

exit /b 0

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

    if "%WRITE_AGGREGATES%"=="1" (
        if "!CASE_MISSING!"=="0" (
            >> "%SUMMARY%" echo %%C,preflight,,,,0,,ok
        ) else (
            >> "%SUMMARY%" echo %%C,preflight,,,,1,,missing required NF2FF input files
        )
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
        if "%WRITE_AGGREGATES%"=="1" >> "%SUMMARY%" echo %%C,%MODE%,,,,2,,sync failed
        set "BATCH_FAILED=1"
    ) else (
        set "CASE_OUT=%RESULTS%\%%C\%MODE%"
        if exist "!CASE_OUT!" (
            if "%ALLOW_DELETE_OLD%"=="1" (
                rmdir /s /q "!CASE_OUT!"
            ) else (
                set "CASE_OUT=%RESULTS%\%%C\%MODE%_!RUN_STAMP!"
            )
        )
        mkdir "!CASE_OUT!"

        call :cleanup_processes

        set "USE_PRECICE_OVERRIDE=%MODE_FLAG%"
        set "NO_PAUSE=1"
        set "START_FM_OVERRIDE=1"
        set "START_PRECSUMO_OVERRIDE=1"

        for /f %%T in ('powershell -NoProfile -Command "[DateTimeOffset]::Now.ToUnixTimeSeconds()"') do set "START_EPOCH=%%T"
        for /f %%S in ('powershell -NoProfile -Command "Get-Date -Format \"yyyy-MM-ddTHH:mm:ss\""') do set "START_ISO=%%S"

        set "RUN_LOG=%LOGDIR%\%%C_%MODE%_!RUN_STAMP!.log"
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
        if exist "%FMROOT%\precice_debug_output.txt" copy /y "%FMROOT%\precice_debug_output.txt" "!CASE_OUT!\precice_debug_output_fm.txt" >nul 2>nul
        if exist "%ROOT%cosumo\precice_debug_output.txt" copy /y "%ROOT%cosumo\precice_debug_output.txt" "!CASE_OUT!\precice_debug_output_csumo.txt" >nul 2>nul
        if /i "%MODE%"=="precice" (
            if exist "%FMROOT%\precice_debug_output.txt" copy /y "%FMROOT%\precice_debug_output.txt" "%RESULTS%\%%C_precice_debug_output_fm.txt" >nul 2>nul
            if exist "%ROOT%cosumo\precice_debug_output.txt" copy /y "%ROOT%cosumo\precice_debug_output.txt" "%RESULTS%\%%C_precice_debug_output_csumo.txt" >nul 2>nul
        )

        if not "!RC!"=="0" set "BATCH_FAILED=1"
        if "%WRITE_AGGREGATES%"=="1" >> "%SUMMARY%" echo %%C,%MODE%,!START_ISO!,!END_ISO!,!DURATION_SEC!,!RC!,"!CASE_OUT!","log=!RUN_LOG!"

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
> "%MATRIX%" echo [//]: # ^(run_nf2ff_matrix_wrapper arguments: %RAW_ARGS%^)
>> "%MATRIX%" echo [//]: # ^(selected cases: %CASES%^)
>> "%MATRIX%" echo [//]: # ^(selected modes: %RUN_MODES%^)
>> "%MATRIX%" echo.
>> "%MATRIX%" echo ^| case ^| precice rc ^| precice sec ^| dimr rc ^| dimr sec ^|
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
exit /b 0
