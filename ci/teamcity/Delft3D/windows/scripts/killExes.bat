echo off
REM taskkill does not automatically kill child processes, and mpiexec spawns some
call :kill_program dimr.exe
call :kill_program mpiexec.exe
call :kill_program hydra_pmi_proxy.exe
call :kill_program mormerge.exe
call :kill_program d_hydro.exe
set errorlevel=0
goto :eof

:kill_program
set program_name=%~1
tasklist | find /i "%%program_name%%" > NUL 2>&1
if errorlevel 1 (
    echo %%program_name%% is not running.
) else (
    echo Executing 'taskkill /f /im %%program_name%% /t'
    taskkill /f /im %%program_name%% /t > NUL 2>&1
)
exit /b 0