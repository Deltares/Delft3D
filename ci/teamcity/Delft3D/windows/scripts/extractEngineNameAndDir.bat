@echo off
setlocal enabledelayedexpansion

if "%~1"=="" (
 echo Error: Missing argument, please provide 'engine_name:engine_dir' as argument.
 exit /b 1
) 

set engine_name_and_dir=%~1
echo engine_name_and_dir: '%engine_name_and_dir%'
for /f "tokens=1,2 delims=:" %%a in ("%engine_name_and_dir%") do (
    set engine_name=%%a
    set engine_dir=%%b
)
echo ##teamcity[setParameter name='engine_name' value='!engine_name!']
echo ##teamcity[setParameter name='engine_dir' value='!engine_dir!']