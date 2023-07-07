@echo off
set version=5.10
set serial=20220712
set toolFound=false
set userProcDef=false


if exist ..\bin\waqpb_export.exe       set toolFound=true

set userProcDefLoc=%~dp0\..\default


if %toolFound%==true (
    echo This will execute: waqpb_export.exe -version%version% -serial%serial%
    echo.
    echo This command will 'export' the data in the csv-tables to a proc_def,
    echo a procesm.asc and latex files for the manuals.
    echo.
    echo Run waqpb_help.bat for more information
    echo.
    pause
    call %~dp0\run_waqpb_export.bat -version%version% -serial%serial% %userProcDefLoc%
    echo.
    pause
) else (
    echo.
    echo waqpb_export.exe not found!
    echo Please check cmake_configuration, or build the waq proces library tools.
    echo You can build it using build_all\all.sln
    echo.
    echo Run waqpb_help.bat for more information
    echo.
    pause
)
