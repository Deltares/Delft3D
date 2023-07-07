@echo off
set toolFound=false
set toolx64=false
set cmakeConfiguration=build_all

if exist ..\bin\waqpb_import.exe       set toolFound=true

set userProcDefLoc=%~dp0\..\default

if %toolFound%==true  (
    echo This will execute: waqpb_import.exe
    echo.
    echo This command will 'import' changes from the proces.asc file.
    echo Make sure it exists!
    echo.
    echo Run waqpb_help.bat for more information
    echo.
    pause
    call %~dp0\run_waqpb_import.bat %userProcDefLoc%
    echo.
    pause
) else (
    echo.
    echo waqpb_import.exe not found!
    echo Please check cmake_configuration, or build the waq proces library tools.
    echo You can build it using build_all\all.sln
    echo.
    echo Run waqpb_help.bat for more information
    echo.
    pause
)
