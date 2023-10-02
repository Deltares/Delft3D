@echo off
set version=6.1
set serial=20230609
set toolFound=false
set cmakeConfiguration=build_dwaq

if exist ..\..\..\..\%cmakeConfiguration%\waqpb_export\Release\waqpb_export.exe     set toolFound=true
echo Searching in cmake build: %cmakeConfiguration%


set procDefLoc=%cd%\..\default


if %toolFound%==true (
    echo This will execute: waqpb_export.exe -version%version% -serial%serial%
    echo.
    echo This command will 'export' the data in the csv-tables to a proc_def,
    echo a procesm.asc and latex files for the manuals.
    echo.
    echo Run waqpb_help.bat for more information
    echo.
    pause
    call ..\..\..\..\%cmakeConfiguration%\x64\Release\dwaq\scripts\run_waqpb_export.bat -version%version% -serial%serial% %procDefLoc%

    rem copy files to installation directory
    if exist ..\..\..\..\%cmakeConfiguration%\x64\dwaq\resources\ (
        copy ..\default\proc_def.* ..\..\..\..\..\..\..\%cmakeConfiguration%\x64\dwaq\resources\
        copy ..\default\csvFiles\*.csv ..\..\..\..\..\..\..\%cmakeConfiguration%\x64\dwaq\resources\csvFiles
    )
    if exist ..\..\..\..\%cmakeConfiguration%\x64\Release\dwaq\resources\ (
        copy ..\default\proc_def.* ..\..\..\..\%cmakeConfiguration%\x64\Release\dwaq\resources\
        copy ..\default\csvFiles\*.csv ..\..\..\..\%cmakeConfiguration%\x64\Release\dwaq\resources\csvFiles
    )
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
