REM @echo off

REM set globalErrorLevel=0

echo copy third party dlls...

rem Usage:
rem > copy-third-party-dlls.cmd <targetdir>

rem with:
rem   <targetdir>               : Target directory where the dlls are going to be installed by the script

rem To be used by Visual Studio's post build events for dflowfm-interacter, dflowfm-cli and dflowfm-dll


set TargetDir=%1
set SourceDir=%~dp0\release-1911-x64\bin


"xcopy.exe" /D /Y /R "%SourceDir%\xerces-c_3_2.dll" "%TargetDir%"\ > nul
"xcopy.exe" /D /Y /R "%SourceDir%\expat.dll" "%TargetDir%"\ > nul
"xcopy.exe" /D /Y /R "%SourceDir%\libpq.dll" "%TargetDir%"\ > nul
"xcopy.exe" /D /Y /R "%SourceDir%\proj_6_1.dll" "%TargetDir%"\ > nul
"xcopy.exe" /D /Y /R "%SourceDir%\sqlite3.dll" "%TargetDir%"\ > nul
"xcopy.exe" /D /Y /R "%SourceDir%\spatialite.dll" "%TargetDir%"\ > nul
"xcopy.exe" /D /Y /R "%SourceDir%\libmysql.dll" "%TargetDir%"\ > nul
"xcopy.exe" /D /Y /R "%SourceDir%\geos_c.dll" "%TargetDir%"\ > nul
"xcopy.exe" /D /Y /R "%SourceDir%\openjp2.dll" "%TargetDir%"\ > nul
"xcopy.exe" /D /Y /R "%SourceDir%\proj.dll" "%TargetDir%"\ > nul
"xcopy.exe" /D /Y /R "%SourceDir%\freexl.dll" "%TargetDir%"\ > nul
"xcopy.exe" /D /Y /R "%SourceDir%\iconv.dll" "%TargetDir%"\ > nul
"xcopy.exe" /D /Y /R "%SourceDir%\libxml2.dll" "%TargetDir%"\ > nul
"xcopy.exe" /D /Y /R "%SourceDir%\geos.dll" "%TargetDir%"\ > nul