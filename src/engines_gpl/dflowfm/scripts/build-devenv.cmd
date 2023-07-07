rem @echo off

set usage="false"

if "%1" == "" set usage="true"
if "%2" == "" set usage="true"
if "%3" == "" set usage="true"
if "%4" == "" set usage="true"

if %usage%=="true" (
  echo Usage: %~nx0 solution configuration platform target
  echo.
  echo solution ....... solution file name
  echo configuration .. Release Debug
  echo platform ....... Win32 x64
  echo target ......... Build Clean
  goto eof
)

set solution=%1
set configuration=%2
set platform=%3
set target=%4

del build.log

if "%VS140COMNTOOLS%" == "" (
    echo ##teamcity[buildStatus status='FAILURE' text='Visual Studio 2012 is missing.']
    exit /B 1
) 


call "%VS140COMNTOOLS%\vsvars32.bat" 


devenv.exe %solution% /%target% "%configuration%|%platform%" /Out build.log

IF %ERRORLEVEL% == 0 GOTO END
type build.log
echo ##teamcity[buildStatus status='FAILURE' text='{build.status.text} in compilation, check build log']
EXIT /B 1

:END
type build.log

:eof