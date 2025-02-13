@echo off
REM Check if Git is installed
git --version >nul 2>&1
IF ERRORLEVEL 1 (
    echo Git is not installed or not found in PATH. Please install Git and try again.
    exit /b 1
)

REM Get the commit time of the current commit
git show -s --format=%%ci HEAD > temp.txt
SET /P GIT_HEAD_TIME=<temp.txt
DEL temp.txt

REM Set the TeamCity parameter
echo ##teamcity[setParameter name='env.TIME_ISO_8601' value='%GIT_HEAD_TIME%']

REM Remove the variable
set GIT_HEAD_TIME=