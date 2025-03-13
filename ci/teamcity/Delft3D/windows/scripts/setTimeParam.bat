@echo off
REM Check if Git is installed
git --version >nul 2>&1
IF ERRORLEVEL 1 (
    echo Git is not installed or not found in PATH. Please install Git and try again.
    exit /b 1
)

REM Get the commit time of the current commit
git show -s --format=%%ci HEAD > temp.txt
IF ERRORLEVEL 1 (
    echo Failed to get the commit time. Please check your Git repository.
    exit /b 1
)

SET /P GIT_HEAD_TIME=<temp.txt
DEL temp.txt

REM Check if GIT_HEAD_TIME is set
IF NOT DEFINED GIT_HEAD_TIME (
    echo Failed to set GIT_HEAD_TIME. Please check the script.
    exit /b 1
)

REM Set the TeamCity parameter
echo ##teamcity[setParameter name='env.TIME_ISO_8601' value='%GIT_HEAD_TIME%']

REM Remove the variable
SET GIT_HEAD_TIME=