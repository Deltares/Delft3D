rem This script is used for adding the svn revision to the properties of
rem a .Net assembly.
rem It is used by the .Net projects in ..\RTCToolsOpenMI, but has to be
rem positioned in the present dir, where the solution is, because the
rem callers of this script (the pre-build-events in the .Net projects)
rem refer to $(SolutionDir)

setlocal enabledelayedexpansion
@ECHO off

SET ORG_DIR=%CD%

SET argcount=0
FOR %%x IN (%*) DO SET /A argcount+=1

IF %argcount%==0 (
    ECHO "invalid arguments: [path] (infile) (outfile)"
    ECHO "usage: replace SVNREV text with revision number, SVNROOT with repository"
    ECHO "path: execute at specific location"
    ECHO "[optional] infile: file to read, defaults to AssemblyInfo.cs.svn"
    ECHO "[optional] outfile: file to create, defaults to AssemblyInfo.cs"
    GOTO EOF
)

ECHO "%argcount% arguments received"

IF %argcount%==1 (
    ECHO "setting default in and out files."
    SET INTEXTFILE=AssemblyInfo.cs.svn
    SET OUTTEXTFILE=AssemblyInfo.cs
) ELSE (
    ECHO "setting custom in and out files."
    SET INTEXTFILE=%2
    SET OUTTEXTFILE=%3
) 

CD %1

ECHO "executing in directory %1"

REM GET THE SVN VERSION NUMBER AND  REVISION PATH

FOR /f "tokens=1,* delims=¶" %%A IN ('svn info') DO (
	ECHO %%A | findstr /I "^Revision" && SET REV_BUF=%%A
	ECHO %%A | findstr /I "^URL" && SET ROOT_BUF=%%A
    ECHO %%A | findstr /I /C:"Repository Root" && SET AFTER_BUF=%%A
)

IF NOT DEFINED REV_BUF (
    SET SVN_REV=0
	SET SVN_ROOT=
) ELSE (
    SET SVN_REV=%REV_BUF:~10%
    SET SVN_ROOT=%ROOT_BUF:~5%
    SET "FIND=*%AFTER_BUF:~17%
    CALL SET SVN_ROOT=%%SVN_ROOT:!FIND!=%%
)

ECHO "using rev %SVN_REV% and root %SVN_ROOT%"

REM SUBSTITUTE THE VERSION NUMBER IN TEMPLATE
SET SEARCHTEXT=SVNREV
SET SEARCHROOT=SVNROOT

IF EXIST %OUTTEXTFILE% (
    ECHO "removing %OUTTEXTFILE%"
    DEL %OUTTEXTFILE%
)

FOR /f "tokens=1,* delims=¶" %%A IN ( '"type %INTEXTFILE%"') DO (
    SET string=%%A
    SET modified=!string:%SEARCHTEXT%=%SVN_REV%!
    SET modified=!modified:%SEARCHROOT%=%SVN_ROOT%!
    ECHO !modified! >> %OUTTEXTFILE%
)

CD %ORG_DIR%

:EOF
