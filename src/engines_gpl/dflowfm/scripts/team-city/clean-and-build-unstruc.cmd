@echo off

cd %~dp0/../..

if "%1" == "x64" (
	set bindir=bin\x64\Release
	set targetname=unstruc_x64.exe
) else (
	set bindir=bin\Win32\Release
	set targetname=unstruc.exe
)

call clean.cmd %1
call build.cmd %1
IF %ERRORLEVEL% == 0 GOTO copytonet
goto END

:copytonet
echo Copying artifact to P-disk...
set pdir=\\deltapdc\PROJECT\dflowfm\software\releases_windows\nightly_build
set netuser=deltares\d3dsup
set netpasswd=dhsd3d07 
net use "%pdir%" %netpasswd% /user:%netuser% /persistent:no

echo xcopy /C /Q /Y %bindir%\unstruc.exe "%pdir%\%targetname%"
echo F | xcopy /C /Q /Y %bindir%\unstruc.exe "%pdir%\%targetname%"
rem The echo F above is a user response to the 'directory or file' question by xcopy.

net use "%pdir%" /delete
echo Copy done
goto END

:END
echo done