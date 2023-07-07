@ECHO OFF

SETLOCAL
SETLOCAL EnableDelayedExpansion

SET HOMEDIR=d:\pijl

SET UNSTRUCDIRS=unstruc_clean_svn unstruc_svn unstruc_second_svn unstruc_sedmor_svn

SET BINDIRS=x64\Release x64\Debug

SET OMP_NUM_THREADS=3


SET num=0
(for %%a in (%UNSTRUCDIRS%) do (
	(for %%b in (%BINDIRS%) do (
		SET /A num+=1
		SET _EXEDIR[!num!]=%HOMEDIR%\%%a\bin\%%b
		SET _UNSTDIR[!num!]=%%a
		SET _BINDIR[!num!]=%%b
			
	))
))


ECHO.
ECHO -----------
ECHO EXECUTABLES
ECHO -----------
ECHO.

for /l %%i in (1, 1, %num%) do (
	echo      %%i: !_UNSTDIR[%%i]!\!_BINDIR[%%i]!
)
ECHO.

SET /P i=Select executable 1-%num%:

IF %i%==0 (
   SET i=1
)

SET UNSTRUCDIR=!_UNSTDIR[%i%]!
SET BINDIR=!_BINDIR[%i%]!
SET RESDIR=%HOMEDIR%\%UNSTRUCDIR%\res
SET EXEDIR=!_EXEDIR[%i%]!
SET UNSTRUCEXE=%EXEDIR%\dflowfm.exe

rem SET OPTS=--autostart

rem disable OpenGL in debug mode
if not x%BINDIR:debug=%==x%BINDIR% (
	SET OPTS=%OPTS% --display:OpenGL=0
	SET OMP_NUM_THREADS=1
)

if not exist unstruc.ini copy %RESDIR%\unstruc.ini .
if not exist isocolour.hls copy %RESDIR%\isocolour.hls .
if not exist interact.ini  copy %RESDIR%\interact.ini .

SET CMD=%UNSTRUCEXE% %OPTS% %1
ECHO %CMD%
%CMD%