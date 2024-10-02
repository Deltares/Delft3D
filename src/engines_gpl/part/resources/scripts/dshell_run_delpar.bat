@echo off

echo .
echo D-Particle Tracking - command line interface
echo .
echo The path to the "run_delpar" script has been added to
echo the PATH environment variable for easy access.
echo .
echo To run D-Particle Tracking:
echo - change to the directory containing the input file
echo - type the command: run_delpar name.inp
echo .

cd /d %USERPROFILE%

set path=%~dp0\;%PATH%

start /B
exit
