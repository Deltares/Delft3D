@Echo Off
SetLocal EnableExtensions EnableDelayedExpansion
call d:\checkouts\oss\.venv\Scripts\activate.bat
python.exe TestBench.py -c  --config .\configs\dimr\dimr_dflowfm_waq_coupling_1D2D_win64.xml   --parallel --log-level DEBUG %1 %2 %3 %4
rem --filter "testcase=" 
IF %ERRORLEVEL% == 0 GOTO END
echo ERROR: run_testbank_v3.bat: TestBench returns code %ERRORLEVEL%
:END
pause


