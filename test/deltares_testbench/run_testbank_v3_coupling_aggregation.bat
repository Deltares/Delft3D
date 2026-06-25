@Echo Off
SetLocal EnableExtensions EnableDelayedExpansion
call d:\checkouts\oss\.venv\Scripts\activate.bat
python.exe TestBench.py -c  --config configs\dimr\dimr_dflowfm_waq_coupling_aggregation_win64.xml  --filter "testcase=e02_f029_c305"   --parallel --log-level DEBUG  %1 %2 %3 %4
IF %ERRORLEVEL% == 0 GOTO END
echo ERROR: run_testbank_v3.bat: TestBench returns code %ERRORLEVEL%
:END
pause


