@ echo off

rem should be handled in the testbank_config.xml: set OMP_NUM_THREADS=1
set OMP_NUM_THREADS=1

    rem -c
    rem -r --autocommit
    rem --only-post-process
    rem --filter "testcase=e100_f02_c00,e25_:maxruntime=<3000.0"
    rem --teamcity
    rem --log-level DEBUG

setlocal
set path=c:\users\markus\AppData\Local\anaconda3;%PATH%

set PROC_DEF_DIR=%~dp0%\data\engines\teamcity_artifacts\x64\dflowfm\default
echo %PROC_DEF_DIR%


rem c:\users\markus\AppData\Local\anaconda3\python.exe TestBench.py --username k2LH7TL14fR41wsjRXId --password WlNJ8jO9eNjltuRxJVFJ7wl1eHTbQyLK69wuvjUJ -c --config configs\delft3d4\coup203_win64.xml

rem c:\users\markus\AppData\Local\anaconda3\python.exe TestBench.py --username k2LH7TL14fR41wsjRXId --password WlNJ8jO9eNjltuRxJVFJ7wl1eHTbQyLK69wuvjUJ -c --config configs\dimr\dimr_dflowfm_waq_coupling_1d2d_win64.xml --filter "testcase=e02_f029_c5"

rem WAQ
rem c:\users\markus\AppData\Local\anaconda3\python.exe TestBench.py --username k2LH7TL14fR41wsjRXId --password WlNJ8jO9eNjltuRxJVFJ7wl1eHTbQyLK69wuvjUJ -c --config configs\dimr\dimr_dwaq_win64.xml
rem c:\users\markus\AppData\Local\anaconda3\python.exe TestBench.py --username k2LH7TL14fR41wsjRXId --password WlNJ8jO9eNjltuRxJVFJ7wl1eHTbQyLK69wuvjUJ -c --config configs\dimr\dimr_dwaq_win64.xml --filter "testcase=e03_f01_c07"
rem c:\users\markus\AppData\Local\anaconda3\python.exe TestBench.py --username k2LH7TL14fR41wsjRXId --password WlNJ8jO9eNjltuRxJVFJ7wl1eHTbQyLK69wuvjUJ -c --config configs\dimr\dimr_dwaq_win64.xml --filter "testcase=e03_f10_c02"
rem c:\users\markus\AppData\Local\anaconda3\python.exe TestBench.py --username k2LH7TL14fR41wsjRXId --password WlNJ8jO9eNjltuRxJVFJ7wl1eHTbQyLK69wuvjUJ -c --config configs\dimr\dimr_dwaq_win64.xml --filter "testcase=e03_f01"
rem c:\users\markus\AppData\Local\anaconda3\python.exe TestBench.py --username k2LH7TL14fR41wsjRXId --password WlNJ8jO9eNjltuRxJVFJ7wl1eHTbQyLK69wuvjUJ -c --config configs\dimr\dimr_dwaq_validation_win64.xml

rem c:\users\markus\AppData\Local\anaconda3\python.exe TestBench.py --username k2LH7TL14fR41wsjRXId --password WlNJ8jO9eNjltuRxJVFJ7wl1eHTbQyLK69wuvjUJ -c --config configs\dimr\dimr_dwaq_win64.xml --filter "testcase=e03_f01_c07"
rem c:\users\markus\AppData\Local\anaconda3\python.exe TestBench.py --username k2LH7TL14fR41wsjRXId --password WlNJ8jO9eNjltuRxJVFJ7wl1eHTbQyLK69wuvjUJ -c --config configs\dimr\dimr_dwaq_win64.xml --filter "testcase=e03_f01_c08"

rem PART
rem c:\users\markus\AppData\Local\anaconda3\python.exe TestBench.py --username k2LH7TL14fR41wsjRXId --password WlNJ8jO9eNjltuRxJVFJ7wl1eHTbQyLK69wuvjUJ -c --config configs\dimr\dimr_dpart_win64.xml --filter "testcase=e05_f04_c101_oil"
rem c:\users\markus\AppData\Local\anaconda3\python.exe TestBench.py --username k2LH7TL14fR41wsjRXId --password WlNJ8jO9eNjltuRxJVFJ7wl1eHTbQyLK69wuvjUJ -c --config configs\dimr\dimr_dpart_win64.xml --filter "testcase=e05_f01_c201"
rem c:\users\markus\AppData\Local\anaconda3\python.exe TestBench.py --username k2LH7TL14fR41wsjRXId --password WlNJ8jO9eNjltuRxJVFJ7wl1eHTbQyLK69wuvjUJ -c --config configs\dimr\dimr_dpart_fm_win64.xml --filter "testcase=e05_f02_fm_c01_tracer"

rem COUPLING
rem c:\users\markus\AppData\Local\anaconda3\python.exe TestBench.py --username k2LH7TL14fR41wsjRXId --password WlNJ8jO9eNjltuRxJVFJ7wl1eHTbQyLK69wuvjUJ -c --config configs\dimr\dimr_dflowfm_waq_coupling_win64.xml --filter "testcase=e02_f029_c001"
rem c:\users\markus\AppData\Local\anaconda3\python.exe TestBench.py --username k2LH7TL14fR41wsjRXId --password WlNJ8jO9eNjltuRxJVFJ7wl1eHTbQyLK69wuvjUJ -c --config configs\dimr\dimr_dflowfm_waq_coupling_win64.xml --filter "testcase=e02_f029_c101"
rem c:\users\markus\AppData\Local\anaconda3\python.exe TestBench.py --username k2LH7TL14fR41wsjRXId --password WlNJ8jO9eNjltuRxJVFJ7wl1eHTbQyLK69wuvjUJ -c --config configs\dimr\dimr_dflowfm_waq_coupling_aggregation_win64.xml --filter "testcase=e02_f029_c204"
rem c:\users\markus\AppData\Local\anaconda3\python.exe TestBench.py --username k2LH7TL14fR41wsjRXId --password WlNJ8jO9eNjltuRxJVFJ7wl1eHTbQyLK69wuvjUJ -c --config configs\dimr\dimr_dflowfm_waq_coupling_win64.xml --filter "testcase=e02_f029_c001"

rem D-FLOWM FM + processes
rem c:\users\markus\AppData\Local\anaconda3\python.exe TestBench.py --username k2LH7TL14fR41wsjRXId --password WlNJ8jO9eNjltuRxJVFJ7wl1eHTbQyLK69wuvjUJ -c --config configs\dimr\dimr_dflowfm_processes_win64.xml --filter "testcase=e02_f030_c001"

rem D-FLOWM FM 1D/2D
rem c:\users\markus\AppData\Local\anaconda3\python.exe TestBench.py --username k2LH7TL14fR41wsjRXId --password WlNJ8jO9eNjltuRxJVFJ7wl1eHTbQyLK69wuvjUJ -c --config configs\dimr\dimr_dflowfm_waq_coupling_1D2D_win64.xml --filter "testcase=e02_f029_c501"
rem c:\users\markus\AppData\Local\anaconda3\python.exe TestBench.py --username k2LH7TL14fR41wsjRXId --password WlNJ8jO9eNjltuRxJVFJ7wl1eHTbQyLK69wuvjUJ -c --config configs\dimr\dimr_dflowfm_waq_coupling_1D2D_win64.xml --filter "testcase=e02_f029_c510"
rem c:\users\markus\AppData\Local\anaconda3\python.exe TestBench.py --username k2LH7TL14fR41wsjRXId --password WlNJ8jO9eNjltuRxJVFJ7wl1eHTbQyLK69wuvjUJ -c --config configs\dimr\dimr_dflowfm_waq_coupling_1D2D_win64.xml --filter "testcase=e02_f029_c516"

rem - D-PART - FM en klassiek
c:\users\markus\AppData\Local\anaconda3\python.exe TestBench.py --username k2LH7TL14fR41wsjRXId --password WlNJ8jO9eNjltuRxJVFJ7wl1eHTbQyLK69wuvjUJ -c --config configs\dimr\dimr_dpart_fm_win64.xml --filter "testcase=e05_f06_fm"
rem c:\users\markus\AppData\Local\anaconda3\python.exe TestBench.py --username k2LH7TL14fR41wsjRXId --password WlNJ8jO9eNjltuRxJVFJ7wl1eHTbQyLK69wuvjUJ -c --config configs\dimr\dimr_dpart_fm_win64.xml
rem c:\users\markus\AppData\Local\anaconda3\python.exe TestBench.py --username k2LH7TL14fR41wsjRXId --password WlNJ8jO9eNjltuRxJVFJ7wl1eHTbQyLK69wuvjUJ -c --config configs\dimr\dimr_dpart_win64.xml --filter "testcase=e05_f03"

rem WAQ-RTC
rem c:\users\markus\AppData\Local\anaconda3\python.exe TestBench.py --username k2LH7TL14fR41wsjRXId --password WlNJ8jO9eNjltuRxJVFJ7wl1eHTbQyLK69wuvjUJ -c --config configs\dimr\dimr_dwaq_drtc_win64.xml

rem WAQ-tools
rem c:\users\markus\AppData\Local\anaconda3\python.exe TestBench.py --username k2LH7TL14fR41wsjRXId --password WlNJ8jO9eNjltuRxJVFJ7wl1eHTbQyLK69wuvjUJ -c --config configs\dwaq_tools_win64.xml

IF %ERRORLEVEL% == 0 GOTO END
echo ERROR: run_testbank_v3.bat: TestBench returns code %ERRORLEVEL%

:END
endlocal
rem pause
