@ echo off
    rem For local parallel execution on Windows. No special setup is required. The -localonly flag allows MPI to run
    rem without the hydra service.

set dimrdir=\\directory.intra\PROJECT\d-hydro\dimrset\latest


cd fm
call "%dimrdir%\x64\bin\run_dflowfm.bat" "--partition:ndomains=3:icgsolver=6" weirtimeseries.mdu
cd ..

call "%dimrdir%\x64\bin\run_dimr_parallel.bat" 3 dimr_config.xml


    rem To prevent the DOS box from disappearing immediately: remove the rem on the following line
rem pause
