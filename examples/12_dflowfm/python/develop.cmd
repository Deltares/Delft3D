rem make sure we have python modules installed

cd %~dp0\..\utils_gpl\OpenEarthTools
python setup.py develop

cd %~dp0\dflowfm
python setup.py develop

cd %~dp0\dflowfm_merge
python setup.py develop

rem cd %~dp0\dflowfm_merge
rem python setup.py develop
