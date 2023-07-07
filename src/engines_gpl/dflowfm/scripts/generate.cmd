set SAVEDIR=%CD%
cd ..\packages\dflowfm_lib\include\
python ..\..\..\scripts\template\generate.py --fortranfiles-dir=..\..\..\scripts\fortranfiles.txt --template-dir=..\..\dflowfm_lib\templates\ --verbose
cd %SAVEDIR%
