@echo off
REM List all subfolders in the current directory
for /d %%D in (*) do (
    echo Checking folder: %%D
    cd %%D
    if exist run.bat (
        echo "##teamcity[testStarted name='%%D' captureStandardOutput='true']"
        call run.bat
        echo "##teamcity[testFinished name='%%D']"

    ) else (
        echo No run script in %%D
    )
    cd ..
)
