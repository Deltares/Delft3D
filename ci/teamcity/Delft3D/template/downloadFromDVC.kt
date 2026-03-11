package Delft3D.template

import jetbrains.buildServer.configs.kotlin.*
import jetbrains.buildServer.configs.kotlin.buildSteps.*
import jetbrains.buildServer.configs.kotlin.buildFeatures.*
import jetbrains.buildServer.configs.kotlin.triggers.*

object TemplateDownloadFromDVC : Template({
    name = "Download doc folders."
    description = "Download all doc folders via doc.dvc files, recursively for engine dir."
    buildNumberPattern = "%build.vcs.number%"

    params {
        param("env.AWS_ACCESS_KEY_ID", DslContext.getParameter("s3_dsctestbench_accesskey"))
        password("env.AWS_SECRET_ACCESS_KEY", DslContext.getParameter("s3_dsctestbench_secret"))
    }

    steps {
        script {
            name = "split engine_name_and_dir"
            scriptContent = "call ci/teamcity/Delft3D/windows/scripts/extractEngineNameAndDir.bat %engine_name_and_dir%"
        }
        script {
            name = "Install DVC locally (no system-wide)"
            scriptContent = """
                @echo off
                echo === Installing DVC in isolated venv ===

                if exist ".dvc-venv" rmdir /s /q ".dvc-venv"

                python -m venv .dvc-venv
                call .dvc-venv\\Scripts\\activate.bat

                python -m pip install --upgrade pip
                python -m pip install "dvc[s3]"
                python -m pip install --editable ./ci/python --force-reinstall

                echo === DVC installed successfully ===
                dvc --version
            """.trimIndent()
        }
        script {
            name = "DVC Pull all doc.dvc files recursively"
            scriptContent = """
                @echo off
                echo === DVC doc pull started for engine_dir: %engine_dir% ===

                set "BASE_PATH=test\\deltares_testbench\\data\\cases\\%engine_dir%"
                set "DVC_EXE=%%cd%%\\.dvc-venv\\Scripts\\dvc.exe"
                set "TEMP_LIST=%%TEMP%%\\dvc_doc_%%RANDOM%%.txt"

                if not exist "%%BASE_PATH%%" (
                    echo [ERROR] Base path not found: %%BASE_PATH%%
                    exit /b 1
                )

                pushd "%%BASE_PATH%%"

                echo [INFO] 1/2 Pulling root doc.dvc ...
                "%%DVC_EXE%%" pull doc.dvc

                echo [INFO] 2/2 Collecting and pulling f[0-9]* doc.dvc files in batches of 100 (to reduce memory usage) ...

                setlocal EnableDelayedExpansion
                set "BATCH="
                set "COUNT=0"
                set "TOTAL=0"

                rem === Safe collection using temp file ===
                dir /s /b doc.dvc 2^>nul ^| findstr /i "\\f[0-9]" > "!TEMP_LIST!" 2^>nul

                if exist "!TEMP_LIST!" (
                    for /f "delims=" %%%%a in ('type "!TEMP_LIST!"') do (
                        set /a TOTAL+=1
                        set /a COUNT+=1
                        set "BATCH=!BATCH! "%%%%a""

                        if !COUNT! equ 100 (
                            echo [BATCH] Pulling next 100 files (total: !TOTAL!)...
                            "%%DVC_EXE%%" pull !BATCH!
                            set "BATCH="
                            set "COUNT=0"
                        )
                    )
                    if not "!BATCH!"=="" (
                        echo [BATCH] Pulling remaining !COUNT! files (total: !TOTAL!)...
                        "%%DVC_EXE%%" pull !BATCH!
                    )
                    del "!TEMP_LIST!"
                ) else (
                    echo [INFO] No f[0-9]* doc.dvc files found.
                )

                echo [INFO] Total f[0-9]* doc.dvc files processed: !TOTAL!

                endlocal
                popd
                echo === DVC doc pull completed ===
            """.trimIndent()
        }
    }
})