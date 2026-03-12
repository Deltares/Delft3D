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
            name = "DVC Pull all doc.dvc files recursively"
            scriptContent = """
                @echo off
                setlocal EnableDelayedExpansion

                echo === DVC doc pull started for engine_dir: %engine_dir% ===

                set "BASE_PATH=test\\deltares_testbench\\data\\cases\\%engine_dir%"
                set "ENGINE_DIR=%engine_dir%"

                if not exist "%%BASE_PATH%%" (
                    echo [ERROR] Base path not found: %%BASE_PATH%%
                    echo ##teamcity[buildProblem description="DVC base path not found: %%BASE_PATH%%" identity="dvc_base_path_missing"]
                    exit /b 1
                )

                pushd "%%BASE_PATH%%"

                echo [INFO] 1/2 Pulling root doc.dvc ...
                dvc pull doc.dvc || (
                    echo [ERROR] Failed to pull root doc.dvc
                    echo ##teamcity[buildProblem description="DVC pull failed: root doc.dvc (!ENGINE_DIR!)" identity="dvc_pull_root_!ENGINE_DIR!"]
                )

                echo [INFO] 2/2 Pulling ONLY feature doc.dvc files in batches of 100 (to limit memory usage)...

                set "BATCH="
                set "COUNT=0"
                set "BATCH_COUNT=0"

                for /f "delims=" %%%%a in ('dir /s /b doc.dvc 2^>nul') do (
                    echo "%%%%a" | findstr /i "\\f[0-9]" >nul
                    if not errorlevel 1 (
                        set /a COUNT+=1
                        set "BATCH=!BATCH! "%%%%a""

                        if !COUNT! equ 100 (
                            set /a BATCH_COUNT+=1
                            echo [BATCH !BATCH_COUNT!] Pulling next 100 doc.dvc files...
                            dvc pull !BATCH! || (
                                echo [ERROR] Failed to pull batch !BATCH_COUNT!
                                echo ##teamcity[buildProblem description="DVC pull failed: batch !BATCH_COUNT! (!ENGINE_DIR!)" identity="dvc_batch_!BATCH_COUNT!_!ENGINE_DIR!"]
                            )
                            set "BATCH="
                            set "COUNT=0"
                        )
                    )
                )

                if not "!BATCH!"=="" (
                    set /a BATCH_COUNT+=1
                    echo [BATCH !BATCH_COUNT!] Pulling remaining files...
                    dvc pull !BATCH! || (
                        echo [ERROR] Failed to pull final batch !BATCH_COUNT!
                        echo ##teamcity[buildProblem description="DVC pull failed: final batch !BATCH_COUNT! (!ENGINE_DIR!)" identity="dvc_batch_final_!ENGINE_DIR!"]
                    )
                )

                endlocal
                popd
                echo === DVC doc pull completed ===
            """.trimIndent()
        }
    }
})