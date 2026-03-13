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
                    echo ##teamcity[buildProblem description='DVC base path not found: %%BASE_PATH%%' identity='dvc_base_path_missing']
                    exit /b 1
                )

                pushd "%%BASE_PATH%%"

                echo [INFO] Pulling ALL feature doc.dvc files in batches of 100 (to limit memory use)...

                set "BATCH="
                set "COUNT=0"
                set "TOTAL_DETECTED=0"
                set "BATCH_COUNT=0"

                echo [DETECTION START] Listing every doc.dvc the script sees before any pull:
                for /r %%%%a in (doc.dvc) do (
                    echo "%%%%~a" ^| findstr /i "f[0-9]" ^>nul
                    if not errorlevel 1 (
                        echo [DETECTED] %%%%~a
                        set /a TOTAL_DETECTED+=1
                        set /a COUNT+=1
                        set "BATCH=!BATCH! "%%%%~a""

                        if !COUNT! equ 100 (
                            set /a BATCH_COUNT+=1
                            echo [BATCH !BATCH_COUNT!] Pulling next 100 doc.dvc files...
                            dvc pull !BATCH! || call :report_failure "batch !BATCH_COUNT!"
                            echo [PULL OK] Batch !BATCH_COUNT! completed successfully
                            set "BATCH="
                            set "COUNT=0"
                        )
                    )
                )

                if not "!BATCH!"=="" (
                    set /a BATCH_COUNT+=1
                    echo [BATCH !BATCH_COUNT!] Pulling remaining files...
                    dvc pull !BATCH! || call :report_failure "final batch !BATCH_COUNT!"
                    echo [PULL OK] Final batch !BATCH_COUNT! completed successfully
                )

                echo [DETECTION END] Total detected: !TOTAL_DETECTED! doc.dvc files

                echo [VERIFICATION START] Final agent check: confirming every detected doc.dvc was actually pulled (doc folder now exists on disk)...
                set "VERIFIED_COUNT=0"
                set "MISSING_COUNT=0"
                for /r %%%%a in (doc.dvc) do (
                    echo "%%%%~a" ^| findstr /i "f[0-9]" ^>nul
                    if not errorlevel 1 (
                        echo "%%%%~a" ^| findstr /i "\\doc\\doc\.dvc" ^>nul
                        if errorlevel 1 (
                            set "DOC_DIR=%%%%~dpa"
                            if exist "!DOC_DIR!doc\\" (
                                echo [VERIFIED] %%%%~a → doc folder materialized
                                set /a VERIFIED_COUNT+=1
                            ) else (
                                echo [MISSING] %%%%~a → doc folder NOT materialized
                                set /a MISSING_COUNT+=1
                            )
                        )
                    )
                )
                echo [VERIFICATION END] Verified: !VERIFIED_COUNT!   Missing: !MISSING_COUNT!

                endlocal
                popd
                echo === DVC doc pull completed ===

                if !MISSING_COUNT! gtr 0 (
                    echo [ERROR] !MISSING_COUNT! doc folders failed to materialize for !ENGINE_DIR!
                    echo ##teamcity[buildProblem description='!MISSING_COUNT! doc folders failed to materialize !ENGINE_DIR!' identity='dvc_missing_folders']
                    exit /b 1
                )

                goto :eof

                :report_failure
                echo [ERROR] Failed to pull %%~1
                echo ##teamcity[buildProblem description='DVC pull failed: %%~1 !ENGINE_DIR!' identity='dvc_pull_%%~1_!ENGINE_DIR!']
                exit /b 1
            """.trimIndent()
        }
    }
})