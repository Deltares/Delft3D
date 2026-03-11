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

                if not exist "%%BASE_PATH%%" (
                    echo [ERROR] Base path not found: %%BASE_PATH%%
                    echo Make sure VCS checkout runs BEFORE this template!
                    exit /b 1
                )

                echo [INFO] Collecting all doc.dvc files recursively...

                powershell -NoProfile -ExecutionPolicy Bypass -Command ^
                    "$base = '%%BASE_PATH%%'; " ^
                    "$dvc = '%%DVC_EXE%%'; " ^
                    "$files = Get-ChildItem -Path $base -Filter 'doc.dvc' -Recurse -File | Select-Object -ExpandProperty FullName; " ^
                    "if ($files) { " ^
                    "  Write-Host '[INFO] Pulling' $files.Count 'doc.dvc files in ONE command...'; " ^
                    "  & $dvc pull $files " ^
                    "} else { " ^
                    "  Write-Host '[WARN] No doc.dvc files found!' " ^
                    "}"
            """.trimIndent()
        }
    }
})