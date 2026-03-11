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
                echo === DVC doc pull started for engine_dir: %engine_dir% ===

                set "BASE_PATH=test\\deltares_testbench\\data\\cases\\%engine_dir%"

                if not exist "%%BASE_PATH%%" (
                    echo [ERROR] Base path not found: %%BASE_PATH%%
                    echo Make sure VCS checkout runs BEFORE this template!
                    exit /b 1
                )

                echo [INFO] Searching for doc.dvc files recursively under %%BASE_PATH%%...
                pushd "%%BASE_PATH%%"

                set "COUNT=0"
                for /r %%%%f in (doc.dvc) do (
                    if exist "%%%%f" (
                        set /a COUNT+=1
                        echo [DVC] Pulling only doc data for: %%f
                        dvc pull "%%%%f"
                    )
                )

                popd
                echo [INFO] Processed %%COUNT%% doc.dvc file(s).
                echo === DVC doc pull completed ===
            """.trimIndent()
        }
    }
})