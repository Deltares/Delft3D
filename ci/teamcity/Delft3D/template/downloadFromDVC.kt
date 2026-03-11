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
            name = "Create destination directory"
            scriptContent = "mkdir %engine_dir%"
        }
        script {
            name = "DVC Checkout all doc.dvc files recursively"
            scriptContent = """
                @echo off
                echo === DVC doc checkout started for engine_dir: %engine_dir% ===
                pushd "%engine_dir%"

                for /r %%%%f in (doc.dvc) do (
                    if exist "%%%%f" (
                        echo [DVC] Pulling data for: %%%%f
                        dvc pull "%%%%f"
                    )
                )

                popd
                echo === DVC doc checkout completed ===
            """.trimIndent()
        }
    }
})