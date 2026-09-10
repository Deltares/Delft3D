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
        // DVC remote credentials (s3://delft3d-testbench)
        param("env.AWS_ACCESS_KEY_ID", "%dvc_testbench_accesskey%")
        password("env.AWS_SECRET_ACCESS_KEY", "%dvc_testbench_secret%")
        // Avoid IMDS hangs on non-EC2 Windows agents
        param("env.AWS_EC2_METADATA_DISABLED", "true")
    }

    steps {
        script {
            name = "split engine_name_and_dir"
            scriptContent = "call ci/teamcity/Delft3D/windows/scripts/extractEngineNameAndDir.bat %engine_name_and_dir%"
        }
        // Build-side install: TeamCity creates a venv and pip-installs dvc + dvc-s3
        // so agents do not need a preinstalled dvc on PATH.
        python {
            name = "DVC Pull all doc.dvc files recursively"
            environment = venv {
                requirementsFile = "ci/teamcity/Delft3D/windows/scripts/dvc-docs-requirements.txt"
            }
            command = file {
                filename = "ci/teamcity/Delft3D/windows/scripts/pull_docs_for_engine.py"
                scriptArguments = "--engine-dir %engine_dir%"
            }
        }
    }
})
