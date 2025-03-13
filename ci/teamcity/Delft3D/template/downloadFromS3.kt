package Delft3D.template

import jetbrains.buildServer.configs.kotlin.*
import jetbrains.buildServer.configs.kotlin.buildSteps.*
import jetbrains.buildServer.configs.kotlin.buildFeatures.*
import jetbrains.buildServer.configs.kotlin.triggers.*


object TemplateDownloadFromS3 : Template({
    name = "Download based on git commit time the S3 cases"
    description = "Take git commit time and download engines from MinIO bucket."
    buildNumberPattern = "%build.vcs.number%"

    params {
        // Environment variables that are overwritten in the build.
        param("env.TIME_ISO_8601", "")
        param("GIT_HEAD_TIME", "")
    }

    steps {
        script {
            name = "split engine_name_and_dir"
            scriptContent = """call ci/teamcity/Delft3D/windows/scripts/extractEngineNameAndDir.bat %engine_name_and_dir%""".trimIndent()
        }
        script {
            name = "Set time variable step"
            scriptContent = """call ci/teamcity/Delft3D/windows/scripts/setTimeParam.bat""".trimIndent()
        }
        python {
            name = "Checkout Testbench cases from MinIO"
            environment = venv {
                requirementsFile = "ci/teamcity/Delft3D/documentation/scripts/requirements.txt"
            }
            command = file {
                filename = "ci/teamcity/Delft3D/documentation/scripts/download_docs_from_s3.py"
                scriptArguments = "--engine_dir %engine_dir% --iso_time \"%env.TIME_ISO_8601%\""
            }
        }
    }

    features {
        provideAwsCredentials {
            awsConnectionId = "doc_download_connection"
        }
    }
})
