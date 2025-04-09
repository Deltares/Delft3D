package Delft3D.verschilanalyse

import jetbrains.buildServer.configs.kotlin.*
import jetbrains.buildServer.configs.kotlin.buildSteps.*
import jetbrains.buildServer.configs.kotlin.buildFeatures.*

object ReportVerschilanalyse: BuildType({
    name = "Report"
    description = "Report verschilanalyse outcome and send email."
    maxRunningBuilds = 1

    artifactRules = """
        report.zip
    """.trimIndent()

    params {
        param("report_prefix", "output/weekly/latest/report")

        param("env.TEAMCITY_SERVER_URL", DslContext.serverUrl)
        param("env.EMAIL_FROM", "black-ops@deltares.nl")
        param("env.EMAIL_SERVER", "smtp.directory.intra")
        param("env.EMAIL_PORT", "25")
        param("env.EMAIL_RECIPIENTS", "black-ops@deltares.nl")
    }

    vcs {
        root(DslContext.settingsRoot)
        cleanCheckout = true
    }

    steps {
        script {
            name = "Download Verschillentool report"
            scriptContent = """
                aws --endpoint-url=https://s3.deltares.nl \
                    s3 cp s3://devops-test-verschilanalyse/%report_prefix%/report.zip report.zip
            """.trimIndent()
            dockerImage = "amazon/aws-cli:2.22.7"
            dockerImagePlatform = ScriptBuildStep.ImagePlatform.Linux
            dockerRunParameters = """
                --rm
                --entrypoint=/bin/bash
                --volume="%env.AWS_SHARED_CREDENTIALS_FILE%:/root/.aws/credentials:ro"
            """.trimIndent()
        }
        script {
            name = "Unzip report"
            scriptContent = "unzip -d report ./report.zip"
        }
        python {
            id = "generate_summary"
            name = "Generate summary"
            pythonVersion = customPython {
                executable = "python3.11"
            }
            environment = venv {
                requirementsFile = ""
                pipArgs = "--editable ./ci/python[verschilanalyse]"
            }
            command = module {
                module = "ci_tools.verschilanalyse.summarize_verschillentool_output"
                scriptArguments = """
                    --verschillentool-output-dir=./report/verschillentool_output
                    --output-dir=./report
                """.trimIndent()
            }
        }
        python {
            name = "Send email"
            executionMode = BuildStep.ExecutionMode.RUN_ON_FAILURE
            pythonVersion = customPython {
                executable = "python3.11"
            }
            environment = venv {
                requirementsFile = ""
                pipArgs = "--editable ./ci/python[verschilanalyse]"
            }
            command = module {
                module = "ci_tools.verschilanalyse.send_verschilanalyse_email"
                scriptArguments = """
                    --build-id=%teamcity.build.id%
                    --status=%teamcity.build.step.status.generate_summary%
                    --teamcity-server-url=%env.TEAMCITY_SERVER_URL%
                    --build-type-id=%system.teamcity.buildType.id%
                    --email-from=%env.EMAIL_FROM%
                    --email-server=%env.EMAIL_SERVER%
                    --email-port=%env.EMAIL_PORT%
                    --email-recipients=%env.EMAIL_RECIPIENTS%
                """.trimIndent()
            }
        }
    }

    dependencies {
        snapshot(StartVerschilanalyse) {}
    }

    features {
        perfmon {}
        swabra {}
        provideAwsCredentials {
            awsConnectionId = "minio_verschilanalyse_connection"
        }
        dockerSupport {
            loginToRegistry = on {
                dockerRegistryId = "PROJECT_EXT_133,PROJECT_EXT_81"
            }
        }
    }

    requirements {
        contains("teamcity.agent.jvm.os.name", "Linux")
    }
})