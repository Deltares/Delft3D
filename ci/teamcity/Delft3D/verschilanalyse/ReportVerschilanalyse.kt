package Delft3D.verschilanalyse

import jetbrains.buildServer.configs.kotlin.*
import jetbrains.buildServer.configs.kotlin.buildSteps.*
import jetbrains.buildServer.configs.kotlin.buildFeatures.*
import Delft3D.template.*
import java.io.File

object ReportVerschilanalyse: BuildType({
    name = "Report"
    description = "Report verschilanalyse outcome and send email."
    maxRunningBuilds = 1

    artifactRules = """
        current_logs.zip
        reference_logs.zip
        verschillen.zip
        summaries
    """.trimIndent()

    params {
        param("current_prefix", "output/weekly/latest")
        param("reference_prefix", "output/release/2025.01")
        param("send_email", "true")

        param("env.TEAMCITY_SERVER_URL", DslContext.serverUrl.replace(Regex("/+$"), ""))
        param("env.EMAIL_SERVER", "smtp.directory.intra")
        param("env.EMAIL_PORT", "25")
        param("env.EMAIL_FROM", "black-ops@deltares.nl")
        param("env.EMAIL_TO", "dflowfm-verschilanalyse@deltares.nl")
    }

    vcs {
        root(DslContext.settingsRoot)
        cleanCheckout = true
    }

    steps {
        script {
            name = "Download logs and verschillentool output"
            val script = File(DslContext.baseDir, "verschilanalyse/scripts/download_reports.sh")
            scriptContent = Util.readScript(script)
            dockerImage = "containers.deltares.nl/docker-proxy/amazon/aws-cli:2.32.14"
            dockerImagePlatform = ScriptBuildStep.ImagePlatform.Linux
            dockerRunParameters = """
                --rm
                --entrypoint=/bin/bash
                --volume="%env.AWS_SHARED_CREDENTIALS_FILE%:/root/.aws/credentials:ro"
            """.trimIndent()
        }
        script {
            name = "Unzip logs and verschillentool output"
            val script = File(DslContext.baseDir, "verschilanalyse/scripts/unzip_reports.sh")
            scriptContent = Util.readScript(script)
        }
        python {
            name = "Generate summaries"
            pythonVersion = customPython {
                executable = "python3.11"
            }
            environment = venv {
                requirementsFile = ""
                pipArgs = "--editable ./ci/python[verschilanalyse]"
            }
            command = module {
                module = "ci_tools.verschilanalyse.generate_summaries"
                scriptArguments = """
                    --current-log-dir=current_logs
                    --reference-log-dir=reference_logs
                    --verschillen-dir=verschillen
                    --output-dir=summaries
                    --s3-current-prefix=s3://devops-test-verschilanalyse/%current_prefix%
                    --s3-reference-prefix=s3://devops-test-verschilanalyse/%reference_prefix%
                    --report-build-url=%env.TEAMCITY_SERVER_URL%/buildConfiguration/%system.teamcity.buildType.id%/%teamcity.build.id%
                    --artifact-base-url=%env.TEAMCITY_SERVER_URL%/repository/download/%system.teamcity.buildType.id%/%teamcity.build.id%:id
                """.trimIndent()
            }
        }
        python {
            name = "Send email"
            executionMode = BuildStep.ExecutionMode.RUN_ON_FAILURE
            conditions {
                equals("send_email", "true")
            }
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
                    --email-server=%env.EMAIL_SERVER%
                    --email-port=%env.EMAIL_PORT%
                    --email-from=%env.EMAIL_FROM%
                    --email-to=%env.EMAIL_TO%
                    --email-content=summaries/email_content.html
                    --attachment=summaries/verschillentool_summary.xlsx
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
        dockerRegistryConnections {
            loginToRegistry = on {
                dockerRegistryId = "DOCKER_REGISTRY_DELFT3D"
            }
        }
        xmlReport {
            reportType = XmlReport.XmlReportType.JUNIT
            rules = """
                +:summaries/verschilanalyse_junit.xml
            """.trimIndent()
        }
    }

    requirements {
        contains("teamcity.agent.jvm.os.name", "Linux")
    }
})
