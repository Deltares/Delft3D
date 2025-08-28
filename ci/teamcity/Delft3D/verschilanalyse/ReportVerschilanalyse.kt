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
            dockerImage = "containers.deltares.nl/docker-proxy/amazon/aws-cli:2.22.7"
            dockerImagePlatform = ScriptBuildStep.ImagePlatform.Linux
            dockerRunParameters = """
                --rm
                --entrypoint=/bin/bash
                --volume="%env.AWS_SHARED_CREDENTIALS_FILE%:/root/.aws/credentials:ro"
            """.trimIndent()
        }
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
    }

    requirements {
        contains("teamcity.agent.jvm.os.name", "Linux")
    }
})
