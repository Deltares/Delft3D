package Delft3D.verschilanalyse

import jetbrains.buildServer.configs.kotlin.*
import jetbrains.buildServer.configs.kotlin.buildSteps.*
import jetbrains.buildServer.configs.kotlin.buildFeatures.*

object ReportVerschilanalyse: BuildType({
    name = "Report verschilanalyse"

    artifactRules = """
        report => report
    """.trimIndent()

    params {
        param("report_prefix", "output/weekly/latest/report")
    }

    vcs {
        root(DslContext.settingsRoot)
    }

    steps {
        script {
            name = "Download Verschillentool report"
            scriptContent = """
                set -eo pipefail
                mkdir report
                docker run --rm \
                    --volume "${'$'}{AWS_SHARED_CREDENTIALS_FILE}:/root/.aws/credentials:ro" \
                    --volume "${'$'}(pwd)/report/:/report/" docker.io/amazon/aws-cli:2.22.7 \
                    --endpoint-url=https://s3.deltares.nl \
                    s3 sync s3://devops-test-verschilanalyse/%report_prefix% /report/
            """.trimIndent()
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
    }

    requirements {
        contains("teamcity.agent.jvm.os.name", "Linux")
    }
})