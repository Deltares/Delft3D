package Delft3D.verschilanalyse

import java.io.File
import jetbrains.buildServer.configs.kotlin.*
import jetbrains.buildServer.configs.kotlin.buildSteps.*
import jetbrains.buildServer.configs.kotlin.buildFeatures.*

import Delft3D.verschilanalyse.ReportVerschilanalyse


object StartVerschilanalyse : BuildType({
    name = "Submit"
    description = "Submit verschilanalyse models to H7."
    maxRunningBuilds = 1

    vcs {
        root(DslContext.settingsRoot)
        cleanCheckout = true
    }

    params {
        param("harbor_webhook.image.tag", "latest")
        param(
            "harbor_webhook.image.url", 
            sequenceOf(
                "containers.deltares.nl",
                DslContext.getParameter("va_harbor_project"),
                "${DslContext.getParameter("va_harbor_repository")}:latest"
            ).joinToString(separator="/")
        )         
        param("reference_prefix", "output/release/2025.01")
        checkbox(
            "use_latest_weekly_reference_output",
            "true",
            display = ParameterDisplay.NORMAL,
            label = "Use latest weekly reference output",
            description = "Use the output of the latest successful weekly verschilanalyse as a reference for this verschilanalyse.",
            checked = "true", 
            unchecked = "false",
        )
        param("output_prefix", "output/weekly/latest")
        param("model_filter", "")
    }

    triggers {
        if (DslContext.getParameter("va_harbor_webhook_enabled", "false").lowercase() == "true") {
            // TeamCity webhook plugin docs: https://github.com/tcplugins/tcWebHookTrigger
            // I couldn't find a webhook event payload example in the Harbor documenations,
            // but this GitHub issue comment has an example:
            // https://github.com/keel-hq/keel/issues/510#issuecomment-647014097
            trigger {
                type = "webhookBuildTrigger"
                param("webhook.build.trigger.path.mappings", """
                    name=harbor_webhook.type::path=${'$'}.type::required=true
                    name=harbor_webhook.image.digest::path=${'$'}.event_data.resources[0].digest::required=true
                    name=harbor_webhook.image.tag::path=${'$'}.event_data.resources[0].tag::required=true
                    name=harbor_webhook.image.url::path=${'$'}.event_data.resources[0].resource_url::required=true
                    name=harbor_webhook.repository::path=${'$'}.event_data.repository.name::required=true
                    name=harbor_webhook.project::path=${'$'}.event_data.repository.namespace::required=true
                """.trimIndent())
                param("webhook.build.trigger.path.filters", """
                    name=harbor_webhook.type::template=${'$'}{harbor_webhook.type}::regex=PUSH_ARTIFACT
                    name=harbor_webhook.project::template=${'$'}{harbor_webhook.project}::regex=${DslContext.getParameter("va_harbor_project")}
                    name=harbor_webhook.repository::template=${'$'}{harbor_webhook.repository}::regex=${DslContext.getParameter("va_harbor_repository")}
                    name=harbor_webhook.image.tag::template=${'$'}{harbor_webhook.image.tag}::regex=${DslContext.getParameter("va_harbor_webhook_image_tag_regex")}
                    name=output_prefix::template=output/weekly/${'$'}{harbor_webhook.image.tag}::regex=output/weekly/${DslContext.getParameter("va_harbor_webhook_image_tag_regex")}
                """.trimIndent())
                param("webhook.build.trigger.include.payload", "true")
            }
        }
    }

    steps {
        python {
            name = "Update references to the for the latest successful weekly verschilanalyse"
            conditions { 
                equals("use_latest_weekly_reference_output", "true")
            }
            workingDir = "ci/teamcity/Delft3D/verschilanalyse/scripts"
            pythonVersion = customPython {
                executable = "python3.11"
            }
            environment = venv {
                requirementsFile = "requirements.txt"
            }
            command = file {
                filename = "find_latest_weekly_output.py"
            }
        }
        sshUpload { 
            name = "Upload bundle"
            transportProtocol = SSHUpload.TransportProtocol.SCP
            sourcePath = """
                ci/teamcity/Delft3D/verschilanalyse/bundle => bundle.tar.gz
            """.trimIndent()
            targetUrl = "h7.directory.intra"
            authMethod = password {
                username = "%h7_account_username%"
                password = "%h7_account_password%"
            }
        }
        sshExec {
            name = "Schedule verschilanalyse run"
            commands = """
                set -eo pipefail

                rm -rf bundle
                mkdir bundle
                tar -xzvf bundle.tar.gz -C bundle
                rm -f bundle.tar.gz

                export TEAMCITY_SERVER_URL='${DslContext.serverUrl.replace(Regex("/+$"), "")}'
                export VCS_ROOT_ID='${DslContext.settingsRoot.id}'
                export VCS_REVISION='%build.vcs.number%'
                export REPORT_BUILD_TYPE_ID='${ReportVerschilanalyse.id}'
                export START_BUILD_TYPE_ID='${StartVerschilanalyse.id}'
                export BUILD_ID='%teamcity.build.id%'
                export BRANCH_NAME='%teamcity.build.branch%'

                pushd bundle
                ./start_verschilanalyse.sh \
                    --apptainer='oras://%harbor_webhook.image.url%' \
                    --reference-prefix='%reference_prefix%' \
                    --output-prefix='%output_prefix%' \
                    --model-filter='%model_filter%'
                popd
            """.trimIndent()
            targetUrl = "h7.directory.intra"
            authMethod = password {
                username = "%h7_account_username%"
                password = "%h7_account_password%"
            }
        }
    }

    features {
        swabra {}
        provideAwsCredentials {
            awsConnectionId = "minio_verschilanalyse_connection"
        }
    }

    requirements {
        contains("teamcity.agent.jvm.os.name", "Linux")
    }
})