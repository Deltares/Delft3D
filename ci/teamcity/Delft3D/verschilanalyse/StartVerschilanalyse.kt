package Delft3D.verschilanalyse

import java.io.File
import jetbrains.buildServer.configs.kotlin.*
import jetbrains.buildServer.configs.kotlin.buildSteps.*
import jetbrains.buildServer.configs.kotlin.buildFeatures.*
import jetbrains.buildServer.configs.kotlin.triggers.finishBuildTrigger

import Delft3D.verschilanalyse.ReportVerschilanalyse


object StartVerschilanalyse : BuildType({
    name = "Submit"
    description = "Submit verschilanalyse models to H7."
    maxRunningBuilds = 1

    vcs {
        root(DslContext.settingsRoot)
        cleanCheckout = true
    }

    if (DslContext.getParameter("enable_verschilanalyse_trigger").lowercase() == "true") {
        triggers {
            finishBuildTrigger {
                buildType = "${PinAndTag.id}"
                successfulOnly = true
                // The default branch is triggered manually via a DIMRBakker PinAndTag build
                //   and this results to this VA output to be automatically compared to the latest release candidate.
                // Release branches should not get triggered by the DIMRBakker build chain because 
                //   we don't want to do automatic comparisons to the latest release candidate.
                // When we have to do special VA tool runs, for example comparing release branches within each other, 
                //   we should not use this DIMRBakker configured finishBuildTrigger. 
                branchFilter = """
                    +:<default>
                    -:all/release/*
                """.trimIndent()
            }
        }
    }   

    params {
        text("va_harbor_protocol", "docker", display = ParameterDisplay.PROMPT)
        text("reference_prefix", "output/release/2025.01", display = ParameterDisplay.PROMPT)
        checkbox(
            "use_latest_weekly_reference_output",
            "true",
            display = ParameterDisplay.PROMPT,
            label = "Use latest weekly reference output",
            description = "Use the output of the latest successful weekly verschilanalyse as a reference for this verschilanalyse.",
            checked = "true", 
            unchecked = "false",
        )
        text("current_prefix", "output/weekly/development", display = ParameterDisplay.PROMPT)
        text("models_path", "input", display = ParameterDisplay.PROMPT)
        text("model_filter", "", display = ParameterDisplay.PROMPT)
        text("json_configs_path", "config", display = ParameterDisplay.PROMPT)
        checkbox(
            "run_models",
            "true",
            display = ParameterDisplay.PROMPT,
            label = "Run models on H7",
            description = "Run models on Slurm before running Verschillentool. Disable to reuse existing output at current_prefix.",
            checked = "true",
            unchecked = "false",
        )
        checkbox(
            "send_email",
            "true",
            display = ParameterDisplay.PROMPT,
            label = "Send email report",
            description = "Send email with verschilanalyse results after completion.",
            checked = "true", 
            unchecked = "false",
        )
    }

    steps {
        python {
            name = "Use the latest weekly verschilanalyse output as reference"
            conditions { 
                equals("use_latest_weekly_reference_output", "true")
            }
            pythonVersion = customPython {
                executable = "python3.11"
            }
            environment = venv {
                requirementsFile = ""
                pipArgs = "--editable ./ci/python[verschilanalyse]"
            }
            command = module {
                module = "ci_tools.verschilanalyse.find_latest_weekly_output"
            }
        }
        sshUpload { 
            name = "Upload bundle"
            transportProtocol = SSHUpload.TransportProtocol.SCP
            sourcePath = """
                ci/teamcity/Delft3D/verschilanalyse/bundle => bundle-%teamcity.build.id%.tar.gz
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

                export TEAMCITY_SERVER_URL='${DslContext.serverUrl.replace(Regex("/+$"), "")}'
                export VCS_ROOT_ID='${DslContext.settingsRoot.id}'
                export VCS_REVISION='%build.vcs.number%'
                export REPORT_BUILD_TYPE_ID='${ReportVerschilanalyse.id}'
                export START_BUILD_TYPE_ID='${StartVerschilanalyse.id}'
                export BUILD_ID='%teamcity.build.id%'
                export BRANCH_NAME='%teamcity.build.branch%'
                export SEND_EMAIL='%send_email%'
                export RUN_MODELS='%run_models%'

                # Create the builds dir if it does not exist
                builds_dir="/p/devops-dsc/verschilanalyse/builds"
                mkdir -p "${'$'}{builds_dir}"
                # remove old build directories to clear space
                find "${'$'}{builds_dir}" -mindepth 1 -maxdepth 1 -type d -mtime +7 -execdir rm -rf {} +

                # Create new build directory
                va_home="${'$'}{builds_dir}/%teamcity.build.id%"
                mkdir -p "${'$'}{va_home}"

                # Extract the bundle to the build dir
                bundle_dir="${'$'}{va_home}/bundle"
                echo "bundle dir: ${'$'}{bundle_dir}"
                rm -rf "${'$'}{bundle_dir}"
                mkdir "${'$'}{bundle_dir}"
                tar -xzvf bundle-%teamcity.build.id%.tar.gz -C "${'$'}{bundle_dir}"
                rm -f bundle-%teamcity.build.id%.tar.gz

                # start the VA, use dep. variables from the upstream Publish build to
                # define what apptainer image we should use. 
                # The above PinAndTag finishBuildTrigger depends on the Publish build
                pushd "${'$'}{bundle_dir}"
                echo ./start_verschilanalyse.sh \
                    --apptainer='%va_harbor_protocol%://%dep.${Publish.id}.destination_image_specific%' \
                    --current-prefix='%current_prefix%' \
                    --reference-prefix='%reference_prefix%' \
                    --models-path='%models_path%' \
                    --model-filter='%model_filter%' \
                    --json-configs-path='%json_configs_path%' \
                    --run-models='%run_models%' \
                    --va-home="${'$'}{va_home}"
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
    }

    requirements {
        contains("teamcity.agent.jvm.os.name", "Linux")
    }
})
