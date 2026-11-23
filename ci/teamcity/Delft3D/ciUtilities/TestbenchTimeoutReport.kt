package Delft3D.ciUtilities

import jetbrains.buildServer.configs.kotlin.*
import jetbrains.buildServer.configs.kotlin.buildSteps.*
import jetbrains.buildServer.configs.kotlin.triggers.*

import Delft3D.template.*


object TestbenchTimeoutReport : BuildType({
    id("TestbenchTimeoutReport")
    name = "TestBench timeout report"
    buildNumberPattern = "%build.vcs.number%"
    description = """
        Report TestBench case durations versus XML maxRunTime on the latest default branch.
        Not part of the DIMRset release pipeline. Publishes boxplots and a table ordered by closeness to timeout.
    """.trimIndent()

    artifactRules = """
        timeout-report/** => timeout-report
    """.trimIndent()

    templates(
        TemplateDockerRegistry,
        TemplateBuildConcurrency
    )

    vcs {
        root(DslContext.settingsRoot)
        cleanCheckout = true
    }

    params {
        param("docker_image", "containers.deltares.nl/delft3d-dev/delft3d-python:alma8-python3.12")
        param("email_to", "black-ops@deltares.nl,Julien.Groenenboom@deltares.nl")
        checkbox(
            "send_email",
            "true",
            label = "Send email",
            checked = "true",
            unchecked = "false"
        )
        param("env.TEAMCITY_SERVER_URL", DslContext.serverUrl.replace(Regex("/+$"), ""))
        param("teamcity_user", DslContext.getParameter("teamcity_user"))
        password("teamcity_pass", DslContext.getParameter("teamcity_pass"))
    }

    if (DslContext.getParameter("enable_testbench_timeout_report_trigger").lowercase() == "true") {
        triggers {
            // Cadence only: collect default-branch Test history. Not a DIMRset weekly/release job.
            schedule {
                schedulingPolicy = weekly {
                    dayOfWeek = ScheduleTrigger.DAY.Monday
                    hour = 6
                }
                branchFilter = "+:<default>"
                triggerBuild = always()
                withPendingChangesOnly = false
            }
        }
    }

    steps {
        script {
            name = "Generate timeout report"
            workingDir = "ci/python"
            scriptContent = """
                #!/usr/bin/env bash
                set -euo pipefail
                uv sync --extra=testbench_timeout_report
                email_args=()
                if [ "%send_email%" = "true" ]; then
                  email_args+=(--email-to "%email_to%")
                fi
                uv run python -m ci_tools.testbench_timeout_report \
                    --server "%env.TEAMCITY_SERVER_URL%" \
                    --configs-root "%teamcity.build.checkoutDir%/test/deltares_testbench/configs" \
                    --csv "%teamcity.build.checkoutDir%/ci/teamcity/Delft3D/vars/dimr_testbench_table.csv" \
                    --output-dir "%teamcity.build.checkoutDir%/timeout-report" \
                    --report-url "%env.TEAMCITY_SERVER_URL%/repository/download/%system.teamcity.buildType.id%/%teamcity.build.id%:id/timeout-report/report.html" \
                    "${'$'}{email_args[@]}"
            """.trimIndent()
            dockerImage = "%docker_image%"
            dockerImagePlatform = ScriptBuildStep.ImagePlatform.Linux
            dockerPull = true
            dockerRunParameters = """
                --mount type=volume,source=uv-cache-python-ci-tools,destination=/root/.cache/uv
                --env UV_LINK_MODE=copy
                --env TEAMCITY_USERNAME=%teamcity_user%
                --env TEAMCITY_PASSWORD=%teamcity_pass%
                --rm
            """.trimIndent()
        }
    }

    failureConditions {
        executionTimeoutMin = 60
    }

    requirements {
        contains("teamcity.agent.jvm.os.name", "Linux")
    }
})
