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
        timeout-report/**
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
        param("last_n", "100")
        param("top_n", "40")
        param("email_to", "robin.vanwestrenen@deltares.nl")
        checkbox(
            "send_email",
            "true",
            label = "Send email",
            checked = "true",
            unchecked = "false"
        )
        param("env.EMAIL_SERVER", "smtp.directory.intra")
        param("env.EMAIL_PORT", "25")
        param("env.EMAIL_FROM", "black-ops@deltares.nl")
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
                uv run python -m ci_tools.testbench_timeout_report \
                    --server "%env.TEAMCITY_SERVER_URL%" \
                    --last-n "%last_n%" \
                    --top-n "%top_n%" \
                    --configs-root "%teamcity.build.checkoutDir%/test/deltares_testbench/configs" \
                    --csv "%teamcity.build.checkoutDir%/ci/teamcity/Delft3D/vars/dimr_testbench_table.csv" \
                    --output-dir "%teamcity.build.checkoutDir%/timeout-report" \
                    --report-url "%env.TEAMCITY_SERVER_URL%/buildConfiguration/%system.teamcity.buildType.id%/%teamcity.build.id%"
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
        script {
            name = "Send email"
            executionMode = BuildStep.ExecutionMode.ALWAYS
            conditions { equals("send_email", "true") }
            workingDir = "ci/python"
            scriptContent = """
                #!/usr/bin/env bash
                set -euo pipefail
                uv run python -m ci_tools.testbench_timeout_report.send_email \
                    --email-server "%env.EMAIL_SERVER%" \
                    --email-port "%env.EMAIL_PORT%" \
                    --email-from "%env.EMAIL_FROM%" \
                    --email-to "%email_to%" \
                    --email-content "%teamcity.build.checkoutDir%/timeout-report/email.html"
            """.trimIndent()
            dockerImage = "%docker_image%"
            dockerImagePlatform = ScriptBuildStep.ImagePlatform.Linux
            dockerPull = true
            dockerRunParameters = """
                --mount type=volume,source=uv-cache-python-ci-tools,destination=/root/.cache/uv
                --env UV_LINK_MODE=copy
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
