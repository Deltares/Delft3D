import jetbrains.buildServer.configs.kotlin.*
import jetbrains.buildServer.configs.kotlin.buildSteps.*
import jetbrains.buildServer.configs.kotlin.buildFeatures.*

import Delft3D.template.*

object PinAndTag : BuildType({

    templates(
        TemplateMonitorPerformance
    )

    name = "Pin and Tag container build"
    buildNumberPattern = "%build.vcs.number%"
    maxRunningBuilds = 1

    features {
        approval {
            approvalRules = "group:DIMR_BAKKERS:1"
        }
    }

    vcs {
        root(DslContext.settingsRoot)
        branchFilter = """
            +:<default>
            +:main
            +:all/release/*
        """.trimIndent()
    }
    
    if (DslContext.getParameter("enable_release_publisher").lowercase() == "true") {
        dependencies {
            dependency(Publish) {
                snapshot {
                    onDependencyFailure = FailureAction.FAIL_TO_START
                    onDependencyCancel = FailureAction.CANCEL
                }
            }
        }
    }

    requirements {
        exists("env.PYTHON_PATH")
        contains("teamcity.agent.jvm.os.name", "Windows")
    }

    params {
        text("release_version", "2.29.xx",
            label = "Release version",
            description = "e.g. '2.29.03' or '2025.02'",
            display = ParameterDisplay.PROMPT)
        param("DIMRset_ver", "%release_version%")
        param("dimrbakker_username", DslContext.getParameter("dimrbakker_username"))
        password("dimrbakker_password", DslContext.getParameter("dimrbakker_password"))
        password("dimrbakker_personal_access_token", DslContext.getParameter("dimrbakker_personal_access_token"))
        param("dry_run", if (DslContext.getParameter("enable_pin_and_tag").lowercase() == "true") "" else "--dry-run")
    }

    steps {
        python {
            name = "Pin and tag builds"
            command = module {
                module = "ci_tools.dimrset_delivery.step_2_pin_and_tag_builds"
                scriptArguments = """ 
                    --build_id "%teamcity.build.id%"
                    --teamcity-username "%dimrbakker_username%"
                    --teamcity-password "%dimrbakker_password%"
                    --git-username "deltares-service-account"
                    --git-PAT "%github_deltares-service-account_access_token%"
                    %dry_run%
                """.trimIndent()
            }
            workingDir = "ci/python"
            environment = venv {
                requirementsFile = ""
                pipArgs = "--editable .[all]"
            }
            executionMode = BuildStep.ExecutionMode.ALWAYS
        }
    }
})
