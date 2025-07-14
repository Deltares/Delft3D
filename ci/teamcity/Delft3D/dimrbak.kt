import jetbrains.buildServer.configs.kotlin.*
import jetbrains.buildServer.configs.kotlin.buildSteps.*
import jetbrains.buildServer.configs.kotlin.buildFeatures.*
import jetbrains.buildServer.configs.kotlin.triggers.*

import Delft3D.template.*
import Delft3D.linux.*
import Delft3D.windows.*

object DIMRbak : BuildType({

    templates(
        TemplateMonitorPerformance
    )

    name = "Publish DIMRset"
    buildNumberPattern = "%build.vcs.number%"
    maxRunningBuilds = 1

    features {
        approval {
            approvalRules = "group:DIMR_BAKKERS:1"
        }
    }

    artifactRules = """
        +:ci/DIMRset_delivery/output/*.html
        +:ci/DIMRset_delivery/src/*.xlsx
    """.trimIndent()

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
            snapshot(AbsoluteId("DIMR_To_NGHS")) {
                onDependencyFailure = FailureAction.FAIL_TO_START
                onDependencyCancel = FailureAction.CANCEL
            }
            dependency(LinuxTest) {
                snapshot {
                    onDependencyFailure = FailureAction.FAIL_TO_START
                    onDependencyCancel = FailureAction.CANCEL
                }
            }
            dependency(WindowsTest) {
                snapshot {
                    onDependencyFailure = FailureAction.FAIL_TO_START
                    onDependencyCancel = FailureAction.CANCEL
                }
            }
            dependency(LinuxUnitTest) {
                snapshot {
                    onDependencyFailure = FailureAction.FAIL_TO_START
                    onDependencyCancel = FailureAction.CANCEL
                }
            }
            dependency(WindowsUnitTest) {
                snapshot {
                    onDependencyFailure = FailureAction.FAIL_TO_START
                    onDependencyCancel = FailureAction.CANCEL
                }
            }
            dependency(LinuxRunAllDockerExamples) {
                snapshot {
                    onDependencyFailure = FailureAction.FAIL_TO_START
                    onDependencyCancel = FailureAction.CANCEL
                }
            }
            dependency(LinuxLegacyDockerTest) {
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
        password("dimrbakker_password", "credentialsJSON:43ca5761-31e9-4289-97f3-c060a4007293")
        password("dimrbakker_personal_access_token", "credentialsJSON:8af5f616-4c9b-4f2c-9cd2-b5cc8cc4592d")
    }

    steps {
        python {
            name = "Execute DIMRbakker script"
            command = file {
                filename = "run_dimr_automation.py"
                scriptArguments = """
                    --username "%dimrbakker_username%"
                    --password "%dimrbakker_password%"
                    --git-PAT "%dimrbakker_personal_access_token%"
                """.trimIndent()
            }
            workingDir = "ci/DIMRset_delivery/src"
            environment = venv {
                requirementsFile = "../requirements.txt"
            }
        }
    }
})