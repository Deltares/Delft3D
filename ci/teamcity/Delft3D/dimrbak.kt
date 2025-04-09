import jetbrains.buildServer.configs.kotlin.*
import jetbrains.buildServer.configs.kotlin.buildSteps.*
import jetbrains.buildServer.configs.kotlin.buildFeatures.*

import Delft3D.template.*

object DIMRbak : BuildType({

    templates(
        TemplateMonitorPerformance
    )

    name = "Publish DIMRset"
    buildNumberPattern = "%build.vcs.number%"
    maxRunningBuilds = 1

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

    features {
        approval {
            approvalRules = "group:DIMR_BAKKERS:1"
        }
    }

    dependencies {
        snapshot(AbsoluteId("DIMR_To_NGHS")) {
            onDependencyFailure = FailureAction.FAIL_TO_START
            onDependencyCancel = FailureAction.CANCEL
        }
    }

    requirements {
        exists("env.PYTHON_PATH")
        contains("teamcity.agent.jvm.os.name", "Windows")
    }

    params {
        param("dimrbakker_username", DslContext.getParameter("dimrbakker_username"))
        password("dimrbakker_password", "credentialsJSON:bb9a9cdd-82d2-41d4-b3e8-357d87fcecac")
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