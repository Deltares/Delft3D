import jetbrains.buildServer.configs.kotlin.*
import jetbrains.buildServer.configs.kotlin.buildSteps.*
import jetbrains.buildServer.configs.kotlin.buildFeatures.*

import Delft3D.template.*
import PublishToGui

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
        +:ci/python/ci_tools/dimrset_delivery/output/*.html
        +:ci/python/ci_tools/dimrset_delivery/output/*.txt
        +:ci/python/*.xlsx
        +:ci/python/*.txt
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
            dependency(PublishToGui) {
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
        param("dry_run", if (DslContext.getParameter("enable_dimrbak").lowercase() == "true") "" else "--dry-run")
    }

    steps {
        python {
            name = "Assert access rights"
            command = module {
                module = "ci_tools.dimrset_delivery.assert_preconditions"
                scriptArguments = """
                    --build_id "%teamcity.build.id%"
                    --teamcity-username "%dimrbakker_username%"
                    --teamcity-password "%dimrbakker_password%"
                    --jira-username "%dimrbakker_username%"
                    --jira-PAT "%dimrbakker_password%"
                    --ssh-username "%dimrbakker_username%"
                    --ssh-password "%dimrbakker_password%"
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
        }
        python {
            name = "Download artifacts from TeamCity and on file share using H7"
            command = module {
                module = "ci_tools.dimrset_delivery.download_and_install_artifacts"
                scriptArguments = """
                    --build_id "%teamcity.build.id%"
                    --teamcity-username "%dimrbakker_username%"
                    --teamcity-password "%dimrbakker_password%"
                    --ssh-username "%dimrbakker_username%"
                    --ssh-password "%dimrbakker_password%"
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
        python {
            name = "Generate test report summary"
            command = module {
                module = "ci_tools.dimrset_delivery.teamcity_test_results"
                scriptArguments = """
                    --build_id "%teamcity.build.id%"
                    --teamcity-username "%dimrbakker_username%"
                    --teamcity-password "%dimrbakker_password%"
                    %dry_run%
                """.trimIndent()
            }
            workingDir = "ci/python"
            environment = venv {
                requirementsFile = ""
                pipArgs = "--editable .[all]"
            }
        }
        python {
            name = "Update Excel sheet"
            command = module {
                module = "ci_tools.dimrset_delivery.update_excel_sheet"
                scriptArguments = """
                    --build_id "%teamcity.build.id%"
                    --teamcity-username "%dimrbakker_username%"
                    --teamcity-password "%dimrbakker_password%"
                    --ssh-username "%dimrbakker_username%"
                    --ssh-password "%dimrbakker_password%"
                    %dry_run%
                """.trimIndent()
            }
            workingDir = "ci/python"
            environment = venv {
                requirementsFile = ""
                pipArgs = "--editable .[all]"
            }
        }
        python {
            name = "Prepare email template"
            command = module {
                module = "ci_tools.dimrset_delivery.prepare_email"
                scriptArguments = """
                    --build_id "%teamcity.build.id%"
                    --teamcity-username "%dimrbakker_username%"
                    --teamcity-password "%dimrbakker_password%"
                    %dry_run%
                """.trimIndent()
            }
            workingDir = "ci/python"
            environment = venv {
                requirementsFile = ""
                pipArgs = "--editable .[all]"
            }
        }
        python {
            name = "Generate DIMRset release notes"
            command = module {
                module = "ci_tools.dimrset_delivery.publish_release_changelog"
                scriptArguments = """
                    --build_id "%teamcity.build.id%"
                    --jira-username "%dimrbakker_username%"
                    --jira-PAT "%dimrbakker_password%"
                    --git-username "deltares-service-account"
                    --git-PAT "%github_deltares-service-account_access_token%"
                    --ssh-username "%dimrbakker_username%"
                    --ssh-password "%dimrbakker_password%"
                    %dry_run%
                """.trimIndent()
            }
            workingDir = "ci/python/"
            environment = venv {
                requirementsFile = ""
                pipArgs = "--editable .[all]"
            }
        }
    }
})
