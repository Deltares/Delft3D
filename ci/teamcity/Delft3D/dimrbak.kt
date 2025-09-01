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
        password("dimrbakker_password", "credentialsJSON:43ca5761-31e9-4289-97f3-c060a4007293")
        password("dimrbakker_personal_access_token", "credentialsJSON:8af5f616-4c9b-4f2c-9cd2-b5cc8cc4592d")
        param("dry_run", if (DslContext.getParameter("enable_dimrbak").lowercase() == "true") "" else "--dry-run")
    }

    steps {
        python {
            name = "Assert access rights"
            command = module {
                module = "ci_tools.dimrset_delivery.step_0_assert_preconditions"
                scriptArguments = """
                    --build_id "%teamcity.build.id%"
                    --atlassian-username "%dimrbakker_username%"
                    --atlassian-password "%dimrbakker_password%"
                    --teamcity-username "%dimrbakker_username%"
                    --teamcity-password "%dimrbakker_password%"
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
                module = "ci_tools.dimrset_delivery.step_1_download_and_install_artifacts"
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
        python {
            name = "Generate test report summary"
            command = module {
                module = "ci_tools.dimrset_delivery.step_3_teamcity_test_results"
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
                module = "ci_tools.dimrset_delivery.step_4_update_excel_sheet"
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
                module = "ci_tools.dimrset_delivery.step_5_prepare_email"
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
            name = "Update public wiki"
            command = module {
                module = "ci_tools.dimrset_delivery.step_6_update_public_wiki"
                scriptArguments = """
                    --build_id "%teamcity.build.id%"
                    --atlassian-username "%dimrbakker_username%"
                    --atlassian-password "%dimrbakker_password%"
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
    }
})