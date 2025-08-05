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
        +:ci/DIMRset_delivery/output/*.html
        +:ci/DIMRset_delivery/src/*.xlsx
        +:ci/DIMRset_delivery/src/*.txt
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
        param("dry_run", if (DslContext.getParameter("enable_dimrbak").lowercase() == "true") "false" else "true")
    }

    steps {

        
        python {
            name = "Generate test report summary"
            command = file {
                filename = "teamcity_retrieve_engine_test_status_dpc.py"
                scriptArguments = """
                    --username "%svn_buildserver_username%"
                    --password "%svn_buildserver_password%"
                    --build_id "%teamcity.build.id%"
                """.trimIndent()
            }
            workingDir = "ci/DIMRset_delivery/src"
            environment = venv {
                requirementsFile = "../requirements.txt"
            }
        }
        python {
            name = "Assert access rights"
            command = file {
                filename = "step1_assert_preconditions.py"
                scriptArguments = """
                    --atlassian-username "%dimrbakker_username%"
                    --atlassian-password "%dimrbakker_password%"
                    --teamcity-username "%dimrbakker_username%"
                    --teamcity-password "%dimrbakker_password%"
                    --ssh-username "%dimrbakker_username%"
                    --ssh-password "%dimrbakker_password%"
                    --git-username "%dimrbakker_username%"
                    --git-PAT "%dimrbakker_personal_access_token%"
                    --build_id "%teamcity.build.id%"
                    %if ("%dry_run%" == "true") "--dry-run" else ""%
                """.trimIndent()
            }
            workingDir = "ci/DIMRset_delivery/src"
            environment = venv {
                requirementsFile = "../requirements.txt"
            }
        }
        python {
            name = "Download artifacts from TeamCity and on file share using H7"
            command = file {
                filename = "step2_download_and_install_artifacts.py"
                scriptArguments = """
                    --teamcity-username "%dimrbakker_username%"
                    --teamcity-password "%dimrbakker_password%"
                    --ssh-username "%dimrbakker_username%"
                    --ssh-password "%dimrbakker_password%"
                    --build_id "%teamcity.build.id%"
                    %if ("%dry_run%" == "true") "--dry-run" else ""%
                """.trimIndent()
            }
            workingDir = "ci/DIMRset_delivery/src"
            environment = venv {
                requirementsFile = "../requirements.txt"
            }
        }
        python {
            name = "Update Excel sheet"
            command = file {
                filename = "step3_update_excel_sheet.py"
                scriptArguments = """
                    --teamcity-username "%dimrbakker_username%"
                    --teamcity-password "%dimrbakker_password%"
                    --ssh-username "%dimrbakker_username%"
                    --ssh-password "%dimrbakker_password%"
                    --build_id "%teamcity.build.id%"
                    %if ("%dry_run%" == "true") "--dry-run" else ""%
                """.trimIndent()
            }
            workingDir = "ci/DIMRset_delivery/src"
            environment = venv {
                requirementsFile = "../requirements.txt"
            }
        }
        python {
            name = "Prepare email template"
            command = file {
                filename = "step4_prepare_email.py"
                scriptArguments = """
                    --teamcity-username "%dimrbakker_username%"
                    --teamcity-password "%dimrbakker_password%"
                    --build_id "%teamcity.build.id%"
                    %if ("%dry_run%" == "true") "--dry-run" else ""%
                """.trimIndent()
            }
            workingDir = "ci/DIMRset_delivery/src"
            environment = venv {
                requirementsFile = "../requirements.txt"
            }
        }
        python {
            name = "Update public wiki"
            command = file {
                filename = "step5_update_public_wiki.py"
                scriptArguments = """
                    --atlassian-username "%dimrbakker_username%"
                    --atlassian-password "%dimrbakker_password%"
                    --teamcity-username "%dimrbakker_username%"
                    --teamcity-password "%dimrbakker_password%"
                    --build_id "%teamcity.build.id%"
                    %if ("%dry_run%" == "true") "--dry-run" else ""%
                """.trimIndent()
            }
            workingDir = "ci/DIMRset_delivery/src"
            environment = venv {
                requirementsFile = "../requirements.txt"
            }
        }
        python {
            name = "Pin and tag builds"
            command = file {
                filename = "step6_pin_and_tag_builds.py"
                scriptArguments = """
                    --teamcity-username "%dimrbakker_username%"
                    --teamcity-password "%dimrbakker_password%"
                    --git-username "%dimrbakker_username%"
                    --git-PAT "%dimrbakker_personal_access_token%"
                    --build_id "%teamcity.build.id%"
                    %if ("%dry_run%" == "true") "--dry-run" else ""%
                """.trimIndent()
            }
            workingDir = "ci/DIMRset_delivery/src"
            environment = venv {
                requirementsFile = "../requirements.txt"
            }
        }
    }
})