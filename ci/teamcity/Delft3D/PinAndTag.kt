import jetbrains.buildServer.configs.kotlin.*
import jetbrains.buildServer.configs.kotlin.buildSteps.*
import jetbrains.buildServer.configs.kotlin.buildFeatures.*
import jetbrains.buildServer.configs.kotlin.triggers.finishBuildTrigger

import Delft3D.template.*

object PinAndTag : BuildType({

    templates(
        TemplateMonitorPerformance
    )

    name = "Pin and tag"
    description = "Pin and tag all the builds in the chain."
    buildNumberPattern = "%build.vcs.number%"
    maxRunningBuilds = 1

    val branchFilters = """
        +:<default>
        +:main
        +:all/release/*
    """.trimIndent()

    vcs {
        root(DslContext.settingsRoot)
        branchFilter = branchFilters
    }

    requirements {
        exists("env.PYTHON_PATH")
        contains("teamcity.agent.jvm.os.name", "Windows")
    }

    params {
        param("release_version", "%dep.${Publish.id}.release_version%")
        param("dimrbakker_username", DslContext.getParameter("dimrbakker_username"))
        password("dimrbakker_password", DslContext.getParameter("dimrbakker_password"))
        password("dimrbakker_personal_access_token", DslContext.getParameter("dimrbakker_personal_access_token"))
        param("dry_run", if (DslContext.getParameter("enable_pin_and_tag").lowercase() == "true") "" else "--dry-run")
    }

    steps {
        python {
            name = "Pin and tag TeamCity builds"
            command = module {
                module = "ci_tools.dimrset_delivery.pin_and_tag_builds"
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
            executionMode = BuildStep.ExecutionMode.ALWAYS
        }
        python {
            name = "Tag DIMRset release in Git"
            command = module {
                module = "ci_tools.dimrset_delivery.git_tagging"
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

    if (DslContext.getParameter("enable_release_publisher").lowercase() == "true") {
        triggers {
            finishBuildTrigger {
                buildType = "Delft3D_Publish"
                successfulOnly = true
                branchFilter = branchFilters
            }
        }
    }   
})
