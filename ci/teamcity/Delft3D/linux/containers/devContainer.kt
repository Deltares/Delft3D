package Delft3D.linux.containers

import jetbrains.buildServer.configs.kotlin.*
import jetbrains.buildServer.configs.kotlin.buildFeatures.*
import jetbrains.buildServer.configs.kotlin.buildSteps.*
import jetbrains.buildServer.configs.kotlin.triggers.*
import Delft3D.template.*
import Delft3D.step.*
import java.io.File

import Trigger

object LinuxDevContainer : BuildType({
    name = "Dev Container"
    description = "Test Build the dev container"
    buildNumberPattern = "%build.vcs.number%"

    templates(
        TemplatePublishStatus,
        TemplateMergeRequest,
        TemplateMonitorPerformance,
        TemplateDockerRegistry
    )

    vcs {
        root(DslContext.settingsRoot)
        cleanCheckout = true
    }

    params {
        param("intel_oneapi_version", "2024")
        param("reverse.dep.${LinuxBuildTools.id}.intel_oneapi_version", "2024")
        param("intel_fortran_compiler", "ifx")
        select("build_type", "Release", display = ParameterDisplay.PROMPT, options = listOf("Release", "RelWithDebInfo", "Debug"))
        param("harbor_repo", "containers.deltares.nl/delft3d-dev/delft3d-third-party-libs")


    }

    if (DslContext.getParameter("enable_third_party_libs_trigger").lowercase() == "true") {
        triggers {
            vcs { // Only trigger builds when dockerfiles are modified.
                triggerRules = """
                    +:ci/dockerfiles/linux/buildtools.Dockerfile
                    +:ci/dockerfiles/linux/third-party-libs.Dockerfile
                    +:.devcontainer/delft3d/dockerfile
                """.trimIndent()
                branchFilter = "+:<default>"
            }
        }
    }

    steps {
        mergeTargetBranch {}
        exportJiraIssueId {
            paramName = "env.JIRA_ISSUE_ID"
        }
        dockerCommand {
            name = "Build"
            commandType = build {
                source = file {
                    path = ".devcontainer/delft3d/Dockerfile"
                }
                platform = DockerCommandStep.ImagePlatform.Linux
                contextDir = "."
                commandArgs = """
                    --pull
                    """.trimIndent()
            }
        }
    }

    dependencies {
        dependency(Trigger) {
            snapshot {
                onDependencyFailure = FailureAction.FAIL_TO_START
                onDependencyCancel = FailureAction.CANCEL
            }
        }
        dependency(LinuxThirdPartyLibs) {
            snapshot {
                onDependencyFailure = FailureAction.FAIL_TO_START
                onDependencyCancel = FailureAction.CANCEL
            }
        }
    }

    requirements {
        equals("teamcity.agent.jvm.os.name", "Linux")
    }
})
