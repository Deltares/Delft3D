package Delft3D.windows

import java.io.File
import jetbrains.buildServer.configs.kotlin.*
import jetbrains.buildServer.configs.kotlin.buildFeatures.*
import jetbrains.buildServer.configs.kotlin.buildSteps.*
import jetbrains.buildServer.configs.kotlin.triggers.*
import jetbrains.buildServer.configs.kotlin.triggers.schedule
import Delft3D.template.*
import Delft3D.step.*

object WindowsBuildEnvironment : BuildType({

    id("WindowsBuildEnvironmentI24")
    description = "Build-environment container images used to build the Delft3D software on Windows."

    templates(
        TemplateMergeRequest,
        TemplatePublishStatus,
        TemplateMonitorPerformance,
        TemplateDockerRegistry,
        TemplateBuildConcurrency
    )

    name = "Delft3D Windows build environment containers"
    buildNumberPattern = "%build.vcs.number%"

    params {
        param("trigger.type", "")
        param("dockerfile", "")
        param("toolchain.share", "")
        param("container.tag", "")
    }

    vcs {
        root(DslContext.settingsRoot)
        cleanCheckout = true
    }

    steps {
        powerShell {
            name = "Initialize build parameters"
            platform = PowerShellStep.Platform.x64
            scriptMode = script {
                val script = File(DslContext.baseDir, "windows/scripts/buildEnvironmentSetParams.ps1")
                content = Util.readScript(script)
            }
        }
        powerShell {
            name = "Get tooling from network share"
            platform = PowerShellStep.Platform.x64
            workingDir = "ci/dockerfiles/windows"
            scriptMode = script {
                content = """
                    # Define the source directory
                    ${'$'}sourceDir = "%toolchain.share%"

                    # Get the current working directory
                    ${'$'}destinationDir = Get-Location

                    # Copy the files from the source to the destination
                    Copy-Item -Path ${'$'}sourceDir\* -Destination ${'$'}destinationDir -Recurse

                    # List all the files in the destination directory
                    Get-ChildItem -Path ${'$'}destinationDir -Recurse
                """.trimIndent()
            }
        }
        dockerCommand {
            name = "Docker build dhydro"
            commandType = build {
                source = file {
                    path = "%dockerfile%"
                }
                contextDir = "ci/dockerfiles/windows"
                platform = DockerCommandStep.ImagePlatform.Windows
                namesAndTags = """
                    containers.deltares.nl/delft3d-dev/delft3d-buildtools-windows:%container.tag%
                    containers.deltares.nl/delft3d-dev/delft3d-buildtools-windows:%variant%-%build.vcs.number%
                """.trimIndent()
                commandArgs = "--no-cache"
            }
        }
        dockerCommand {
            name = "Docker push"
            commandType = push {
                namesAndTags = """
                    containers.deltares.nl/delft3d-dev/delft3d-buildtools-windows:%variant%-%build.vcs.number%
                """.trimIndent()
            }
        }
        dockerCommand {
            name = "Docker push tag"
            enabled = DslContext.getParameter("enable_environment_container_publishing").lowercase() == "true"
            commandType = push {
                namesAndTags = """
                    containers.deltares.nl/delft3d-dev/delft3d-buildtools-windows:%container.tag%
                """.trimIndent()
            }
            conditions {
                equals("trigger.type", "vcs")
            }
        }
    }

    features {
        matrix {
            param("variant", listOf(
                value("i24", label = "Intel OneAPI 2024 / VS2022"),
                value("i26", label = "Intel OneAPI 2026 / VS2026"),
            ))
        }
    }

    triggers {
        vcs {
            triggerRules = """
                +:ci/dockerfiles/windows/Dockerfile-dhydro-vs2022-i24
                +:ci/dockerfiles/windows/Dockerfile-dhydro-vs2026-i26
                +:ci/dockerfiles/windows/set-env.cmd
                +:ci/dockerfiles/windows/set-env-vs2022.cmd
                +:ci/teamcity/Delft3D/windows/buildEnvironment.kt
                +:ci/teamcity/Delft3D/windows/scripts/buildEnvironmentSetParams.ps1
            """.trimIndent()
            branchFilter = "+:<default>".trimIndent()
            buildParams {
                param("trigger.type", "vcs")
            }
        }
        schedule {
            schedulingPolicy = weekly {
                dayOfWeek = ScheduleTrigger.DAY.Sunday
                hour = 10
                minute = 0
            }
            branchFilter = "+:<default>"
            triggerBuild = always()
            withPendingChangesOnly = false
            buildParams {
                param("trigger.type", "schedule")
            }
        }
    }

    failureConditions {
        executionTimeoutMin = 360
    }
})
