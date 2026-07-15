package Delft3D.windows

import jetbrains.buildServer.configs.kotlin.*
import jetbrains.buildServer.configs.kotlin.buildFeatures.*
import jetbrains.buildServer.configs.kotlin.buildSteps.*
import jetbrains.buildServer.configs.kotlin.triggers.*
import jetbrains.buildServer.configs.kotlin.triggers.schedule
import Delft3D.template.*
import Delft3D.step.*

object WindowsBuildEnvironment : BuildType({

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
        // Per-variant settings, selected by the matrix "variant" dimension.
        param("container.tag.i24", "vs2022-intel2024-ltsc2025")
        param("toolchain.share.i24", "\\\\directory.intra\\project\\d-hydro\\dsc-tools\\toolchain2024")
        param("dockerfile.i24", "ci/dockerfiles/windows/Dockerfile-dhydro-vs2022-i24")
        param("container.tag.i26", "vs2026-intel2026-ltsc2025")
        param("toolchain.share.i26", "\\\\directory.intra\\project\\d-hydro\\dsc-tools\\toolchain2026")
        param("dockerfile.i26", "ci/dockerfiles/windows/Dockerfile-dhydro-vs2026-i26")
    }

    vcs {
        root(DslContext.settingsRoot)
        cleanCheckout = true
    }

    steps {
        powerShell {
            name = "Get tooling from network share"
            platform = PowerShellStep.Platform.x64
            workingDir = "ci/dockerfiles/windows"
            scriptMode = script {
                content = """
                    # Define the source directory
                    ${'$'}sourceDir = "%toolchain.share.%variant%%"

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
                    path = "%dockerfile.%variant%%"
                }
                contextDir = "ci/dockerfiles/windows"
                platform = DockerCommandStep.ImagePlatform.Windows
                namesAndTags = """
                    containers.deltares.nl/delft3d-dev/delft3d-buildtools-windows:%container.tag.%variant%%
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
                    containers.deltares.nl/delft3d-dev/delft3d-buildtools-windows:%container.tag.%variant%%
                """.trimIndent()
            }
            conditions {
                equals("trigger.type", "vcs")
            }
        }
    }

    features {
        matrix {
            param("variant", listOf(value("i24"), value("i26")))
        }
    }

    triggers {
        vcs {
            triggerRules = """
                +:ci/dockerfiles/windows/Dockerfile-dhydro-vs2022-i24
                +:ci/dockerfiles/windows/Dockerfile-dhydro-vs2026-i26
                +:ci/teamcity/Delft3D/windows/buildEnvironment.kt
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
