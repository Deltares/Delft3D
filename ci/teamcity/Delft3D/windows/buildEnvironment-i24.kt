package Delft3D.windows

import jetbrains.buildServer.configs.kotlin.*
import jetbrains.buildServer.configs.kotlin.buildFeatures.*
import jetbrains.buildServer.configs.kotlin.buildSteps.*
import jetbrains.buildServer.configs.kotlin.triggers.*
import jetbrains.buildServer.configs.kotlin.triggers.schedule
import Delft3D.template.*
import Delft3D.step.*

object WindowsBuildEnvironmentI24 : BuildType({

    description = "Build-environment container image to build our Delf3D software in."

    templates(
        TemplateMergeRequest,
        TemplatePublishStatus,
        TemplateMonitorPerformance
    )

    name = "Delft3D build environment intel 2024 container"
    buildNumberPattern = "%build.vcs.number%"

    params {
        param("trigger.type", "")
        param("container.tag", "vs2022-intel2024")
    }

    vcs {
        root(DslContext.settingsRoot)
        cleanCheckout = true
    }

    steps {
        mergeTargetBranch {}
        powerShell {
            name = "Get tooling from network share"
            platform = PowerShellStep.Platform.x64
            workingDir = "ci/dockerfiles/windows"
            scriptMode = script {
                content = """
                    # Define the source directory
                    ${'$'}sourceDir = "\\directory.intra\project\d-hydro\dsc-tools\toolchain2024"
                    
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
                    path = "ci/dockerfiles/windows/Dockerfile-dhydro-vs2022-i24"
                }
                contextDir = "ci/dockerfiles/windows"
                platform = DockerCommandStep.ImagePlatform.Windows
                namesAndTags = """
                    containers.deltares.nl/delft3d-dev/delft3d-buildtools-windows:%container.tag%
                    containers.deltares.nl/delft3d-dev/delft3d-buildtools-windows:%build.vcs.number%
                """.trimIndent()
                commandArgs = "--no-cache"
            }
        }
        dockerCommand {
            name = "Docker push"
            commandType = push {
                namesAndTags = """
                    containers.deltares.nl/delft3d-dev/delft3d-buildtools-windows:%build.vcs.number%
                """.trimIndent()
            }
        }
        dockerCommand {
            name = "Docker push"
            commandType = push {
                namesAndTags = """
                    containers.deltares.nl/delft3d-dev/delft3d-buildtools-windows:%container.tag%
                """.trimIndent()
            }
            enabled = "%trigger.type%" == "vcs"
        }
    }

    triggers {
        vcs {
            triggerRules = "+:ci/dockerfiles/windows/**".trimIndent()
            branchFilter = "+:<default>".trimIndent()
            param("trigger.type", "vcs")
        }
        schedule {
            schedulingPolicy = weekly {
                dayOfWeek = ScheduleTrigger.DAY.Sunday
                hour = 0
                minute = 0
            }
            branchFilter = "+:<default>"
            triggerBuild = always()
            withPendingChangesOnly = false
            param("trigger.type", "schedule")
        }
    }

    failureConditions {
        executionTimeoutMin = 360
    }

    features {
        dockerSupport {
            loginToRegistry = on {
                dockerRegistryId = "DOCKER_REGISTRY_DELFT3D_DEV"
            }
        }
    }
})