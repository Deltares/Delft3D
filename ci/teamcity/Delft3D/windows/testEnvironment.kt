package Delft3D.windows

import jetbrains.buildServer.configs.kotlin.*
import jetbrains.buildServer.configs.kotlin.buildFeatures.*
import jetbrains.buildServer.configs.kotlin.buildSteps.*
import jetbrains.buildServer.configs.kotlin.triggers.*
import jetbrains.buildServer.configs.kotlin.triggers.schedule
import Delft3D.template.*
import Delft3D.step.*

object WindowsTestEnvironment : BuildType({

    description = "Test-environment container image to test our Delf3D software in."

    templates(
        TemplateMergeRequest,
        TemplatePublishStatus,
        TemplateMonitorPerformance,
        TemplateDockerRegistry
    )

    name = "Delft3D test environment container"
    buildNumberPattern = "%build.vcs.number%"

    params {
        param("trigger.type", "")
        param("container.tag", "test-environment")
    }

    vcs {
        root(DslContext.settingsRoot)
        cleanCheckout = true
    }

    steps {
        powerShell {
            name = "Get tooling from network share"
            platform = PowerShellStep.Platform.x64
            scriptMode = script {
                content = """
                    # Get the current working directory
                    ${'$'}destinationDir = "ci\\dockerfiles\\windows"

                    # Copy the files from the source to the destination
                    Copy-Item -Path "test\\deltares_testbench\\pip\\win-requirements.txt" -Destination ${'$'}destinationDir

                    # List all the files in the destination directory
                    Get-ChildItem -Path ${'$'}destinationDir
                """.trimIndent()
            }
        }
        dockerCommand {
            name = "Docker build dhydro test-environment container"
            commandType = build {
                source = file {
                    path = "ci/dockerfiles/windows/Dockerfile-dhydro-test-environment"
                }
                contextDir = "ci/dockerfiles/windows"
                platform = DockerCommandStep.ImagePlatform.Windows
                namesAndTags = """
                    containers.deltares.nl/delft3d-dev/test/delft3d-test-environment-windows:%container.tag%
                    containers.deltares.nl/delft3d-dev/test/delft3d-test-environment-windows:%build.vcs.number%
                """.trimIndent()
                commandArgs = "--no-cache"
            }
        }
        dockerCommand {
            name = "Docker push"
            commandType = push {
                namesAndTags = """
                    containers.deltares.nl/delft3d-dev/test/delft3d-test-environment-windows:%build.vcs.number%
                """.trimIndent()
            }
        }
        dockerCommand {
            name = "Docker push"
            enabled = DslContext.getParameter("enable_environment_container_publishing").lowercase() == "true"
            commandType = push {
                namesAndTags = """
                    containers.deltares.nl/delft3d-dev/test/delft3d-test-environment-windows:%container.tag%
                """.trimIndent()
            }
            conditions {
                matches("trigger.type", "vcs|schedule")
            }
        }
    }

    triggers {
        vcs {
            triggerRules = """
                +:ci/dockerfiles/windows/Dockerfile-dhydro-test-environment
                +:ci/teamcity/Delft3D/windows/testEnvironment.kt
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