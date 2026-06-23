package Delft3D.windows

import jetbrains.buildServer.configs.kotlin.*
import jetbrains.buildServer.configs.kotlin.buildFeatures.*
import jetbrains.buildServer.configs.kotlin.buildSteps.*
import jetbrains.buildServer.configs.kotlin.triggers.*
import jetbrains.buildServer.configs.kotlin.triggers.schedule
import Delft3D.template.*
import Delft3D.step.*

object WindowsCollectEnvironment : BuildType({

    description = "Collect-environment container image for prepping and verifying Delft3D Windows binaries."

    templates(
        TemplateMergeRequest,
        TemplatePublishStatus,
        TemplateMonitorPerformance,
        TemplateDockerRegistry
    )

    name = "Delft3D collect environment container"
    buildNumberPattern = "%build.vcs.number%"

    params {
        param("trigger.type", "")
        param("container.tag", "collect-environment-ltsc2025")
    }

    vcs {
        root(DslContext.settingsRoot)
        cleanCheckout = true
    }

    steps {
        dockerCommand {
            name = "Docker build collect environment container"
            commandType = build {
                source = file {
                    path = "ci/dockerfiles/windows/Dockerfile-dhydro-collect"
                }
                contextDir = "ci/dockerfiles/windows"
                platform = DockerCommandStep.ImagePlatform.Windows
                namesAndTags = """
                    containers.deltares.nl/delft3d-dev/collect-windows:%container.tag%
                    containers.deltares.nl/delft3d-dev/collect-windows:%build.vcs.number%
                """.trimIndent()
                commandArgs = "--no-cache"
            }
        }
        dockerCommand {
            name = "Docker push with commit hash in tag"
            commandType = push {
                namesAndTags = """
                    containers.deltares.nl/delft3d-dev/collect-windows:%build.vcs.number%
                """.trimIndent()
            }
        }
        dockerCommand {
            name = "Docker push with production tag"
            enabled = DslContext.getParameter("enable_environment_container_publishing").lowercase() == "true"
            commandType = push {
                namesAndTags = """
                    containers.deltares.nl/delft3d-dev/collect-windows:%container.tag%
                """.trimIndent()
            }
            conditions {
                equals("trigger.type", "vcs")
            }
        }
    }

    triggers {
        vcs {
            triggerRules = """
                +:ci/dockerfiles/windows/**
                +:ci/teamcity/Delft3D/windows/collectEnvironment.kt
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
