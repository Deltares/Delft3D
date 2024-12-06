package Delft3D.windows

import jetbrains.buildServer.configs.kotlin.*
import jetbrains.buildServer.configs.kotlin.buildFeatures.*
import jetbrains.buildServer.configs.kotlin.buildSteps.*
import jetbrains.buildServer.configs.kotlin.triggers.vcs


import Delft3D.template.*

object WindowsBuildEnvironment : BuildType({

    templates(
        TemplateMergeRequest,
        TemplateMergeTarget,
        TemplatePublishStatus,
        TemplateMonitorPerformance
    )

    name = "Delft3D build environment container"
    buildNumberPattern = "%build.vcs.number%"
    description = "Delft3D Windows build container."

    vcs {
        root(DslContext.settingsRoot)
        cleanCheckout = true
    }

    steps {
        powerShell {
            name = "Get tooling from network share"
            platform = PowerShellStep.Platform.x64
            workingDir = "ci/dockerfiles"
            scriptMode = script {
                content = """
                    # Define the source directory
                    ${'$'}sourceDir = "\\dfs-trusted.directory.intra\dfs\Teamcity\Docker\Windows\dhydro-vs2019"
                    
                    # Get the current working directory
                    ${'$'}destinationDir = Get-Location
                    
                    # Copy the files from the source to the destination
                    Copy-Item -Path ${'$'}sourceDir\* -Destination ${'$'}destinationDir -Recurse
                """.trimIndent()
            }
        }
        script {
            name = "print dockerfile"
            scriptContent = """type .\ci\dockerfiles\Dockerfile-dhydro"""
        }
        dockerCommand {
            name = "Docker build dhydro"
            commandType = build {
                source = file {
                    path = "ci/dockerfiles/Dockerfile-dhydro"
                }
                contextDir = "ci/dockerfiles"
                platform = DockerCommandStep.ImagePlatform.Windows
                namesAndTags = """
                    containers.deltares.nl/delft3d-dev/delft3d-buildtools-windows:vs2019-oneapi2023
                    containers.deltares.nl/delft3d-dev/delft3d-buildtools-windows:%build.vcs.number%
                """.trimIndent()
                commandArgs = "--no-cache"
            }
        }
        dockerCommand {
            name = "Docker push"
            commandType = push {
                namesAndTags = """
                    containers.deltares.nl/delft3d-dev/delft3d-buildtools-windows:vs2019-oneapi2023
                    containers.deltares.nl/delft3d-dev/delft3d-buildtools-windows:%build.vcs.number%
                """.trimIndent()
            }
        }
    }

    triggers {
        vcs {
            triggerRules = """
                +:ci/dockerfiles/**
            """.trimIndent()
            branchFilter = "+:<default>"
        }
    }

    failureConditions {
        executionTimeoutMin = 360
    }

    features {
        dockerSupport {
            loginToRegistry = on {
                dockerRegistryId = "PROJECT_EXT_225"
            }
        }
    }
})