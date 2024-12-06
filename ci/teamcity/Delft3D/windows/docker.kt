package Delft3D.windows

import jetbrains.buildServer.configs.kotlin.*
import jetbrains.buildServer.configs.kotlin.buildFeatures.*
import jetbrains.buildServer.configs.kotlin.buildSteps.*

import Delft3D.template.*

object WindowsDocker : BuildType({

    templates(
        TemplateMergeRequest,
        TemplateMergeTarget,
        TemplatePublishStatus,
        TemplateMonitorPerformance
    )

    name = "Docker Build"
    buildNumberPattern = "%build.vcs.number%"
    description = "DIMRset Windows build container."

    vcs {
        root(DslContext.settingsRoot)
        cleanCheckout = true
    }

    steps {
        powerShell {
            name = "Get tooling from network share"
            id = "Get_tooling_from_network_share"
            platform = PowerShellStep.Platform.x64
            workingDir = "ci/dockerfiles"
            scriptMode = script {
                content = """
                    # Define the source directory
                    ${'$'}sourceDir = "\\dfs-trusted.directory.intra\dfs\Teamcity\Docker\Windows\dhydro-vs2022"
                    
                    # Get the current working directory
                    ${'$'}destinationDir = Get-Location
                    
                    # Copy the files from the source to the destination
                    Copy-Item -Path ${'$'}sourceDir\* -Destination ${'$'}destinationDir -Recurse
                """.trimIndent()
            }
        }
        script {
            name = "print dockerfile"
            id = "print_dockerfile"
            scriptContent = """type .\ci\dockerfiles\Dockerfile-dhydro"""
        }
        dockerCommand {
            name = "Docker build dhydro"
            id = "build_1"
            commandType = build {
                source = file {
                    path = "ci/dockerfiles/Dockerfile-dhydro"
                }
                contextDir = "ci/dockerfiles"
                platform = DockerCommandStep.ImagePlatform.Windows
                namesAndTags = """
                    containers.deltares.nl/delft3d-dev/delft3d-buildtools-windows:latest
                    containers.deltares.nl/delft3d-dev/delft3d-buildtools-windows:%build.vcs.number%
                """.trimIndent()
                commandArgs = "--no-cache"
            }
        }
        dockerCommand {
            name = "Docker push"
            id = "Docker_push"
            commandType = push {
                namesAndTags = """
                    containers.deltares.nl/delft3d-dev/delft3d-buildtools-windows:latest
                    containers.deltares.nl/delft3d-dev/delft3d-buildtools-windows:%build.vcs.number%
                """.trimIndent()
            }
        }
    }

    triggers {
        vcs {
            triggerRules = """
                -:.
                +:ci/dockerfiles/**
            """.trimIndent()
            branchFilter = "+:none/task/DEVOPSDSC-332-add-initial-windows-dockerfile"
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