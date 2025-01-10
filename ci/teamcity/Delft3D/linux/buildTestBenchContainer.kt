package Delft3D.linux

import jetbrains.buildServer.configs.kotlin.*
import jetbrains.buildServer.configs.kotlin.buildFeatures.*
import jetbrains.buildServer.configs.kotlin.buildSteps.*
import jetbrains.buildServer.configs.kotlin.failureConditions.*
import Delft3D.template.*
import Delft3D.step.*

object LinuxBuildTestbenchContainer : BuildType({

    templates(
        TemplateMergeRequest,
    )
    name = "Build Testbench Container"
    description = "Create a container with the testbench (also for H7)"

    buildNumberPattern = "%build.vcs.number%"
    artifactRules = "container.txt"

    vcs {
        root(DslContext.settingsRoot)
        cleanCheckout = true
    }

    steps {
        mergeTargetBranch {}
        dockerCommand {
            name = "Build docker testbench container"
            commandType = build {
                source = file {
                    path = "test/deltares_testbench/ci/dockerfiles/testbench.Dockerfile"
                }
                contextDir = "."
                platform = DockerCommandStep.ImagePlatform.Linux
                namesAndTags = "containers.deltares.nl/delft3d/test/testbench:testbench-%build.vcs.number%"
                commandArgs = "--no-cache"
            }
        }
        dockerCommand {
            name = "Push docker testbench container"
            commandType = push {
                namesAndTags = "containers.deltares.nl/delft3d/test/testbench:testbench-%build.vcs.number%"
            }
        }
        script {
            name = "Create artifact"
            scriptContent = """
                file="container.txt"
                echo "containers.deltares.nl/delft3d/test/testbench:testbench-%build.vcs.number%" > ${'$'}file
            """.trimIndent()
        }
    }

    features {
        dockerSupport {
            loginToRegistry = on {
                dockerRegistryId = "DOCKER_REGISTRY_DELFT3D"
            }
        }
    }
})