package Delft3D.linux.containers

import jetbrains.buildServer.configs.kotlin.*
import jetbrains.buildServer.configs.kotlin.buildFeatures.*
import jetbrains.buildServer.configs.kotlin.buildSteps.*
import jetbrains.buildServer.configs.kotlin.triggers.*
import Delft3D.template.*
import Delft3D.step.*
import java.io.File

object LinuxBuildTools : BuildType({
    name = "Build tools"
    description = "Build the Delft3D Linux build-tools container image and push it to Harbor."

    templates(
        TemplatePublishStatus,
        TemplateMergeRequest
    )

    vcs {
        root(DslContext.settingsRoot)
        cleanCheckout = true
    }

    params {
        param("intel_oneapi_version", "2023")
        param("harbor_repo", "containers.deltares.nl/delft3d-dev/delft3d-buildtools")

        // Environment variables that must be overwritten in the build.
        param("env.IMAGE_TAG", "")
        param("env.CACHE_FROM_ARGS", "")
        param("env.JIRA_ISSUE_ID", "")
    }

    steps {
        mergeTargetBranch {}
        exportJiraIssueId {
            paramName = "env.JIRA_ISSUE_ID"
        }
        script {
            name = "Initialize build parameters"
            val script = File(DslContext.baseDir, "linux/containers/scripts/buildToolsSetParams.sh")
            scriptContent = Util.readScript(script)
        }
        dockerCommand {
            name = "Build"
            commandType = build {
                source = file {
                    path = "ci/dockerfiles/linux/buildtools.Dockerfile"
                }
                platform = DockerCommandStep.ImagePlatform.Linux
                contextDir = "."
                namesAndTags = "%harbor_repo%:%env.IMAGE_TAG%"
                commandArgs = """
                    --pull
                    --build-arg INTEL_ONEAPI_VERSION=%intel_oneapi_version%
                    --cache-to type=registry,ref=%harbor_repo%:%env.IMAGE_TAG%-cache,mode=max,image-manifest=true
                    %env.CACHE_FROM_ARGS%
                """.trimIndent()
            }
        }
        if (DslContext.getParameter("environment") == "production") {
            dockerCommand {
                name = "Push"
                commandType = push {
                    namesAndTags = "%harbor_repo%:%env.IMAGE_TAG%"
                    removeImageAfterPush = true
                }
            }
        }
        dockerCommand {
            name = "Prune cache mounts"
            executionMode = BuildStep.ExecutionMode.ALWAYS
            commandType = other {
                subCommand = "builder"
                commandArgs = "prune --force --filter type=exec.cachemount"
            }
        }
    }

    features {
        perfmon {}
        dockerSupport {
            loginToRegistry = on {
                dockerRegistryId = "DOCKER_REGISTRY_DELFT3D_DEV"
            }
        }
    }

    requirements {
        equals("teamcity.agent.jvm.os.name", "Linux")
    }
})

