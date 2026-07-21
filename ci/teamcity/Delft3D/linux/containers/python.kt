package Delft3D.linux.containers

import jetbrains.buildServer.configs.kotlin.*
import jetbrains.buildServer.configs.kotlin.buildFeatures.*
import jetbrains.buildServer.configs.kotlin.buildSteps.*
import jetbrains.buildServer.configs.kotlin.triggers.*
import Delft3D.template.*
import Delft3D.step.*
import java.io.File

object LinuxPython : BuildType({
    name = "Linux Python"
    description = "Container image used to run python workloads in TeamCity."
    buildNumberPattern = "%build.vcs.number%"

    templates(
        TemplateLinuxAgent,
        TemplatePublishStatus,
        TemplateMergeRequest,
        TemplateMonitorPerformance,
        TemplateDockerRegistry,
        TemplateBuildConcurrency
    )

    vcs {
        root(DslContext.settingsRoot)
        cleanCheckout = true
    }

    params {
        param("almalinux_base_version", "8")
        param("python_version", "3.12")
        param("base_image", "containers.deltares.nl/docker-proxy/almalinux/%almalinux_base_version%-base:latest")
        param("harbor_repo", "containers.deltares.nl/delft3d-dev/delft3d-python")

        // Environment variables that must be overwritten in the build.
        param("env.IMAGE_TAG", "")
        param("env.CACHE_FROM_ARGS", "")
        param("env.JIRA_ISSUE_ID", "")
    }

    steps {
        exportJiraIssueId {
            paramName = "env.JIRA_ISSUE_ID"
        }
        script {
            name = "Initialize build parameters"
            val script = File(DslContext.baseDir, "linux/containers/scripts/pythonSetParams.sh")
            scriptContent = Util.readScript(script)
        }
        dockerCommand {
            name = "Build"
            commandType = build {
                source = file {
                    path = "ci/dockerfiles/linux/python.Dockerfile"
                }
                platform = DockerCommandStep.ImagePlatform.Linux
                contextDir = "."
                namesAndTags = "%harbor_repo%:%env.IMAGE_TAG%"
                commandArgs = """
                    --pull
                    --build-arg BASE_IMAGE_URL=%base_image%
                    --build-arg PYTHON_VERSION=%python_version%
                    --cache-to type=registry,ref=%harbor_repo%:%env.IMAGE_TAG%-cache,mode=max,image-manifest=true
                    %env.CACHE_FROM_ARGS%
                """.trimIndent()
            }
        }
        dockerCommand {
            name = "Push"
            commandType = push {
                namesAndTags = "%harbor_repo%:%env.IMAGE_TAG%"
                removeImageAfterPush = true
            }
        }
    }
})

