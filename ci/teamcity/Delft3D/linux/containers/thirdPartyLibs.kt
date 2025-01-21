package Delft3D.linux.containers

import jetbrains.buildServer.configs.kotlin.*
import jetbrains.buildServer.configs.kotlin.buildFeatures.*
import jetbrains.buildServer.configs.kotlin.buildSteps.*
import jetbrains.buildServer.configs.kotlin.triggers.*
import Delft3D.template.*
import Delft3D.step.*
import java.io.File

object LinuxThirdPartyLibs : BuildType({
    name = "Third-party libraries"
    description = "Build the Delft3D Linux third-party-libs container image and push it to Harbor."

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
        param("reverse.dep.${LinuxBuildTools.id}.intel_oneapi_version", "2023")
        param("intel_fortran_compiler", "ifort")
        param("build_type", "release")
        param("harbor_repo", "containers.deltares.nl/delft3d-dev/delft3d-third-party-libs")

        // Environment variables that must be overwritten in the build.
        param("env.DEBUG", "")
        param("env.BUILDTOOLS_IMAGE_TAG", "")
        param("env.IMAGE_TAG", "")
        param("env.CACHE_FROM_ARGS", "")
        param("env.JIRA_ISSUE_ID", "")
    }

    triggers {
        vcs { // Only trigger builds when dockerfiles are modified.
            triggerRules = """
                +:ci/dockerfiles/linux/buildtools.Dockerfile
                +:ci/dockerfiles/linux/third-party-libs.Dockerfile
            """.trimIndent()
            branchFilter = """
                +:<default>
                +:merge-requests/*
            """.trimIndent()
        }
    }

    steps {
        mergeTargetBranch {}
        exportJiraIssueId {
            paramName = "env.JIRA_ISSUE_ID"
        }
        script {
            name = "Initialize build parameters"
            val script = File(DslContext.baseDir, "linux/containers/scripts/thirdPartyLibsSetParams.sh")
            scriptContent = Util.readScript(script)
        }
        dockerCommand {
            name = "Build"
            commandType = build {
                source = file {
                    path = "ci/dockerfiles/linux/third-party-libs.Dockerfile"
                }
                platform = DockerCommandStep.ImagePlatform.Linux
                contextDir = "."
                namesAndTags = "%harbor_repo%:%env.IMAGE_TAG%"
                commandArgs = """
                    --pull
                    --build-arg BUILDTOOLS_IMAGE_TAG=%env.BUILDTOOLS_IMAGE_TAG%
                    --build-arg INTEL_ONEAPI_VERSION=%intel_oneapi_version%
                    --build-arg INTEL_FORTRAN_COMPILER=%intel_fortran_compiler%
                    --build-arg DEBUG=%env.DEBUG%
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
        dockerCommand {
            name = "Prune cache mounts"
            executionMode = BuildStep.ExecutionMode.ALWAYS
            commandType = other {
                subCommand = "builder"
                commandArgs = "prune --force --filter type=exec.cachemount"
            }
        }
    }

    dependencies {
        dependency(LinuxBuildTools) {
            snapshot {
                onDependencyFailure = FailureAction.FAIL_TO_START
                onDependencyCancel = FailureAction.CANCEL
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