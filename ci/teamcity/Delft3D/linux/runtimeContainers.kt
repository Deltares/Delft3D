package Delft3D.linux

import jetbrains.buildServer.configs.kotlin.*
import jetbrains.buildServer.configs.kotlin.buildFeatures.*
import jetbrains.buildServer.configs.kotlin.buildSteps.*
import Delft3D.template.*
import Delft3D.step.*
import Delft3D.linux.containers.*

object LinuxRuntimeContainers : BuildType({

    description = "Build two separate container images: one for running the Delft3D software and the other for executing its tests."

    templates(
        TemplateMergeRequest,
        TemplatePublishStatus,
        TemplateMonitorPerformance,
        TemplateDockerRegistry
    )

    name = "Runtime Containers"
    buildNumberPattern = "%dep.${LinuxBuild.id}.product%: %build.vcs.number%"

    params {
        param("runtime_container_image", "containers.deltares.nl/delft3d-dev/delft3d-runtime-container:alma%almalinux_version%-%dep.${LinuxBuild.id}.product%-%build.vcs.number%")
        param("testbench_container_image", "containers.deltares.nl/delft3d-dev/test/delft3d-test-container:alma%almalinux_version%-%dep.${LinuxBuild.id}.product%-%build.vcs.number%")
    }

    features {
        matrix {
           param("almalinux_version", listOf(
              value("8", label = "AlmaLinux 8"),
              value("9", label = "AlmaLinux 9"),
              value("10", label = "AlmaLinux 10")
           ))
        }
    }

    vcs {
        root(DslContext.settingsRoot)
        cleanCheckout = true
    }

    steps {
        mergeTargetBranch {}
        exec {
            name = "Copy example and readme.txt"
            path = "ci/teamcity/Delft3D/linux/scripts/copyExampleAndReadMe.sh"
        }
        dockerCommand {
            name = "Docker build Delft3D runtime image"
            commandType = build {
                source = file {
                    path = "ci/teamcity/Delft3D/linux/docker/runtimeContainer.Dockerfile"
                }
                contextDir = "."
                platform = DockerCommandStep.ImagePlatform.Linux
                namesAndTags = """
                    runtime-container
                    %runtime_container_image%
                """.trimIndent()
                commandArgs = """
                    --provenance=false
                    --pull
                    --no-cache
                    --build-arg BASE_IMAGE=containers.deltares.nl/docker-proxy/library/almalinux:%almalinux_version%
                    --build-arg GIT_COMMIT=%build.vcs.number%
                    --build-arg GIT_BRANCH=%teamcity.build.branch%
                    --build-arg BUILDTOOLS_IMAGE_TAG=%dep.${LinuxBuild.id}.build_tools_image_tag%
                """.trimIndent()
                // --provenance=false is to prevent metadata to be pushed as unknown/unknown os/arch https://docs.docker.com/build/metadata/attestations/attestation-storage/
            }
        }
        dockerCommand {
            name = "Docker build testbench container image"
            commandType = build {
                source = file {
                    path = "ci/teamcity/Delft3D/linux/docker/testContainer.Dockerfile"
                }
                contextDir = "."
                platform = DockerCommandStep.ImagePlatform.Linux
                namesAndTags = "%testbench_container_image%"
                commandArgs = """
                    --build-arg GIT_COMMIT=%build.vcs.number%
                    --build-arg GIT_BRANCH=%teamcity.build.branch%
                """.trimIndent()
            }
        }
        dockerCommand {
            name = "Docker push"
            commandType = push {
                namesAndTags = """
                    %runtime_container_image%
                    %testbench_container_image%
                """.trimIndent()
            }
        }
    }
    dependencies {
        dependency(LinuxCollect) {
            snapshot {
                onDependencyFailure = FailureAction.FAIL_TO_START
                onDependencyCancel = FailureAction.CANCEL
            }

            artifacts {
                artifactRules = """
                    dimrset_lnx64_*.tar.gz!lnx64/bin/** => dimrset/bin
                    dimrset_lnx64_*.tar.gz!lnx64/lib/** => dimrset/lib
                    ?:dimrset_lnx64_*.tar.gz!lnx64/share/** => dimrset/share
                """.trimIndent()
            }
        }
        artifacts(AbsoluteId("Wanda_WandaCore_Wanda4TrunkX64")) {
            cleanDestination = true
            buildRule = lastSuccessful()
            artifactRules = "Bin64.zip!/Release/Wandadef.dat=>wanda/bin/Wandadef.dat"
        }
        artifacts(AbsoluteId("Wanda_WandaCore_Wanda4TrunkX64LinuxAlma8")) {
            cleanDestination = true
            buildRule = lastSuccessful()
            artifactRules = "build.zip!/lib/*=>wanda/lib"
        }
    }
})
