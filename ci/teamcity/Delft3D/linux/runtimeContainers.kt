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

    params {
        param("file_path", "dimrset_linux_%dep.${LinuxBuild.id}.product%_%build.vcs.number%.tar.gz")
    }

    vcs {
        root(DslContext.settingsRoot)
        cleanCheckout = true
    }

    steps {
        step {
            name = "Download artifact from Nexus"
            type = "RawDownloadNexusLinux"
            executionMode = BuildStep.ExecutionMode.DEFAULT
            param("artifact_path", "/07_day_retention/dimrset/%file_path%")
            param("nexus_repo", "/delft3d-dev")
            param("nexus_username", "%nexus_username%")
            param("download_to", ".")
            param("nexus_password", "%nexus_password%")
            param("nexus_url", "https://artifacts.deltares.nl/repository")
        }
        script {
            name = "Extract artifact"
            enabled = false
            scriptContent = """
                echo "Extracting %file_path%..."

                tar -xzf %file_path%

                mkdir dimrset

                cp -r lnx64/bin dimrset/bin

                cp -r lnx64/lib dimrset/lib

                cp -r lnx64/share dimrset/share
            """.trimIndent()
        }
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
        artifacts(AbsoluteId("Wanda_WandaCore_Wanda4TrunkX64LinuxAlma8")) {
            cleanDestination = true
            buildRule = build("939")
            artifactRules = """
                build.zip!/lib/*.so=>wanda/lib
                build.zip!/lib/Wandadef.dat=>wanda/bin/Wandadef.dat
            """.trimIndent()
        }
    }
})
