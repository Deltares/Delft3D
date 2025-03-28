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
        TemplateMonitorPerformance
    )

    name = "Runtime-environment Containers"
    buildNumberPattern = "%dep.${LinuxBuild.id}.product%: %build.vcs.number%"

    vcs {
        root(DslContext.settingsRoot)
        cleanCheckout = true
    }

    steps {
        mergeTargetBranch {}
        script {
            name = "Remove system libraries"
            workingDir = "dimrset/lib"
            scriptContent = """
                rm -fv libuuid.so.* \
                    libdl.so.* \
                    librt.so.* \
                    libpthread.so.* \
                    libm.so.* \
                    libgcc_s.so.* \
                    libz.so.* \
                    libselinux.so.* \
                    libbz2.so.* \
                    libcom_err.so.* \
                    libcurl.so.* \
                    libgssapi_krb5.so.* \
                    libidn2.so.* \
                    libk5crypto.so.* \
                    libkeyutils.so.* \
                    libkrb5.so.* \
                    libkrb5support.so.* \
                    liblzma.so.* \
                    libnghttp2.so.* \
                    libpcre2-8.so.* \
                    libresolv.so.* \
                    libsasl2.so.* \
                    libsqlite3.so.* \
                    libunistring.so.* \
                    libxml2.so.* \
                    libzstd.so.*
            """.trimIndent()
        }
        script {
            name = "Set execute rights"
            scriptContent = """
                chmod a+x dimrset/bin/*
            """.trimIndent()
        }
        dockerCommand {
            name = "Docker build runtime-environment image"
            commandType = build {
                source = file {
                    path = "ci/teamcity/Delft3D/linux/docker/runtimeEnvironment.Dockerfile"
                }
                contextDir = "."
                platform = DockerCommandStep.ImagePlatform.Linux
                namesAndTags = """
                    runtime-environment
                    containers.deltares.nl/delft3d/delft3d-runtime-environment:alma8-%build.vcs.number%
                """.trimIndent()
                commandArgs = """
                    --pull
                    --no-cache
                    --build-arg GIT_COMMIT=%build.vcs.number%
                    --build-arg GIT_BRANCH=%teamcity.build.branch%
                    --build-arg BUILDTOOLS_IMAGE_TAG=%dep.${LinuxBuildTools.id}.env.IMAGE_TAG%
                """.trimIndent()
            }
        }
        dockerCommand {
            name = "Docker build testbench-environment image"
            commandType = build {
                source = file {
                    path = "ci/teamcity/Delft3D/linux/docker/testEnvironment.Dockerfile"
                }
                contextDir = "."
                platform = DockerCommandStep.ImagePlatform.Linux
                namesAndTags = "containers.deltares.nl/delft3d/test/delft3d-test-environment:alma8-%build.vcs.number%"
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
                    containers.deltares.nl/delft3d/delft3d-runtime-environment:alma8-%build.vcs.number%
                    containers.deltares.nl/delft3d/test/delft3d-test-environment:alma8-%build.vcs.number%
                """.trimIndent()
            }
        }
    }
    features {
        dockerSupport {
            loginToRegistry = on {
                dockerRegistryId = "PROJECT_EXT_133,PROJECT_EXT_81"
            }
        }
        dockerSupport {
            loginToRegistry = on {
                dockerRegistryId = "DOCKER_REGISTRY_DELFT3D_DEV"
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
                artifactRules = "dimrset_lnx64_*.tar.gz!/lnx64 => dimrset"
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
