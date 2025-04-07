package Delft3D.linux

import jetbrains.buildServer.configs.kotlin.*
import jetbrains.buildServer.configs.kotlin.buildFeatures.*
import jetbrains.buildServer.configs.kotlin.buildSteps.*
import Delft3D.template.*
import Delft3D.step.*
import Delft3D.linux.thirdParty.*

object LinuxDocker : BuildType({

    description = "Build the Delft3D container image and an image for running the testbenches."

    templates(
        TemplateMergeRequest,
        TemplatePublishStatus,
        TemplateMonitorPerformance
    )

    name = "Docker Build"
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
                chmod a+x intel/mpi/bin/*
            """.trimIndent()
        }
        script {
            name = "Copy example to docker directory"
            scriptContent = """
                mkdir ./example && cp -r examples/dflowfm/08_dflowfm_sequential_dwaves/* ./example
            """.trimIndent()
        }
        dockerCommand {
            name = "Docker build DIMRset image"
            commandType = build {
                source = file {
                    path = "ci/teamcity/Delft3D/linux/docker/build.Dockerfile"
                }
                contextDir = "."
                platform = DockerCommandStep.ImagePlatform.Linux
                namesAndTags = """
                    dimrset
                    containers.deltares.nl/delft3d/delft3dfm:alma8-%build.vcs.number%
                """.trimIndent()
                commandArgs = """
                    --pull
                    --no-cache
                    --build-arg GIT_COMMIT=%build.vcs.number%
                    --build-arg GIT_BRANCH=%teamcity.build.branch%
                """.trimIndent()
            }
        }
        dockerCommand {
            name = "Docker build testbench image"
            commandType = build {
                source = file {
                    path = "ci/teamcity/Delft3D/linux/docker/test.Dockerfile"
                }
                contextDir = "."
                platform = DockerCommandStep.ImagePlatform.Linux
                namesAndTags = "containers.deltares.nl/delft3d/test/delft3dfm:alma8-%build.vcs.number%"
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
                    containers.deltares.nl/delft3d/delft3dfm:alma8-%build.vcs.number%
                    containers.deltares.nl/delft3d/test/delft3dfm:alma8-%build.vcs.number%
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

        artifacts(LinuxThirdPartyDownloadIntelMpi) {
            buildRule = lastSuccessful()
            cleanDestination = true
            artifactRules = """
                intelmpi.tar.gz!/bin/hydra_* => intel/mpi/bin/
                intelmpi.tar.gz!/bin/mpiexec* => intel/mpi/bin/
                intelmpi.tar.gz!/bin/mpirun => intel/mpi/bin/
                intelmpi.tar.gz!/lib  => intel/mpi/lib
            """.trimIndent()
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