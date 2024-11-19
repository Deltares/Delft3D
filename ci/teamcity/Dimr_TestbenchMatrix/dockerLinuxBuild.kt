package testbenchMatrix

import jetbrains.buildServer.configs.kotlin.*
import jetbrains.buildServer.configs.kotlin.buildFeatures.dockerSupport
import jetbrains.buildServer.configs.kotlin.buildSteps.*

object DockerLinuxBuild : BuildType({

    name = "Docker Linux Build"
    buildNumberPattern = "%build.revisions.short%"
    description = "Build DIMRset Linux container."

    vcs {
        root(DslContext.settingsRoot)
    }

    steps {
        script {
            name = "Remove system libraries"
            id = "Remove_system_libraries"
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
            id = "Set_execute_rights"
            scriptContent = """
                chmod a+x dimrset/bin/*
                chmod a+x intel/mpi/bin/*
            """.trimIndent()
        }
        dockerCommand {
            name = "Docker build DIMRset"
            id = "Docker_build_dimrset"
            commandType = build {
                source = file {
                    path = "ci/teamcity/Dimr_TestbenchMatrix/docker/dimrset.Dockerfile"
                }
                contextDir = "."
                platform = DockerCommandStep.ImagePlatform.Linux
                namesAndTags = """
                    dimrset
                    containers.deltares.nl/delft3d/delft3dfm:alma8-%build.revisions.short%
                """.trimIndent()
                commandArgs = """
                    --pull
                    --no-cache
                    --build-arg GIT_COMMIT=%build.revisions.revision%
                    --build-arg GIT_BRANCH=%teamcity.build.branch%
                """.trimIndent()
            }
        }
        dockerCommand {
            name = "Docker build testbench"
            id = "Docker_build_testbench"
            commandType = build {
                source = file {
                    path = "ci/teamcity/Dimr_TestbenchMatrix/docker/testbench.Dockerfile"
                }
                contextDir = "."
                platform = DockerCommandStep.ImagePlatform.Linux
                namesAndTags = "containers.deltares.nl/delft3d/test/delft3dfm:alma8-%build.revisions.short%"
                commandArgs = """
                    --build-arg GIT_COMMIT=%build.revisions.revision%
                    --build-arg GIT_BRANCH=%teamcity.build.branch%
                """.trimIndent()
            }
        }
        dockerCommand {
            name = "Docker push"
            id = "Docker_push"
            commandType = push {
                namesAndTags = """
                    containers.deltares.nl/delft3d/delft3dfm:alma8-%build.revisions.short%
                    containers.deltares.nl/delft3d/test/delft3dfm:alma8-%build.revisions.short%
                """.trimIndent()
            }
        }
    }
    features {
        dockerSupport {
            id = "DockerSupport"
            cleanupPushedImages = true
            loginToRegistry = on {
                dockerRegistryId = "PROJECT_EXT_133,PROJECT_EXT_81"
            }
        }
    }
    dependencies {
        dependency(AbsoluteId("Delft3DSobek_OssBuilds_Alma8LinuxTest_1aDimrCollectorDailyLnx64")) {
            snapshot {
            }

            artifacts {
                artifactRules = "dimrset_lnx64_*.tar.gz!/lnx64 => dimrset"
            }
        }

        artifacts(AbsoluteId("Delft3D_ThirdParty_Linux_IntelMpi")) {
            buildRule = lastSuccessful()
            cleanDestination = true
            artifactRules = """
                intelmpi.tar.gz!/bin/hydra_* => intel/mpi/bin/
                intelmpi.tar.gz!/bin/mpiexec* => intel/mpi/bin/
                intelmpi.tar.gz!/lib  => intel/mpi/lib
            """.trimIndent()
        }
        artifacts(AbsoluteId("Wanda_WandaCore_Wanda4TrunkX64")) {
            id = "ARTIFACT_DEPENDENCY_1122"
            buildRule = lastSuccessful()
            artifactRules = "Bin64.zip!/Release/Wandadef.dat=>wanda/bin/Wandadef.dat"
        }
        artifacts(AbsoluteId("Wanda_WandaCore_Wanda4TrunkX64LinuxAlma8")) {
            id = "ARTIFACT_DEPENDENCY_2182"
            buildRule = lastSuccessful()
            artifactRules = "build.zip!/lib/*=>wanda/lib"
        }
    }
})