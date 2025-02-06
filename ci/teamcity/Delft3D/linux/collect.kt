package Delft3D.linux

import jetbrains.buildServer.configs.kotlin.*
import jetbrains.buildServer.configs.kotlin.buildFeatures.*
import jetbrains.buildServer.configs.kotlin.buildSteps.*
import jetbrains.buildServer.configs.kotlin.failureConditions.*
import Delft3D.template.*
import Delft3D.step.*

object LinuxCollect : BuildType({

    templates(
        TemplateMergeRequest,
        TemplatePublishStatus,
        TemplateMonitorPerformance
    )

    name = "Collect"
    buildNumberPattern = "%dep.${LinuxBuild.id}.product%: %build.vcs.number%"
    description = "DIMRset collector for Linux."

    allowExternalStatus = true
    artifactRules = """
        #teamcity:symbolicLinks=as-is
        lnx64 => dimrset_lnx64_%build.vcs.number%.tar.gz!lnx64
        dimr_version_lnx64.txt => dimrset_lnx64_%build.vcs.number%.tar.gz!lnx64
        dimr_version*txt => version
    """.trimIndent()

    vcs {
        root(DslContext.settingsRoot)
        cleanCheckout = true
    }

    steps {
        mergeTargetBranch {}
        exec {
            name = "Run artifacts_cleaner.py"
            path = "/usr/bin/python3"
            arguments = "src/scripts_lgpl/artifacts_cleaner.py --product dimrset --root ."
            conditions {
                equals("dep.${LinuxBuild.id}.product", "fm-suite")
            }
        }
        exec {
            name = "Generate list of version numbers (from what-strings)"
            path = "/usr/bin/python3"
            arguments = "ci/DIMRset_delivery/scripts/list_all_what_strings.py --srcdir lnx64 --output dimr_version_lnx64.txt"
        }
        script {
            name = "Copy libraries"
            workingDir = "lnx64/lib"
            scriptContent = """
                #!/usr/bin/env bash
                cp -av /opt/apps/intelmkl/2023.1.0/mkl/2023.1.0/lib/intel64/libmkl_core.so* .
                cp -av /opt/apps/intelmkl/2023.1.0/mkl/2023.1.0/lib/intel64/libmkl_avx*.so* .
                cp -av /opt/apps/intelmkl/2023.1.0/mkl/2023.1.0/lib/intel64/libmkl_def*.so* .
                cp -av /opt/apps/intelmkl/2023.1.0/mkl/2023.1.0/lib/intel64/libmkl_intel_thread.so* .
                cp -av /opt/apps/intelmkl/2023.1.0/mkl/2023.1.0/lib/intel64/libmkl_sequential.so* .
                cp -av /opt/apps/netcdf/4.9.2_4.6.1_intel2023.1.0/lib/libnetcdff.so* .
                cp -av /opt/apps/netcdf/4.9.2_4.6.1_intel2023.1.0/lib/libnetcdf.so* .
                cp -av /opt/apps/hdf5/1.14.0_intel2023.1.0/lib/libhdf5.so* .
                cp -av /opt/apps/hdf5/1.14.0_intel2023.1.0/lib/libhdf5_hl.so* .
            """.trimIndent()
        }
    }

    failureConditions {
        executionTimeoutMin = 180
        errorMessage = true
        failOnText {
            conditionType = BuildFailureOnText.ConditionType.REGEXP
            pattern = "Artifacts path .* not found"
            failureMessage = "Artifacts are missing"
            reverse = false
        }
        failOnText {
            conditionType = BuildFailureOnText.ConditionType.CONTAINS
            pattern = "Failed to resolve artifact dependency"
            failureMessage = "Unable to collect all dependencies"
            reverse = false
            stopBuildOnFailure = true
        }
    }

    dependencies {
        dependency(LinuxBuild) {
            snapshot {
                onDependencyFailure = FailureAction.FAIL_TO_START
                onDependencyCancel = FailureAction.CANCEL
            }

            artifacts {
                artifactRules = "oss_artifacts_lnx64_*.tar.gz!lnx64/** => lnx64"
            }
        }
        dependency(AbsoluteId("Delft3DSobek_OssBuilds_Alma8LinuxTest_FbcToolsBuildOssX64Alma8CMakeReleaseLinux64")) {
            snapshot {
                onDependencyFailure = FailureAction.FAIL_TO_START
                onDependencyCancel = FailureAction.CANCEL
            }
            artifacts {
                buildRule = lastSuccessful()
                artifactRules = """
                    FBCTools*.tar.gz!bin/* => lnx64/bin
                    FBCTools*.tar.gz!lib/* => lnx64/lib
                    FBCTools*.tar.gz!share/* => lnx64/share/drtc
                """.trimIndent()
            }
        }
    }
    requirements {
        equals("teamcity.agent.jvm.os.name", "Linux")
    }
})