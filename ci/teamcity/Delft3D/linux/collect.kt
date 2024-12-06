package Delft3D.linux

import jetbrains.buildServer.configs.kotlin.*
import jetbrains.buildServer.configs.kotlin.buildFeatures.*
import jetbrains.buildServer.configs.kotlin.buildSteps.*
import jetbrains.buildServer.configs.kotlin.failureConditions.*

import Delft3D.template.*

object LinuxCollect : BuildType({

    templates(
        TemplateMergeRequest,
        TemplateMergeTarget,
        TemplatePublishStatus,
        TemplateMonitorPerformance
    )

    name = "Collect"
    buildNumberPattern = "%build.vcs.number%"
    description = "DIMRset collector for Linux."

    allowExternalStatus = true
    artifactRules = """
        lnx64 => dimrset_lnx64_%build.vcs.number%.tar.gz!lnx64
        dimr_version_lnx64.txt => dimrset_lnx64_%build.vcs.number%.tar.gz!lnx64
        dimr_version*txt => version
    """.trimIndent()

    vcs {
        root(DslContext.settingsRoot)
        cleanCheckout = true
    }

    steps {
        script {
            name = "Copy libraries"
            workingDir = "lnx64/lib"
            scriptContent = """
                cp -v /opt/apps/intelmkl/2023.1.0/mkl/2023.1.0/lib/intel64/libmkl_core* .
                cp -v /opt/apps/intelmkl/2023.1.0/mkl/2023.1.0/lib/intel64/libmkl_avx* .
                cp -v /opt/apps/intelmkl/2023.1.0/mkl/2023.1.0/lib/intel64/libmkl_def* .
                cp -v /opt/apps/intelmkl/2023.1.0/mkl/2023.1.0/lib/intel64/libmkl_intel_thread* .
                cp -v /opt/apps/intelmkl/2023.1.0/mkl/2023.1.0/lib/intel64/libmkl_sequential* .
                cp -v /opt/apps/netcdf/4.9.2_4.6.1_intel2023.1.0/lib/libnetcdff.so.7 .
                cp -v /opt/apps/netcdf/4.9.2_4.6.1_intel2023.1.0/lib/libnetcdf.so.19 .
                cp -v /opt/apps/hdf5/1.14.0_intel2023.1.0/lib/libhdf5.so.310 .
                cp -v /opt/apps/hdf5/1.14.0_intel2023.1.0/lib/libhdf5_hl.so.310 .
            """.trimIndent()
        }
        exec {
            name = "Run artifacts_cleaner.py"
            path = "/usr/bin/python3"
            arguments = "src/scripts_lgpl/artifacts_cleaner.py --product dimrset --root ."
        }
        exec {
            name = "Generate list of version numbers (from what-strings)"
            path = "/usr/bin/python3"
            arguments = "ci/DIMRset_delivery/scripts/list_all_what_strings.py --srcdir lnx64 --output dimr_version_lnx64.txt"
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
        artifacts(AbsoluteId("Delft3DSobek_OssBuilds_Alma8LinuxTest_FbcToolsBuildOssX64Alma8CMakeReleaseLinux64")) {
            buildRule = lastSuccessful()
            artifactRules = """
                FBCTools*.tar.gz!bin/* => lnx64/bin
                FBCTools*.tar.gz!lib/* => lnx64/lib
                FBCTools*.tar.gz!share/* => lnx64/share/drtc
            """.trimIndent()
        }
    }
    requirements {
        equals("teamcity.agent.jvm.os.name", "Linux")
    }
})