package Delft3D.linux

import jetbrains.buildServer.configs.kotlin.*
import jetbrains.buildServer.configs.kotlin.buildFeatures.*
import jetbrains.buildServer.configs.kotlin.buildSteps.*
import jetbrains.buildServer.configs.kotlin.failureConditions.*
import Delft3D.template.*
import Delft3D.step.*

object LinuxCollect : BuildType({

    description = "Prepping the binaries for testing/release."

    templates(
        TemplateMergeRequest,
        TemplatePublishStatus,
        TemplateMonitorPerformance
    )

    name = "Collect"
    buildNumberPattern = "%dep.${LinuxBuild.id}.product%: %build.vcs.number%"

    allowExternalStatus = true
    artifactRules = """
        #teamcity:symbolicLinks=as-is
        lnx64 => dimrset_lnx64_%build.vcs.number%.tar.gz!lnx64
        dimrset_version_lnx64.txt => dimrset_lnx64_%build.vcs.number%.tar.gz!lnx64
        dimrset_version*txt => version
    """.trimIndent()

    params {
        param("file_path", "dimrset_linux_%dep.${LinuxBuild.id}.product%_%build.vcs.number%.tar.gz")
    }

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
            name = "Remove system libraries"
            workingDir = "lnx64/lib"
            path = "ci/teamcity/Delft3D/linux/scripts/removeSysLibs.sh"
        }
        script {
            name = "Set execute rights"
            scriptContent = """
                chmod a+x lnx64/bin/*
            """.trimIndent()
        }
        exec {
            name = "Generate list of version numbers (from what-strings)"
            path = "/usr/bin/python3"
            arguments = "ci/python/ci_tools/dimrset_delivery/scripts/list_all_what_strings.py --srcdir lnx64 --output dimrset_version_lnx64.txt"
        }
        script {
            name = "Prepare artifact to upload"
            scriptContent = """
                echo "Creating %file_path%..."
                tar -czf %file_path% lnx64 dimrset_version_lnx64.txt
            """.trimIndent()
        }
        step {
            name = "Upload artifact to Nexus"
            type = "RawUploadNexusLinux"
            executionMode = BuildStep.ExecutionMode.DEFAULT
            param("file_path", "%file_path%")
            param("nexus_username", "%nexus_username%")
            param("nexus_password", "%nexus_password%")
            param("nexus_repo", "/delft3d-dev")
            param("nexus_url", "https://artifacts.deltares.nl/repository")
            param("target_path", "/07_day_retention/dimrset/%file_path%")
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
        dependency(LinuxBuild2D3DSP) {
            snapshot {
                onDependencyFailure = FailureAction.FAIL_TO_START
                onDependencyCancel = FailureAction.CANCEL
            }

            artifacts {
                artifactRules = "?:oss_artifacts_lnx64_*.tar.gz!lnx64/lib/libflow2d3d_sp.so => lnx64/lib"
            }
        }
        dependency(LinuxBuild) {
            snapshot {
                onDependencyFailure = FailureAction.FAIL_TO_START
                onDependencyCancel = FailureAction.CANCEL
            }

            artifacts {
                artifactRules = "oss_artifacts_lnx64_*.tar.gz!lnx64/** => lnx64"
            }
        }
    }
    requirements {
        equals("teamcity.agent.jvm.os.name", "Linux")
    }
})
