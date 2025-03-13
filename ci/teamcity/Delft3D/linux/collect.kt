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
    }
    requirements {
        equals("teamcity.agent.jvm.os.name", "Linux")
    }
})