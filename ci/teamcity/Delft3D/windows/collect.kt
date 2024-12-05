package Delft3D.windows

import jetbrains.buildServer.configs.kotlin.*
import jetbrains.buildServer.configs.kotlin.buildFeatures.*
import jetbrains.buildServer.configs.kotlin.buildSteps.*
import jetbrains.buildServer.configs.kotlin.failureConditions.*

import Delft3D.template.*

object WindowsCollect : BuildType({

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
        x64 => dimrset_x64_%build.vcs.number%.zip!x64
        dimrset_version_x64.txt => dimrset_x64_%build.vcs.number%.zip!x64
        dimrset_version*txt => version
    """.trimIndent()

    vcs {
        root(DslContext.settingsRoot)
        cleanCheckout = true
    }

    steps {
        python {
            name = "Run artifacts_cleaner.py"
            command = file {
                filename = "src/scripts_lgpl/artifacts_cleaner.py"
                scriptArguments = "--product dimrset --root ."
            }
        }
        python {
            name = "Generate list of version numbers (from what-strings)"
            command = file {
                filename = """ci/DIMRset_delivery/scripts/list_all_what_strings.py"""
                scriptArguments = "--srcdir x64 --output dimrset_version_x64.txt"
            }
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
        dependency(WindowsBuild) {
            snapshot {
                onDependencyFailure = FailureAction.FAIL_TO_START
                onDependencyCancel = FailureAction.CANCEL
            }

            artifacts {
                artifactRules = """
                    oss_artifacts_x64_*.zip!/x64/bin/** => x64/bin
                    oss_artifacts_x64_*.zip!/x64/lib/** => x64/lib
                    oss_artifacts_x64_*.zip!/x64/share/** => x64/share
                """.trimIndent()
            }
        }
        dependency(AbsoluteId("FbcTools_FbcToolsBuildOssX64CMakeReleaseWin64")) {
            snapshot {
                onDependencyFailure = FailureAction.FAIL_TO_START
                onDependencyCancel = FailureAction.CANCEL
            }

            artifacts {
                artifactRules = """
                    *.dll => x64/lib
                    *.xsd => x64/share/drtc
                """.trimIndent()
            }
        }
    }
    requirements {
        exists("env.PYTHON_PATH")
        contains("teamcity.agent.jvm.os.name", "Windows")
    }
})