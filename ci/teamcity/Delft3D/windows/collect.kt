package Delft3D.windows

import jetbrains.buildServer.configs.kotlin.*
import jetbrains.buildServer.configs.kotlin.buildFeatures.*
import jetbrains.buildServer.configs.kotlin.buildSteps.*
import jetbrains.buildServer.configs.kotlin.failureConditions.*
import Delft3D.template.*
import Delft3D.step.*

object WindowsCollect : BuildType({

    description = "Prepping the binaries for testing/release and verify the signing and directory structure."

    templates(
        TemplateMergeRequest,
        TemplatePublishStatus,
        TemplateMonitorPerformance
    )

    name = "Collect"
    buildNumberPattern = "%dep.${WindowsBuild.id}.product%: %build.vcs.number%"

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
        mergeTargetBranch {}
        python {
            name = "Run artifacts_cleaner.py"
            command = file {
                filename = "src/scripts_lgpl/artifacts_cleaner.py"
                scriptArguments = "--product dimrset --root ."
            }
            conditions {
                equals("dep.${WindowsBuild.id}.product", "fm-suite")
            }
        }
        python {
            name = "Generate list of version numbers (from what-strings)"
            command = file {
                filename = """ci/DIMRset_delivery/scripts/list_all_what_strings.py"""
                scriptArguments = "--srcdir x64 --output dimrset_version_x64.txt"
            }
        }
        python {
            name = "Verify (un)signed binaries and directory structure"
            command = file {
                filename = "ci/DIMRset_delivery/src/validate_signing.py"
                scriptArguments = """
                    "ci\\DIMRset_delivery\\src\\%dep.${WindowsBuild.id}.product%-binaries.json" 
                    "C:\\Program Files (x86)\\Microsoft Visual Studio\\2022\\BuildTools\\Common7\\Tools\\VsDevCmd.bat" 
                    "x64"
                """.trimIndent()
            }
            conditions {
                equals("dep.${WindowsBuild.id}.product", "fm-suite")
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
        dependency(AbsoluteId("${DslContext.getParameter("delft3d_signing_project_root")}_Sign")) {
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
    }
    requirements {
        exists("env.PYTHON_PATH")
        contains("teamcity.agent.jvm.os.name", "Windows")
    }
})