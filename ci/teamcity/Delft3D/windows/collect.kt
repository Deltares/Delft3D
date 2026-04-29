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
        TemplateMonitorPerformance,
        TemplateDockerRegistry
    )

    name = "Collect"
    buildNumberPattern = "%dep.${WindowsBuild.id}.product%: %build.vcs.number%"

    allowExternalStatus = true
    artifactRules = """
        x64 => dimrset_x64_%build.vcs.number%.zip!x64
        dimrset_version_x64.txt => dimrset_x64_%build.vcs.number%.zip!x64
        dimrset_version*txt => version
    """.trimIndent()

    params {
        param("file_path", "dimrset_windows_%dep.${WindowsBuild.id}.product%_%build.vcs.number%.zip")
        param("container.tag", "vs2022-intel2024")
    }

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
            dockerImage = "containers.deltares.nl/delft3d-dev/delft3d-buildtools-windows:%container.tag%"
            dockerImagePlatform = PythonBuildStep.ImagePlatform.Windows
            dockerPull = true
            conditions {
                equals("dep.${WindowsBuild.id}.product", "fm-suite")
            }
        }      
        powerShell {
            name = "Copy DLLs"
            scriptMode = script {
            content = """
                Copy-Item "C:\Windows\System32\vcomp140.dll" -Destination "x64\lib" -Force
                Copy-Item "C:\Windows\System32\ucrtbased.dll" -Destination "x64\lib" -Force
            """.trimIndent()
            }
            dockerImage = "containers.deltares.nl/delft3d-dev/delft3d-buildtools-windows:%container.tag%"
            dockerImagePlatform = PowerShellBuildStep.ImagePlatform.Windows
            dockerPull = true
        }
        python {
            name = "Generate list of version numbers (from what-strings)"
            command = file {
                filename = """ci/python/ci_tools/dimrset_delivery/scripts/list_all_what_strings.py"""
                scriptArguments = "--srcdir x64 --output dimrset_version_x64.txt"
            }
            dockerImage = "containers.deltares.nl/delft3d-dev/delft3d-buildtools-windows:%container.tag%"
            dockerImagePlatform = PythonBuildStep.ImagePlatform.Windows
            dockerPull = true
        }
        python {
            name = "Verify (un)signed binaries and directory structure"
            command = file {
                filename = "ci/python/ci_tools/dimrset_delivery/validate_signing.py"
                scriptArguments = """
                    "ci\\python\\ci_tools\\dimrset_delivery\\%dep.${WindowsBuild.id}.product%-binaries.json" 
                    "C:\\Program Files\\Microsoft Visual Studio\\2022\\Professional\\Common7\\Tools\\VsDevCmd.bat"
                    "x64"
                """.trimIndent()
            }
            dockerImage = "containers.deltares.nl/delft3d-dev/delft3d-buildtools-windows:%container.tag%"
            dockerImagePlatform = PythonBuildStep.ImagePlatform.Windows
            dockerPull = true
            conditions {
                matches("dep.${WindowsBuild.id}.product", "(fm-suite|all-testbench)")
                matches("dep.${WindowsBuild.id}.build_type", "Release")
            }
        }
        powerShell {
            name = "Prepare artifact to upload"
            scriptMode = script {
                content = """
                    ${'$'}ErrorActionPreference = "Stop"

                    Write-Host "Creating %file_path% ..."

                    Compress-Archive -Path "x64", "dimrset_version_x64.txt" -DestinationPath %file_path% -Force

                    Write-Host "ZIP created: %file_path%"
                """.trimIndent()
            }
            dockerImage = "containers.deltares.nl/delft3d-dev/delft3d-buildtools-windows:%container.tag%"
            dockerImagePlatform = PowerShellBuildStep.ImagePlatform.Windows
            dockerPull = true
        }
        step {
            name = "Upload artifact to Nexus"
            type = "RawUploadNexusWindows2"
            executionMode = BuildStep.ExecutionMode.DEFAULT
            param("file_path", "%file_path%")
            param("nexus_username", "%nexus_username%")
            param("nexus_password", "%nexus_password%")
            param("nexus_repo", "/delft3d-dev")
            param("nexus_url", "https://artifacts.deltares.nl/repository")
            param("retention_period", "07_day_retention")
            param("target_path", "/dimrset/%file_path%")
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
                    ?:oss_artifacts_x64_*.zip!/x64/share/** => x64/share
                """.trimIndent()
            }
        }
    }
    requirements {
        contains("teamcity.agent.jvm.os.name", "Windows")
    }
})
