package Delft3D.linux

import jetbrains.buildServer.configs.kotlin.*
import jetbrains.buildServer.configs.kotlin.buildFeatures.*
import jetbrains.buildServer.configs.kotlin.buildSteps.*
import jetbrains.buildServer.configs.kotlin.failureConditions.*
import Delft3D.template.*
import Delft3D.step.*

object LinuxPyTest : BuildType({
    templates(
        TemplateMergeRequest,
        TemplatePublishStatus,
        TemplateMonitorPerformance
    )
 
    name = "Pytest"
    buildNumberPattern = "%build.vcs.number%"
    description = "Run and publish pytests, check format using ruff."

    vcs {
        root(DslContext.settingsRoot)
        cleanCheckout = true
    }

    steps {
        python {
            pythonVersion = customPython {
                executable = "python3.9"
            }
            command = pytest {
                installToolPackage = false
                isTestReportingEnabled = false
            }
            dockerImage = "containers.deltares.nl/delft3d/test/testbench:testbench-%build.vcs.number%"
            dockerRunParameters = "--rm --pull always -v ./logs:/data/logs -w /data"
        }
        script {
            name = "Ruff format check"
            scriptContent = """
                ruff format --check .
                ruff check --output-format=junit --output-file=logs/ruff_check_results.xml --select F4,F5,F6,F7,W,I
            """.trimIndent()
            dockerImage = "containers.deltares.nl/delft3d/test/testbench:testbench-%build.vcs.number%"
            dockerImagePlatform = ScriptBuildStep.ImagePlatform.Linux
            dockerPull = true
            dockerRunParameters = "--rm -v ./logs:/data/logs -w /data"
        }
    }

    failureConditions {
        executionTimeoutMin = 30
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

    features {
        dockerSupport {
            loginToRegistry = on {
                dockerRegistryId = "DOCKER_REGISTRY_DELFT3D"
            }
        }
        dockerSupport {
            loginToRegistry = on {
                dockerRegistryId = "PROJECT_EXT_133,PROJECT_EXT_81"
            }
        }
        xmlReport {
            reportType = XmlReport.XmlReportType.JUNIT
            rules = """
                +:logs/pytest_results.xml
                +:logs/ruff_check_results.xml
            """.trimIndent()
        }
    }

    dependencies {
        dependency(LinuxBuildTestbenchContainer) {
            snapshot {
                onDependencyFailure = FailureAction.FAIL_TO_START
                onDependencyCancel = FailureAction.CANCEL
            }
        }
    }

    requirements {
        equals("teamcity.agent.jvm.os.name", "Linux")
    }

})