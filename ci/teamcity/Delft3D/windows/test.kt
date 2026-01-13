package Delft3D.windows

import java.io.File
import jetbrains.buildServer.configs.kotlin.*
import jetbrains.buildServer.configs.kotlin.buildFeatures.*
import jetbrains.buildServer.configs.kotlin.buildSteps.*
import jetbrains.buildServer.configs.kotlin.triggers.*
import jetbrains.buildServer.configs.kotlin.failureConditions.*
import Delft3D.template.*
import Delft3D.step.*

import Trigger
import CsvProcessor

object WindowsTest : BuildType({

    description = "Run TestBench.py on a list of testbench XML files."

    templates(
        TemplateMergeRequest,
        TemplatePublishStatus,
        TemplateMonitorPerformance,
        TemplateDockerRegistry
    )

    name = "Test"
    buildNumberPattern = "%product%: %build.vcs.number%"

    artifactRules = """
        test\deltares_testbench\data\cases\**\*.pdf      => pdf
        test\deltares_testbench\data\cases\**\*.dia      => logging
        test\deltares_testbench\data\cases\**\*.log      => logging
        test\deltares_testbench\logs                     => logging
        test\deltares_testbench\copy_cases               => copy_cases.zip
    """.trimIndent()

    val filePath = "${DslContext.baseDir}/vars/dimr_testbench_table.csv"
    val processor = CsvProcessor(filePath, "win64")
    val lines = File(filePath).readLines()
    val windowsLines = lines.filter { line -> line.contains("win64")}
    val configs = windowsLines.map { line ->
        line.split(",")[1]
    }
    val linesForAll = windowsLines.filter { line -> line.split(",")[2] == "TRUE" }
    val selectedConfigs = linesForAll.map { line -> line.split(",")[1] }

    vcs {
        root(DslContext.settingsRoot)
        cleanCheckout = true
    }

    params {
        select("configfile", processor.activeConfigs.joinToString(","),
            allowMultiple = true,
            options = processor.configs.zip(processor.labels) { config, label -> label to config },
            display = ParameterDisplay.PROMPT
        )
        param("container.tag", "%build.vcs.number%")
        param("product", "unknown")
        checkbox("copy_cases", "false", label = "Copy cases", description = "ZIP a complete copy of the ./data/cases directory.", display = ParameterDisplay.PROMPT, checked = "true", unchecked = "false")
        text("case_filter", "", label = "Case filter", display = ParameterDisplay.PROMPT, allowEmpty = true)
        param("s3_dsctestbench_accesskey", DslContext.getParameter("s3_dsctestbench_accesskey"))
        password("s3_dsctestbench_secret", DslContext.getParameter("s3_dsctestbench_secret"))
        param("file_path", "dimrset_windows_%dep.${WindowsBuild.id}.product%_%build.vcs.number%.zip")

    }

    features {
        matrix {
            id = "matrix"
            param("configfile", processor.activeConfigs.mapIndexed { index, config ->
                value(config, label = processor.activeLabels[index])
            })
        }
    }

    steps {
        mergeTargetBranch {}
        step {
            name = "Download artifact from Nexus"
            type = "RawDownloadNexusWindows"
            executionMode = BuildStep.ExecutionMode.DEFAULT
            param("artifact_path", "/07_day_retention/dimrset/%file_path%")
            param("nexus_repo", "/delft3d-dev")
            param("nexus_username", "%nexus_username%")
            param("download_to", ".")
            param("nexus_password", "%nexus_password%")
            param("nexus_url", "https://artifacts.deltares.nl/repository")
        }
        powerShell {
            name = "Extract artifact"
            enabled = false
            scriptMode = script {
                content = """
                    ${'$'}ErrorActionPreference = "Stop"

                    ${'$'}dest = "test/deltares_testbench/data/engines/teamcity_artifacts/x64"

                    Write-Host "Extracting %file_path% ..."

                    Expand-Archive -Path %file_path% -DestinationPath "temp_extract"

                    robocopy "temp_extract/x64" ${'$'}dest /E /XC /XN /XO
                """.trimIndent()
            }
        }
        python {
            name = "Run TestBench.py"
            id = "RUNNER_testbench"
            workingDir = "test/deltares_testbench/"
            command = file {
                filename = "TestBench.py"
                scriptArguments = """
                    --username "%s3_dsctestbench_accesskey%"
                    --password "%s3_dsctestbench_secret%"
                    --compare
                    --config "configs/%configfile%"
                    --filter "testcase=%case_filter%"
                    --log-level DEBUG
                    --parallel
                    --teamcity
                """.trimIndent()
            }
            dockerImage = "containers.deltares.nl/delft3d-dev/test/delft3d-test-environment-windows:%container.tag%"
            dockerImagePlatform = PythonBuildStep.ImagePlatform.Windows
            dockerPull = true
            dockerRunParameters = "--memory %teamcity.agent.hardware.memorySizeMb%m --cpus %teamcity.agent.hardware.cpuCount%"
        }
        script {
            name = "Copy cases"
            executionMode = BuildStep.ExecutionMode.RUN_ON_FAILURE
            conditions { equals("copy_cases", "true") }
            workingDir = "test/deltares_testbench"
            scriptContent = "xcopy \"data\\cases\" \"copy_cases\" /E /I /Y"
        }
    }

    dependencies {
        dependency(Trigger) {
            snapshot {
                onDependencyFailure = FailureAction.FAIL_TO_START
            }
        }
        dependency(WindowsCollect) {
            snapshot {
                onDependencyFailure = FailureAction.FAIL_TO_START
                onDependencyCancel = FailureAction.CANCEL
            }
            artifacts {
                cleanDestination = true
                artifactRules = "dimrset_x64_*.zip!/x64/**=>test/deltares_testbench/data/engines/teamcity_artifacts/x64"
            }
        }
        dependency(WindowsTestEnvironment) {
            snapshot {
                onDependencyFailure = FailureAction.FAIL_TO_START
                onDependencyCancel = FailureAction.CANCEL
            }
        }
        artifacts(AbsoluteId("Wanda_WandaCore_Wanda4TrunkX64")) {
            buildRule = lastSuccessful()
            cleanDestination = true
            artifactRules = "Bin64.zip!/Release/*.*=>test/deltares_testbench/data/engines/teamcity_artifacts/x64/bin"
        }
        artifacts(AbsoluteId("Wanda_WandaCore_Wanda4TrunkX64")) {
            buildRule = lastSuccessful()
            cleanDestination = true
            artifactRules = "Bin64.zip!/Release/*.*=>test/deltares_testbench/data/engines/teamcity_artifacts/wanda/x64"
        }
    }

    failureConditions {
        executionTimeoutMin = 90
        errorMessage = true
        failOnText {
            conditionType = BuildFailureOnText.ConditionType.CONTAINS
            pattern = "[ERROR  ]"
            failureMessage = "There was an ERROR in the TestBench.py output."
            reverse = false
        }
    }
})
