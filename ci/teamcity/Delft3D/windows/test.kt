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
        param("container.tag", "test-environment")
        param("product", "unknown")
        checkbox("copy_tested_cases", "false", label = "Copy tested cases", description = "ZIP a copy of the ./data/cases directory (wil include only cases that ran in this job).", display = ParameterDisplay.PROMPT, checked = "true", unchecked = "false")
        checkbox("copy_failed_cases", "false", label = "Copy failed cases", description = "ZIP a copy of the ./data/cases directory (will include only cases that failed this job).", display = ParameterDisplay.PROMPT, checked = "true", unchecked = "false")
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
        step {
            name = "Download artifact from Nexus"
            type = "RawDownloadNexusWindows2"
            executionMode = BuildStep.ExecutionMode.DEFAULT
            param("artifact_path", "/07_day_retention/dimrset/%file_path%")
            param("nexus_repo", "/delft3d-dev")
            param("nexus_username", "%nexus_username%")
            param("download_to", "/downloads")
            param("nexus_password", "%nexus_password%")
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
        // script is necessary to dynamically set the copy-failed-cases depending on the paramter
        script {

            name = "Run TestBench.py"
            id = "RUNNER_testbench"
            workingDir = "test/deltares_testbench/"
                scriptContent = """
                    @echo off

                    echo ===== uv cache check =====
                    uv cache dir
                    dir C:\uv\cache
                    dir /b /s C:\uv\cache 2>nul | find /c /i ".whl"
                    echo ==========================

                    set argsList=--username %s3_dsctestbench_accesskey% ^
                    --password %s3_dsctestbench_secret% ^
                    --compare ^
                    --config configs/%configfile% ^
                    --filter testcase=%case_filter% ^
                    --log-level DEBUG ^
                    --parallel ^
                    --teamcity

                    if "%copy_failed_cases%"=="true" (
                        set argsList=%%argsList%% --copy-failed-cases
                    )

                    rem Fresh venv for this branch; wheels come from the image's uv cache (fast/offline if unchanged)
                    rem DEBUG: --offline forces cache-only; a miss fails loudly instead of silently downloading
                    uv venv
                    if errorlevel 1 exit /b 1
                    uv pip install --offline -r pip/win-requirements.txt
                    if errorlevel 1 exit /b 1

                    call .venv\Scripts\activate.bat
                    python TestBench.py %%argsList%%
            """.trimIndent()

            dockerImage = "containers.deltares.nl/delft3d-dev/test/delft3d-test-environment-windows:%container.tag%"
            dockerImagePlatform = ScriptBuildStep.ImagePlatform.Windows
            dockerPull = true
            dockerRunParameters = """
                --memory %teamcity.agent.hardware.memorySizeMb%m
                --cpus %teamcity.agent.hardware.cpuCount%
                --env UV_LINK_MODE=copy
            """.trimIndent()
        }
        script {
            name = "Copy cases"
            executionMode = BuildStep.ExecutionMode.RUN_ON_FAILURE
            conditions { equals("copy_tested_cases", "true") }
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
