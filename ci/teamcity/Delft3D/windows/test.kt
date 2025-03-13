package Delft3D.windows

import java.io.File
import jetbrains.buildServer.configs.kotlin.*
import jetbrains.buildServer.configs.kotlin.buildFeatures.*
import jetbrains.buildServer.configs.kotlin.buildSteps.*
import jetbrains.buildServer.configs.kotlin.triggers.*
import Delft3D.template.*
import Delft3D.step.*

import Trigger
import CsvProcessor

object WindowsTest : BuildType({

    description = "Run TestBench.py on a list of testbench XML files. (This is not containerized as we'd need more Docker-pool agents.)"

    templates(
        TemplateMergeRequest,
        TemplatePublishStatus,
        TemplateMonitorPerformance
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
        param("product", "unknown")
        checkbox("copy_cases", "false", label = "Copy cases", description = "ZIP a complete copy of the ./data/cases directory.", display = ParameterDisplay.PROMPT, checked = "true", unchecked = "false")
        text("case_filter", "", label = "Case filter", display = ParameterDisplay.PROMPT, allowEmpty = true)
        param("s3_dsctestbench_accesskey", DslContext.getParameter("s3_dsctestbench_accesskey"))
        password("s3_dsctestbench_secret", "credentialsJSON:7e8a3aa7-76e9-4211-a72e-a3825ad1a160")
        
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
        python {
            name = "Run TestBench.py"
            workingDir = "test/deltares_testbench/"
            environment = venv {
                requirementsFile = "pip/win-requirements.txt"
            }
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
        }
        script {
            name = "Copy cases"
            executionMode = BuildStep.ExecutionMode.RUN_ON_FAILURE
            conditions { equals("copy_cases", "true") }
            workingDir = "test/deltares_testbench"
            scriptContent = "cp -r data/cases copy_cases"
        }
        script {
            name = "Kill dimr.exe, mpiexec.exe, and hydra_pmi_proxy.exe"
            executionMode = BuildStep.ExecutionMode.ALWAYS
            scriptContent = """
                echo off
                REM taskkill does not automatically kill child processes, and mpiexec spawns some
                call :kill_program dimr.exe
                call :kill_program mpiexec.exe
                call :kill_program hydra_pmi_proxy.exe
                call :kill_program mormerge.exe
                call :kill_program d_hydro.exe
                set errorlevel=0
                goto :eof
                
                :kill_program
                set program_name=%~1
                tasklist | find /i "%%program_name%%" > NUL 2>&1
                if errorlevel 1 (
                    echo %%program_name%% is not running.
                ) else (
                    echo Executing 'taskkill /f /im %%program_name%% /t'
                    taskkill /f /im %%program_name%% /t > NUL 2>&1
                )
                exit /b 0
            """.trimIndent()
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

    requirements {
        startsWith("teamcity.agent.jvm.os.name", "Windows 1")
    }
})
