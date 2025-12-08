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

    artifactRules = "artifacts/**"

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
        powerShell {
            name = "Run TestBench.py in persistent container"
            workingDir = "%system.teamcity.build.checkoutDir%"
            scriptMode = script {
            content = """
                ${'$'}container = "delft3d-test-%teamcity.build.id%"

                ${'$'}innerScript = @'
                Set-Location C:\deltares_testbench

                python TestBench.py `
                    --username '%s3_dsctestbench_accesskey%' `
                    --password '%s3_dsctestbench_secret%' `
                    --compare `
                    --config 'configs/%configfile%' `
                    --filter 'testcase=%case_filter%' `
                    --log-level DEBUG `
                    --parallel `
                    --teamcity

                ${'$'}testExit = ${'$'}LASTEXITCODE

                Write-Host '##teamcity[blockOpened name=''Collecting selected artifacts inside container'']'

                ${'$'}inside = 'C:\artifacts_inside'
                New-Item -Path ${'$'}inside -ItemType Directory -Force | Out-Null

                robocopy 'data\cases' "${'$'}inside\cases" *.diag *.log *.pdf /S /NJH /NJS /NP | Out-Null

                if (Test-Path 'logs') {
                    robocopy 'logs' "${'$'}inside\logs" /E /NJH /NJS /NP | Out-Null
                }

                if ('%copy_cases%' -eq 'true') {
                    Compress-Archive -Path 'data\cases\*' -DestinationPath "${'$'}inside\copy_cases.zip" -Force -CompressionLevel Optimal
                    
                    ${'$'}size = "{0:N1}" -f ((Get-Item "${'$'}inside\copy_cases.zip").Length / 1MB)
                    
                    Write-Host "copy_cases.zip created inside container (${'$'}size MB)"
                }

                Write-Host '##teamcity[blockClosed name=''Collecting selected artifacts inside container'']'

                exit ${'$'}testExit
                '@

                ${'$'}encoded = [Convert]::ToBase64String([Text.Encoding]::Unicode.GetBytes(${'$'}innerScript))

                docker run --name ${'$'}container `
                    -v "%system.teamcity.build.checkoutDir%\test\deltares_testbench\data\engines:C:\deltares_testbench\data\engines" `
                    --memory %teamcity.agent.hardware.memorySizeMb%m `
                    --cpus %teamcity.agent.hardware.cpuCount% `
                    containers.deltares.nl/delft3d-dev/test/delft3d-test-environment-windows:%container.tag% `
                    powershell -NoLogo -EncodedCommand ${'$'}encoded
                """.trimIndent()
            }
        }
        powerShell {
            name = "Extract artifacts from stopped container"
            executionMode = BuildStep.ExecutionMode.ALWAYS
            scriptMode = script {
                content = """
                    ${'$'}container = "delft3d-test-%teamcity.build.id%"
                    ${'$'}target    = "%teamcity.build.checkoutDir%\artifacts"

                    New-Item -Path ${'$'}target -ItemType Directory -Force | Out-Null
                    docker cp "${'$'}container:C:/artifacts_inside/." "${'$'}target\"
                """.trimIndent()
            }
        }
        powerShell {
            name = "Cleanup container"
            executionMode = BuildStep.ExecutionMode.ALWAYS
            scriptMode = script {
                content = """
                    docker rm -f "delft3d-test-%teamcity.build.id%"
                """.trimIndent()
            }
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
