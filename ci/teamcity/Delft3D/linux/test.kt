package Delft3D.linux

import java.io.File
import jetbrains.buildServer.configs.kotlin.*
import jetbrains.buildServer.configs.kotlin.buildFeatures.*
import jetbrains.buildServer.configs.kotlin.buildSteps.*
import jetbrains.buildServer.configs.kotlin.triggers.*
import Delft3D.template.*
import Delft3D.step.*

import Trigger
import CsvProcessor

object LinuxTest : BuildType({

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
    val processor = CsvProcessor(filePath, "lnx64")
    val lines = File(filePath).readLines()
    val linuxLines = lines.filter { line -> line.contains("lnx64")}
    val configs = linuxLines.map { line ->
        line.split(",")[1]
    }
    val linesForAll = linuxLines.filter { line -> line.split(",")[2] == "TRUE" }
    val selectedConfigs = linesForAll.map { line -> line.split(",")[1] }

    vcs {
        root(DslContext.settingsRoot)
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
                value(config, processor.activeLabels[index])
            })
        }
        dockerSupport {
            cleanupPushedImages = true
            loginToRegistry = on {
                dockerRegistryId = "PROJECT_EXT_133,PROJECT_EXT_81"
            }
        }
    }

    steps {
        mergeTargetBranch {}
        python {
            name = "Run TestBench.py"
            workingDir = "test/deltares_testbench/"
            pythonVersion = customPython {
                executable = "python3.9"
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
                    --override-paths "from[local]=/dimrset,root[local]=/opt,from[engines_to_compare]=/dimrset,root[engines_to_compare]=/opt,from[engines]=/dimrset,root[engines]=/opt"
                """.trimIndent()
            }
            dockerImage = "containers.deltares.nl/delft3d/test/delft3dfm:alma8-%build.vcs.number%"
            dockerImagePlatform = PythonBuildStep.ImagePlatform.Linux
            dockerPull = true
            dockerRunParameters = """
                --rm
                --pull always
                --shm-size 8G
            """.trimIndent()
        }
        dockerCommand {
            name = "Remove container"
            executionMode = BuildStep.ExecutionMode.ALWAYS
            commandType = other {
                subCommand = "rmi"
                commandArgs = "containers.deltares.nl/delft3d/test/delft3dfm:alma8-%build.vcs.number%"
            }
        }
        dockerCommand {
            name = "Prune"
            executionMode = BuildStep.ExecutionMode.ALWAYS
            commandType = other {
                subCommand = "system"
                commandArgs = "prune -f"
            }
        }
        script {
            name = "Copy cases"
            executionMode = BuildStep.ExecutionMode.RUN_ON_FAILURE
            conditions { equals("copy_cases", "true") }
            workingDir = "test/deltares_testbench"
            scriptContent = "cp -r data/cases copy_cases"
        }
    }

    dependencies {
        dependency(Trigger) {
            snapshot {
                onDependencyFailure = FailureAction.FAIL_TO_START
            }
        }
        dependency(LinuxDocker) {
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
