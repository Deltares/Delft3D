package testbenchMatrix

import java.io.File
import jetbrains.buildServer.configs.kotlin.*
import jetbrains.buildServer.configs.kotlin.buildFeatures.*
import jetbrains.buildServer.configs.kotlin.buildSteps.*
import jetbrains.buildServer.configs.kotlin.triggers.*

import testbenchMatrix.Trigger

object Windows : BuildType({

    name = "Windows"
    buildNumberPattern = "%dep.${Trigger.id}.build.revisions.short%"

    artifactRules = """
        test\deltares_testbench\data\cases\**\*.pdf      => pdf
        test\deltares_testbench\data\cases\**\*.dia      => logging
        test\deltares_testbench\data\cases\**\*.log      => logging
        test\deltares_testbench\logs                     => logging
        test\deltares_testbench\copy_cases               => copy_cases.zip
    """.trimIndent()

    val filePath = "${DslContext.baseDir}/vars/dimr_testbench_table.csv"
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
        select("configfile", selectedConfigs.joinToString(","),
            allowMultiple = true,
            options = configs,
            display = ParameterDisplay.PROMPT
        )
        param("s3_dsctestbench_accesskey", "j3WxZe0x3LB6cHgg5vp9")
        password("s3_dsctestbench_secret", "credentialsJSON:7e8a3aa7-76e9-4211-a72e-a3825ad1a159")
    }

    features {
        matrix {
            id = "matrix"
            param("configfile", selectedConfigs.map { config ->
                value(config)
            })
        }
        pullRequests {
            id = "merge_request"
            provider = gitlab {
                authType = token {
                    token = "%gitlab_private_access_token%"
                }
                filterSourceBranch = "+:*"
                ignoreDrafts = true
            }
        }
        commitStatusPublisher {
            id = "Delft3D_gitlab"
            enabled = true
            vcsRootExtId = "${DslContext.settingsRoot.id}"
            publisher = gitlab {
                authType = vcsRoot()
            }
        }
        perfmon {
            id = "perfmon"
        }
    }

    steps {
        script {
            name = "Merge main into branch"
            id = "Merge_main_into_branch"

            conditions {
                contains("teamcity.build.branch", "merge-requests")
            }
            workingDir = "."
            scriptContent = """
                git remote add temporary 'https://svc_teamcity_gitdsc:%gitlab_private_access_token%@git.deltares.nl/oss/delft3d.git'
                git fetch temporary refs/merge-requests/*:refs/remotes/temporary/merge-requests/* --quiet
                git checkout temporary/%teamcity.build.branch%/merge
                python -c "import subprocess; commit_id=subprocess.check_output(['git','rev-parse', 'HEAD'], universal_newlines=True).strip(); print('##teamcity[addBuildTag \'merge commit ID: '+commit_id+'\']')"
                git remote remove temporary
            """.trimIndent()
        }

        python {
            name = "Run TestBench.py"
            id = "Run_Testbench"
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
                    --log-level DEBUG
                    --parallel
                    --teamcity
                """.trimIndent()
            }
        }
        script {
            name = "Kill dimr.exe, mpiexec.exe, and hydra_pmi_proxy.exe"
            id = "Kill_processes"
            executionMode = BuildStep.ExecutionMode.ALWAYS
            scriptContent = """
                echo off
                REM taskkill does not automatically kill child processes, and mpiexec spawns some
                call :kill_program dimr.exe
                call :kill_program mpiexec.exe
                call :kill_program hydra_pmi_proxy.exe
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
        dependency(AbsoluteId("Dimr_DimrCollectors_1aDimrCollectorDailyWin64")) {
            snapshot {
                onDependencyFailure = FailureAction.FAIL_TO_START
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