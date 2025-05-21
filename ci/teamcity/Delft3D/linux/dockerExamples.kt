package Delft3D.linux

import jetbrains.buildServer.configs.kotlin.*
import jetbrains.buildServer.configs.kotlin.buildSteps.*
import jetbrains.buildServer.configs.kotlin.failureConditions.*
import Delft3D.linux.*
import Delft3D.template.*

import Trigger

object LinuxRunAllDockerExamples : BuildType({

    description = "Run all Docker example cases for fm/ and all/ merge-requests."

    templates(
        TemplateMergeRequest,
        TemplateDockerRegistry,
        TemplatePublishStatus,
        TemplateMonitorPerformance
    )

    name = "Run all docker examples"
    buildNumberPattern = "%dep.${LinuxBuild.id}.product%: %build.vcs.number%"

    vcs {
        root(DslContext.settingsRoot)
        cleanCheckout = true
    }
    
    steps {
        python {
            name = "Download examples using TestBench.py"
            workingDir = "test/deltares_testbench/"
            environment = venv {
                requirementsFile = "pip/win-requirements.txt"
            }
            command = file {
                filename = "TestBench.py"
                scriptArguments = """
                    --username "%s3_dsctestbench_accesskey%"
                    --password "%s3_dsctestbench_secret%"
                    --reference
                    --config "configs/singularity/dimr/dimr_smoke_test_win64.xml"
                    --filter "testcase=e100_f00_c00"
                    --skip-run
                    --skip-post-processing
                    --log-level DEBUG
                    --parallel
                    --teamcity
                """.trimIndent()
            }
        }
        script {
            name = "Move examples to the right location"
            scriptContent = """
                rm -rf ./examples/dflowfm/08_dflowfm_sequential_dwaves
                mv test/deltares_testbench/data/cases/e100_f00_c00 ./examples/dflowfm/08_dflowfm_sequential_dwaves
            """.trimIndent()
        }
        script {
            name = "Execute run_all_examples_docker.sh"
            scriptContent = """
                cd ./examples/dflowfm/
                ./run-all-examples-docker.sh --image "containers.deltares.nl/delft3d/delft3d-runtime-container:alma8-%build.vcs.number%"
            """.trimIndent()
        }
    }
  
    failureConditions {
        executionTimeoutMin = 180
        errorMessage = true
        failOnText {
            conditionType = BuildFailureOnText.ConditionType.CONTAINS
            pattern = "KILLED BY SIGNAL"
            failureMessage = "Bad termination of one of your application processes"
            reverse = false
        }
    }

    dependencies {
        dependency(Trigger) {
            snapshot {
                onDependencyFailure = FailureAction.FAIL_TO_START
            }
        }
        dependency(LinuxRuntimeContainers) {
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
