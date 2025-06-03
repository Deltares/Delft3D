package Delft3D.linux

import jetbrains.buildServer.configs.kotlin.*
import jetbrains.buildServer.configs.kotlin.buildFeatures.*
import jetbrains.buildServer.configs.kotlin.buildSteps.*
import jetbrains.buildServer.configs.kotlin.failureConditions.*
import Delft3D.linux.*
import Delft3D.step.*
import Delft3D.template.*

import Trigger

object LinuxLegacyDockerTest : BuildType({

    description = "Run old Docker tests with new container."

    templates(
        TemplateMergeRequest,
        TemplateDockerRegistry,
        TemplatePublishStatus,
        TemplateMonitorPerformance
    )

    name = "Legacy Docker Test"
    buildNumberPattern = "%dep.${LinuxBuild.id}.product%: %build.vcs.number%"

    vcs {
        root(DslContext.settingsRoot)
        cleanCheckout = true
    }

    params {
        param("env.DOCKER_IMAGE", "containers.deltares.nl/delft3d/delft3d-runtime-container:alma8-%build.vcs.number%")
    }

    features {
        matrix {
           param("configfile", listOf(
              value("docker/dimr/dimr_dflowfm_lnx64.xml"),
              value("docker/dimr/dimr_smoke_test_lnx64.xml")
           ))
        }
    }

    artifactRules = """
        test/deltares_testbench/data/cases/**/*.pdf      => pdf
        test/deltares_testbench/data/cases/**/*.dia      => logging
        test/deltares_testbench/data/cases/**/*.log      => logging
        test/deltares_testbench/logs                     => logging
    """.trimIndent()

    steps {
        mergeTargetBranch {}
        python {
            name = "Run legacy Docker script"
            workingDir = "test/deltares_testbench"
            environment = venv {
                requirementsFile = "pip/lnx-requirements.txt"
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
                    --override-paths "from[local]=/dimrset,root[local]=/opt,from[engines_to_compare]=/dimrset,root[engines_to_compare]=/opt,from[engines]=/dimrset,root[engines]=/opt"
                """.trimIndent()
            }
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
