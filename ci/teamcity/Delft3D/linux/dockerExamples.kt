package Delft3D.linux

import jetbrains.buildServer.configs.kotlin.*
import jetbrains.buildServer.configs.kotlin.buildSteps.*
import jetbrains.buildServer.configs.kotlin.failureConditions.*
import Delft3D.linux.*
import Delft3D.template.*

object LinuxRunAllDockerExamples : BuildType({
    name = "Run all docker examples"

    templates(
        TemplateDockerRegistry,
        TemplateMonitorPerformance
    )

    vcs {
        root(DslContext.settingsRoot)
        cleanCheckout = true
    }
    
    steps {
        script {
            name = "Execute run_all_examples_docker.sh"
            scriptContent = """
                cd ./examples/dflowfm/
                ./run-all-examples-docker.sh --image "containers.deltares.nl/delft3d/delft3dfm:alma8-%build.vcs.number%"
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
