package Delft3D.dvc

import jetbrains.buildServer.configs.kotlin.*
import jetbrains.buildServer.configs.kotlin.buildFeatures.*
import jetbrains.buildServer.configs.kotlin.buildSteps.*
import jetbrains.buildServer.configs.kotlin.failureConditions.*
import jetbrains.buildServer.configs.kotlin.triggers.*
import Delft3D.template.*
import Delft3D.step.*

object DvcGarbageCollection : BuildType({

    description = "Routinely run garbage collection for DVC files"

    templates(
        TemplatePublishStatus,
        TemplateMonitorPerformance
    )

    name = "Garbage Collection"

    allowExternalStatus = false

    params {
        param("s3_delft3d_testbench_accesskey", DslContext.getParameter("s3_delft3d_testbench_accesskey"))
        password("s3_delft3d_testbench_secret", DslContext.getParameter("s3_delft3d_testbench_secret"))

    }

    vcs {
	root(DslContext.settingsRoot)
	cleanCheckout = true
    }

    steps {
	script {
            name = "Run garbage collector"
	    id = "DvcGcExecute"
	    scriptContent = """
                set -e 
                git fetch --unshallow 
        		uv venv --python=3.12 .venv
                uv pip sync test/deltares_testbench/pip/lnx-dev-requirements.txt
                source .venv/bin/activate
                export AWS_ACCESS_KEY_ID='%s3_delft3d_testbench_accesskey%'
                export AWS_SECRET_ACCESS_KEY='%s3_delft3d_testbench_secret%'
                dvc config -l
                dvc fetch -aT -R 
                dvc gc -aT --cloud -f -v -r storage
                """
        }
    }


    triggers {
        schedule {
            schedulingPolicy = weekly {
                dayOfWeek = ScheduleTrigger.DAY.Sunday
                hour = 20
                minute = 0
            }
            branchFilter = "+:<default>"
            triggerBuild = always()
            withPendingChangesOnly = false
            param("trigger.type", "schedule")
        }
    }


    failureConditions {
        executionTimeoutMin = 180
        errorMessage = true
    }

    requirements {
        equals("teamcity.agent.jvm.os.name", "Linux")
    }
})
