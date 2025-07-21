package Delft3D.linux.apptainerTest

import jetbrains.buildServer.configs.kotlin.*
import jetbrains.buildServer.configs.kotlin.failureConditions.BuildFailureOnText
import jetbrains.buildServer.configs.kotlin.failureConditions.failOnText
import Delft3D.template.*

object ApptainerTestDPart : BuildType({
    templates(TemplateTestApptainer)
    name = "Test D-Part"
    description = "D-Part in context of a Singularity container, Linux64"

    params {
        param("testbench.configfile", "configs/singularity/dimr/dpart_lnx64.xml")
        param("testbench.name", "dimrrelease-dpart-singularity")
    }

    failureConditions {
        executionTimeoutMin = 240
        failOnText {
            conditionType = BuildFailureOnText.ConditionType.CONTAINS
            pattern = "Download exception occurred"
            reverse = false
        }
    }
})
