package Delft3D.linux.apptainerTest

import jetbrains.buildServer.configs.kotlin.*
import Delft3D.template.*

object ApptainerTestDFlowFmLnx64 : BuildType({
    templates(TemplateTestApptainer)
    name = "Test D-Flow FM (lnx64)"
    description = "D-FlowFM only tests in a Dimr context of a Singularity container, Linux64"

    artifactRules = """
        test\deltares_testbench\data\cases\*\*.pdf=>pdf
        test\deltares_testbench\data\cases\*\result.txt=>logging
        test\deltares_testbench\data\cases\*\*.dia=>logging
        test\deltares_testbench\data\cases\*\*.log=>logging
        test\deltares_testbench\data\cases\*\*\*.log=>logging
        test\deltares_testbench\logs\*.log =>logging
        test\deltares_testbench\failed\** => failed_cases.zip
        test\deltares_testbench\failed\list.txt => failed_cases.zip
    """.trimIndent()

    params {
        param("testbench.configfile", "configs/singularity/dimr/dimr_dflowfm_lnx64.xml")
        param("testbench.name", "dimrrelease-dflowfm-singularity")
    }

    failureConditions {
        executionTimeoutMin = 180
    }
})
