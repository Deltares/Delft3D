package Delft3D.linux.apptainerTest

import jetbrains.buildServer.configs.kotlin.*
import Delft3D.template.*

object ApptainerTestApptainerSmoke : BuildType({
    templates(TemplateTestApptainer)
    name = "DIMR singularity smoke test"

    artifactRules = """
        test\deltares_testbench\data\cases\**\*.pdf=>pdf
        test\deltares_testbench\data\cases\*\result.txt => logging
        test\deltares_testbench\data\cases\**\*.dia => logging
        test\deltares_testbench\data\cases\**\*.log => logging
        test\deltares_testbench\logs\**\*.log => logging
        test\deltares_testbench\failed\** => failed_cases.zip
        test\deltares_testbench\failed\list.txt => failed_cases.zip
    """.trimIndent()

    params {
        param("testbench.configfile", "configs/singularity/dimr/dimr_smoke_test_lnx64.xml")
        text("testbench.filter", "", description = "e: everything in the config file", allowEmpty = true)
        param("testbench.name", "dimr_smoke_test_lnx64")
    }

    failureConditions {
        executionTimeoutMin = 240
    }
})
