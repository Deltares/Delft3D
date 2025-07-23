package Delft3D.linux.apptainerTest

import jetbrains.buildServer.configs.kotlin.*
import Delft3D.template.*

object ApptainerTestDFlowFmDWavesLnx64FriesianInletRealistic : BuildType({
    templates(TemplateTestApptainer)
    name = "Test D-Flow FM, D-Waves (lnx64) - FriesianInletRealistic"

    params {
        param("testbench.configfile", "configs/singularity/dimr/dimr_dflowfm_dwaves_lnx64.xml")
        text("testbench.filter", "e100_f02_c03", description = "e: everything in the config file", allowEmpty = true)
        param("testbench.name", "dimrrelease-dflowfm-dwaves-FriesianInletRealistic-singularity")
    }

    failureConditions {
        executionTimeoutMin = 240
    }
})
