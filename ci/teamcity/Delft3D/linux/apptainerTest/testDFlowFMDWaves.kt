package Delft3D.linux.apptainerTest

import jetbrains.buildServer.configs.kotlin.*
import Delft3D.template.*

object ApptainerTestDFlowFMDWaves : BuildType({
    templates(TemplateTestApptainer)
    name = "Test D-Flow FM, D-Waves (lnx64)"
    description = "D-FlowFM and D-Waves in a Dimr context of a Singularity container, Linux64"

    params {
        param("testbench.configfile", "configs/singularity/dimr/dimr_dflowfm_dwaves_lnx64.xml")
        text("testbench.filter", "e100:maxruntime=<3000.0", description = "e: everything in the config file", allowEmpty = true)
        param("testbench.name", "dimrrelease-dflowfm-dwaves-singularity")
    }

    failureConditions {
        executionTimeoutMin = 240
    }
})
