package Delft3D.linux.apptainerTest

import jetbrains.buildServer.configs.kotlin.*
import Delft3D.template.*

object ApptainerTestDFlowFmDRtcDWavesLnx64 : BuildType({
    templates(TemplateTestApptainer)
    name = "Test D-Flow FM, D-RTC, D-Waves (lnx64)"
    description = "D-FlowFM, D-Waves and D-RTC in a Dimr context of a Singularity container, Linux64"

    params {
        param("testbench.configfile", "configs/singularity/dimr/dimr_dflowfm_drtc_dwaves_lnx64.xml")
        param("testbench.name", "dimrrelease-dflowfm-drtc-dwaves-singularity")
    }

    failureConditions {
        executionTimeoutMin = 240
    }
})
