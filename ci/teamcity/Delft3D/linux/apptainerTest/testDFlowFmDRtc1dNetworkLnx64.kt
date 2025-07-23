package Delft3D.linux.apptainerTest

import jetbrains.buildServer.configs.kotlin.*
import Delft3D.template.*

object ApptainerTestDFlowFmDRtc1dNetworkLnx64 : BuildType({
    templates(TemplateTestApptainer)
    name = "Test D-Flow FM, D-RTC (1D network, lnx64)"
    description = "D-FlowFM and D-RTC in a Dimr context of a Singularity container, Linux64"

    params {
        param("testbench.configfile", "configs/singularity/dimr/dimr_dflowfm_drtc_1d-network_lnx64.xml")
        param("testbench.name", "dimrrelease-dflowfm-drtc-1d-network-singularity")
    }

    failureConditions {
        executionTimeoutMin = 240
    }
})
