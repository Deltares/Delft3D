package Delft3D.linux.apptainerTest

import jetbrains.buildServer.configs.kotlin.*
import Delft3D.template.*

object ApptainerTestDWaves : BuildType({
    templates(TemplateTestApptainer)
    name = "Test D-Waves"
    description = "D-Waves in a DIMR context of a Singularity container, Linux64"

    params {
        param("testbench.configfile", "configs/singularity/dimr/dimr_dwaves_lnx64.xml")
        param("testbench.name", "dimrrelease-dwaves-singularity")
    }

    failureConditions {
        executionTimeoutMin = 240
    }
})
