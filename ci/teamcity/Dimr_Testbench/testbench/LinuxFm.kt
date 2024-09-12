package testbench

import jetbrains.buildServer.configs.kotlin.*

object LinuxFm : BuildType({

    name = "Linux: FM"
    templates(LinuxTestTemplate)

    params {
        param("branch", "fm")
    }
})