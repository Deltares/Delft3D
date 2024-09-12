package testbench

import jetbrains.buildServer.configs.kotlin.*

import testbench.*

object LinuxAll : BuildType({
    templates("Dimr_Testbench_LinuxTestTemplate")
    name = "Linux (all)"

    params {
        param("branch", "all")
    }
})