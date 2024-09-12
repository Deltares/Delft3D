package testbench

import jetbrains.buildServer.configs.kotlin.*

import testbench.*

object LinuxAll : BuildType({
    templates(LinuxTestTemplate)
    name = "Linux (all)"

    params {
        param("branch", "all")
    }
})