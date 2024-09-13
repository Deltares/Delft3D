package test

import jetbrains.buildServer.configs.kotlin.*

object Test: Project({

    name = "Test"

    template(LinuxTemplate)
    template(WindowsTemplate)

    buildType(LinuxAll)
    buildType(WindowsAll)

    buildType(LinuxFm)
    buildType(WindowsFm)

    buildType(LinuxRr)
    buildType(WindowsRr)

    buildType(LinuxWaq)
    buildType(WindowsWaq)

    buildType(LinuxPart)
    buildType(WindowsPart)

    buildType(LinuxWave)
    buildType(WindowsWave)

    buildTypesOrder = arrayListOf(LinuxAll, WindowsAll, LinuxFm, WindowsFm, LinuxRr, WindowsRr, LinuxWaq, WindowsWaq, LinuxPart, WindowsPart, LinuxWave, WindowsWave)

})