package build

import jetbrains.buildServer.configs.kotlin.*

object Build: Project({

    name = "Build"

    buildType(LinuxBuild)
    buildType(WindowsBuild)

    buildTypesOrder = arrayListOf(LinuxBuild, WindowsBuild)

})