package deploy

import jetbrains.buildServer.configs.kotlin.*

object Deploy: Project({

    name = "Deploy"

    buildType(LinuxDeploy)
    buildType(WindowsDeploy)

    buildTypesOrder = arrayListOf(LinuxDeploy, WindowsDeploy)

})