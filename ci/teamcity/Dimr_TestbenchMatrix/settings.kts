import jetbrains.buildServer.configs.kotlin.*

import testbenchMatrix.Trigger
import testbenchMatrix.DockerLinuxBuild
import testbenchMatrix.Linux
import testbenchMatrix.Windows
import testbenchMatrix.Release

version = "2024.03"

project {
    description = "contact: BlackOps (black-ops@deltares.nl)"

    buildType(Trigger)
    buildType(DockerLinuxBuild)
    buildType(Linux)
    buildType(Windows)
    buildType(Release)

    buildTypesOrder = arrayListOf(Trigger, DockerLinuxBuild, Linux, Windows, Release)
}