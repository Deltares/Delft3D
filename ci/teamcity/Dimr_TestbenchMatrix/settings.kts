import java.io.File
import jetbrains.buildServer.configs.kotlin.*
import jetbrains.buildServer.configs.kotlin.buildFeatures.commitStatusPublisher
import jetbrains.buildServer.configs.kotlin.buildFeatures.pullRequests
import jetbrains.buildServer.configs.kotlin.buildSteps.python
import jetbrains.buildServer.configs.kotlin.triggers.VcsTrigger
import jetbrains.buildServer.configs.kotlin.triggers.vcs
import jetbrains.buildServer.configs.kotlin.buildSteps.script

import testbenchMatrix.Trigger
import testbenchMatrix.Linux
import testbenchMatrix.Windows
import testbenchMatrix.LinuxApprove
import testbenchMatrix.WindowsApprove
import testbenchMatrix.Release

version = "2024.03"

project {
    description = "contact: BlackOps (black-ops@deltares.nl)"

    buildType(Trigger)
    buildType(Linux)
    buildType(Windows)
    buildType(LinuxApprove)
    buildType(WindowsApprove)
    buildType(Release)

    buildTypesOrder = arrayListOf(Trigger, Linux, Windows, LinuxApprove, WindowsApprove, Release)
}