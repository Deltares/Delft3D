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

version = "2024.03"

project {
    description = "contact: BlackOps"

    buildType(Trigger)
    buildType(Windows)
    buildType(Linux)

    buildTypesOrder = arrayListOf(Trigger, Linux, Windows)
}