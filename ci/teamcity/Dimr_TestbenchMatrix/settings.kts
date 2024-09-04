import java.io.File
import jetbrains.buildServer.configs.kotlin.*
import jetbrains.buildServer.configs.kotlin.buildFeatures.commitStatusPublisher
import jetbrains.buildServer.configs.kotlin.buildFeatures.pullRequests
import jetbrains.buildServer.configs.kotlin.buildSteps.python
import jetbrains.buildServer.configs.kotlin.triggers.VcsTrigger
import jetbrains.buildServer.configs.kotlin.triggers.vcs
import jetbrains.buildServer.configs.kotlin.buildSteps.script

import trigger
import linux
import windows

version = "2024.03"

project {
    description = "contact: BlackOps"

    buildType(trigger)
    buildType(windows)
    buildType(linux)

    buildTypesOrder = arrayListOf(Trigger, Linux, Windows)
}