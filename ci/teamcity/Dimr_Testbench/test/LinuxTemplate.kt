package test

import jetbrains.buildServer.configs.kotlin.*
import jetbrains.buildServer.configs.kotlin.triggers.vcs
import jetbrains.buildServer.configs.kotlin.buildSteps.script

import build.*

object LinuxTemplate : Template({

    name = "Linux Test Template"

    vcs {
        root(DslContext.settingsRoot)
    }

    dependencies {
        snapshot(build.LinuxBuild) {
            onDependencyFailure = FailureAction.CANCEL
            onDependencyCancel = FailureAction.CANCEL
        }
    }

    requirements {
        equals("teamcity.agent.jvm.os.name", "Linux")
    }
})