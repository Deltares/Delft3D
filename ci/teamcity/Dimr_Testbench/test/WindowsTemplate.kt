package test

import jetbrains.buildServer.configs.kotlin.*
import jetbrains.buildServer.configs.kotlin.triggers.vcs
import jetbrains.buildServer.configs.kotlin.buildSteps.script

import build.*

object WindowsTemplate : Template({

    name = "Windows Test Template"

    vcs {
        root(DslContext.settingsRoot)
    }

    dependencies {
        snapshot(build.WindowsBuild) {
            onDependencyFailure = FailureAction.CANCEL
            onDependencyCancel = FailureAction.CANCEL
        }
    }

    requirements {
        startsWith("teamcity.agent.jvm.os.name", "Windows 1")
    }
})
