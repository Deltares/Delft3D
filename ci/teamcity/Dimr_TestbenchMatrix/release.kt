package testbenchMatrix

import jetbrains.buildServer.configs.kotlin.*
import jetbrains.buildServer.configs.kotlin.buildFeatures.approval

object Release : BuildType({

    name = "Release"
    buildNumberPattern = "%build.revisions.short%"
    maxRunningBuilds = 1

    vcs {
        root(DslContext.settingsRoot)
        branchFilter = """
            +:<default>
            +:all/release/*
        """.trimIndent()
    }

    dependencies {
        snapshot(Linux) {
            reuseBuilds = ReuseBuilds.NO
        }
        snapshot(Windows) {
            reuseBuilds = ReuseBuilds.NO
        }
    }
})