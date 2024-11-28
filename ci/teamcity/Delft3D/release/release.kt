package release

import jetbrains.buildServer.configs.kotlin.*
import jetbrains.buildServer.configs.kotlin.buildFeatures.approval

import testbench.*

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
        snapshot(TestbenchLinux) {
            reuseBuilds = ReuseBuilds.NO
        }
        snapshot(TestbenchWindows) {
            reuseBuilds = ReuseBuilds.NO
        }
    }
})