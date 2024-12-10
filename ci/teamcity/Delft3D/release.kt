import jetbrains.buildServer.configs.kotlin.*
import jetbrains.buildServer.configs.kotlin.buildFeatures.*

import Delft3D.template.*
import Delft3D.linux.*
import Delft3D.windows.*

object Release : BuildType({

    templates(
        TemplatePublishStatus,
        TemplateMonitorPerformance
    )

    name = "Release"
    buildNumberPattern = "%build.vcs.number%"
    maxRunningBuilds = 1

    vcs {
        root(DslContext.settingsRoot)
        branchFilter = """
            +:<default>
            +:all/release/*
        """.trimIndent()
    }

    dependencies {
        snapshot(LinuxTest) {
            reuseBuilds = ReuseBuilds.NO
        }
        snapshot(WindowsTest) {
            reuseBuilds = ReuseBuilds.NO
        }
    }
})