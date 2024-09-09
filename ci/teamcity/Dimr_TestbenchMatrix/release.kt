package testbenchMatrix

import jetbrains.buildServer.configs.kotlin.*
import jetbrains.buildServer.configs.kotlin.buildFeatures.approval

object Release : BuildType({
    name = "Release"

    buildNumberPattern = "%build.revisions.revision%"
    maxRunningBuilds = 1

    vcs {
        root(DslContext.settingsRoot)
        branchFilter = """
            +:<default>
            +:all/release/*
        """.trimIndent()
    }

    features {
        approval {
            approvalRules = """
                user:svc_dimr_approve_linux
                user:svc_dimr_approve_windows
            """.trimIndent()
        }
    }

    dependencies {
        snapshot(Trigger) {
            reuseBuilds = ReuseBuilds.NO
        }
    }
})