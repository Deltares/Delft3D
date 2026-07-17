package Delft3D.template

import jetbrains.buildServer.configs.kotlin.Template
import jetbrains.buildServer.configs.kotlin.BuildsLimitingMode

object TemplateBuildConcurrency : Template({
    name = "Build Concurrency Settings"

    // add branch-specific rules as needed
    maxRunningBuildsPerBranch = """
        pull/*:1
    """.trimIndent()

    runningBuildsLimitingMode = BuildsLimitingMode.CANCEL_OLDEST_RUNNING_BUILD
})