package Delft3D.template

import jetbrains.buildServer.configs.kotlin.Template
import jetbrains.buildServer.configs.kotlin.BuildsLimitingMode

object TemplateBuildConcurrency : Template({
    name = "Build Concurrency Settings"

    // add branch-specific rules as needed
    // do not forget to remove dev branch!!
    maxRunningBuildsPerBranch = """
        pull/*:1
        tc/task/DEVOPSCICD-7-limit-pr-build-concurrency:1
    """.trimIndent()

    runningBuildsLimitingMode = BuildsLimitingMode.CANCEL_OLDEST_RUNNING_BUILD
})