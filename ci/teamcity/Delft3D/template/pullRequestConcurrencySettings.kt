package Delft3D.template

import jetbrains.buildServer.configs.kotlin.Template
import jetbrains.buildServer.configs.kotlin.BuildsLimitingMode

object TemplatePullRequestConcurrency : Template({
    name = "Pull Request Concurrency Settings"

    maxRunningBuildsPerBranch = """
        pull/*:1
        tc/task/DEVOPSCICD-7-limit-pr-build-concurrency:1
    """.trimIndent()
    runningBuildsLimitingMode = BuildsLimitingMode.CANCEL_OLDEST_RUNNING_BUILD
})