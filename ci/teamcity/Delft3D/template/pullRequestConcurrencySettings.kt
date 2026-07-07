package Delft3D.template

import jetbrains.buildServer.configs.kotlin.Template
import jetbrains.buildServer.configs.kotlin.buildTypes.BuildsLimitingMode

object TemplatePullRequestConcurrency : Template({
    name = "Pull Request Concurrency Settings"

    maxRunningBuildsPerBranch = "pull/*:1"
    runningBuildsLimitingMode = BuildsLimitingMode.CANCEL_OLDEST_RUNNING_BUILD
})