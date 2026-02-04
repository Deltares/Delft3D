package Delft3D.step

import jetbrains.buildServer.configs.kotlin.*
import jetbrains.buildServer.configs.kotlin.buildSteps.*

fun BuildSteps.mergeTargetBranch(init: ScriptBuildStep.() -> Unit): ScriptBuildStep {
    val result = ScriptBuildStep(init)
    step(result)
    result.name = "Merge target into branch"
    result.conditions {
        contains("teamcity.build.branch", "pull")
    }
    result.workingDir = "."
    result.scriptContent = """
        git --version
        git config user.name "Delft3D CI"
        git config user.email "black-ops@deltares.nl"
        git remote add temporary "https://deltares-service-account:%github_deltares-service-account_access_token%@github.com/Deltares/delft3d.git"
        git fetch temporary refs/heads/%teamcity.pullRequest.target.branch%:refs/remotes/temporary/%teamcity.pullRequest.target.branch% --quiet
        git merge temporary/%teamcity.pullRequest.target.branch%
    """.trimIndent()
    return result
}

fun BuildSteps.cleanupTemporaryRemote(init: ScriptBuildStep.() -> Unit): ScriptBuildStep {
    val result = ScriptBuildStep(init)
    step(result)
    result.name = "Cleanup temporary remote"
    result.conditions {
        contains("teamcity.build.branch", "pull")
    }
    result.workingDir = "."
    result.scriptContent = """
        git remote remove temporary
    """.trimIndent()
    return result
}