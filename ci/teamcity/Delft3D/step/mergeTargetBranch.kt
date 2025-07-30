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
        git remote add temporary "https://deltares-service-account:%github_deltares-service-account_access_token%@github.com/Deltares/delft3d.git"
        git fetch temporary refs/pull/*:refs/remotes/temporary/pull/* --quiet
        git checkout temporary/%teamcity.build.branch%/merge
        git remote remove temporary
    """.trimIndent()
    return result
}
