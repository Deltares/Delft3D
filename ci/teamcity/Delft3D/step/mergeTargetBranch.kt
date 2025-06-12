package Delft3D.step

import jetbrains.buildServer.configs.kotlin.*
import jetbrains.buildServer.configs.kotlin.buildSteps.*

fun BuildSteps.mergeTargetBranch(init: ScriptBuildStep.() -> Unit): ScriptBuildStep {
    val result = ScriptBuildStep(init)
    step(result)
    result.name = "Merge target into branch"
    result.conditions {
        contains("teamcity.build.branch", "merge-requests")
    }
    result.workingDir = "."
    result.scriptContent = """
        git --version
        git remote add temporary "https://svc_teamcity_gitdsc:%gitlab_private_access_token%@git.deltares.nl/oss/delft3d.git"
        git fetch temporary refs/merge-requests/*:refs/remotes/temporary/merge-requests/* --quiet
        git checkout temporary/%teamcity.build.branch%/merge
        git remote remove temporary
    """.trimIndent()
    return result
}
