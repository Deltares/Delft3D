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
        pr_num=%teamcity.build.branch%
        current_head=$(git rev-parse temporary/pull/$pr_num/head)
        git checkout temporary/$pr_num/merge
        pr_parent=$(git rev-parse HEAD^2)
        if [ "$pr_parent" != "$current_head" ]; then
            echo "Merge conflict detected: merge ref is outdated."
            exit 1
        fi
        git remote remove temporary
    """.trimIndent()
    return result
}
