package Delft3D.step

import jetbrains.buildServer.configs.kotlin.*
import jetbrains.buildServer.configs.kotlin.buildSteps.*
import java.io.File

/**
 * Extract the Delft3D JIRA issue ID from the branch name and export it.
 * This only works if the branch name follows the Delft3D git branch name rule.
 * See https://git.deltares.nl/oss/delft3d#branch-naming
 * 
 * By default this step exports the teamcity parameter `env.JIRA_ISSUE_ID` using a teamcity
 * service message. But the parameter name can be overridden by changing the `paramName`
 * variable.
 * 
 * This build step uses `sed -n` to extract the Jira issue ID from the branch name. By default
 * the sed command should work for Delft3D branch naming convention.
 * 
 * If the current branch is the default branch, this step is skipped and nothing is exported.
 * If the current branch is not the default branch and it does not contain a JIRA issue ID, 
 * this step should fail.
 */
class ExportJiraIssueId : ScriptBuildStep {
    var paramName: String = "env.JIRA_ISSUE_ID"

    constructor(init: ExportJiraIssueId.() -> Unit) {
        init()

        conditions {
            exists("teamcity.build.branch")
            equals("teamcity.build.branch.is_default", "false")
        }
        name = "Export the Jira issue id"

        val script = File(DslContext.baseDir, "linux/scripts/exportJiraIssueIdSetParam.sh")
        scriptContent = Util.readScript(script).replace("%param_name%", paramName)
    }
}
  
fun BuildSteps.exportJiraIssueId(init: ExportJiraIssueId.() -> Unit): BuildStep {
    val result = ExportJiraIssueId(init)
    step(result)
    return result
}
