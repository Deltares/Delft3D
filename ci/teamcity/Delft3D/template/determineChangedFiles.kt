package Delft3D.template

import jetbrains.buildServer.configs.kotlin.*
import jetbrains.buildServer.configs.kotlin.buildFeatures.*
import jetbrains.buildServer.configs.kotlin.buildSteps.script

object TemplateDetermineChangedFiles : Template({

    name = "Determine changed files"
    description = "Get the names of files that are changed compared to base branch, optionally filtered by a glob"


    steps {
        script {
        name = "Determine files changed"
        id = "RUNNER_changedFiles"
        workingDir = "ci/teamcity/Delft3D/ciUtilities/scripts"
        scriptContent = """
            #!/usr/bin/env bash
            ls
            ./determineChangedFileNames.sh --target-branch main --source-branch "tc/task/UNST-10270-detect-changed-files" --file-pattern="f90" --teamcity
            
        """.trimIndent()
        }
    }

    features{
         pullRequests {
            provider = github {
                authType = token {
                    token = "%github_deltares-service-account_access_token%"
                }
                filterSourceBranch = "+:*"
                filterAuthorRole = PullRequests.GitHubRoleFilter.MEMBER_OR_COLLABORATOR
                ignoreDrafts = false
            }
        }
    }
})
