package Delft3D.template

import jetbrains.buildServer.configs.kotlin.*
import jetbrains.buildServer.configs.kotlin.buildFeatures.*
import Delft3D.step.*

object TemplateMergeRequest : Template({

    name = "Pull request"
    description = "Merge the target branch so the pipeline can run on a pull request."

    steps {
        mergeTargetBranch {}
        cleanupTemporaryRemote {}
    }

    features {
        pullRequests {
            provider = github {
                authType = token {
                    token = "%github_deltares-service-account_access_token%"
                }
                filterAuthorRole = PullRequests.GitHubRoleFilter.MEMBER
                filterSourceBranch = "+:*"
                ignoreDrafts = true
            }
        }
    }
})
