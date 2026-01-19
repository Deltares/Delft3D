package Delft3D.template

import jetbrains.buildServer.configs.kotlin.*
import jetbrains.buildServer.configs.kotlin.buildFeatures.*

object TemplateMergeRequest : Template({

    name = "Pull Request"
    description = "Support running pipeline on pull requests."

    features {
        pullRequests {
            provider = github {
                authType = token {
                    token = "%github_deltares-service-account_access_token%"
                }
                filterAuthorRole = PullRequests.GitHubRoleFilter.MEMBER_OR_COLLABORATOR
                filterSourceBranch = "+:*"
                ignoreDrafts = true
            }
        }
    }
})
