package Delft3D.template

import jetbrains.buildServer.configs.kotlin.*
import jetbrains.buildServer.configs.kotlin.buildFeatures.*

object TemplateMergeRequest : Template({

    name = "Merge Request"
    description = "Support running pipeline on merge requests."

    features {
        pullRequests {
            provider = gitlab {
                authType = token {
                    token = "%gitlab_private_access_token%"
                }
                filterSourceBranch = "+:*"
                ignoreDrafts = true
            }
        }
    }
})