package build

import jetbrains.buildServer.configs.kotlin.*
import jetbrains.buildServer.configs.kotlin.buildFeatures.pullRequests

object WindowsBuild : BuildType({

    name = "Windows"

    features {
        pullRequests {
            id = "merge_request"
            provider = gitlab {
                authType = token {
                    token = "%gitlab_private_access_token%"
                }
                filterSourceBranch = """
                    +:*
                """.trimIndent()
            }
        }
    }
    requirements {
        startsWith("teamcity.agent.jvm.os.name", "Windows 1")
    }
})