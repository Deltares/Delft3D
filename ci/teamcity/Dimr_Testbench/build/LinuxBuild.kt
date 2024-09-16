package build

import jetbrains.buildServer.configs.kotlin.*
import jetbrains.buildServer.configs.kotlin.buildFeatures.pullRequests

object LinuxBuild : BuildType({

    name = "Linux"

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
        equals("teamcity.agent.jvm.os.name", "Linux")
    }
})