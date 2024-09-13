package test

import jetbrains.buildServer.configs.kotlin.*
import jetbrains.buildServer.configs.kotlin.buildFeatures.pullRequests

object LinuxWaq : BuildType({

    val branchPrefix = "waq"

    templates(LinuxTemplate)
    name = "Linux: ${branchPrefix}"
    val actualConfigs = Configs.linuxBranchConfigs["${branchPrefix}"] ?: emptyList()

    features {
        matrix {
            id = "matrix"
            param("configfile", actualConfigs.map { config ->
                value(config)
            })
        }
        pullRequests {
            id = "merge_request"
            provider = gitlab {
                authType = token {
                    token = "%gitlab_private_access_token%"
                }
                filterSourceBranch = """
                    -:refs/heads/${branchPrefix}/doc/*
                    +:refs/heads/${branchPrefix}/*"
                    -:*
                """.trimIndent()
            }
        }
    }
})