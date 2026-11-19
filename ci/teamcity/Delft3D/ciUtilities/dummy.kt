
package Delft3D.windows

import jetbrains.buildServer.configs.kotlin.*
import jetbrains.buildServer.configs.kotlin.buildFeatures.*
import jetbrains.buildServer.configs.kotlin.buildSteps.*
import jetbrains.buildServer.configs.kotlin.failureConditions.*

import Delft3D.template.*

object DummyBuild: BuildType({
    templates(
        TemplateDetermineChangedFiles,
        TemplateLinuxAgent,
    )

    name = "dummy"
    description = "Echo out some TC variables for testing. "


    params {
        param("file_pattern", "")
    }

    steps {
        script{
            scriptContent = """
                #!/usr/bin/env bash
                    
                echo "Hello world"
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
