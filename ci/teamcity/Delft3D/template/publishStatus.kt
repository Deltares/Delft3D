package Delft3D.template

import jetbrains.buildServer.configs.kotlin.*
import jetbrains.buildServer.configs.kotlin.buildFeatures.*

object TemplatePublishStatus : Template({

    name = "Publish Status"
    description = "Send build status to GitHub."

    features {
        if (DslContext.getParameter("enable_commit_status_publisher").lowercase() == "true") {
            commitStatusPublisher {
                enabled = true
                vcsRootExtId = "${DslContext.settingsRoot.id}"
                publisher = github {
                    githubUrl = "https://api.github.com"
                    authType = vcsRoot()
                }
            }
        }
    }
})