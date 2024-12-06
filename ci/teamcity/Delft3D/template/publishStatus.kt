package Delft3D.template

import jetbrains.buildServer.configs.kotlin.*
import jetbrains.buildServer.configs.kotlin.buildFeatures.*

object TemplatePublishStatus : Template({

    name = "Publish Status"
    description = "Send build status to GitLab."

    features {
        if (DslContext.getParameter("environment") == "production") {
            commitStatusPublisher {
                enabled = true
                vcsRootExtId = "${DslContext.settingsRoot.id}"
                publisher = gitlab {
                    authType = vcsRoot()
                }
            }
        }
    }
})