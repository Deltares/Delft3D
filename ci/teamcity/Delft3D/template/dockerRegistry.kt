package Delft3D.template

import jetbrains.buildServer.configs.kotlin.*
import jetbrains.buildServer.configs.kotlin.buildFeatures.*

object TemplateDockerRegistry : Template({

    name = "Docker Registry"
    description = "Login to Docker Registry."

    features {
        dockerSupport {
            loginToRegistry = on {
                dockerRegistryId = "DOCKER_REGISTRY_DELFT3D"
            }
        }
    }
})