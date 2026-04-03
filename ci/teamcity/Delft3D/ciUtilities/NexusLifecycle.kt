package Delft3D.ciUtilities

import jetbrains.buildServer.configs.kotlin.*
import jetbrains.buildServer.configs.kotlin.triggers.*
import jetbrains.buildServer.configs.kotlin.buildFeatures.dockerRegistryConnections

object NexusLifecycle : BuildType({
    name = "Nexus Lifecycle"
    buildNumberPattern = "%build.vcs.number%"
    
    vcs {
        root(DslContext.settingsRoot)
    }

    steps {
        step {
            id = "LifecycleSyftWindows"
            type = "LifecycleSyftWindows"
        }
        step {
            id = "LifecycleCdxgenWindows"
            type = "LifecycleCdxgenWindows"
        }
    }

    features {
        dockerRegistryConnections {
            loginToRegistry = on {
                dockerRegistryId = "PROJECT_EXT_304"
            }
        }
    }

    // if (DslContext.getParameter("enable_sigrid_trigger").lowercase() == "true") {
    //     triggers {
    //         vcs {
    //             branchFilter = "+:<default>"
    //             perCheckinTriggering = false
    //         }
    //     }
    // }
})
