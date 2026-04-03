package Delft3D.ciUtilities

import jetbrains.buildServer.configs.kotlin.*
import jetbrains.buildServer.configs.kotlin.triggers.*

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
