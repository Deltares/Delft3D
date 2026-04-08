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
            id = "LifecycleSyftLinux"
            type = "LifecycleSyftLinux"
        }
        step {
            id = "LifecycleCdxgenLinux"
            type = "LifecycleCdxgenLinux"
        }
        step {
            id = "LifecycleNexusIqLinux"
            type = "LifecycleNexusIqLinux"
            param("nexus_iq_username", "%nexus_iq_username%")
            param("nexus_iq_password", "%nexus_iq_password%")
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
