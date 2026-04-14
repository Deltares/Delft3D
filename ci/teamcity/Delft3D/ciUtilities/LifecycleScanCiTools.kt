package Delft3D.ciUtilities

import jetbrains.buildServer.configs.kotlin.*
import jetbrains.buildServer.configs.kotlin.triggers.*
import jetbrains.buildServer.configs.kotlin.buildFeatures.dockerRegistryConnections

object LifecycleScanCiTools : BuildType({
    name = "Lifecycle Scan CiTools"
    buildNumberPattern = "%build.vcs.number%"
    
    vcs {
        root(DslContext.settingsRoot)
    }

    steps {
        step {
            id = "LifecycleSyftLinux"
            type = "LifecycleSyftLinux"
            param("scan_target", "ci/python")
        }
        step {
            id = "LifecycleCdxgenLinux"
            type = "LifecycleCdxgenLinux"
            param("scan_target", "ci/python")
        }
        step {
            id = "LifecycleNexusIqLinux"
            type = "LifecycleNexusIqLinux"
            param("nexus_iq_application_id", "delft3d-ci-tools")
            param("nexus_iq_username", "%nexus_iq_username%")
            param("nexus_iq_password", "%nexus_iq_password%")
            param("scan_target", "ci/python")
        }
    }

    features {
        dockerRegistryConnections {
            loginToRegistry = on {
                dockerRegistryId = "PROJECT_EXT_304"
            }
        }
    }

    if (DslContext.getParameter("enable_lifecycle_trigger").lowercase() == "true") {
        triggers {
            vcs {
                branchFilter = "+:<default>"
                perCheckinTriggering = false
            }
        }
    }
})
