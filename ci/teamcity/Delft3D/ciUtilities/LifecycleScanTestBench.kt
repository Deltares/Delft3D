package Delft3D.ciUtilities

import jetbrains.buildServer.configs.kotlin.*
import jetbrains.buildServer.configs.kotlin.triggers.*
import jetbrains.buildServer.configs.kotlin.buildFeatures.dockerRegistryConnections

object LifecycleScanTestBench : BuildType({
    name = "Lifecycle Scan TestBench"
    buildNumberPattern = "%build.vcs.number%"
    
    vcs {
        root(DslContext.settingsRoot)
    }

    steps {
        step {
            id = "LifecycleSyftLinux"
            type = "LifecycleSyftLinux"
            param("scan_target", "test/deltares_testbench")
        }
        step {
            id = "LifecycleCdxgenLinux"
            type = "LifecycleCdxgenLinux"
            param("scan_target", "test/deltares_testbench")
        }
        // step {
        //     id = "LifecycleNexusIqLinux"
        //     type = "LifecycleNexusIqLinux"
        //     param("nexus_iq_username", "%nexus_iq_username%")
        //     param("nexus_iq_password", "%nexus_iq_password%")
        //     param("exclude_dirs", "test/**,ci/**,doc/**,tools/**")
        // }
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
