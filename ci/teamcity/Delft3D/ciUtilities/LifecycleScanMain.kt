package Delft3D.ciUtilities

import jetbrains.buildServer.configs.kotlin.*
import jetbrains.buildServer.configs.kotlin.triggers.*
import jetbrains.buildServer.configs.kotlin.buildFeatures.dockerRegistryConnections

object LifecycleScanMain : BuildType({
    name = "Lifecycle Scan Main"
    buildNumberPattern = "%build.vcs.number%"
    
    vcs {
        root(DslContext.settingsRoot)
    }

    steps {
        step {
            id = "LifecycleSyftLinux"
            type = "LifecycleSyftLinux"
            param("exclude_dirs", "test/deltares_testbench/**,ci/python/**")
        }
        step {
            id = "LifecycleCdxgenLinux"
            type = "LifecycleCdxgenLinux"
            param("exclude_dirs", "test/deltares_testbench/**,ci/python/**")
        }
        step {
            id = "LifecycleNexusIqLinux"
            type = "LifecycleNexusIqLinux"
            param("nexus_iq_application_id", "delft3d-main")
            param("nexus_iq_username", "%nexus_iq_username%")
            param("nexus_iq_password", "%nexus_iq_password%")
            param("exclude_dirs", "test/deltares_testbench/**,ci/python/**")
        }
    }

    features {
        dockerRegistryConnections {
            loginToRegistry = on {
                dockerRegistryId = "PROJECT_EXT_304"
            }
        }
    }

    triggers {
        schedule {
            schedulingPolicy = weekly {
                dayOfWeek = ScheduleTrigger.DAY.Wednesday
                hour = 10
                minute = 30
            }

            branchFilter = "+:refs/heads/all/task/SWFABRIEK-63-lifecycle-pipeline-2"

            triggerBuild = always()
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
