package Delft3D.ciUtilities

import jetbrains.buildServer.configs.kotlin.*
import jetbrains.buildServer.configs.kotlin.triggers.*
import jetbrains.buildServer.configs.kotlin.buildFeatures.dockerRegistryConnections
import jetbrains.buildServer.configs.kotlin.buildSteps.script

object LifecycleScanMain : BuildType({
    name = "Lifecycle Scan Main"
    buildNumberPattern = "%build.vcs.number%"
    
    vcs {
        root(DslContext.settingsRoot)
    }
    
    params {
        param("nexus_iq_application_id", "Delft3D-main")
    }
    
    steps {   
        script {
            name = "Compute Nexus IQ application ID"
            scriptContent = """
                set -euo pipefail

                echo "Branch: ${'$'}BRANCH"

                VERSION="${'$'}{BRANCH##*/}"

                LIFECYCLE_ID="Delft3D-${'$'}VERSION"

                echo "Lifecycle ID: ${'$'}LIFECYCLE_ID"

                echo "##teamcity[setParameter name='nexus_iq_application_id' value='${'$'}LIFECYCLE_ID']"
            """.trimIndent()
        }
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
            param("nexus_iq_application_id", "%nexus_iq_application_id%")
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
                hour = 13
                minute = 25
            }

            branchFilter = "+:all/task/SWFABRIEK-63-lifecycle-pipeline-2"

            triggerBuild = always()
            withPendingChangesOnly = false
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
