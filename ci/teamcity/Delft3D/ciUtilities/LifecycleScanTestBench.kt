package Delft3D.ciUtilities

import jetbrains.buildServer.configs.kotlin.*
import jetbrains.buildServer.configs.kotlin.triggers.*
import Delft3D.template.*

object LifecycleScanTestBench : BuildType({
    name = "Lifecycle Scan TestBench"
    buildNumberPattern = "%build.vcs.number%"
    
    vcs {
        root(DslContext.settingsRoot)
    }

    templates(
        TemplateDockerRegistry
    )

    artifactRules = """
            test/deltares_testbench/syft-bom.json => sbom
            test/deltares_testbench/cdxgen-bom.json => sbom
    """.trimIndent()

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
        step {
            id = "LifecycleNexusIqLinux"
            type = "LifecycleNexusIqLinux"
            param("nexus_iq_application_id", "delft3d-testbench")
            param("nexus_iq_username", "%nexus_iq_username%")
            param("nexus_iq_password", "%nexus_iq_password%")
            param("scan_target", "test/deltares_testbench")
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
