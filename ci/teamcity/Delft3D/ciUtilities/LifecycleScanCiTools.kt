package Delft3D.ciUtilities

import jetbrains.buildServer.configs.kotlin.*
import jetbrains.buildServer.configs.kotlin.triggers.*
import Delft3D.template.*

object LifecycleScanCiTools : BuildType({
    name = "Lifecycle Scan CiTools"
    buildNumberPattern = "%build.vcs.number%"
    
    vcs {
        root(DslContext.settingsRoot)
    }

    templates(
        TemplateDockerRegistry,
        TemplateBuildConcurrency
    )

    artifactRules = """
            ci/python/syft-bom.json => sbom
            ci/python/cdxgen-bom.json => sbom
    """.trimIndent()

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

    if (DslContext.getParameter("enable_lifecycle_trigger").lowercase() == "true") {
        triggers {
            vcs {
                branchFilter = "+:<default>"
                perCheckinTriggering = false
            }
        }
    }
})
