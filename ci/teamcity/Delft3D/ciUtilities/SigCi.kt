package Delft3D.ciUtilities

import jetbrains.buildServer.configs.kotlin.*
import jetbrains.buildServer.configs.kotlin.triggers.*

object SigCi : BuildType({
    name = "Sig Ci"
    
    vcs {
        root(DslContext.settingsRoot)
    }

    steps {
        step {
            name = "Upload to sigrid using recipe"
            type = "SigridCiUploadTemplate"
            param("sourceDir", ".")
            param("system", "dflow-flexible")
            param("plugin.docker.imagePlatform", "")
            param("targetquality", "3.5")
            param("plugin.docker.imageId", "")
            param("publish", "--publish")
            param("showupload", "--showupload")
            param("sigridciRepoUrl", "https://github.com/Software-Improvement-Group/sigridci")
            param("teamcity.step.phase", "")
            param("plugin.docker.run.parameters", "")
            param("customer", "deltares")
            param("include", "src/engines_gpl")
        }
    }

    if (DslContext.getParameter("enable_sigrid_trigger").lowercase() == "true") {
        triggers {
            vcs {
                branchFilter = "+:<default>"
                perCheckinTriggering = false
            }
        }
    }
})
