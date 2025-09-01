package Delft3D.ciUtilities

import jetbrains.buildServer.configs.kotlin.*

object SigCi : BuildType({
    name = "Sig Ci"
    
    steps {
        step {
            name = "Upload to sigrid using recipe"
            type = "${DslContext.getParameter("deltares_recipes_root")}_SigridCiUploadTemplate"
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
        }
    }
})
