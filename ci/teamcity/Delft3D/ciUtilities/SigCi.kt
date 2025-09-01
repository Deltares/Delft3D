package _Self.buildTypes

import jetbrains.buildServer.configs.kotlin.*

object SigCi : BuildType({
    name = "Sig Ci"

    steps {
        step {
            name = "Upload to sigrid using recipe"
            id = "Upload_to_sigrid_using_recipe"
            type = "Temporary_PersonalBuildsBart_SigridCiUploadTemplate"
            executionMode = BuildStep.ExecutionMode.DEFAULT
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
