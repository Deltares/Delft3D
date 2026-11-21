package Delft3D.template

import jetbrains.buildServer.configs.kotlin.*
import jetbrains.buildServer.configs.kotlin.buildFeatures.*
import jetbrains.buildServer.configs.kotlin.buildSteps.script

object TemplateDetermineChangedFiles : Template({

    name = "Determine changed files"
    description = "Get the names of files that are changed compared to base branch, optionally filtered by a glob"

    outputParams {
        param("changedFiles", "%changedFiles%")
    }

    steps {
        script {
        name = "Determine files changed"
        id = "RUNNER_changedFiles"
        scriptContent = """
            ci/teamcity/Delft3D/ciUtilities/scripts/determineChangedFileNames.sh --target-branch main --source-branch "tc/task/UNST-10270-detect-changed-files" --file-pattern="f90" --teamcity            
        """.trimIndent()
        }
    }

})
