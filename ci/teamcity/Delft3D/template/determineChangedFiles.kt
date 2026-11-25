package Delft3D.template

import jetbrains.buildServer.configs.kotlin.*
import jetbrains.buildServer.configs.kotlin.buildFeatures.*
import jetbrains.buildServer.configs.kotlin.pipelines.*
import jetbrains.buildServer.configs.kotlin.buildSteps.*
import jetbrains.buildServer.configs.kotlin.failureConditions.*

object TemplateDetermineChangedFiles: Template({

    name = "Determine changed files"
    description = "Determine all files that was "

    params{
         param("changedFiles.sourceBranch", "" )
         param("changedFiles.targetBranch", "main")
         param("changedFiles.filter", "")
    }

    steps {
        script {
        name = "Determine files changed"
        scriptContent = """
            ci/teamcity/Delft3D/ciUtilities/scripts/determineChangedFileNames.sh --target-branch %changedFiles.targetBranch% --source-branch %changedFiles.sourceBranch% --filter="%changedFiles.filter%" > changedFiles.txt
        """.trimIndent()
        }
    }
})
