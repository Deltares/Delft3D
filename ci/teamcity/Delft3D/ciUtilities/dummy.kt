
package Delft3D.ciUtilities

import jetbrains.buildServer.configs.kotlin.*
import jetbrains.buildServer.configs.kotlin.buildFeatures.*
import jetbrains.buildServer.configs.kotlin.pipelines.*
import jetbrains.buildServer.configs.kotlin.buildSteps.*
import jetbrains.buildServer.configs.kotlin.failureConditions.*

import Delft3D.template.*

object DummyBuild: BuildType({
    templates(
        TemplateLinuxAgent,
        TemplateMergeRequest,
        TemplateDetermineChangedFiles,
    )

    name = "dummy"
    description = "Echo out some TC variables for testing. "

    vcs {
        root(DslContext.settingsRoot)
        cleanCheckout = true
    }

    params{
         text("changedFiles.sourceBranch",
              "",
              label = "Source Branch",
              description = "The branch that will compared to target",
              display = ParameterDisplay.PROMPT
          )
         text("changedFiles.targetBranch",
              "main",
              label = "Target Branch",
              description = "The branch that will be compared to base",
              display = ParameterDisplay.PROMPT
          )
         text("changedFiles.filter",
              "",
              label = "File filter",
              description = "Grep pattern to filter the changed files, leave empty to include all files",
              display = ParameterDisplay.PROMPT,
              allowEmpty=true
          )
    }

    steps {
        script {
        name = "Determine files changed"
        id = "RUNNER_DUMMY"
        scriptContent = """
            cat changedFiles.txt
        """.trimIndent()
        }
    }

    // artifactRules = "**/changedFiles.txt => changedFiles"



})
