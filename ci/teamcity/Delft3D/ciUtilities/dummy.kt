
package Delft3D.ciUtilities

import jetbrains.buildServer.configs.kotlin.*
import jetbrains.buildServer.configs.kotlin.buildFeatures.*
import jetbrains.buildServer.configs.kotlin.buildSteps.*
import jetbrains.buildServer.configs.kotlin.failureConditions.*

import Delft3D.template.*

object DummyBuild: BuildType({
    templates(
        TemplateLinuxAgent,
        TemplateDetermineChangedFiles,
    )

    name = "dummy"
    description = "Echo out some TC variables for testing. "

    vcs {
        root(DslContext.settingsRoot)
        cleanCheckout = true
    }

    outputParams {
        param("changedFiles", "")
    }

    steps {
        script {
        name = "Determine files changed"
        id = "RUNNER_DUMMY"
        scriptContent = """
            git fetch -a 
            ci/teamcity/Delft3D/ciUtilities/scripts/determineChangedFileNames.sh --target-branch f9d2d06df8fa --source-branch 83e2ef92c3c8 --file-pattern="f90" --teamcity            
        """.trimIndent()
        }
    }

})
