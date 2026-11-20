
package Delft3D.ciUtilities

import jetbrains.buildServer.configs.kotlin.*
import jetbrains.buildServer.configs.kotlin.buildFeatures.*
import jetbrains.buildServer.configs.kotlin.buildSteps.*
import jetbrains.buildServer.configs.kotlin.failureConditions.*

import Delft3D.template.*

object DummyBuild: BuildType({
    templates(
        TemplateLinuxAgent,
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
            ci/teamcity/Delft3D/ciUtilities/scripts/determineChangedFileNames.sh --target-branch main --source-branch "tc/task/UNST-10270-detect-changed-files" --file-pattern="f90" --teamcity            
        """.trimIndent()
        }
    }

})
