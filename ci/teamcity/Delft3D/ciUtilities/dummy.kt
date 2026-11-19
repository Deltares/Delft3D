
package Delft3D.windows

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

    steps {
        script{
            
            scriptContent = """
                #!/usr/bin/env bash
                    
                """.trimIndent()
        }
    }

})
