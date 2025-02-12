package Delft3D.template

import jetbrains.buildServer.configs.kotlin.*
import jetbrains.buildServer.configs.kotlin.buildSteps.*
import jetbrains.buildServer.configs.kotlin.buildFeatures.*
import jetbrains.buildServer.configs.kotlin.triggers.*


object TemplateSetTimeVariable : Template({
    name = "Set time variable template."

    steps {
        script {
            name = "Set time variable step."
            id = "SET_TIME_VARIABLE"

            scriptContent = """
                # Get the commit time of the current commit
                GIT_HEAD_TIME=$(git show -s --format=%ci HEAD)
                
                # Set the TeamCity parameter
                echo "##teamcity[setParameter name='TIME_ISO_8601' value='${GIT_HEAD_TIME}']"
            """.trimIndent()
        }
    }
})
