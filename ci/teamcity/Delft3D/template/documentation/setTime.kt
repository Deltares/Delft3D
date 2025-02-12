package Delft3D.template

import jetbrains.buildServer.configs.kotlin.*
import jetbrains.buildServer.configs.kotlin.buildSteps.*
import jetbrains.buildServer.configs.kotlin.buildFeatures.*
import jetbrains.buildServer.configs.kotlin.triggers.*
import java.io.File

object TemplateSetTimeVariable : Template({
    name = "Set time variable template."

    steps {
        script {
            name = "Set time variable step."
            id = "SET_TIME_VARIABLE"
            val script = File(DslContext.baseDir, "windows/scripts/setTimeParam.bat")
            scriptContent = Util.readScript(script)
        }
    }
})