package Delft3D.template

import jetbrains.buildServer.configs.kotlin.*
import jetbrains.buildServer.configs.kotlin.buildSteps.*
import jetbrains.buildServer.configs.kotlin.buildFeatures.*
import jetbrains.buildServer.configs.kotlin.triggers.*
import java.io.File

object TemplateSetTimeVariable : Template({
    name = "Set time variable template."

    params {
        // Environment variables that are overwritten in the build.
        param("env.TIME_ISO_8601", "")
        param("GIT_HEAD_TIME", "")
    }

    steps {
        script {
            name = "Set time variable step."
            id = "SET_TIME_VARIABLE"
            val script = File(DslContext.baseDir, "windows/scripts/setTimeParam.bat")
            scriptContent = Util.readScript(script)
        }
    }
})