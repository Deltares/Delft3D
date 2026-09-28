package Delft3D.template

import jetbrains.buildServer.configs.kotlin.*
import jetbrains.buildServer.configs.kotlin.buildFeatures.*

object TemplateMonitorPerformance : Template({

    name = "Monitor performance"
    description = "Record agent performance during the build."

    features {
        perfmon {
        }
    }
})