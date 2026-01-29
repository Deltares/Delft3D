package Delft3D.template

import jetbrains.buildServer.configs.kotlin.*
import jetbrains.buildServer.configs.kotlin.buildFeatures.*

object TemplateMonitorPerformance : Template({

    name = "Monitor Performance"
    description = "Record system performance statistics during build."

    features {
        perfmon {
        }
    }
})