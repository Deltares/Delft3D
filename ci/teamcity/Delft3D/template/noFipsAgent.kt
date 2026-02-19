package Delft3D.template

import jetbrains.buildServer.configs.kotlin.*

object TemplateNoFipsAgent : Template({

    name = "No FIPS agent"
    description = "Don't run this build on a FIPS compliant agent."

    requirements {
        doesNotExist("env.FIPS")
    }
})