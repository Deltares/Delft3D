package Delft3D.template

import jetbrains.buildServer.configs.kotlin.*

object TemplateLinuxAgent : Template({

    name = "Linux agent"
    description = "Run on Linux agent (with or without FIPS)."

    requirements {
        equals("teamcity.agent.jvm.os.name", "Linux")
    }
})