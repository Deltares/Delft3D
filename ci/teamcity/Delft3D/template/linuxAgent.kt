package Delft3D.template

import jetbrains.buildServer.configs.kotlin.*

object TemplateLinuxAgent : Template({

    name = "Linux agent (without FIPS)"
    description = "Run on Linux agent (without FIPS)."

    requirements {
        equals("teamcity.agent.jvm.os.name", "Linux")
        doesNotExist("env.FIPS")
    }
})