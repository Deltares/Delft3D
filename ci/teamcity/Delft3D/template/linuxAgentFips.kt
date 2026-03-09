package Delft3D.template

import jetbrains.buildServer.configs.kotlin.*

object TemplateLinuxAgentFips : Template({

    name = "Linux agent (with FIPS)"
    description = "Run on Linux agent (with FIPS)."

    requirements {
        equals("teamcity.agent.jvm.os.name", "Linux")
        exists("env.FIPS")
    }
})