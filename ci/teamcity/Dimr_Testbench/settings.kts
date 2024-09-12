import jetbrains.buildServer.configs.kotlin.*

import testbench.*

version = "2024.03"

project {

    description = "contact: BlackOps (black-ops@deltares.nl)"

    template(LinuxTestTemplate)

    buildType(LinuxAll)
    buildType(LinuxFm)

    buildTypesOrder = arrayListOf(LinuxAll, LinuxFm)

}