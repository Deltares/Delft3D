import jetbrains.buildServer.configs.kotlin.*

import Verschilanalyse.StartVerschilanalyse

version = "2024.03"

project {
    description = "contact: BlackOps (black-ops@deltares.nl)"

    buildType(StartVerschilanalyse)

    buildTypesOrder = arrayListOf(StartVerschilanalyse)
}