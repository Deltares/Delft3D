import jetbrains.buildServer.configs.kotlin.*

import build.*
import test.*
import deploy.*

version = "2024.03"

project {

    description = "contact: BlackOps (black-ops@deltares.nl)"

    subProject(Build)
    subProject(Test)
    subProject(Deploy)

    subProjectsOrder = arrayListOf(Build, Test, Deploy)

}