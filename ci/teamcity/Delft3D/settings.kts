import jetbrains.buildServer.configs.kotlin.*

import build.*
import build.thirdParty.*
import testbench.*
import release.*

version = "2024.03"

project {

    description = "contact: BlackOps (black-ops@deltares.nl)"

    subProject {
        id("Build")
        name = "Build"
        subProject {
            id("ThirdParty")
            name = "Third Party"
            buildType(DownloadIntelMpi)
            buildTypesOrder = arrayListOf(
                DownloadIntelMpi
            )
        }
        buildType(BuildDockerLinux)
        buildTypesOrder = arrayListOf(
            BuildDockerLinux
        )
    }

    subProject {
        id("Test")
        name = "Test"
        buildType(TestbenchTrigger)
        buildType(TestbenchLinux)
        buildType(TestbenchWindows)
        buildTypesOrder = arrayListOf(
            TestbenchTrigger,
            TestbenchLinux,
            TestbenchWindows
        )
    }

    subProject {
        id("Release")
        name = "Release"
        buildType(Release)
        buildTypesOrder = arrayListOf(
            Release
        )
    }

    subProjectsOrder = arrayListOf(
        RelativeId("Build"),
        RelativeId("Test"),
        RelativeId("Release")
    )

}