import jetbrains.buildServer.configs.kotlin.*

import Delft3D.*
import Delft3D.linux.*
import Delft3D.linux.thirdParty.*
import Delft3D.windows.*
import Delft3D.template.*

version = "2024.03"

project {

    description = "contact: BlackOps (black-ops@deltares.nl)"

    template(TemplateMergeRequest)
    template(TemplateMergeTarget)
    template(TemplatePublishStatus)
    template(TemplateMonitorPerformance)

    subProject {
        id("Linux")
        name = "Linux"
        subProject {
            id("LinuxThirdParty")
            name = "Third Party"
            buildType(LinuxThirdPartyDownloadIntelMpi)
            buildTypesOrder = arrayListOf(
                LinuxThirdPartyDownloadIntelMpi
            )
        }
        buildType(LinuxBuild)
        buildType(LinuxCollect)
        buildType(LinuxDocker)
        buildType(LinuxTest)
        buildTypesOrder = arrayListOf(
            LinuxBuild,
            LinuxCollect,
            LinuxDocker,
            LinuxTest
        )
    }

    subProject {
        id("Windows")
        name = "Windows"

        buildType(WindowsBuild)
        buildType(WindowsCollect)
        buildType(WindowsTest)
        buildTypesOrder = arrayListOf(
            WindowsBuild,
            WindowsCollect,
            WindowsTest
        )
    }

    subProjectsOrder = arrayListOf(
        RelativeId("Linux"),
        RelativeId("Windows")
    )

    buildType(Trigger)
    buildType(Release)

    buildTypesOrder = arrayListOf(
        Trigger,
        Release
    )

}