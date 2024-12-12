import jetbrains.buildServer.configs.kotlin.*
import jetbrains.buildServer.configs.kotlin.projectFeatures.*

import Delft3D.*
import Delft3D.linux.*
import Delft3D.linux.thirdParty.*
import Delft3D.windows.*
import Delft3D.template.*

import Delft3D.verschilanalyse.*

version = "2024.03"

project {
    params {
        password("delft3d-dev-secret", "credentialsJSON:75eb18ff-a859-4d78-aa74-206d10865c2e")
    }

    description = "contact: BlackOps (black-ops@deltares.nl)"

    template(TemplateMergeRequest)
    template(TemplateDetermineProduct)
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

        buildType(WindowsBuildEnvironment)
        buildType(WindowsBuild)
        buildType(WindowsCollect)
        buildType(WindowsTest)
        buildTypesOrder = arrayListOf(
            WindowsBuildEnvironment,
            WindowsBuild,
            WindowsCollect,
            WindowsTest
        )
    }

    subProject(VerschilanalyseProject)

    subProjectsOrder = arrayListOf(
        RelativeId("Linux"),
        RelativeId("Windows"),
        VerschilanalyseProject
    )

    buildType(Trigger)
    buildType(Release)

    buildTypesOrder = arrayListOf(
        Trigger,
        Release
    )

    features {
        dockerRegistry {
            id = "DOCKER_REGISTRY_DELFT3D_DEV"
            name = "Docker Registry Delft3d-dev"
            url = "https://containers.deltares.nl/harbor/projects/21/repositories"
            userName = "robot${'$'}delft3d-dev+push-pull"
            password = "%delft3d-dev-secret%"
        }
    }
}