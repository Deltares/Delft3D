import jetbrains.buildServer.configs.kotlin.*
import jetbrains.buildServer.configs.kotlin.projectFeatures.*

import Delft3D.*
import Delft3D.linux.*
import Delft3D.linux.thirdParty.*
import Delft3D.windows.*
import Delft3D.template.*

import Delft3D.verschilanalyse.*

version = "2024.12"

project {
    params {
        param("delft3d-user", "robot${'$'}delft3d+delft3d-push-pull")
        password("delft3d-secret", "credentialsJSON:eb73cbd9-d17e-4bbe-ab3e-fdabe1eeddb0")

        param("delft3d-dev-user", "robot${'$'}delft3d-dev+push-pull")
        password("delft3d-dev-secret", "credentialsJSON:75eb18ff-a859-4d78-aa74-206d10865c2e")

        param("product", "dummy_value")
    }

    description = "contact: BlackOps (black-ops@deltares.nl)"

    template(TemplateMergeRequest)
    template(TemplateDetermineProduct)
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
        buildType(LinuxBuildTestbenchContainer)
        buildType(LinuxPyTest)
        buildTypesOrder = arrayListOf(
            LinuxBuild,
            LinuxCollect,
            LinuxDocker,
            LinuxTest,
            LinuxBuildTestbenchContainer,
            LinuxPyTest
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
            id = "DOCKER_REGISTRY_DELFT3D"
            name = "Docker Registry Delft3d"
            url = "https://containers.deltares.nl/harbor/projects/9/repositories"
            userName = "%delft3d-user%"
            password = "%delft3d-secret%"
        }
        dockerRegistry {
            id = "DOCKER_REGISTRY_DELFT3D_DEV"
            name = "Docker Registry Delft3d-dev"
            url = "https://containers.deltares.nl/harbor/projects/21/repositories"
            userName = "%delft3d-dev-user%"
            password = "%delft3d-dev-secret%"
        }
    }
}