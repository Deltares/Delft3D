import jetbrains.buildServer.configs.kotlin.*
import jetbrains.buildServer.configs.kotlin.projectFeatures.*

import Delft3D.*
import Delft3D.linux.*
import Delft3D.linux.thirdParty.*
import Delft3D.linux.containers.*
import Delft3D.windows.*
import Delft3D.template.*

import Delft3D.ciUtilities.*
import Delft3D.verschilanalyse.*

version = "2024.12"

project {

    description = "contact: BlackOps (black-ops@deltares.nl)"

    params {
        param("delft3d-user", "robot${'$'}delft3d+delft3d-push-pull")
        password("delft3d-secret", "credentialsJSON:eb73cbd9-d17e-4bbe-ab3e-fdabe1eeddb0")

        param("delft3d-dev-user", "robot${'$'}delft3d-dev+push-pull")
        password("delft3d-dev-secret", "credentialsJSON:75eb18ff-a859-4d78-aa74-206d10865c2e")

        param("s3_dsctestbench_accesskey", DslContext.getParameter("s3_dsctestbench_accesskey"))
        password("s3_dsctestbench_secret", "credentialsJSON:7e8a3aa7-76e9-4211-a72e-a3825ad1a160")

        param("product", "dummy_value")
    }

    template(TemplateMergeRequest)
    template(TemplateDetermineProduct)
    template(TemplatePublishStatus)
    template(TemplateMonitorPerformance)
    template(TemplateFailureCondition)
    template(TemplateValidationDocumentation)
    template(TemplateFunctionalityDocumentation)
    template(TemplateDownloadFromS3)

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
        subProject {
            id("BuildContainers")
            name = "Build-environment Containers"
            buildType(LinuxBuildTools)
            buildType(LinuxThirdPartyLibs)
            buildTypesOrder = listOf(
                LinuxBuildTools,
                LinuxThirdPartyLibs,
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
        buildType(WindowsBuildEnvironmentI24)
        buildType(WindowsBuild)
        buildType(WindowsCollect)
        buildType(WindowsTest)
        buildTypesOrder = arrayListOf(
            WindowsBuildEnvironment,
            WindowsBuildEnvironmentI24,
            WindowsBuild,
            WindowsCollect,
            WindowsTest
        )
    }

    subProject {
        id("Documentation")
        name = "Documentation"

        buildType(ValidationDocumentMatrix)
        buildType(FunctionalityDocumentMatrix)
        buildTypesOrder = arrayListOf(
            ValidationDocumentMatrix,
            FunctionalityDocumentMatrix
        )
    }

    subProject {
        id("CiUtilities")
        name = "CI utilities"
        description = """
            Build and test the utilities used in the Delft3D TeamCity project.
        """.trimIndent()

        buildType(TestPythonCiTools)
        buildType(CopyExamples)
    }

    subProject(VerschilanalyseProject)

    subProjectsOrder = arrayListOf(
        RelativeId("Linux"),
        RelativeId("Windows"),
        RelativeId("Documentation"),
        RelativeId("CiUtilities"),
        VerschilanalyseProject
    )

    buildType(Trigger)
    buildTypesOrder = arrayListOf(
        Trigger
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
        awsConnection {
            id = "doc_download_connection"
            name = "Deltares MinIO connection"
            credentialsType = static {
                accessKeyId = DslContext.getParameter("s3_dsctestbench_accesskey")
                secretAccessKey = "credentialsJSON:7e8a3aa7-76e9-4211-a72e-a3825ad1a160"
                useSessionCredentials = false
            }
            allowInSubProjects = true
            allowInBuilds = true
        }
    }
}