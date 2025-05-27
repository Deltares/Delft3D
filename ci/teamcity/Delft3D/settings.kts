import jetbrains.buildServer.configs.kotlin.*
import jetbrains.buildServer.configs.kotlin.projectFeatures.*

import Delft3D.*
import Delft3D.linux.*
import Delft3D.linux.containers.*
import Delft3D.windows.*
import Delft3D.template.*

import Delft3D.ciUtilities.*
import Delft3D.verschilanalyse.*

version = "2025.03"

project {

    description = "contact: BlackOps (black-ops@deltares.nl)"

    params {
        param("delft3d-user", "robot${'$'}delft3d")
        password("delft3d-secret", "credentialsJSON:1dee1a48-252e-42fd-b600-6bf52d940513")

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
    template(TemplateDockerRegistry)

    subProject {
        id("Linux")
        name = "Linux"
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
        buildType(LinuxRuntimeContainers)
        buildType(LinuxRunAllDockerExamples)
        buildType(LinuxLegacyDockerTest)
        buildType(LinuxTest)
        buildType(LinuxUnitTest)
        buildTypesOrder = arrayListOf(
            LinuxBuild,
            LinuxCollect,
            LinuxRuntimeContainers,
            LinuxRunAllDockerExamples,
            LinuxLegacyDockerTest,
            LinuxUnitTest,
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
        buildType(WindowsUnitTest)
        buildType(WindowsBuildDflowfmInteracter)
        buildTypesOrder = arrayListOf(
            WindowsBuildEnvironment,
            WindowsBuildEnvironmentI24,
            WindowsBuild,
            WindowsCollect,
            WindowsTest,
            WindowsUnitTest,
            WindowsBuildDflowfmInteracter,
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
    buildType(DIMRbak)
    buildType(Publish)
    buildTypesOrder = arrayListOf(
        Trigger,
        DIMRbak,
        Publish
    )

    features {
        dockerRegistry {
            id = "DOCKER_REGISTRY_DELFT3D"
            name = "Docker Registry Delft3d"
            url = "https://containers.deltares.nl/"
            userName = "%delft3d-user%"
            password = "%delft3d-secret%"
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
