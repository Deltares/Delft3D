import jetbrains.buildServer.configs.kotlin.*
import jetbrains.buildServer.configs.kotlin.projectFeatures.*

import Delft3D.*
import Delft3D.linux.*
import Delft3D.linux.containers.*
import Delft3D.linux.container_smoketest.*
import Delft3D.windows.*
import Delft3D.template.*

import Delft3D.ciUtilities.*
import Delft3D.verschilanalyse.*

version = "2025.07"

project {

    description = "contact: BlackOps (black-ops@deltares.nl)"

    params {
        param("delft3d-user", DslContext.getParameter("delft3d-user"))
        password("delft3d-secret", DslContext.getParameter("delft3d-secret"))

        param("s3_dsctestbench_accesskey", DslContext.getParameter("s3_dsctestbench_accesskey"))
        password("s3_dsctestbench_secret", "credentialsJSON:7e8a3aa7-76e9-4211-a72e-a3825ad1a160")

        param("nexus_username", DslContext.getParameter("nexus_username"))
        password("nexus_password", DslContext.getParameter("nexus_password"))

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
        subProject {
            id("SmokeTestsContainerH7")
            name = "Smoke tests container on H7"
            buildType(LinuxSubmitH7ContainerSmokeTest)
            buildType(LinuxReceiveH7ContainerSmokeTest)
            buildTypesOrder = listOf(
                LinuxSubmitH7ContainerSmokeTest,
                LinuxReceiveH7ContainerSmokeTest,
            )
        }        
        buildType(LinuxBuild)
        buildType(LinuxBuild2D3DSP)
        buildType(LinuxCollect)
        buildType(LinuxRuntimeContainers)
        buildType(LinuxRunAllContainerExamples)
        buildType(LinuxTest)
        buildType(LinuxUnitTest)
        buildTypesOrder = arrayListOf(
            LinuxBuild,
            LinuxBuild2D3DSP,
            LinuxCollect,
            LinuxRuntimeContainers,
            LinuxRunAllContainerExamples,
            LinuxUnitTest,
            LinuxTest
        )
    }

    subProject {
        id("Windows")
        name = "Windows"

        buildType(WindowsBuildEnvironmentI24)
        buildType(WindowsTestEnvironment)
        buildType(WindowsBuild)
        buildType(WindowsBuild2D3DSP)
        buildType(WindowsCollect)
        buildType(WindowsTest)
        buildType(WindowsUnitTest)
        buildType(WindowsBuildDflowfmInteracter)
        buildTypesOrder = arrayListOf(
            WindowsBuildEnvironmentI24,
            WindowsTestEnvironment,
            WindowsBuild,
            WindowsBuild2D3DSP,
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
        buildType(TestBenchValidation)
        buildType(TestFortranStyler)
        buildType(CopyExamples)
        buildType(SigCi)

        buildTypesOrder = arrayListOf(
            TestPythonCiTools, TestBenchValidation, TestFortranStyler, CopyExamples, SigCi
        )
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
    buildType(PublishToGui)
    buildType(DIMRbak)
    buildType(Publish)
    buildTypesOrder = arrayListOf(
        Trigger,
        PublishToGui,
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
        feature {
            type = "OAuthProvider"
            param("displayName", "Keeper Vault Delft3d")
            param("secure:client-secret", "credentialsJSON:bcf00886-4ae4-4c0a-9701-4e37efab8504")
            param("providerType", "teamcity-ksm")
        }
    }
}
