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

version = "2026.1"

project {

    description = "Build, test, collect, and publish Delft3D. Contact: BlackOps (black-ops@deltares.nl)."

    params {
        param("delft3d-user", DslContext.getParameter("delft3d-user"))
        password("delft3d-secret", DslContext.getParameter("delft3d-secret"))

        param("dvc_testbench_accesskey", DslContext.getParameter("dvc_testbench_accesskey"))
        password("dvc_testbench_secret", DslContext.getParameter("dvc_testbench_secret"))

        param("nexus_username", DslContext.getParameter("nexus_username"))
        password("nexus_password", DslContext.getParameter("nexus_password"))
        password("nexus_nuget_apikey", DslContext.getParameter("nexus_nuget_apikey"))
        param("nexus_iq_username", DslContext.getParameter("nexus_iq_username"))
        password("nexus_iq_password", DslContext.getParameter("nexus_iq_password"))
        param("env.UV_INDEX_URL", "https://%nexus_username%:%nexus_password%@internal-artifacts.deltares.nl/repository/python-internal/simple/")
        param("product", "dummy_value")

    }

    template(TemplateLinuxAgent)
    template(TemplateLinuxAgentFips)
    template(TemplateLinuxAgentNoFips)
    template(TemplateMergeRequest)
    template(TemplateDetermineProduct)
    template(TemplatePublishStatus)
    template(TemplateMonitorPerformance)
    template(TemplateFailureCondition)
    template(TemplateValidationDocumentation)
    template(TemplateFunctionalityDocumentation)
    template(TemplateDownloadFromDVC)
    template(TemplateDockerRegistry)
    template(TemplateBuildConcurrency)

    subProject {
        id("Linux")
        name = "Linux"
        description = "Compile, unit tests, TestBench, and containers on Linux."
        subProject {
            id("BuildContainers")
            name = "Environment containers"
            description = "Linux images used to compile and run CI Python."
            buildType(LinuxBuildTools)
            buildType(LinuxThirdPartyLibs)
            buildType(LinuxDevContainer)
            buildType(LinuxPython)
            buildTypesOrder = listOf(
                LinuxBuildTools,
                LinuxThirdPartyLibs,
                LinuxDevContainer,
                LinuxPython,
            )
        }        
        subProject {
            id("SmokeTestsContainerH7")
            name = "H7 container smoke tests"
            description = "Submit and collect container smoke tests on H7."
            buildType(LinuxSubmitH7ContainerSmokeTest)
            buildType(LinuxReceiveH7ContainerSmokeTest)
            buildTypesOrder = listOf(
                LinuxSubmitH7ContainerSmokeTest,
                LinuxReceiveH7ContainerSmokeTest,
            )
        }        
        buildType(LinuxConanPackages)
        buildType(LinuxBuild)
        buildType(LinuxBuild2D3DSP)
        buildType(LinuxCollect)
        buildType(LinuxRuntimeContainers)
        buildType(LinuxRunAllContainerExamples)
        buildType(LinuxTest)
        buildType(LinuxUnitTest)
        buildTypesOrder = arrayListOf(
            LinuxConanPackages,
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
        description = "Compile, unit tests, and TestBench on Windows."

        buildType(WindowsBuildEnvironment)
        buildType(WindowsTestEnvironment)
        buildType(WindowsCollectEnvironment)
        buildType(WindowsConanPackages)
        buildType(WindowsBuild)
        buildType(WindowsBuild2D3DSP)
        buildType(WindowsCollect)
        buildType(WindowsTest)
        buildType(WindowsUnitTest)
        buildType(WindowsBuildDflowfmInteracter)
        buildTypesOrder = arrayListOf(
            WindowsBuildEnvironment,
            WindowsTestEnvironment,
            WindowsCollectEnvironment,
            WindowsConanPackages,
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
        description = "Functionality and validation PDF reports."

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
            Checks: Python CI tools, TestBench, Fortran styler, Shell.
            Scans: Sigrid, Nexus IQ (product / TestBench / Python CI tools).
            Delivery: copy DIMRset examples to the P-drive.
        """.trimIndent()

        buildType(TestPythonCiTools)
        buildType(TestBenchValidation)
        buildType(TestFortranStyler)
        buildType(RunBashBatonUtilities)
        buildType(SigCi)
        buildType(LifecycleScanMain)
        buildType(LifecycleScanTestBench)
        buildType(LifecycleScanCiTools)
        buildType(CopyExamples)

        buildTypesOrder = arrayListOf(
            TestPythonCiTools,
            TestBenchValidation,
            TestFortranStyler,
            RunBashBatonUtilities,
            SigCi,
            LifecycleScanMain,
            LifecycleScanTestBench,
            LifecycleScanCiTools,
            CopyExamples,
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
    buildType(PinAndTag)
    buildTypesOrder = arrayListOf(
        Trigger,
        PublishToGui,
        DIMRbak,
        Publish,
        PinAndTag
    )
        
    features {
        dockerRegistry {
            id = "DOCKER_REGISTRY_DELFT3D"
            name = "Delft3D Docker registry"
            url = "https://containers.deltares.nl/"
            userName = "%delft3d-user%"
            password = "%delft3d-secret%"
        }
        feature {
            type = "OAuthProvider"
            param("displayName", "Keeper Vault Delft3d")
            param("secure:client-secret", "credentialsJSON:bcf00886-4ae4-4c0a-9701-4e37efab8504")
            param("providerType", "teamcity-ksm")
        }
    }
}
