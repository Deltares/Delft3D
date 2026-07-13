package Delft3D.windows

import jetbrains.buildServer.configs.kotlin.*
import jetbrains.buildServer.configs.kotlin.buildFeatures.*
import jetbrains.buildServer.configs.kotlin.buildSteps.*
import jetbrains.buildServer.configs.kotlin.failureConditions.*
import Delft3D.template.*
import Delft3D.step.*

object WindowsBuild : BuildType({

    description = "CMake build."

    templates(
        TemplateMergeRequest,
        TemplatePublishStatus,
        TemplateMonitorPerformance,
        TemplateFailureCondition,
        TemplateDockerRegistry,
        TemplateBuildConcurrency
    )
 
    name = "Build"
    buildNumberPattern = "%product%: %build.vcs.number%"

    allowExternalStatus = true
    artifactRules = """
        #teamcity:symbolicLinks=as-is
        **/*.log => logging
        build_%product%/install/** => oss_artifacts_x64_%build.vcs.number%.zip!x64
        unit-test-report-windows.xml
    """.trimIndent()

    params {
        param("container.tag", "vs2022-intel2024-ltsc2025")
        param("env.CONAN_HOME", "C:/conan-cache")
        select("build_type", "Release", display = ParameterDisplay.PROMPT, options = listOf("Release", "Debug"))
        select("product", "auto-select", display = ParameterDisplay.PROMPT, options = listOf("auto-select", "all-testbench", "fm-suite", "d3d4-suite", "fm-testbench", "d3d4-testbench", "waq-testbench", "part-testbench", "rr-testbench", "wave-testbench", "swan-testbench"))
        param("nexus_conan_username", DslContext.getParameter("nexus_conan_username"))
        password("nexus_conan_password", DslContext.getParameter("nexus_conan_password"))
    }

    vcs {
        root(DslContext.settingsRoot)
        cleanCheckout = true
        checkoutDir = "ossbuild-win"
    }

    steps {
        mergeTargetBranch {
            dockerImage = "containers.deltares.nl/delft3d-dev/delft3d-buildtools-windows:%container.tag%"
            dockerImagePlatform = ScriptBuildStep.ImagePlatform.Windows
            dockerPull = true
        }
        python {
            name = "Determine product by branch prefix"
            command = file {
                filename ="""ci\\teamcity\\Delft3D\\windows\\scripts\\determineProduct.py"""
                scriptArguments = "%product% %teamcity.build.branch% %teamcity.build.branch.is_default% %build.vcs.number% %teamcity.pullRequest.source.branch%"
            }
            dockerImage = "containers.deltares.nl/delft3d-dev/delft3d-buildtools-windows:%container.tag%"
            dockerImagePlatform = PythonBuildStep.ImagePlatform.Windows
            dockerPull = true
        }
        script {
            name = "Add version attributes"
            workingDir = "./src/version_includes"
            scriptContent = """
                echo #define BUILD_NR "%build.vcs.number%" > checkout_info.h
                echo #define BRANCH "%teamcity.build.branch%" >> checkout_info.h
            """.trimIndent()
            dockerImage = "containers.deltares.nl/delft3d-dev/delft3d-buildtools-windows:%container.tag%"
            dockerImagePlatform = ScriptBuildStep.ImagePlatform.Windows
            dockerPull = true
        }
        script {
            name = "Build"
            scriptContent = """
                call C:/set-env-vs2022.cmd

                python run_conan.py initialize deltares --ci
                if %%errorlevel%% neq 0 exit /b %%errorlevel%%

                python build.py --config %product% --build --build-type %build_type% --ci --build-dir build_%product% --install-dir build_%product%/install
                if %%errorlevel%% neq 0 exit /b %%errorlevel%%

                ctest --test-dir ./build_%product% --build-config %build_type% --output-junit ../unit-test-report-windows.xml --output-on-failure
                if %%errorlevel%% neq 0 exit /b %%errorlevel%%
            """.trimIndent()
            dockerImage = "containers.deltares.nl/delft3d-dev/delft3d-buildtools-windows:%container.tag%"
            dockerImagePlatform = ScriptBuildStep.ImagePlatform.Windows
            dockerPull = true
            dockerRunParameters = "--memory %teamcity.agent.hardware.memorySizeMb%m --cpus %teamcity.agent.hardware.cpuCount% --mount type=volume,source=delft3d-conan-cache,target=C:/conan-cache -e CONAN_LOGIN_USERNAME_DELFT3D_CONAN_DEV=%nexus_conan_username% -e CONAN_PASSWORD_DELFT3D_CONAN_DEV=%nexus_conan_password%"
        }
    }

    features {
        xmlReport {
            reportType = XmlReport.XmlReportType.JUNIT
            rules = "+:unit-test-report-windows.xml"
        }
    }

    failureConditions {
        executionTimeoutMin = 120
    }

    requirements {
        doesNotEqual("teamcity.agent.jvm.os.name", "Windows Server 2022")
    }
})
