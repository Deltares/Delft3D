package Delft3D.windows

import jetbrains.buildServer.configs.kotlin.*
import jetbrains.buildServer.configs.kotlin.buildFeatures.*
import jetbrains.buildServer.configs.kotlin.buildSteps.*
import Delft3D.template.*
import Delft3D.step.*

object WindowsBuild2D3DSP : BuildType({

    description = "CMake build 2D3D single-precision. This is part of the Delft3D 4 delivery."

    templates(
        TemplateMergeRequest,
        TemplatePublishStatus,
        TemplateMonitorPerformance,
        TemplateDockerRegistry
    )
 
    name = "Build 2D3D single-precision"
    buildNumberPattern = "%product%: %build.vcs.number%"

    allowExternalStatus = true
    artifactRules = """
        #teamcity:symbolicLinks=as-is
        build_flow2d3d/install/bin/flow2d3d_sp.dll => oss_artifacts_x64_%build.vcs.number%.zip!x64/bin
    """.trimIndent()

    params {
        param("product", "auto-select")
        param("container.tag", "vs2022-intel2024-ltsc2025")
        param("env.CONAN_HOME", "C:/conan-cache")
        select("build_type", "Release", display = ParameterDisplay.PROMPT, options = listOf("Release", "Debug"))
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
            name = "Set single-precision"
            workingDir = "./src/utils_lgpl/deltares_common/scripts"
            scriptContent = """
                call singleprecision.bat
            """.trimIndent()
            dockerImage = "containers.deltares.nl/delft3d-dev/delft3d-buildtools-windows:%container.tag%"
            dockerImagePlatform = ScriptBuildStep.ImagePlatform.Windows
            dockerPull = true
        }
        script {
            name = "Build"
            conditions {
                matches("product", """^(d3d4-(suite|testbench))|(all-testbench)$""")
            }
            scriptContent = """
                call C:/set-env-vs2022.cmd

                python run_conan.py initialize deltares --ci
                if %%errorlevel%% neq 0 exit /b %%errorlevel%%

                python build.py --config flow2d3d --build --build-type %build_type% --ci --build-dir build_flow2d3d --install-dir build_flow2d3d/install
                if %%errorlevel%% neq 0 exit /b %%errorlevel%%
            """.trimIndent()
            dockerImage = "containers.deltares.nl/delft3d-dev/delft3d-buildtools-windows:%container.tag%"
            dockerImagePlatform = ScriptBuildStep.ImagePlatform.Windows
            dockerPull = true
            dockerRunParameters = "--memory %teamcity.agent.hardware.memorySizeMb%m --cpus %teamcity.agent.hardware.cpuCount% --mount type=volume,source=delft3d-conan-cache,target=C:/conan-cache -e CONAN_LOGIN_USERNAME_DELFT3D_CONAN_DEV=%nexus_conan_username% -e CONAN_PASSWORD_DELFT3D_CONAN_DEV=%nexus_conan_password%"
        }
    }
    requirements {
        doesNotEqual("teamcity.agent.jvm.os.name", "Windows Server 2022")
    }
})
