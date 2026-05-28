package Delft3D.windows

import jetbrains.buildServer.configs.kotlin.*
import jetbrains.buildServer.configs.kotlin.buildFeatures.*
import jetbrains.buildServer.configs.kotlin.buildSteps.*
import Delft3D.template.*
import Delft3D.step.*

object WindowsBuild2D3DSP : BuildType({

    description = "CMake build 2D3D single-precision. This is necessary for Delft3D 4."

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
        build_flow2d3d/install/lib/flow2d3d_sp.dll => oss_artifacts_x64_%build.vcs.number%.zip!x64/lib
    """.trimIndent()

    params {
        param("product", "auto-select")
        param("intel_fortran_compiler", "ifx")
        param("container.tag", "vs2022-intel2024")
        param("generator", """"Visual Studio 17 2022"""")
        param("enable_code_coverage_flag", "OFF")
        select("build_type", "Release", display = ParameterDisplay.PROMPT, options = listOf("Release", "Debug"))
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

                cmake ./src/cmake -G %generator% -T fortran=%intel_fortran_compiler% -D CMAKE_BUILD_TYPE=%build_type% -D CONFIGURATION_TYPE:STRING=flow2d3d -B build_flow2d3d -D CMAKE_INSTALL_PREFIX=build_flow2d3d/install -D ENABLE_CODE_COVERAGE=%enable_code_coverage_flag%

                cd build_flow2d3d

                cmake --build . -j --target install --config %build_type%
            """.trimIndent()
            dockerImage = "containers.deltares.nl/delft3d-dev/delft3d-buildtools-windows:%container.tag%"
            dockerImagePlatform = ScriptBuildStep.ImagePlatform.Windows
            dockerPull = true
            dockerRunParameters = "--memory %teamcity.agent.hardware.memorySizeMb%m --cpus %teamcity.agent.hardware.cpuCount%"
        }
    }
})
