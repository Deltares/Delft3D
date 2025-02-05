package Delft3D.windows

import jetbrains.buildServer.configs.kotlin.*
import jetbrains.buildServer.configs.kotlin.buildFeatures.*
import jetbrains.buildServer.configs.kotlin.buildSteps.*
import jetbrains.buildServer.configs.kotlin.failureConditions.*
import Delft3D.template.*
import Delft3D.step.*

object WindowsBuild : BuildType({
    templates(
        TemplateMergeRequest,
        TemplatePublishStatus,
        TemplateMonitorPerformance,
        TemplateFailureCondition
    )
 
    name = "Build"
    buildNumberPattern = "%product%: %build.vcs.number%"
    description = "Windows build."

    allowExternalStatus = true
    artifactRules = """
        #teamcity:symbolicLinks=as-is
        **/*.log => logging
        build_%product%/install/** => oss_artifacts_x64_%build.vcs.number%.zip!x64
    """.trimIndent()

    params {
        param("intel_fortran_compiler", "ifort")
        param("enable_code_coverage_flag", "OFF")
        param("generator", """"Visual Studio 16 2019"""")
        param("env.PATH", """%env.PATH%;"C:/Program Files/CMake/bin/"""")
        param("build_type", "Release")
        select("product", "auto-select", display = ParameterDisplay.PROMPT, options = listOf("auto-select", "all-testbench", "fm-suite", "d3d4-suite", "fm-testbench", "d3d4-testbench", "waq-testbench", "part-testbench", "rr-testbench", "wave-testbench", "swan-testbench"))
    }

    vcs {
        root(DslContext.settingsRoot)
        cleanCheckout = true
        checkoutDir = "ossbuild-lnx64"
    }

    steps {
        mergeTargetBranch {
            dockerImage = "containers.deltares.nl/delft3d-dev/delft3d-buildtools-windows:vs2019-oneapi2023"
            dockerImagePlatform = ScriptBuildStep.ImagePlatform.Windows
            dockerPull = true
        }
        python {
            name = "Determine product by branch prefix"
            command = script {
                content="""
                    if "%product%" == "auto-select":
                        if "merge-request" in "%teamcity.build.branch%":
                            product = "%teamcity.pullRequest.source.branch%".split("/")[0]
                        else:
                            product = "%teamcity.build.branch%".split("/")[0]
                        if "%teamcity.build.branch.is_default%" == "true":
                            product = "all"
                        print(f"##teamcity[setParameter name='product' value='{product}-testbench']")
                        print(f"##teamcity[buildNumber '{product}-testbench: %build.vcs.number%']")
                """.trimIndent()
            }
            dockerImage = "containers.deltares.nl/delft3d-dev/delft3d-buildtools-windows:vs2019-oneapi2023"
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
            dockerImage = "containers.deltares.nl/delft3d-dev/delft3d-buildtools-windows:vs2019-oneapi2023"
            dockerImagePlatform = ScriptBuildStep.ImagePlatform.Windows
            dockerPull = true
        }
        script {
            name = "Build"
            scriptContent = """
                cmake ./src/cmake -G %generator% -T fortran=%intel_fortran_compiler% -D CMAKE_BUILD_TYPE=%build_type% -D CONFIGURATION_TYPE:STRING=%product% -B build_%product% -D CMAKE_INSTALL_PREFIX=build_%product%/install -D ENABLE_CODE_COVERAGE=%enable_code_coverage_flag%

                cd build_%product%

                cmake --build . -j --target install --config %build_type%
            """.trimIndent()
            dockerImage = "containers.deltares.nl/delft3d-dev/delft3d-buildtools-windows:vs2019-oneapi2023"
            dockerImagePlatform = ScriptBuildStep.ImagePlatform.Windows
            dockerPull = true
            dockerRunParameters = "--memory %teamcity.agent.hardware.memorySizeMb%m --cpus %teamcity.agent.hardware.cpuCount%"
        }
        powerShell {
            name = "Add FBC-tools"
            scriptMode = script {
                content="""
                    robocopy fbctools build_%product%\install /E /XC /XN /XO
                """.trimIndent()
            }
        }
    }

    dependencies {
        dependency(AbsoluteId("FbcTools_FbcToolsBuildOssX64CMakeReleaseWin64")) {
            snapshot {
                onDependencyFailure = FailureAction.FAIL_TO_START
                onDependencyCancel = FailureAction.CANCEL
            }
            artifacts {
                artifactRules = """
                    *.dll => fbctools/lib
                    *.xsd => fbctools/share/drtc
                """.trimIndent()
            }
        }
    }

    features {
        dockerSupport {
            loginToRegistry = on {
                dockerRegistryId = "DOCKER_REGISTRY_DELFT3D_DEV"
            }
        }
    }

})