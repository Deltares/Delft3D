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
        TemplateMonitorPerformance
    )
 
    name = "Build"
    buildNumberPattern = "%build.vcs.number%"
    description = "Windows build."

    allowExternalStatus = true
    artifactRules = """
        #teamcity:symbolicLinks=as-is
        **/*.log => logging
        %install_dir%/** => oss_artifacts_x64_%build.vcs.number%.zip!x64
    """.trimIndent()

    params {
        param("install_dir", "install_%build_configuration%")
        param("build_configuration", "all")
        param("intel_fortran_compiler", "ifort")
        param("enable_code_coverage_flag", "OFF")
        param("generator", """"Visual Studio 16 2019"""")
        param("env.PATH", """%env.PATH%;"C:/Program Files/CMake/bin/"""")
        param("build_type", "Release")
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
                    if "%product%" == "dummy_value":
                        if "merge-request" in "%teamcity.build.branch%":
                            product = "%teamcity.pullRequest.source.branch%".split("/")[0]
                            print(f"##teamcity[setParameter name='product' value='{product}']")
                        else:
                            product = "%teamcity.build.branch%".split("/")[0]
                            print(f"##teamcity[setParameter name='product' value='{product}']")
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
                cmake ./src/cmake -G %generator% -T fortran=%intel_fortran_compiler% -D CMAKE_BUILD_TYPE=%build_type% -D CONFIGURATION_TYPE:STRING=%build_configuration% -B build_%build_configuration% -D CMAKE_INSTALL_PREFIX=%install_dir% -D ENABLE_CODE_COVERAGE=%enable_code_coverage_flag%

                cd build_%build_configuration%

                cmake --build . -j --target install --config %build_type%
            """.trimIndent()
            dockerImage = "containers.deltares.nl/delft3d-dev/delft3d-buildtools-windows:vs2019-oneapi2023"
            dockerImagePlatform = ScriptBuildStep.ImagePlatform.Windows
            dockerPull = true
            dockerRunParameters = "--memory %teamcity.agent.hardware.memorySizeMb%m --cpus %teamcity.agent.hardware.cpuCount%"
        }
    }

    failureConditions {
        executionTimeoutMin = 1800
        errorMessage = true
        failOnText {
            conditionType = BuildFailureOnText.ConditionType.REGEXP
            pattern = "Artifacts path .* not found"
            failureMessage = "Artifacts are missing"
            reverse = false
        }
        failOnText {
            conditionType = BuildFailureOnText.ConditionType.CONTAINS
            pattern = "Failed to resolve artifact dependency"
            failureMessage = "Unable to collect all dependencies"
            reverse = false
            stopBuildOnFailure = true
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
                    *.dll => %install_dir%/lib
                    *.xsd => %install_dir%/share/drtc
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