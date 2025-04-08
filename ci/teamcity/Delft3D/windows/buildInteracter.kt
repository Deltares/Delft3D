package Delft3D.windows

import jetbrains.buildServer.configs.kotlin.*
import jetbrains.buildServer.configs.kotlin.buildFeatures.*
import jetbrains.buildServer.configs.kotlin.buildSteps.*
import jetbrains.buildServer.configs.kotlin.failureConditions.*
import jetbrains.buildServer.configs.kotlin.triggers.schedule
import Delft3D.template.*
import Delft3D.step.*


object WindowsBuildDflowfmInteracter : BuildType({
    name = "Build D-flow FM Interacter"
    description = "Separate DflowFM Interacter Build"
    templates(
        TemplatePublishStatus,
        TemplateMonitorPerformance,
        TemplateFailureCondition
    )
    allowExternalStatus = true
    artifactRules = """
        #teamcity:symbolicLinks=as-is
        **/*.log => logging
        build_%product%/install/** => oss_artifacts_x64_%build.vcs.number%.zip!x64
    """.trimIndent()
    buildNumberPattern = "%product%: %build.vcs.number%"

    params {
        param("env..INTERACTER_DIR", """.\interacter\bin\win32\x64\""")
        text("product", "dflowfm_interacter", readOnly = true, allowEmpty = true)
        param("container.tag", "vs2022-intel2024")
        param("intel_fortran_compiler", "ifx")
        param("build.vcs.number", "${DslContext.settingsRoot.paramRefs.buildVcsNumber}")
        param("enable_code_coverage_flag", "OFF")
        param("generator", """"Visual Studio 17 2022"""")
        param("env.PATH", """%env.PATH%;"C:/Program Files/CMake/bin/"""")
        select("build_type", "Release", display = ParameterDisplay.PROMPT,
                options = listOf("Release", "Debug"))
    }

    vcs {
        root(DslContext.settingsRoot)
        root(AbsoluteId("ReposDsRoot"), "+:trunk/src/third_party/interacter => ./src/third_party/interacter")

        cleanCheckout = true
        checkoutDir = "ossbuild-lnx64"
    }

    steps {
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
                cmake ./src/cmake -G %generator% -T fortran=%intel_fortran_compiler% -D CMAKE_BUILD_TYPE=%build_type% -D CONFIGURATION_TYPE:STRING=%product% -B build_%product% -D CMAKE_INSTALL_PREFIX=build_%product%/install -D ENABLE_CODE_COVERAGE=%enable_code_coverage_flag%
                
                cd build_%product%
                
                cmake --build . -j --target install --config %build_type%
            """.trimIndent()
            dockerImage = "containers.deltares.nl/delft3d-dev/delft3d-buildtools-windows:%container.tag%"
            dockerImagePlatform = ScriptBuildStep.ImagePlatform.Windows
            dockerPull = true
            dockerRunParameters = "--memory %teamcity.agent.hardware.memorySizeMb%m --cpus %teamcity.agent.hardware.cpuCount%"
        }
    }
    if (DslContext.getParameter("environment") == "production") {
        triggers {
            schedule {
                schedulingPolicy = daily {
                    hour = 20
                }
                branchFilter = "+:<default>"
                triggerBuild = always()
                withPendingChangesOnly = false
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
