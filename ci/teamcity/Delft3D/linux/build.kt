package Delft3D.linux

import jetbrains.buildServer.configs.kotlin.*
import jetbrains.buildServer.configs.kotlin.buildFeatures.*
import jetbrains.buildServer.configs.kotlin.buildSteps.*
import jetbrains.buildServer.configs.kotlin.failureConditions.*
import Delft3D.template.*
import Delft3D.step.*

object LinuxBuild : BuildType({

    templates(
        TemplateMergeRequest,
        TemplateDetermineProduct,
        TemplatePublishStatus,
        TemplateMonitorPerformance
    )

    name = "Build"
    buildNumberPattern = "%build.vcs.number%"
    description = "Linux build."

    allowExternalStatus = true
    artifactRules = """
        #teamcity:symbolicLinks=as-is
        **/*.log => logging
        %install_dir%/** => oss_artifacts_lnx64_%build.vcs.number%.tar.gz!lnx64
    """.trimIndent()

    params {
        param("intel_oneapi_version", "2023")
        param("intel_fortran_compiler", "ifort")
        param("generator", """"Unix Makefiles"""")
        param("install_dir", "build_all/install")
        param("build_type", "Release")
        param("build_configuration", "all")
    }

    vcs {
        root(DslContext.settingsRoot)
        cleanCheckout = true
        checkoutDir = "ossbuild-lnx64"
    }

    steps {
        mergeTargetBranch {}
        script {
            name = "Add version attributes"
            workingDir = "./src/version_includes"
            scriptContent = """
                #!/usr/bin/env bash
                echo '#define BUILD_NR "%build.vcs.number%"' > checkout_info.h
                echo '#define BRANCH "%teamcity.build.branch%"' >> checkout_info.h
            """.trimIndent()
        }
        script {
            name = "Build"
            scriptContent = """
                #!/bin/bash
                set -eo pipefail
                . /opt/intel/oneapi/setvars.sh
                export LD_LIBRARY_PATH=/usr/local/lib:/usr/local/lib64:${'$'}{LD_LIBRARY_PATH}
                export PKG_CONFIG_PATH=/usr/local/lib/pkgconfig:/usr/local/lib64/pkgconfig:${'$'}{PKG_CONFIG_PATH}
                export FC=mpiifort CXX=mpicxx CC=mpiicx
                
                cmake ./src/cmake -G %generator% -D CONFIGURATION_TYPE:STRING=%build_configuration% -D CMAKE_BUILD_TYPE=%build_type% -B build_%build_configuration% -D CMAKE_INSTALL_PREFIX=%install_dir%
                
                cd build_%build_configuration%
                cmake --build . -j --target install --config %build_type%
            """.trimIndent()
            dockerImage = "containers.deltares.nl/delft3d-dev/delft3d-third-party-libs:oneapi-%intel_oneapi_version%-%intel_fortran_compiler%-release"
            dockerImagePlatform = ScriptBuildStep.ImagePlatform.Linux
            dockerRunParameters = "--rm"
        }
        script {
            name = "Copy ESMF binaries"
            scriptContent = """
                #!/usr/bin/env bash
                . /usr/share/Modules/init/bash
                
                # Additional step to copy ESMF stuff needed by D-WAVES
                module load esmf/7.0.0beta_intel2023.1.0
                
                ESMFRWG=`which ESMF_RegridWeightGen`
                LIBESMF=`ldd ${'$'}{ESMFRWG} | grep libesmf.so | awk '{print ${'$'}3}'`
                LIBCILKRTS=`ldd ${'$'}{ESMFRWG} | grep libcilkrts.so | awk '{print ${'$'}3}'`
                
                cp -rf ${'$'}{ESMFRWG}                                         %install_dir%/bin                               &>/dev/null
                cp -rf ${'$'}{LIBESMF}                                         %install_dir%/lib                               &>/dev/null
                cp -rf ${'$'}{LIBCILKRTS}                                      %install_dir%/lib                               &>/dev/null
            """.trimIndent()
        }
    }

    features {
        dockerSupport {
            loginToRegistry = on {
                dockerRegistryId = "DOCKER_REGISTRY_DELFT3D_DEV"
            }
        }
        dockerSupport {
            loginToRegistry = on {
                dockerRegistryId = "PROJECT_EXT_133,PROJECT_EXT_81"
            }
        }
    }

    failureConditions {
        executionTimeoutMin = 60
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

    requirements {
        equals("teamcity.agent.jvm.os.name", "Linux")
    }
})