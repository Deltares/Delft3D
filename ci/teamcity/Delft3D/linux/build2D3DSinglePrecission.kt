package Delft3D.linux

import jetbrains.buildServer.configs.kotlin.*
import jetbrains.buildServer.configs.kotlin.buildFeatures.*
import jetbrains.buildServer.configs.kotlin.buildSteps.*
import Delft3D.template.*
import Delft3D.step.*
import Delft3D.linux.containers.*

object LinuxBuild2D3DSP : BuildType({

    description = "CMake build 2D3D single-precision. This is necessary for Delft3D 4."

    templates(
        TemplateMergeRequest,
        TemplateDetermineProduct,
        TemplatePublishStatus,
        TemplateMonitorPerformance,
        TemplateDockerRegistry
    )

    name = "Build 2D3D single-precision"
    buildNumberPattern = "%product%: %build.vcs.number%"

    allowExternalStatus = true
    artifactRules = """
        #teamcity:symbolicLinks=as-is
        build_flow2d3d/install/lib/libflow2d3d_sp.so => oss_artifacts_lnx64_%build.vcs.number%.tar.gz!lnx64/lib
    """.trimIndent()

    outputParams {
        exposeAllParameters = false
    }

    params {
        param("generator", """"Unix Makefiles"""")
        param("product", "auto-select")
        select("build_type", "%dep.${LinuxThirdPartyLibs.id}.build_type%", display = ParameterDisplay.PROMPT, options = listOf("Release", "RelWithDebInfo", "Debug"))
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
            name = "Set single-precision"
            workingDir = "./src/utils_lgpl/deltares_common/scripts"
            scriptContent = """
                #!/usr/bin/env bash
                ./singleprecision.sh
            """.trimIndent()
        }
        exec {
            name = "Build"
            conditions {
                matches("product", """^(d3d4-(suite|testbench))|(all-testbench)$""")
            }
            path = "ci/teamcity/Delft3D/linux/scripts/build.sh"
            arguments = """
                --generator %generator%
                --product flow2d3d
                --build-type %build_type%
            """.trimIndent()
            dockerImage = "containers.deltares.nl/delft3d-dev/delft3d-third-party-libs:%dep.${LinuxThirdPartyLibs.id}.env.IMAGE_TAG%"
            dockerImagePlatform = ExecBuildStep.ImagePlatform.Linux
            dockerRunParameters = "--rm"
            dockerPull = true
        }
    }

    dependencies {
        dependency(LinuxThirdPartyLibs) {
            snapshot {
                onDependencyFailure = FailureAction.FAIL_TO_START
                onDependencyCancel = FailureAction.CANCEL
            }
        }
    }

    requirements {
        equals("teamcity.agent.jvm.os.name", "Linux")
    }
})
