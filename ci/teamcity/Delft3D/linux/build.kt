package Delft3D.linux

import jetbrains.buildServer.configs.kotlin.*
import jetbrains.buildServer.configs.kotlin.buildFeatures.*
import jetbrains.buildServer.configs.kotlin.buildSteps.*
import jetbrains.buildServer.configs.kotlin.failureConditions.*
import Delft3D.template.*
import Delft3D.step.*
import Delft3D.linux.containers.*

object LinuxBuild : BuildType({

    description = "CMake build."

    templates(
        TemplateLinuxAgent,
        TemplateMergeRequest,
        TemplateDetermineProduct,
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
        install/** => oss_artifacts_lnx64_%build.vcs.number%.tar.gz!lnx64
        unit-test-report-linux.xml
    """.trimIndent()

    outputParams {
        exposeAllParameters = false
        param("product", "%product%")
        param("build_type", "%build_type%")
        param("commit_id", "%build.revisions.revision%")
        param("commit_id_short", "%build.revisions.short%")
        param("build_tools_image_tag", "%dep.${LinuxBuildTools.id}.env.IMAGE_TAG%")
    }

    params {
        param("generator", """"Unix Makefiles"""")
        select("product", "auto-select", display = ParameterDisplay.PROMPT, options = listOf("auto-select", "all-testbench", "fm-suite", "d3d4-suite", "fm-testbench", "d3d4-testbench", "waq-testbench", "part-testbench", "rr-testbench", "wave-testbench", "swan-testbench"))
        select("build_type", "%dep.${LinuxThirdPartyLibs.id}.build_type%", display = ParameterDisplay.PROMPT, options = listOf("Release", "RelWithDebInfo", "Debug"))
        param("nexus_conan_username", DslContext.getParameter("nexus_conan_username"))
        password("nexus_conan_password", DslContext.getParameter("nexus_conan_password"))
        param("env.CONAN_HOME", "/conan-cache")
    }

    vcs {
        root(DslContext.settingsRoot)
        cleanCheckout = true
        checkoutDir = "ossbuild-lnx64"
    }

    steps {
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
                #!/usr/bin/env bash
                source /etc/bashrc
                set -eo pipefail
                export PKG_CONFIG_PATH=/usr/local/lib/pkgconfig:${'$'}PKG_CONFIG_PATH
                export LD_LIBRARY_PATH=/usr/local/lib:${'$'}LD_LIBRARY_PATH
                export CMAKE_PREFIX_PATH=/usr/local:${'$'}CMAKE_PREFIX_PATH
                export CMAKE_INCLUDE_PATH=/usr/local/include:${'$'}CMAKE_INCLUDE_PATH
                export CMAKE_LIBRARY_PATH=/usr/local/lib:${'$'}CMAKE_LIBRARY_PATH

                # Initialize Conan and install pre-built dependencies from Nexus
                python run_conan.py initialize deltares --ci
                python build.py --config %product% --build --build-type %build_type% --ci --build-dir build --install-dir install
            """.trimIndent()
            dockerImage = "containers.deltares.nl/delft3d-dev/delft3d-third-party-libs:%dep.${LinuxThirdPartyLibs.id}.env.IMAGE_TAG%"
            dockerImagePlatform = ScriptBuildStep.ImagePlatform.Linux
            dockerRunParameters = "--rm --mount type=volume,source=delft3d-conan-cache,target=/conan-cache -e CONAN_LOGIN_USERNAME_DELFT3D_CONAN_DEV=%nexus_conan_username% -e CONAN_PASSWORD_DELFT3D_CONAN_DEV=%nexus_conan_password%"
            dockerPull = true
        }
        script {
            name = "Run unit tests"
            scriptContent = """ 
                #!/usr/bin/env bash
                source /etc/bashrc
                set -eo pipefail

                ctest --test-dir build --build-config %build_type% --output-junit ../unit-test-report-linux.xml --output-on-failure
            """.trimIndent()
            dockerImage = "containers.deltares.nl/delft3d-dev/delft3d-third-party-libs:%dep.${LinuxThirdPartyLibs.id}.env.IMAGE_TAG%"
            dockerImagePlatform = ScriptBuildStep.ImagePlatform.Linux
            dockerRunParameters = "--rm --mount type=volume,source=delft3d-conan-cache,target=/conan-cache"
            dockerPull = true
        }
        script {
            name = "Install"
            scriptContent = """
                #!/usr/bin/env bash
                source /etc/bashrc
                set -eo pipefail

                cmake --install build --config %build_type%
            """.trimIndent()
            dockerImage = "containers.deltares.nl/delft3d-dev/delft3d-third-party-libs:%dep.${LinuxThirdPartyLibs.id}.env.IMAGE_TAG%"
            dockerImagePlatform = ScriptBuildStep.ImagePlatform.Linux
            dockerRunParameters = "--rm --mount type=volume,source=delft3d-conan-cache,target=/conan-cache"
            dockerPull = true
        }
    }

    features {
        xmlReport {
            reportType = XmlReport.XmlReportType.JUNIT
            rules = "+:unit-test-report-linux.xml"
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

})
