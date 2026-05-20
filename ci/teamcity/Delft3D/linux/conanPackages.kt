package Delft3D.linux

import jetbrains.buildServer.configs.kotlin.*
import jetbrains.buildServer.configs.kotlin.buildFeatures.*
import jetbrains.buildServer.configs.kotlin.buildSteps.*
import jetbrains.buildServer.configs.kotlin.failureConditions.*
import Delft3D.template.*
import Delft3D.step.*
import Delft3D.linux.containers.*

object LinuxConanPackages : BuildType({

    description = "Build all Conan packages from source and push them to the Deltares Nexus remote."

    templates(
        TemplateLinuxAgent,
        TemplateMergeRequest,
        TemplatePublishStatus,
        TemplateMonitorPerformance,
        TemplateFailureCondition,
        TemplateDockerRegistry
    )

    name = "Conan packages"
    buildNumberPattern = "%build.vcs.number%"

    allowExternalStatus = true

    params {
        param("nexus.username", "")
        password("nexus.password", "credentialsJSON:xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx")
        param("env.CONAN_HOME", "/conan-cache")
    }

    vcs {
        root(DslContext.settingsRoot)
        cleanCheckout = true
        checkoutDir = "ossbuild-lnx64"
    }

    steps {
        script {
            name = "Initialize Conan and build all packages"
            scriptContent = """
                #!/usr/bin/env bash
                source /etc/bashrc
                set -eo pipefail

                # Initialize Conan with Deltares Nexus remotes and local recipes
                python run_conan.py --initialize-conan=deltares --ci

                # Rebuild all packages from local recipes (also regenerates lockfile)
                python run_conan.py --rebuild-recipes --ci
            """.trimIndent()
            dockerImage = "containers.deltares.nl/delft3d-dev/delft3d-third-party-libs:%dep.${LinuxThirdPartyLibs.id}.env.IMAGE_TAG%"
            dockerImagePlatform = ScriptBuildStep.ImagePlatform.Linux
            dockerRunParameters = "--rm --mount type=volume,source=delft3d-conan-cache,target=/conan-cache -e CONAN_LOGIN_USERNAME=%nexus.username% -e CONAN_PASSWORD=%nexus.password%"
            dockerPull = true
        }
        script {
            name = "Upload packages to Nexus"
            scriptContent = """
                #!/usr/bin/env bash
                source /etc/bashrc
                set -eo pipefail

                # Upload all packages in the local cache to the deltares-conan-dev remote
                conan upload "*" --remote=deltares-conan-dev --confirm
            """.trimIndent()
            dockerImage = "containers.deltares.nl/delft3d-dev/delft3d-third-party-libs:%dep.${LinuxThirdPartyLibs.id}.env.IMAGE_TAG%"
            dockerImagePlatform = ScriptBuildStep.ImagePlatform.Linux
            dockerRunParameters = "--rm --mount type=volume,source=delft3d-conan-cache,target=/conan-cache -e CONAN_LOGIN_USERNAME=%nexus.username% -e CONAN_PASSWORD=%nexus.password%"
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
})
