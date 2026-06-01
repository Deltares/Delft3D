package Delft3D.linux

import jetbrains.buildServer.configs.kotlin.*
import jetbrains.buildServer.configs.kotlin.buildFeatures.*
import jetbrains.buildServer.configs.kotlin.buildSteps.*
import jetbrains.buildServer.configs.kotlin.failureConditions.*
import jetbrains.buildServer.configs.kotlin.triggers.*
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
            name = "Build and upload all packages"
            scriptContent = """
                #!/usr/bin/env bash
                source /etc/bashrc
                set -eo pipefail

                python run_conan.py initialize deltares --ci

                python run_conan.py install --rebuild-packages --ci

                python run_conan.py upload --remote=delft3d-conan-dev --ci
            """.trimIndent()
            dockerImage = "containers.deltares.nl/delft3d-dev/delft3d-third-party-libs:%dep.${LinuxThirdPartyLibs.id}.env.IMAGE_TAG%"
            dockerImagePlatform = ScriptBuildStep.ImagePlatform.Linux
            dockerRunParameters = "--rm -e CONAN_LOGIN_USERNAME_DELFT3D_CONAN_DEV=%nexus_conan_username% -e CONAN_PASSWORD_DELFT3D_CONAN_DEV=%nexus_conan_password%"
            dockerPull = true
        }
    }

    triggers {
        vcs {
            triggerRules = """
                +:conan.lock
                +:conanfile.py
                +:run_conan.py
                +:conan/config/**
            """.trimIndent()
            branchFilter = "+:*"
        }
        schedule {
            schedulingPolicy = weekly {
                dayOfWeek = ScheduleTrigger.DAY.Thursday
                hour = 0
                minute = 0
            }
            branchFilter = "+:<default>"
            triggerBuild = always()
            withPendingChangesOnly = false
        }
    }

    failureConditions {
        executionTimeoutMin = 600
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
