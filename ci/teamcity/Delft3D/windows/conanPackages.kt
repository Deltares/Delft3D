package Delft3D.windows

import jetbrains.buildServer.configs.kotlin.*
import jetbrains.buildServer.configs.kotlin.buildFeatures.*
import jetbrains.buildServer.configs.kotlin.buildSteps.*
import jetbrains.buildServer.configs.kotlin.failureConditions.*
import jetbrains.buildServer.configs.kotlin.triggers.*
import Delft3D.template.*
import Delft3D.step.*

object WindowsConanPackages : BuildType({

    description = "Build all Conan packages from source and push them to the Deltares Nexus remote."

    templates(
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
        param("container.tag", "vs2022-intel2024-ltsc2025")
        param("nexus_conan_username", DslContext.getParameter("nexus_conan_username"))
        password("nexus_conan_password", DslContext.getParameter("nexus_conan_password"))
        param("env.CONAN_HOME", "C:/conan-cache")
    }

    vcs {
        root(DslContext.settingsRoot)
        cleanCheckout = true
        checkoutDir = "ossbuild-win"
    }

    steps {
        script {
            name = "Build and upload all packages"
            scriptContent = """
                call C:/set-env-vs2022.cmd

                rem TeamCity points TEMP/TMP/TMPDIR at the agent's temp folder, which is
                rem bind-mounted into the container. PETSc's configure builds AND runs test
                rem executables in the temp dir, and executing a freshly-built binary from a
                rem Windows bind mount inside a container fails ("Cannot run executables
                rem created with C"). Redirect temp to a container-local directory.
                if not exist C:\build-temp mkdir C:\build-temp
                set TEMP=C:\build-temp
                set TMP=C:\build-temp
                set TMPDIR=C:\build-temp

                python run_conan.py initialize deltares --ci
                if %%errorlevel%% neq 0 exit /b %%errorlevel%%

                python run_conan.py install --rebuild-packages --ci --output-folder build
                if %%errorlevel%% neq 0 exit /b %%errorlevel%%

                python run_conan.py upload --remote=delft3d-conan-dev --ci
                if %%errorlevel%% neq 0 exit /b %%errorlevel%%
            """.trimIndent()
            dockerImage = "containers.deltares.nl/delft3d-dev/delft3d-buildtools-windows:%container.tag%"
            dockerImagePlatform = ScriptBuildStep.ImagePlatform.Windows
            dockerRunParameters = "-e CONAN_LOGIN_USERNAME_DELFT3D_CONAN_DEV=%nexus_conan_username% -e CONAN_PASSWORD_DELFT3D_CONAN_DEV=%nexus_conan_password%"
            dockerPull = true
        }
    }

    triggers {
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

    requirements {
        doesNotEqual("teamcity.agent.jvm.os.name", "Windows Server 2022")
    }
})
