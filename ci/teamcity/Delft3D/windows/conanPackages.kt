package Delft3D.windows

import jetbrains.buildServer.configs.kotlin.*
import jetbrains.buildServer.configs.kotlin.buildFeatures.*
import jetbrains.buildServer.configs.kotlin.buildSteps.*
import jetbrains.buildServer.configs.kotlin.failureConditions.*
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
        param("container.tag", "vs2022-intel2024")
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
                python run_conan.py --initialize-conan=deltares --ci
                if %%errorlevel%% neq 0 exit /b %%errorlevel%%

                python run_conan.py --rebuild-recipes --ci
                if %%errorlevel%% neq 0 exit /b %%errorlevel%%

                conan upload "*" --remote=delft3d-conan-dev --confirm
                if %%errorlevel%% neq 0 exit /b %%errorlevel%%
            """.trimIndent()
            dockerImage = "containers.deltares.nl/delft3d-dev/delft3d-buildtools-windows:%container.tag%"
            dockerImagePlatform = ScriptBuildStep.ImagePlatform.Windows
            dockerRunParameters = "-e CONAN_LOGIN_USERNAME_DELFT3D_CONAN_DEV=%nexus_conan_username% -e CONAN_PASSWORD_DELFT3D_CONAN_DEV=%nexus_conan_password%"
            dockerPull = true
        }
    }

    requirements {
        doesNotEqual("teamcity.agent.jvm.os.name", "Windows Server 2025")
    }
})
