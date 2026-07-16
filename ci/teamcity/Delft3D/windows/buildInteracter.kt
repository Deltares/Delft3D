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
        TemplateFailureCondition,
        TemplateDockerRegistry,
        TemplateBuildConcurrency
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
        param("container.tag", "vs2022-intel2024-ltsc2025")
        param("build.vcs.number", "${DslContext.settingsRoot.paramRefs.buildVcsNumber}")
        param("env.CONAN_HOME", "C:/conan-cache")
        param("nexus_conan_username", DslContext.getParameter("nexus_conan_username"))
        password("nexus_conan_password", DslContext.getParameter("nexus_conan_password"))
        select("build_type", "Release", display = ParameterDisplay.PROMPT,
                options = listOf("Release", "Debug"))
    }

    vcs {
        root(DslContext.settingsRoot)
        root(AbsoluteId("ReposDsRoot"), "+:trunk/src/third_party/interacter => ./src/third_party/interacter")

        cleanCheckout = true
        checkoutDir = "ossbuild-win"
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
                call C:/set-env.cmd

                python run_conan.py initialize deltares --ci
                if %%errorlevel%% neq 0 exit /b %%errorlevel%%

                python build.py --config %product% --build --build-type %build_type% --ci --build-dir build_%product% --install-dir build_%product%/install
                if %%errorlevel%% neq 0 exit /b %%errorlevel%%
            """.trimIndent()
            dockerImage = "containers.deltares.nl/delft3d-dev/delft3d-buildtools-windows:%container.tag%"
            dockerImagePlatform = ScriptBuildStep.ImagePlatform.Windows
            dockerPull = true
            dockerRunParameters = "--memory %teamcity.agent.hardware.memorySizeMb%m --cpus %teamcity.agent.hardware.cpuCount% --mount type=volume,source=delft3d-conan-cache,target=C:/conan-cache -e CONAN_LOGIN_USERNAME_DELFT3D_CONAN_DEV=%nexus_conan_username% -e CONAN_PASSWORD_DELFT3D_CONAN_DEV=%nexus_conan_password%"
        }
    }
    if (DslContext.getParameter("enable_schedule_interacter_build").lowercase() == "true") {
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
    requirements {
        doesNotEqual("teamcity.agent.jvm.os.name", "Windows Server 2022")
    }
})
