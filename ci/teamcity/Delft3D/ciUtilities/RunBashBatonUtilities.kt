package Delft3D.ciUtilities

import jetbrains.buildServer.configs.kotlin.*
import jetbrains.buildServer.configs.kotlin.buildSteps.*
import jetbrains.buildServer.configs.kotlin.buildFeatures.*
import jetbrains.buildServer.configs.kotlin.triggers.*

import Delft3D.template.*
import Delft3D.step.*


object RunBashBatonUtilities : BuildType({
    id("RunBashBatonUtilities")

    name = "Run BashBaton Utilities"

    description = """
        Runs BashBaton utilities (codespell, shfmt, shellcheck, bashunit and bashcov) on bash scripts.
    """.trimIndent()

    templates(
        TemplatePublishStatus,
        TemplateMergeRequest,
        TemplateDockerRegistry
    )

    vcs {
        root(DslContext.settingsRoot)
        excludeDefaultBranchChanges = true
        cleanCheckout = true
    }

    // triggers {
    //     vcs { 
    //         // Trigger this build only if there are changes to the files matching these rules.
    //         // Absolute paths match paths relative to the VCS root.
    //         // See: https://www.jetbrains.com/help/teamcity/configuring-vcs-triggers.html#General+Syntax
    //         triggerRules = """
    //             +:ci/teamcity/Delft3D/verschilanalyse/**/*.sh
    //         """.trimIndent()
    //         branchFilter = "+:pull/*"
    //     }
    // }

    steps {
        mergeTargetBranch {}
        script {
            name = "Run unit tests"
            scriptContent = """
                #!/usr/bin/env bash
                echo "[[ bashunit ]]"
                bashunit --version
                echo "[[ shfmt ]]"
                shfmt --version
                echo "[[ shellcheck ]]"
                shellcheck --version
                echo "[[ bashcov ]]"
                bashcov --version
                echo "[[ codespell ]]"
                codespell --version            
                """.trimIndent()
            dockerImage = "containers.deltares.nl/bashbaton-dev/bashbaton:main"
            dockerImagePlatform = ScriptBuildStep.ImagePlatform.Linux
            dockerRunParameters = "--rm"
            dockerPull = true
        }
    }

    requirements {
        contains("teamcity.agent.jvm.os.name", "Linux")
    }
})