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

    val targetPath = "ci/teamcity/Delft3D/verschilanalyse"
    val dockerImageName = "containers.deltares.nl/bashbaton-dev/bashbaton:main"

    triggers {
        vcs {
            triggerRules = """
                +:$targetPath/**/*.sh
            """.trimIndent()

            branchFilter = "+:pull/*"
        }
    }

    steps {
        mergeTargetBranch {}

        script {
            name = "Display versions"
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

            dockerImage = dockerImageName
            dockerImagePlatform = ScriptBuildStep.ImagePlatform.Linux
            dockerRunParameters = "--rm"
            dockerPull = true
        }

        script {
            name = "Run codespell"
            scriptContent = """
                #!/usr/bin/env bash
                codespell --enable-colors $targetPath
            """.trimIndent()

            dockerImage = dockerImageName
            dockerImagePlatform = ScriptBuildStep.ImagePlatform.Linux
            dockerRunParameters = "--rm"
            dockerPull = true
            executionMode = BuildStep.ExecutionMode.RUN_ON_FAILURE
        }

        script {
            name = "Run shfmt"
            scriptContent = """
                #!/usr/bin/env bash
                FORCE_COLOR=1 shfmt --indent 2 --list --diff $targetPath
            """.trimIndent()

            dockerImage = dockerImageName
            dockerImagePlatform = ScriptBuildStep.ImagePlatform.Linux
            dockerRunParameters = "--rm"
            dockerPull = true
            executionMode = BuildStep.ExecutionMode.RUN_ON_FAILURE
        }

        script {
            name = "Run shellcheck"
            scriptContent = """
                #!/usr/bin/env bash
                shellcheck --shell=bash --format=tty --severity=style $targetPath/**/*.sh
            """.trimIndent()

            dockerImage = dockerImageName
            dockerImagePlatform = ScriptBuildStep.ImagePlatform.Linux
            dockerRunParameters = "--rm"
            dockerPull = true
            executionMode = BuildStep.ExecutionMode.RUN_ON_FAILURE
        }

        script {
            name = "will not fail"
            scriptContent = """
                #!/usr/bin/env bash
                echo "Mike has a little lamb"
            """.trimIndent()
            executionMode = BuildStep.ExecutionMode.RUN_ON_FAILURE
        }
    }

    requirements {
        contains("teamcity.agent.jvm.os.name", "Linux")
    }
})