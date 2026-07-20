package Delft3D.ciUtilities

import jetbrains.buildServer.configs.kotlin.*
import jetbrains.buildServer.configs.kotlin.buildSteps.*
import jetbrains.buildServer.configs.kotlin.buildFeatures.*
import jetbrains.buildServer.configs.kotlin.triggers.*

import Delft3D.template.*
import Delft3D.step.*


object TestFortranStyler : BuildType({
    id("TestFortranStyler")
    name = "Test Fortran Styler"
    description = """
        Runs tests and quality checks on the Fortran Styler.
    """.trimIndent()

    // The name `coverage.zip` for the pytest coverage report should not be changed.
    // Using the name `coverage.zip` will ensure TeamCity adds the `Coverage` tab to the build.
    // See: https://www.jetbrains.com/help/teamcity/importing-arbitrary-coverage-results-to-teamcity.html
    artifactRules = """
        +:tools/deltares_fortran_styler/pytest.xml => report
        +:tools/deltares_fortran_styler/htmlcov/* => coverage.zip
    """.trimIndent()

    params {
        param("docker_image", "containers.deltares.nl/delft3d-dev/delft3d-python:alma8-python3.12")
    }

    templates(
        TemplatePublishStatus,
        TemplateMergeRequest,
        TemplateDockerRegistry,
        TemplateBuildConcurrency
    )

    vcs {
        root(DslContext.settingsRoot)
        excludeDefaultBranchChanges = true  // Only include changes made within the branch of this build.
        cleanCheckout = true
    }

    triggers {
        vcs {
            // Trigger this build only if there are changes to the files matching these rules.
            // Absolute paths match paths relative to the VCS root.
            // See: https://www.jetbrains.com/help/teamcity/configuring-vcs-triggers.html#General+Syntax
            triggerRules = """
                +:/tools/deltares_fortran_styler/**/*.py
                +:/tools/deltares_fortran_styler/pyproject.toml
            """.trimIndent()
            branchFilter = "+:pull/*"
        }
    }

    steps {
        script {
            name = "Install dependencies"
            workingDir = "tools/deltares_fortran_styler"
            scriptContent = """
                #!/usr/bin/env bash
                uv sync --extra=dev
            """.trimIndent()
            dockerImage = "%docker_image%"
            dockerImagePlatform = ScriptBuildStep.ImagePlatform.Linux
            dockerPull = true
            dockerRunParameters = """
                --mount type=volume,source=uv-cache-fortran-styler,destination=/root/.cache/uv
                --env UV_LINK_MODE=copy
                --rm
            """.trimIndent()
        }
        script {
            name = "Run checks"
            workingDir = "tools/deltares_fortran_styler"
            scriptContent = """
                #!/usr/bin/env bash
                set -exo pipefail
                uv run pytest --junitxml=pytest.xml --cov-report=html --cov=.
            """.trimIndent()
            dockerImage = "%docker_image%"
            dockerImagePlatform = ScriptBuildStep.ImagePlatform.Linux
            dockerPull = true
            dockerRunParameters = """
                --env UV_LINK_MODE=copy
                --rm
            """.trimIndent()
        }
    }

    features {
        xmlReport {
            reportType = XmlReport.XmlReportType.JUNIT
            rules = """
                +:tools/deltares_fortran_styler/pytest.xml
            """.trimIndent()
        }
    }

    requirements {
        contains("teamcity.agent.jvm.os.name", "Linux")
    }
})
