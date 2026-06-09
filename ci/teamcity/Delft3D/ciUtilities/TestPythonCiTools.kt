package Delft3D.ciUtilities

import jetbrains.buildServer.configs.kotlin.*
import jetbrains.buildServer.configs.kotlin.buildSteps.*
import jetbrains.buildServer.configs.kotlin.buildFeatures.*
import jetbrains.buildServer.configs.kotlin.triggers.*

import Delft3D.template.*
import Delft3D.step.*
import Delft3D.linux.containers.*


object TestPythonCiTools : BuildType({
    id("TestPythonCiTools")
    name = "Test Python CI tools"
    buildNumberPattern = "%build.vcs.number%"
    description = """
        Runs tests and quality checks on the python CI tools (including DIMRset delivery).
    """.trimIndent()

    // The name `coverage.zip` for the pytest coverage report should not be changed.
    // Using the name `coverage.zip` will ensure TeamCity adds the `Coverage` tab to the build.
    // See: https://www.jetbrains.com/help/teamcity/importing-arbitrary-coverage-results-to-teamcity.html
    artifactRules = """
        +:ci/python/*.xml => report
        +:ci/python/htmlcov/* => coverage.zip
    """.trimIndent()

    templates(
        TemplatePublishStatus,
        TemplateMergeRequest
    )

    vcs {
        root(DslContext.settingsRoot)
        excludeDefaultBranchChanges = true  // Only include changes made within the branch of this build.
        cleanCheckout = true
    }

    triggers {
        vcs { 
            branchFilter = "+:pull/*"
        }
    }

    steps {
        script {
            name = "Install dependencies"
            workingDir = "ci/python"
            scriptContent = "uv sync"
            dockerImage = "containers.deltares.nl/delft3d-dev/delft3d-python:3.12"
            dockerImagePlatform = ScriptBuildStep.ImagePlatform.Linux
            dockerPull = true
            dockerRunParameters = """
                --rm
                --mount type=volume,source=uv-cache-python-ci-tools,destination=/root/.cache/uv
            """.trimIndent()
        }
        script {
            name = "Run checks"
            workingDir = "ci/python"
            scriptContent = """
                #!/usr/bin/env bash
                set -exo pipefail
                uv run ruff format --diff
                uv run ruff check --output-format=junit --output-file=ruff.xml
                uv run mypy ci_tools --junit-xml=mypy.xml
                uv run pytest --junitxml=pytest.xml --cov-report=html --cov=.
            """.trimIndent()
            dockerImage = "containers.deltares.nl/delft3d-dev/delft3d-python:3.12"
            dockerImagePlatform = ScriptBuildStep.ImagePlatform.Linux
            dockerPull = true
            dockerRunParameters = "--rm"
        }
    }

    features {
        xmlReport {
            reportType = XmlReport.XmlReportType.JUNIT
            rules = """
                +:ci/python/ruff.xml
                +:ci/python/mypy.xml
                +:ci/python/pytest.xml
            """.trimIndent()
        }
    }

    requirements {
        contains("teamcity.agent.jvm.os.name", "Linux")
    }
})
