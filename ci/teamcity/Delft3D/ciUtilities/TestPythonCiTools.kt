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
        +:ci/python/ruff_format.patch => report
        +:ci/python/htmlcov/* => coverage.zip
    """.trimIndent()

    templates(
        TemplatePublishStatus,
        TemplateMergeRequest,
        TemplateDockerRegistry,
    )

    vcs {
        root(DslContext.settingsRoot)
        excludeDefaultBranchChanges = true  // Only include changes made within the branch of this build.
        cleanCheckout = true
    }

    params {
        param("docker_image", "containers.deltares.nl/delft3d-dev/delft3d-python:alma8-python3.12")
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
            scriptContent = """
                #!/usr/bin/env bash
                uv sync --extra=all
            """.trimIndent()
            dockerImage = "%docker_image%"
            dockerImagePlatform = ScriptBuildStep.ImagePlatform.Linux
            dockerPull = true
            dockerRunParameters = """
                --mount type=volume,source=uv-cache-python-ci-tools,destination=/root/.cache/uv
                --env UV_LINK_MODE=copy
                --rm
            """.trimIndent()
        }
        script {
            name = "Run checks"
            workingDir = "ci/python"
            scriptContent = """
                #!/usr/bin/env bash
                set -exo pipefail
                uv run ruff format --diff . > ruff_format.patch
                uv run ruff check --output-format=junit --output-file=ruff.xml
                uv run mypy ci_tools --junit-xml=mypy.xml
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
