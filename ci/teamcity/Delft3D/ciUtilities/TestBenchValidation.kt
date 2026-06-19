package Delft3D.ciUtilities

import jetbrains.buildServer.configs.kotlin.*
import jetbrains.buildServer.configs.kotlin.buildSteps.*
import jetbrains.buildServer.configs.kotlin.buildFeatures.*
import jetbrains.buildServer.configs.kotlin.triggers.*
import Delft3D.template.*
import Delft3D.step.*
import java.io.File


object TestBenchValidation : BuildType({
    id("TestBenchValidation")
    name = "TestBench validation"
    buildNumberPattern = "%build.vcs.number%"
    description = """
        Runs the TestBench validation. 
        This includes the pytest test-suite, the formatter check, the linter and the type checker.
    """.trimIndent()

    // The name `coverage.zip` for the pytest coverage report should not be changed.
    // Using the name `coverage.zip` will ensure TeamCity adds the `Coverage` tab to the build.
    // See: https://www.jetbrains.com/help/teamcity/importing-arbitrary-coverage-results-to-teamcity.html
    artifactRules = """
        +:test/deltares_testbench/report/*.* => report
        +:test/deltares_testbench/report/htmlcov/* => coverage.zip
    """.trimIndent()

    params {
        param("docker_image", "containers.deltares.nl/delft3d-dev/delft3d-python:alma8-python3.12")
    }

    vcs {
        root(DslContext.settingsRoot)
        excludeDefaultBranchChanges = true  // Only include changes made within the branch of this build.
        cleanCheckout = true
    }

    templates(
        TemplatePublishStatus,
        TemplateMergeRequest,
        TemplateDockerRegistry,
    )

    triggers {
        if (DslContext.getParameter("enable_testbench_validation_trigger").lowercase() == "true") {
            vcs { 
                // Trigger this build only if there are changes to the files matching these rules.
                // Absolute paths match paths relative to the VCS root.
                // See: https://www.jetbrains.com/help/teamcity/configuring-vcs-triggers.html#General+Syntax
                triggerRules = """
                    +:/test/deltares_testbench/**/*.py
                    +:/test/deltares_testbench/TestBench.py
                    +:/test/deltares_testbench/pip/*-requirements.txt
                    +:/test/deltares_testbench/pyproject.toml
                    +:/ci/teamcity/Delft3D/ciUtilities/TestBenchValidation.kt
                """.trimIndent()
                branchFilter = """
                    +:pull/*
                    +:all/release/*
                    +:<default>
                """.trimIndent()
            }
        }
    }

    steps {
        script {
            name = "Install dependencies"
            workingDir = "test/deltares_testbench"
            scriptContent = """
                #!/usr/bin/env bash
                uv venv
                source .venv/bin/activate
                uv pip sync pip/lnx-dev-requirements.txt
            """.trimIndent()
            dockerImage = "%docker_image%"
            dockerImagePlatform = ScriptBuildStep.ImagePlatform.Linux
            dockerPull = true
            dockerRunParameters = """
                --mount type=volume,source=uv-cache-test-bench-validation,destination=/root/.cache/uv
                --env UV_LINK_MODE=copy
                --rm
            """.trimIndent()
        }
        script {
            name = "Run checks"
            workingDir = "test/deltares_testbench"
            scriptContent = """
                #!/usr/bin/env bash
                source .venv/bin/activate
                set -exo pipefail

                mkdir -p report
                ruff format --diff . > report/ruff_format.patch
                ruff check --select F4,F5,F6,F7,W,I --output-format=junit --output-file=report/ruff_check.xml
                pytest --junitxml=report/pytest.xml --cov-report=html:report/htmlcov --cov=.
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
                +:test/deltares_testbench/report/ruff_check.xml
                +:test/deltares_testbench/report/pytest.xml
            """.trimIndent()
        }
    }

    requirements {
        contains("teamcity.agent.jvm.os.name", "Linux")
    }
})
