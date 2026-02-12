package Delft3D.ciUtilities

import jetbrains.buildServer.configs.kotlin.*
import jetbrains.buildServer.configs.kotlin.buildSteps.*
import jetbrains.buildServer.configs.kotlin.buildFeatures.*
import jetbrains.buildServer.configs.kotlin.triggers.*

import Delft3D.template.*
import Delft3D.step.*


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
            // Trigger this build only if there are changes to the files matching these rules.
            // Absolute paths match paths relative to the VCS root.
            // See: https://www.jetbrains.com/help/teamcity/configuring-vcs-triggers.html#General+Syntax
            triggerRules = """
                +:/ci/python/**/*.py
                +:/ci/python/pyproject.toml
                +:/ci/python/uv.lock
            """.trimIndent()
            branchFilter = "+:pull/*"
        }
    }

    steps {
        python {
            name = "Check code formatting"
            workingDir = "ci/python"
            pythonVersion = customPython { executable = "python3.11" }
            environment = venv {
                requirementsFile = ""
                pipArgs = "--editable .[all]"
            }
            command = module {
                module = "ruff"
                scriptArguments = "format --diff"
            }
            executionMode = BuildStep.ExecutionMode.ALWAYS
        }
        python {
            name = "Run linter"
            workingDir = "ci/python"
            pythonVersion = customPython { executable = "python3.11" }
            environment = venv {
                requirementsFile = ""
                pipArgs = "--editable .[all]"
            }
            command = module {
                module = "ruff"
                scriptArguments = "check --output-format=junit --output-file=ruff.xml"
            }
            executionMode = BuildStep.ExecutionMode.ALWAYS
        }
        python {
            name = "Run type checker"
            workingDir = "ci/python"
            pythonVersion = customPython { executable = "python3.11" }
            environment = venv {
                requirementsFile = ""
                pipArgs = "--editable .[all]"
            }
            command = module {
                module = "mypy"
                scriptArguments = "ci_tools --junit-xml=mypy.xml"
            }
            executionMode = BuildStep.ExecutionMode.ALWAYS
        }
        python {
            name = "Run unit tests"
            workingDir = "ci/python"
            pythonVersion = customPython { executable = "python3.11" }
            environment = venv {
                requirementsFile = ""
                pipArgs = "--editable .[all]"
            }
            command = module {
                module = "pytest"
                scriptArguments = """
                    --junitxml=pytest.xml
                    --cov-report=html
                    --cov=.
                """.trimIndent()
            }
            executionMode = BuildStep.ExecutionMode.ALWAYS
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