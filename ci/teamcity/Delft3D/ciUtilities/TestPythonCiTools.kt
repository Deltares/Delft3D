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
    description = """
        Runs tests and quality checks on the python CI tools.
    """.trimIndent()

    artifactRules = """
        +:ci/python/*.xml => report
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
            branchFilter = "+:merge-requests/*"
        }
    }

    steps {
        mergeTargetBranch {}
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
                scriptArguments = "--junitxml=pytest.xml"
            }
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