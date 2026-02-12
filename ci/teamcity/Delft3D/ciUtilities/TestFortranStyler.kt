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
        +:tools/deltares_fortran_styler/*.xml => report
        +:tools/deltares_fortran_styler/htmlcov/* => coverage.zip
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
                +:/tools/deltares_fortran_styler/**/*.py
                +:/tools/deltares_fortran_styler/pyproject.toml
            """.trimIndent()
            branchFilter = "+:pull/*"
        }
    }

    steps {
        python {
            name = "Run unit tests"
            workingDir = "tools/deltares_fortran_styler"
            pythonVersion = customPython { executable = "python3.11" }
            environment = venv {
                requirementsFile = ""
                pipArgs = "--editable .[dev]"
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
                +:tools/deltares_fortran_styler/pytest.xml
            """.trimIndent()
        }
    }

    requirements {
        contains("teamcity.agent.jvm.os.name", "Linux")
    }
})
