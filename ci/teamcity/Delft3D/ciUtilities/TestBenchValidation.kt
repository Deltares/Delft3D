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
        param("env.IMAGE_NAME", "containers.deltares.nl/delft3d-dev/testbench-validation")
        param("env.BUILD_BRANCH", "%teamcity.build.branch%")
        param("env.PULL_REQUEST_SOURCE_BRANCH", "%teamcity.pullRequest.source.branch%")
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
                    +:/test/deltares_testbench/ci/dockerfiles/testbench.Dockerfile
                    +:/test/deltares_testbench/docker-bake.hcl
                    +:/ci/teamcity/Delft3D/ciUtilities/TestBenchValidation.kt
                    +:/ci/teamcity/Delft3D/ciUtilities/scripts/validateReports.sh
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
        dockerCommand {
            name = "Run validation"
            commandType = other {
                subCommand = "buildx"
                workingDir = "test/deltares_testbench"
                commandArgs = "bake validate"
            }
        }
        validateReports {}
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

/**
 * Check if the report files exist, and fail the build if there's something wrong with them.
 */
fun BuildSteps.validateReports(init: ScriptBuildStep.() -> Unit): BuildStep {
    val result = ScriptBuildStep(init)
    result.name = "Validate reports"
    result.workingDir = "test/deltares_testbench"
    val script = File(DslContext.baseDir, "ciUtilities/scripts/validateReports.sh")
    result.scriptContent = Util.readScript(script)
    step(result)
    return result
}
