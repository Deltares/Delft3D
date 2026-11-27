package Delft3D.linux

import java.io.File
import jetbrains.buildServer.configs.kotlin.*
import jetbrains.buildServer.configs.kotlin.buildSteps.*
import Delft3D.template.*
import Delft3D.windows.*
import Delft3D.step.*
import Trigger

object WindowsUnitTest : BuildType({

    name = "Unit tests"
    description = "Run unit tests."
    buildNumberPattern = "%dep.${WindowsBuild.id}.product%: %build.vcs.number%"

    artifactRules = """
        x64/test/**/*.html
    """.trimIndent()

    templates(
        TemplateMergeRequest,
        TemplatePublishStatus,
        TemplateMonitorPerformance,
        TemplateBuildConcurrency
    )

    params {
        param("env.PATH", "%teamcity.build.checkoutDir%\\x64\\bin;%env.PATH%")
    }

    vcs {
        root(DslContext.settingsRoot)
        cleanCheckout = true
    }

    steps {
        python {
            conditions {
                matches("product", """^(fm-(suite|testbench))|(all-testbench)$""")
            }
            name = "EC Module: run_all_tests.py"
            workingDir = "x64/test"
            command = file {
                filename = "run_all_tests.py"
            }
        }
        script {
            conditions {
                matches("product", """^(fm-(suite|testbench))|(all-testbench)$""")
            }
            name = "EC Module: ec_module_test -c internal"
            executionMode = BuildStep.ExecutionMode.ALWAYS
            workingDir = "x64/test"
            scriptContent = """
                ec_module_test.exe -c internal
            """.trimIndent()
        }
        script {
            conditions {
                matches("product", """^(fm-(suite|testbench))|(all-testbench)$""")
            }
            name = "Deltares Common: test_deltares_common"
            executionMode = BuildStep.ExecutionMode.ALWAYS
            workingDir = "x64/test/test_data"
            scriptContent = """
                ..\test_deltares_common.exe
                mv ftnunit.html ..\deltares_common_ftnunit.html
            """.trimIndent()
        }
        script {
            conditions {
                matches("product", """^(fm-(suite|testbench))|(all-testbench)$""")
            }
            name = "IO NetCDF: test_io_netcdf"
            executionMode = BuildStep.ExecutionMode.ALWAYS
            workingDir = "x64/test"
            scriptContent = """
                test_io_netcdf.exe
                mv ftnunit.html io_netcdf_ftnunit.html
            """.trimIndent()
        }
    }

    dependencies {
        dependency(Trigger) {
            snapshot {
                onDependencyFailure = FailureAction.FAIL_TO_START
            }
        }
        dependency(WindowsBuild) {
            artifacts {
                artifactRules = "oss_artifacts_x64_*.zip!** => ."
            }
            snapshot {
                onDependencyFailure = FailureAction.FAIL_TO_START
                onDependencyCancel = FailureAction.CANCEL
            }
        }
    }

    requirements {
        exists("env.PYTHON_PATH")
        contains("teamcity.agent.jvm.os.name", "Windows")
    }
})
