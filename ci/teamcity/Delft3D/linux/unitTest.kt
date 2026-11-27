package Delft3D.linux

import java.io.File
import jetbrains.buildServer.configs.kotlin.*
import jetbrains.buildServer.configs.kotlin.buildSteps.*
import Delft3D.template.*
import Delft3D.linux.*
import Delft3D.step.*
import Trigger

object LinuxUnitTest : BuildType({

    name = "Unit tests"
    description = "Run unit tests."
    buildNumberPattern = "%dep.${LinuxBuild.id}.product%: %build.vcs.number%"

    artifactRules = """
        lnx64/test/**/*.html
    """.trimIndent()

    templates(
        TemplateLinuxAgent,
        TemplateMergeRequest,
        TemplatePublishStatus,
        TemplateMonitorPerformance,
        TemplateBuildConcurrency
    )

    params {
        param("env.PATH", "%teamcity.build.checkoutDir%/lnx64/bin:%env.PATH%")
        param("env.LD_LIBRARY_PATH", "%teamcity.build.checkoutDir%/lnx64/lib:%env.LD_LIBRARY_PATH%")
        param("file_path", "dimrset_linux_%dep.${LinuxBuild.id}.product%_%build.vcs.number%.tar.gz")
    }

    vcs {
        root(DslContext.settingsRoot)
        cleanCheckout = true
    }

    steps {
        step {
            name = "Download artifact from Nexus"
            type = "RawDownloadNexusLinux2"
            executionMode = BuildStep.ExecutionMode.DEFAULT
            param("artifact_path", "/07_day_retention/dimrset/%file_path%")
            param("nexus_repo", "/delft3d-dev")
            param("nexus_username", "%nexus_username%")
            param("download_to", "/downloads")
            param("nexus_password", "%nexus_password%")
        }
        script {
            name = "Extract artifact"
            enabled = false
            scriptContent = """
                echo "Extracting %file_path%..."

                tar -xzf %file_path%
            """.trimIndent()
        }
        python {
            conditions {
                matches("product", """^(fm-(suite|testbench))|(all-testbench)$""")
            }
            name = "EC Module: run_all_tests.py"
            workingDir = "lnx64/test"
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
            workingDir = "lnx64/test"
            scriptContent = """
                ./ec_module_test -c internal
            """.trimIndent()
        }
        script {
            conditions {
                matches("product", """^(fm-(suite|testbench))|(all-testbench)$""")
            }
            name = "Deltares Common: test_deltares_common"
            executionMode = BuildStep.ExecutionMode.ALWAYS
            workingDir = "lnx64/test/test_data"
            scriptContent = """
                ../test_deltares_common
                mv ftnunit.html ../deltares_common_ftnunit.html
            """.trimIndent()
        }
        script {
            conditions {
                matches("product", """^(fm-(suite|testbench))|(all-testbench)$""")
            }
            name = "IO NetCDF: test_io_netcdf"
            executionMode = BuildStep.ExecutionMode.ALWAYS
            workingDir = "lnx64/test"
            scriptContent = """
                ./test_io_netcdf
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
        dependency(LinuxCollect) {
            artifacts {
                artifactRules = "dimrset_lnx64_*.tar.gz!** => ."
            }
            snapshot {
                onDependencyFailure = FailureAction.FAIL_TO_START
                onDependencyCancel = FailureAction.CANCEL
            }
        }
    }
})
