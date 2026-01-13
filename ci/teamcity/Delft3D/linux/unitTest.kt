package Delft3D.linux

import java.io.File
import jetbrains.buildServer.configs.kotlin.*
import jetbrains.buildServer.configs.kotlin.buildSteps.*
import Delft3D.template.*
import Delft3D.linux.*
import Delft3D.step.*
import Trigger

object LinuxUnitTest : BuildType({

    name = "Unit Test"
    description = "Run unit tests."
    buildNumberPattern = "%dep.${LinuxBuild.id}.product%: %build.vcs.number%"

    artifactRules = """
        lnx64/test/**/*.html
        src/test/engines_gpl/dflowfm/packages/test_dflowfm/test_data/*.log
    """.trimIndent()

    templates(
        TemplateMergeRequest,
        TemplatePublishStatus,
        TemplateMonitorPerformance
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
        mergeTargetBranch {}
        step {
            name = "Download artifact from Nexus"
            type = "RawDownloadNexusLinux"
            executionMode = BuildStep.ExecutionMode.DEFAULT
            param("artifact_path", "/07_day_retention/dimrset/%file_path%")
            param("nexus_repo", "/delft3d-dev")
            param("nexus_username", "%nexus_username%")
            param("download_to", ".")
            param("nexus_password", "%nexus_password%")
            param("nexus_url", "https://artifacts.deltares.nl/repository")
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
        script {
            conditions {
                matches("product", """^(fm-(suite|testbench))|(all-testbench)$""")
            }
            name = "D-Flow FM: dflowfm_kernel_test"
            executionMode = BuildStep.ExecutionMode.ALWAYS
            workingDir = "lnx64/test/test_data"
            scriptContent = """
                ../dflowfm_kernel_test
                mv ftnunit.html ../dflowfm_kernel_ftnunit.html
            """.trimIndent()
        }
        script {
            conditions {
                matches("product", """^(fm-(suite|testbench))|(all-testbench)$""")
            }
            name = "D-Flow FM (version string): dflowfm --version > screen.log"
            executionMode = BuildStep.ExecutionMode.ALWAYS
            workingDir = "src/test/engines_gpl/dflowfm/packages/test_dflowfm/test_data"
            scriptContent = """
                dflowfm --version >screen.log 2>&1
            """.trimIndent()
        }
        python {
            conditions {
                matches("product", """^(fm-(suite|testbench))|(all-testbench)$""")
            }
            name = "D-Flow FM (version string): dflowfm_compare_version_output.py"
            executionMode = BuildStep.ExecutionMode.ALWAYS
            workingDir = "src/test/engines_gpl/dflowfm/packages/test_dflowfm/test_data"
            command = file {
                filename = "dflowfm_compare_version_output.py"
                scriptArguments = """
                    --ref dflowfm_version_lnx.log
                    --new screen.log
                """.trimIndent()
            }
        }
        script {
            conditions {
                matches("product", """^(fm-(suite|testbench))|(all-testbench)$""")
            }
            name = "D-Flow FM (version string): diff compare of version string"
            executionMode = BuildStep.ExecutionMode.ALWAYS
            workingDir = "src/test/engines_gpl/dflowfm/packages/test_dflowfm/test_data"
            scriptContent = """
                diff new_noversion.log dflowfm_version_lnx.log
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

    requirements {
        equals("teamcity.agent.jvm.os.name", "Linux")
    }
})
