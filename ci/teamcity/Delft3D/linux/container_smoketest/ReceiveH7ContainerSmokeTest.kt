package Delft3D.linux.container_smoketest

import Delft3D.linux.*
import Delft3D.step.*
import Delft3D.template.*
import jetbrains.buildServer.configs.kotlin.*
import jetbrains.buildServer.configs.kotlin.buildFeatures.*
import jetbrains.buildServer.configs.kotlin.buildSteps.*
import jetbrains.buildServer.configs.kotlin.failureConditions.*

object LinuxReceiveH7ContainerSmokeTest : BuildType({
    
    templates(
        TemplateMonitorPerformance,
        TemplateDockerRegistry
    )

    name = "Receive"
    description = "Receive H7 container smoke tests."
    buildNumberPattern = "%build.vcs.number%"

    artifactRules = """
        test\deltares_testbench\data\cases\**\*.pdf => pdf
        test\deltares_testbench\data\cases\**\*.dia => logging
        test\deltares_testbench\data\cases\**\*.log => logging
        test\deltares_testbench\data\cases\**\*.out => logging
        test\deltares_testbench\logs => logging
        test\deltares_testbench\copy_cases => copy_cases.zip
    """.trimIndent()

    params {
        // H7 smoke test directory
        param("h7_work_directory", "~/smoke/%build.revisions.short%")
        
        // H7 cluster access credentials
        param("h7_account_username", DslContext.getParameter("ad_h7_smoke_test_user"))
        password("h7_account_password", DslContext.getParameter("ad_h7_smoke_test_password"))
        
        param("testbench_container_image", "containers.deltares.nl/delft3d-dev/test/delft3d-test-container:alma8-%dep.${LinuxBuild.id}.product%-%dep.${LinuxBuild.id}.commit_id%")
    }

    vcs {
        root(DslContext.settingsRoot)
        cleanCheckout = true
    }

    steps {
        dockerCommand {
            name = "Download results from H7"
            commandType = other {
                subCommand = "run"
                commandArgs = """
                    --rm
                    -v %teamcity.build.workingDir%:/workspace
                    -w /workspace
                    --entrypoint /bin/bash
                    containers.deltares.nl/docker-proxy/almalinux:latest
                    -c "dnf install -y sshpass openssh-clients && \
                        mkdir -p ~/.ssh test/deltares_testbench/data/cases && \
                        ssh-keyscan h7.directory.intra >> ~/.ssh/known_hosts && \
                        echo \"Downloading files from %h7_work_directory%...\" && \
                        sshpass -p '%h7_account_password%' scp -r %h7_account_username%@h7.directory.intra:%h7_work_directory%/* test/deltares_testbench/data/cases/ && \
                        echo \"Download complete. Files copied:\" && \
                        ls -lh test/deltares_testbench/data/cases/"
                """.trimIndent()
            }
        }
        python {
            name = "run post-processing"
            pythonVersion = customPython { executable = "python3" }
            workingDir = "test/deltares_testbench"
            command = file {
                filename = "TestBench.py"
                scriptArguments = """
                    --username "%s3_dsctestbench_accesskey%"
                    --password "%s3_dsctestbench_secret%"
                    --compare 
                    --skip-run
                    --skip-download cases 
                    --config configs/apptainer/dimr/dimr_smoke_test_lnx64.xml
                    --log-level INFO 
                    --teamcity 
                    --parallel
                """.trimIndent()
            }
            dockerImage = "%testbench_container_image%"
            dockerImagePlatform = PythonBuildStep.ImagePlatform.Linux
            dockerPull = true
            dockerRunParameters = """
                --rm
                --pull always
                --shm-size 8G
                -v %teamcity.build.workingDir%:/data/data/cases
                -v %teamcity.build.workingDir%/test/deltares_testbench:/testbench
            """.trimIndent()
        }
    }

    dependencies { snapshot(LinuxSubmitH7ContainerSmokeTest) {} }
})