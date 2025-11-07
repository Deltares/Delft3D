package Delft3D.linux.container_smoketest

import Delft3D.linux.*
import Delft3D.step.*
import Delft3D.template.*
import jetbrains.buildServer.configs.kotlin.*
import jetbrains.buildServer.configs.kotlin.buildFeatures.*
import jetbrains.buildServer.configs.kotlin.buildSteps.*
import jetbrains.buildServer.configs.kotlin.failureConditions.*

object LinuxSubmitH7ContainerSmokeTest : BuildType({
    templates(
        TemplateMonitorPerformance,
        TemplateDockerRegistry
    )

    name = "Submit"
    description = "Submit H7 container smoke tests."
    buildNumberPattern = "%build.vcs.number%"

    params {
        // Container configuration
        param("runtime_container_image", "containers.deltares.nl/delft3d-dev/delft3d-runtime-container:alma8-%dep.${LinuxBuild.id}.product%-%dep.${LinuxBuild.id}.commit_id%")
        param("testbench_container_image", "containers.deltares.nl/delft3d-dev/test/delft3d-test-container:alma8-%dep.${LinuxBuild.id}.product%-%dep.${LinuxBuild.id}.commit_id%")

        // H7 smoke test directory
        param("h7_work_directory", "smoke/%build.revisions.short%")

        // H7 cluster access credentials
        param("h7_account_username", DslContext.getParameter("ad_h7_smoke_test_user"))
        password("h7_account_password", DslContext.getParameter("ad_h7_smoke_test_password"))

        // TeamCity configuration name for result upload
        param("teamcity_receive_config", "${LinuxReceiveH7ContainerSmokeTest.id}")
    }

    vcs {
        root(DslContext.settingsRoot)
        cleanCheckout = true
    }

    steps {
        python {
            name = "Run TestBench.py to download cases"
            pythonVersion = customPython { executable = "python3" }
            workingDir = "test/deltares_testbench"
            command = file {
                filename = "TestBench.py"
                scriptArguments = """
                    --username "%s3_dsctestbench_accesskey%"
                    --password "%s3_dsctestbench_secret%"
                    --reference
                    --skip-run 
                    --skip-post-processing 
                    --config configs/apptainer/dimr/dimr_smoke_test_lnx64.xml
                    --log-level INFO
                    --parallel
                    --override-paths "from[local]=/dimrset,root[local]=/opt,from[engines_to_compare]=/dimrset,root[engines_to_compare]=/opt,from[engines]=/dimrset,root[engines]=/opt"
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
        script {
            name = "Add SLURM submit scripts based on template and copy run scripts"
            scriptContent = """
                hpc_smoke_path=../../../../ci/teamcity/Delft3D/linux/scripts/hpc-smoke
                cd test/deltares_testbench/data/cases
    
                echo prepare all models with submit scripts
                cp ${'$'}hpc_smoke_path/template_submit.sh .
                sed -i "s|~/apptainer|~/%h7_work_directory%/apptainer|g" ${'$'}hpc_smoke_path/prepare_all_models.sh
                ${'$'}hpc_smoke_path/prepare_all_models.sh h7 --config ${'$'}hpc_smoke_path/dimr_smoke_test_lnx64_slurm_setup.json

                echo and prepare run scripts
                cp ${'$'}hpc_smoke_path/common_utilities.sh .
                cp ${'$'}hpc_smoke_path/run_all_models.sh .
                cp ${'$'}hpc_smoke_path/schedule_teamcity_receive_job.sh .
                cp ${'$'}hpc_smoke_path/schedule_teamcity_receive_job_wrapper.sh .
                sed -i 's/CONFIGURATION_ID="${'$'}1"/CONFIGURATION_ID="%teamcity_receive_config%"/' schedule_teamcity_receive_job_wrapper.sh
                sed -i 's/DEPENDENCY_BUILD_ID="${'$'}2"/DEPENDENCY_BUILD_ID="%teamcity.build.id%"/' schedule_teamcity_receive_job_wrapper.sh
                sed -i 's/VCS_COMMIT_HASH="${'$'}3"/VCS_COMMIT_HASH="%build.vcs.number%"/' schedule_teamcity_receive_job_wrapper.sh
            """.trimIndent()
        }
        script {
            name = "Prepare apptainer folder and execution script"
            scriptContent = """
                mkdir test/deltares_testbench/data/cases/apptainer
                cp src/scripts_lgpl/singularity/execute_singularity_h7.sh test/deltares_testbench/data/cases/apptainer/

                echo download sif
                runtime_tag=${'$'}(echo "%runtime_container_image%" | sed 's/.*://')
                apptainer pull --force --disable-cache "${'$'}{runtime_tag}.sif" "docker://%runtime_container_image%"
                mv "${'$'}{runtime_tag}.sif" test/deltares_testbench/data/cases/apptainer
            """.trimIndent()
        }
        sshUpload {
            name = "Upload test cases and scripts"
            transportProtocol = SSHUpload.TransportProtocol.SFTP
            sourcePath = """
                test/deltares_testbench/data/cases/ => %h7_work_directory%/
            """.trimIndent()
            targetUrl = "h7.directory.intra"
            authMethod = password {
                username = "%h7_account_username%"
                password = "%h7_account_password%"
            }
        }
        sshExec {
            name = "Execute cases on h7"
            commands = """
                cd ~/%h7_work_directory%
                find . -name "*.sh" -exec chmod +x {} \;
                sbatch --partition=4vcpu --chdir=${'$'}PWD ./run_all_models.sh h7 --run_dependent
            """.trimIndent()
            targetUrl = "h7.directory.intra"
            authMethod = password {
                username = "%h7_account_username%"
                password = "%h7_account_password%"
            }
        }
        sshExec {
            name = "Remove folder using 'at' in 7 days."
            commands = """
                cd ~/%h7_work_directory%
                cat > README_CLEANUP.txt << EOF
                This directory contains temporary smoke test files and will be automatically removed in 7 days.
                
                Directory: ~/%h7_work_directory%
                Created: ${'$'}(date)
                Scheduled for removal: ${'$'}(date -d '+7 days')

                This cleanup is managed by the 'at' command to free up disk space.
                Do not store any important files in this directory permanently.
                EOF
                
                # Schedule removal of this directory after 7 days using 'at'
                echo "rm -rf ~/%h7_work_directory%" | at now + 7 days
            """.trimIndent()
            targetUrl = "h7.directory.intra"
            authMethod = password {
                username = "%h7_account_username%"
                password = "%h7_account_password%"
            }
        }
    }

    dependencies {
        dependency(LinuxBuild) {
            snapshot {
                onDependencyFailure = FailureAction.FAIL_TO_START
                onDependencyCancel = FailureAction.CANCEL
            }
        }
        dependency(LinuxRuntimeContainers) {
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
