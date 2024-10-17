package Verschilanalyse

import jetbrains.buildServer.configs.kotlin.*
import jetbrains.buildServer.configs.kotlin.buildSteps.PythonBuildStep
import jetbrains.buildServer.configs.kotlin.buildSteps.python
import jetbrains.buildServer.configs.kotlin.buildSteps.script
import jetbrains.buildServer.configs.kotlin.buildSteps.sshExec

object StartVerschilanalyse : BuildType({
    id("StartVerschilanalyse")
    name = "Start verschilanalyse models"

    params {
        password("smoke_password", "credentialsJSON:f496d729-b353-4853-8fed-d438292e0790")
        param("smoke_account", "fun_delft3d_smoke")
        param("apptainer_tag", "")
        password("minio_secret_key", "credentialsJSON:9ab48219-5651-48f1-8270-8001a10f6681")
        password("harbor_password", "credentialsJSON:94db07fa-36a6-4d75-be47-4928f5ac11da")
        param("minio_access_key", "credentialsJSON:7759a7b1-e7a2-402c-8e62-476f29cf8313")
    }

    vcs {
        root(AbsoluteId("Delft3dGitlab"))
    }

    steps {
        script {
            name = "Temporarily mount the P-drive"
            id = "simpleRunner"
            scriptContent = """net use P: \\directory.intra\PROJECT /user:%smoke_account% %smoke_password%"""
        }
        python {
            name = "Download minio input"
            id = "Download_minio_input"
            workingDir = "/test/deltares_testbench"
            environment = venv {
                requirementsFile = "pip/win-requirements.txt"
            }
            command = module {
                module = "tools.h7.download_folder_from_minio"
                scriptArguments = """--bucket "devops-test-verschilanalyse" --minio-path "test_input" --dest "P:/devops-dsc/verschilanalyse/models/" -u "%minio_access_key%" -p "%minio_secret_key%""""
            }
        }
        script {
            name = "Prepare apptainer"
            id = "Prepare_apptainer"
            scriptContent = """
                echo ##teamcity[setParameter name='apptainer_tag' value='delft3dfm_%dep.Dimr_DimrCollectors_Alma8_6DIMRsetLnx64SingularityWithoutMpiOnAlma8agent.singularityVersionNumberDelft3DFM%_lnx64_sif%dep.Dimr_DimrCollectors_Alma8_6DIMRsetLnx64SingularityWithoutMpiOnAlma8agent.build.counter%']
                copy .\src\scripts_lgpl\singularity\execute_singularity_h7.sh P:\devops-dsc\verschilanalyse\dimrset\
            """.trimIndent()
        }
        script {
            name = "Remove P-drive mount"
            id = "Remove_P_drive_mount"
            executionMode = BuildStep.ExecutionMode.ALWAYS
            scriptContent = "net use P: /delete"
        }
        sshExec {
            name = "Run models on h7"
            id = "Run_models_on_h7"
            commands = """
                # Pull apptainer image
                
                cd /p/devops-dsc/verschilanalyse/dimrset
                
                module purge
                module load apptainer/1.2.5 
                
                export APPTAINER_DOCKER_USERNAME="robot\${'$'}delft3d+h7"
                export APPTAINER_DOCKER_PASSWORD="%harbor_password%"
                apptainer pull -F --disable-cache "%apptainer_tag%.sif" "oras://containers.deltares.nl/delft3d/apptainer/delft3dfm:%apptainer_tag%"
                
                cd /p/devops-dsc/verschilanalyse/models
                
                # Start all models
                folders=`ls -d ./*/`
                
                for folder in ${'$'}folders; do
                	cd ${'$'}folder
                	job_id=`sbatch --parsable "submit_singularity_h7.sh"`
                	job_ids+=("${'$'}job_id")
                	echo "Submitted ${'$'}folder/submit_singularity.sh with job ID ${'$'}job_id"
                	cd ../
                done
                
                # Create a comma-separated list of job IDs for dependencies
                dependency_list=`IFS=,; echo "${'$'}{job_ids[*]}"`
                echo "${'$'}dependency_list"
            """.trimIndent()
            targetUrl = "h7.directory.intra"
            authMethod = password {
                username = "%smoke_account%"
                password = "%smoke_password%"
            }
        }
    }

    dependencies {
        dependency(AbsoluteId("Dimr_DimrCollectors_Alma8_6DIMRsetLnx64SingularityWithoutMpiOnAlma8agent")) {
            snapshot {
            }

            artifacts {
                artifactRules = "install_sif_for_testbench.sh"
            }
        }
    }

    requirements {
        contains("teamcity.agent.jvm.os.name", "Windows 1")
    }
})