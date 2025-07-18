package Delft3D.linux

import jetbrains.buildServer.configs.kotlin.*
import jetbrains.buildServer.configs.kotlin.buildSteps.script
import jetbrains.buildServer.configs.kotlin.triggers.finishBuildTrigger
import jetbrains.buildServer.configs.kotlin.buildFeatures.*

import Delft3D.linux.*

object LinuxBuildApptainerRunTime : BuildType({
    name = "6. DIMRset - lnx64 - Singularity - Without MPI (on Alma8 agent)"
    description = "Build of DIMRset based Singularity through docker, using already created RPM and without MPI libraries."

    artifactRules = """
        src/scripts_lgpl/install_sif_for_testbench.sh
        src/scripts_lgpl/apptainer_setup_harbor.sh
    """.trimIndent()
    buildNumberPattern = "%build.counter%: %build.vcs.number%"
    publishArtifacts = PublishMode.SUCCESSFUL

    params {
        param("singularity_image_file_name", "delft3dfm")
        param("singularityVersionNumberDelft3DFM", "weekly")
        password("dockerPwd", "******", description = "dockerPwd")
        param("singularity_recipe_name", "dflowfm_recipe_external_mpi.def")
    }
    
    features {
        approval {
            approvalRules = "group:DIMR_BAKKERS:1"
        }
    }

    vcs {
        root(DslContext.settingsRoot)

        checkoutMode = CheckoutMode.ON_SERVER
        cleanCheckout = true
    }

    steps {
        script {
            name = "Modify recipe"
            workingDir = "src/scripts_lgpl/singularity"
            scriptContent = """
                #!/bin/bash
                . /usr/share/Modules/init/bash
                
                #module load squashfs-tools
                module load apptainer
                
                # Temporary workaround for TeamCity HTTP 403 error: do not use parameter variables 
                SIFVERSION=`apptainer --version`
                
                # Replace fields in the recipe.def
                sed -i "s/<singularityVersionNumberDelft3DFM>/"%singularityVersionNumberDelft3DFM%_lnx64_sif%build.counter%"/" %singularity_recipe_name%
                sed -i "s/<singularityImageName>/"delft3dfm"/" %singularity_recipe_name%
                sed -i "s/<singularityVersionNumberSingularity>/${'$'}SIFVERSION/" %singularity_recipe_name%
            """.trimIndent()
        }
        script {
            name = "Build container"
            workingDir = "src/scripts_lgpl/singularity"
            scriptContent = """
                #!/bin/bash
                . /usr/share/Modules/init/bash
                
                #module load squashfs-tools
                module load apptainer
                
                # Execute command apptainer -d build --fakeroot <image>.sif recipe.def in order to get debug information
                apptainer build --fakeroot "delft3dfm_%singularityVersionNumberDelft3DFM%_lnx64_sif%build.counter%.sif" "dflowfm_recipe_external_mpi.def"
                
                if (test ${'$'}? -ne 0)
                then
                 echo Apptainer build failed.
                 exit 1 
                fi
            """.trimIndent()
        }
        script {
            name = "Apptainer save for DFS-drive"
            workingDir = "src/scripts_lgpl/singularity"
            scriptContent = """
                tar -cvf delft3dfm_%singularityVersionNumberDelft3DFM%_lnx64_sif%build.counter%.tar delft3dfm_%singularityVersionNumberDelft3DFM%_lnx64_sif%build.counter%.sif execute_singularity.sh readme.txt run_singularity.sh submit_singularity.sh execute_singularity_h7.sh submit_singularity_h7.sh execute_singularity_tc.sh
                gzip delft3dfm_%singularityVersionNumberDelft3DFM%_lnx64_sif%build.counter%.tar
                
                # Copy the artifact to network
                cp delft3dfm_%singularityVersionNumberDelft3DFM%_lnx64_sif%build.counter%.tar.gz /opt/Testdata/DIMR/DIMR_collectors/DIMRset_lnx64_Singularity
            """.trimIndent()
        }
        if (DslContext.getParameter("enable_verschilanalyse_trigger").lowercase() == "true") {
            script {
                name = "Push to Harbor"
                workingDir = "src/scripts_lgpl/singularity"
                scriptContent = """
                    #!/bin/bash
                    . /usr/share/Modules/init/bash
                    
                    #module load squashfs-tools
                    module load apptainer
                    
                    export APPTAINER_DOCKER_USERNAME='%harbor_delft3d_username%'
                    export APPTAINER_DOCKER_PASSWORD='%harbor_delft3d_secret%'
                    apptainer push delft3dfm_%singularityVersionNumberDelft3DFM%_lnx64_sif%build.counter%.sif oras://containers.deltares.nl/delft3d/apptainer/delft3dfm:delft3dfm_%singularityVersionNumberDelft3DFM%_lnx64_sif%build.counter%
                    apptainer push delft3dfm_%singularityVersionNumberDelft3DFM%_lnx64_sif%build.counter%.sif oras://containers.deltares.nl/delft3d/apptainer/delft3dfm:latest
                """.trimIndent()
            }
        }
        script {
            name = "Create install_sif_for_testbench script"
            workingDir = "src/scripts_lgpl/"
            scriptContent = """
                #!/bin/sh
                
                cat > install_sif_for_testbench.sh <<EOL
                #!/bin/bash
                cp /opt/Testdata/DIMR/DIMR_collectors/DIMRset_lnx64_Singularity/delft3dfm_%singularityVersionNumberDelft3DFM%_lnx64_sif%build.counter%.tar.gz .
                gunzip *.gz
                tar -xvf *.tar
                mv delft3dfm_%singularityVersionNumberDelft3DFM%/* .
                rm -rf delft3dfm_%singularityVersionNumberDelft3DFM%/
                rm *.tar 
                EOL
            """.trimIndent()
        }
        script {
            name = "Create apptainer_setup_harbor.sh"
            workingDir = "src/scripts_lgpl/"
            scriptContent = """
                #!/bin/sh
                
                cat > apptainer_setup_harbor.sh <<EOL
                #!/bin/bash
                
                IMAGE_TAG="delft3dfm_%singularityVersionNumberDelft3DFM%_lnx64_sif%build.counter%"
                IMAGE_NAME="containers.deltares.nl/delft3d/apptainer/delft3dfm:${'$'}{IMAGE_TAG}"
                
                apptainer pull "${'$'}{IMAGE_NAME}"
                apptainer tag "${'$'}{IMAGE_NAME}" deltares/delft3dfm:latest
                
                EOL
            """.trimIndent()
        }
    }

    dependencies {
        dependency(LinuxBuildRpm) {
            snapshot {
            }

            artifacts {
                artifactRules = "*.rpm => src/scripts_lgpl/singularity"
            }
        }
    }

    requirements {
        equals("teamcity.agent.jvm.os.name", "Linux")
    }

    cleanup {
        keepRule {
            id = "RetainAll"
            keepAtLeast = allBuilds()
            dataToKeep = historyAndStatistics {
                preserveLogs = true
            }
            applyPerEachBranch = true
        }
        baseRule {
            artifacts(builds = 1, artifactPatterns = "*.tar.gz")
        }
    }
})
