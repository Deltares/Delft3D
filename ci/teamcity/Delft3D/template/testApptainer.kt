package Delft3D.template

import jetbrains.buildServer.configs.kotlin.*
import jetbrains.buildServer.configs.kotlin.buildFeatures.perfmon
import jetbrains.buildServer.configs.kotlin.buildSteps.script
import jetbrains.buildServer.configs.kotlin.triggers.finishBuildTrigger

import Delft3D.linux.*

object TemplateTestApptainer : Template({
    name = "DSCTestbenchTemplateLnx_Singularity"

    artifactRules = """
        test\deltares_testbench\data\cases\*\*.pdf=>pdf
        test\deltares_testbench\data\cases\*\result.txt=>logging
        test\deltares_testbench\data\cases\*.dia=>logging
        test\deltares_testbench\data\cases\**\*.log=>logging
        test\deltares_testbench\logs\*.log =>logging
        test\deltares_testbench\failed\** => failed_cases.zip
        test\deltares_testbench\failed\list.txt => failed_cases.zip
        test\deltares_testbench\copy_cases      => copy_cases.zip
    """.trimIndent()
    buildNumberPattern = "%build.vcs.number.Delft3D_Git%"

    params {
        text("testbench.loglevel", "ERROR", description = "CRITICAL, ERROR, WARNING, INFO or DEBUG", allowEmpty = true)
        checkbox("copy_cases", "false", label = "Copy cases", description = "ZIP a complete copy of the ./data/cases directory", display = ParameterDisplay.PROMPT,
                  checked = "true", unchecked = "false")
        param("conda_environment_name", "/opt/apps/anaconda3/2021.05/envs/DSCTestEnv_Delft3D_Lnx64_conda21.05_02")
        text("testbench.copy_failed", "no", description = "default: no, yes: to artifacts", allowEmpty = true)
        text("testbench.filter", "e", description = "e: everything in the config file", allowEmpty = true)
    }

    vcs {
        root(DslContext.settingsRoot)

        checkoutMode = CheckoutMode.ON_SERVER
        cleanCheckout = true
        checkoutDir = "%testbench.name%"
    }
    steps {
        script {
            name = "Install test binaries from DFS"
            workingDir = "test/deltares_testbench"
            scriptContent = """
                cd data/engines/teamcity_artifacts && \
                chmod a+x * && \
                ./install_sif_for_testbench.sh && \
                chmod -R a+x *
            """.trimIndent()
        }
        script {
            name = "Set execution flags on dependent artifacts"
            workingDir = "test/deltares_testbench"
            scriptContent = "chmod -R a+x data/engines"
        }
        script {
            name = "Setup Conda environment"
            enabled = false
            scriptContent = """
                #!/bin/bash
                . /usr/share/Modules/init/bash
                
                ENV=%conda_environment_name%
                
                module load anaconda3/2021.05
                
                grepenv=`conda env list | grep ${'$'}ENV`
                if [ "${'$'}grepenv" != "" ]; then
                  echo "Conda env '${'$'}ENV' already exist."
                else
                  # Create the environment
                  echo "Conda env '${'$'}ENV' doesn't exist."
                  echo "conda create -p ${'$'}ENV python=3.8 -y ..."
                  conda create -p ${'$'}ENV python=3.8 -y
                  echo "conda init bash ..."
                  conda init bash
                  echo "source activate ${'$'}ENV ..."
                  source activate ${'$'}ENV
                  echo "pip3 install --no-input netcdf4 ..."
                  pip3 install --no-input netcdf4
                  echo "conda install lxml -y ..."
                  conda install lxml -y
                  echo "conda install -c conda-forge parse -y ..."
                  conda install -c conda-forge parse -y
                  echo "conda install python-dateutil -y ..."
                  conda install python-dateutil -y
                  echo "conda install pandas -y ..."
                  conda install pandas -y
                  echo "conda install matplotlib -y ..."
                  conda install matplotlib -y
                  echo "conda install numpy -y ..."
                  conda install numpy -y
                  echo "conda install numexpr -y ..."
                  conda install numexpr -y
                  echo "conda install paramiko -y ..."
                  conda install paramiko -y
                  echo "conda install pathlib -y ..."
                  conda install pathlib -y
                  echo "conda install tk -y ..."
                  conda install tk -y
                  echo "Env ${'$'}ENV created successfully, deactivate"
                  conda deactivate
                fi
            """.trimIndent()
        }
        script {
            name = "Setup python environment using pip"
            workingDir = "test/deltares_testbench"
            scriptContent = """
                #!/bin/bash
                . /usr/share/Modules/init/bash
                
                python3.11 -m venv venv
                source venv/bin/activate
                python3.11 -m pip install --upgrade pip
                python3.11 -m pip install -r pip/lnx-requirements.txt
            """.trimIndent()
        }
        script {
            name = "Run TestBench.py compare"
            workingDir = "test/deltares_testbench"
            scriptContent = """
                #!/bin/bash
                . /usr/share/Modules/init/bash
                
                module load intelmpi/2021.11.0
                module load apptainer/1.2.5
                
                ulimit -s unlimited
                
                source venv/bin/activate
                
                python3.11 TestBench.py --username %s3_dsctestbench_accesskey% --password '%s3_dsctestbench_secret%' --compare --config %testbench.configfile% --filter "testcase=%testbench.filter%" --log-level %testbench.loglevel% --teamcity --parallel
            """.trimIndent()
        }
        script {
            name = "Remove Conda Environment"
            enabled = false
            executionMode = BuildStep.ExecutionMode.ALWAYS
            scriptContent = """
                module load anaconda3/2020.02
                conda remove --force -n FreshTestEnvLnx64_%system.teamcity.buildType.id%_%build.counter% --all -y -v
            """.trimIndent()
        }
        script {
            name = "Copy failed to artifacts"
            executionMode = BuildStep.ExecutionMode.RUN_ON_FAILURE

            conditions {
                equals("testbench.copy_failed", "yes")
            }
            workingDir = "test/deltares_testbench"
            scriptContent = """
                #!/bin/bash
                . /usr/share/Modules/init/bash
                
                python3.11 tools/failed_tests.py lnx64
            """.trimIndent()
        }
        script {
            name = "Copy cases"
            executionMode = BuildStep.ExecutionMode.RUN_ON_FAILURE
            workingDir = "test/deltares_testbench"
            scriptContent = "cp -r data/cases copy_cases"
        }
    }
    triggers {
        finishBuildTrigger {
            buildType = "${LinuxBuildApptainerRunTime.id}"
            branchFilter = """
                +:<default>
                +:all/release/*
            """.trimIndent()
        }
    }

    failureConditions {
        errorMessage = true
    }


    dependencies {
        dependency(LinuxBuildApptainerRunTime) {
            snapshot {
            }

            artifacts {
                artifactRules = "install_sif_for_testbench.sh=>test/deltares_testbench/data/engines/teamcity_artifacts"
            }
        }
    }

    requirements {
        equals("teamcity.agent.jvm.os.name", "Linux")
    }
})
