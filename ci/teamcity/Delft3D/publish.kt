import jetbrains.buildServer.configs.kotlin.*
import jetbrains.buildServer.configs.kotlin.buildSteps.*
import jetbrains.buildServer.configs.kotlin.buildFeatures.*

import Delft3D.template.*
import Delft3D.linux.*
import Delft3D.windows.*

object Publish : BuildType({

    templates(
        TemplateMonitorPerformance,
        TemplateDockerRegistry,
        TemplateFailureCondition
    )

    name = "Publish Container"
    description = "Currently only used for publishing the container."
    buildNumberPattern = "%build.vcs.number%"
    maxRunningBuilds = 1
    allowExternalStatus = true

    artifactRules = """
        #teamcity:symbolicLinks=as-is
        +:examples/dflowfm/examples/**/* => %brand%/examples/
        +:ci/teamcity/Delft3D/linux/docker/readme.txt => %brand%/
        +:%brand%_*.tar => %brand%/
    """.trimIndent()

    vcs {
        root(DslContext.settingsRoot)
        cleanCheckout = true
        branchFilter = """
            +:<default>
            +:main
            +:all/release/*
        """.trimIndent()
    }

    features {
        matrix {
            param("brand", listOf(
                value("delft3dfm"),
                value("dhydro")
            ))
        }
        approval {
            approvalRules = "group:DIMR_BAKKERS:1"
        }
    }

    params {
        select("release_type", "development", display = ParameterDisplay.PROMPT, options = listOf("development", "release"))
        text("release_version", "2.29.xx", 
            label = "Release version", 
            description = "e.g. '2.29.03' or '2025.02'", 
            display = ParameterDisplay.PROMPT)
        text("reverse.dep.*.release_version", "2.29.xx", 
            label = "Release version for dependencies", 
            description = "e.g. '2.29.03' or '2025.02'", 
            display = ParameterDisplay.PROMPT)
        param("reverse.dep.*.product", "all-testbench")
        param("is_latest_development", "false")
        param("commit_id_short", "%dep.${LinuxBuild.id}.commit_id_short%")
        param("source_image", "containers.deltares.nl/delft3d-dev/delft3d-runtime-container:alma10-%dep.${LinuxBuild.id}.product%-%build.vcs.number%")
        param("new_tag", "%release_version%-%release_type%")
        param("destination_image_specific", "containers.deltares.nl/delft3d/%brand%:%new_tag%")
        param("destination_image_generic", "containers.deltares.nl/delft3d/%brand%:%release_type%")
    }

    if (DslContext.getParameter("enable_release_publisher").lowercase() == "true") {
        dependencies {
            dependency(DIMRbak) {
                snapshot {
                    onDependencyFailure = FailureAction.FAIL_TO_START
                    onDependencyCancel = FailureAction.CANCEL
                }
            }
            dependency(LinuxTest) {
                snapshot {
                    onDependencyFailure = FailureAction.FAIL_TO_START
                    onDependencyCancel = FailureAction.CANCEL
                }
            }
            dependency(WindowsTest) {
                snapshot {
                    onDependencyFailure = FailureAction.FAIL_TO_START
                    onDependencyCancel = FailureAction.CANCEL
                }
            }
            dependency(LinuxUnitTest) {
                snapshot {
                    onDependencyFailure = FailureAction.FAIL_TO_START
                    onDependencyCancel = FailureAction.CANCEL
                }
            }
            dependency(WindowsUnitTest) {
                snapshot {
                    onDependencyFailure = FailureAction.FAIL_TO_START
                    onDependencyCancel = FailureAction.CANCEL
                }
            }
            dependency(LinuxRunAllContainerExamples) {
                snapshot {
                    onDependencyFailure = FailureAction.FAIL_TO_START
                    onDependencyCancel = FailureAction.CANCEL
                }
            }
        }
    }

    requirements {
        contains("teamcity.agent.jvm.os.name", "Linux")
    }

    steps {
        dockerCommand {
            name = "Pull source image"
            commandType = other {
                subCommand = "pull"
                commandArgs = "%source_image%"
            }
        }
        dockerCommand {
            name = "Tag images"
            commandType = other {
                subCommand = "tag"
                commandArgs = "%source_image% %destination_image_specific% && docker tag %source_image% %destination_image_generic%"
            }
        }
        dockerCommand {
            name = "Save image as tarball"
            commandType = other {
                subCommand = "save"
                commandArgs = """
                    -o %brand%_%release_version%-%commit_id_short%.tar
                    %destination_image_specific%
                """.trimIndent()
            }
        }
        script {
            name = "Generate Apptainer SIF file"
            workingDir = "src/scripts_lgpl/singularity"
            scriptContent = """
                apptainer pull docker-daemon:%destination_image_specific%
            """.trimIndent()
        }
        dockerCommand {
            name = "Push release image"
            commandType = push {
                namesAndTags = """
                    "%destination_image_specific%"
                """.trimIndent()
            }
            executionMode = BuildStep.ExecutionMode.ALWAYS
        }
        python {
            name = "Set latest development tag parameter"
            command = module {
                module = "ci_tools.harbor.harbor_version_checker"
                scriptArguments = """
                    --harbor-username "%delft3d-user%"
                    --harbor-password "%delft3d-secret%"
                    --new-tag "%new_tag%"
                """.trimIndent()
            }
            workingDir = "ci/python"
            environment = venv {
                requirementsFile = ""
                pipArgs = "--editable .[all]"
            }
        }
        dockerCommand {
            conditions {
                equals("is_latest_development", "true")
            }
            name = "Push rolling development tag"
            commandType = push {
                namesAndTags = """
                    "%destination_image_generic%"
                """.trimIndent()
            }
            executionMode = BuildStep.ExecutionMode.ALWAYS
        }
        script {
            name = "Replace default image in run_docker.sh scripts"
            workingDir = "examples/dflowfm"
            scriptContent = """
                for file in */run_docker.sh; do
                    sed -i 's@^image=[^ ]*@image=%destination_image_specific%@' ${'$'}file
                done
            """.trimIndent()
            executionMode = BuildStep.ExecutionMode.ALWAYS
        }
        script {
            name = "Replace branding delft3dfm->dhydro"
            scriptContent = """
                sed -i 's@delft3dfm@dhydro@' \
                    ci/teamcity/Delft3D/linux/docker/readme.txt \
                    src/scripts_lgpl/singularity/readme.txt \
                    src/scripts_lgpl/singularity/submit_singularity_h7.sh
                sed -i 's@Delft3D FM@D-HYDRO@' \
                    ci/teamcity/Delft3D/linux/docker/readme.txt \
                    src/scripts_lgpl/singularity/readme.txt \
                    src/scripts_lgpl/singularity/submit_singularity_h7.sh
            """.trimIndent()
            executionMode = BuildStep.ExecutionMode.ALWAYS
        }
        exec {
            name = "Create Docker ZIP file in /opt/Testdata/DIMR/DIMR_collectors/DIMRset_lnx64_Docker/"
            path = "ci/teamcity/Delft3D/scripts/makePublishDockerZip.sh"
            arguments = """
                --brand %brand%
                --release-version %release_version%
                --commit-id-short %commit_id_short%
            """.trimIndent()
            executionMode = BuildStep.ExecutionMode.ALWAYS
        }
        script {
            name = "Copy Apptainer packages to share"
            workingDir = "src/scripts_lgpl/singularity"
            scriptContent = """
                tar -vczf %brand%_%new_tag%.tar.gz \
                    %brand%_%new_tag%.sif \
                    readme.txt \
                    run_singularity.sh \
                    execute_singularity_h7.sh \
                    submit_singularity_h7.sh
                
                # Copy the artifact to network
                cp -vf %brand%_%new_tag%.tar.gz /opt/Testdata/DIMR/DIMR_collectors/DIMRset_lnx64_Singularity
            """.trimIndent()
            executionMode = BuildStep.ExecutionMode.ALWAYS
        }
    }
})