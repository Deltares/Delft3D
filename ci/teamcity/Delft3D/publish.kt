import jetbrains.buildServer.configs.kotlin.*
import jetbrains.buildServer.configs.kotlin.buildSteps.*
import jetbrains.buildServer.configs.kotlin.buildFeatures.*

import Delft3D.template.*
import Delft3D.linux.*

object Publish : BuildType({

    templates(
        TemplateMonitorPerformance,
        TemplateDockerRegistry
    )

    name = "Publish Container"
    description = "Currently only used for publishing the container."
    buildNumberPattern = "%build.vcs.number%"
    maxRunningBuilds = 1

    artifactRules = """
        #teamcity:symbolicLinks=as-is
        +:examples/dflowfm/examples/**/* => %brand%/examples/
        -:examples/dflowfm/examples/**/run.sh
        -:examples/dflowfm/examples/**/run.bat
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
        approval {
            approvalRules = "group:DIMR_BAKKERS:1"
        }
        matrix {
            param("brand", listOf(
                value("delft3dfm"),
                value("dhydro")
            ))
        }
    }

    if (DslContext.getParameter("environment") == "production") {
        dependencies {
            snapshot(AbsoluteId("DIMR_To_NGHS")) {
                onDependencyFailure = FailureAction.FAIL_TO_START
                onDependencyCancel = FailureAction.CANCEL
            }
        }
    }

    requirements {
        contains("teamcity.agent.jvm.os.name", "Linux")
    }

    params {
        select("release_type", "weekly", display = ParameterDisplay.PROMPT, options = listOf("daily", "weekly", "release"))
        text("release_version", "%dep.Dimr_DimrCollector.DIMRset_ver%", 
            label = "Release version", 
            description = "e.g. '2.29.03' or '2025.02'", 
            display = ParameterDisplay.PROMPT)
        param("commit_id_short", "%dep.${LinuxBuild.id}.build.revisions.short%")
        param("source_image", "containers.deltares.nl/delft3d/delft3d-runtime-container:alma8-%build.vcs.number%")
        param("destination_image_generic", "containers.deltares.nl/delft3d/%brand%:%release_type%")
        param("destination_image_specific", "containers.deltares.nl/delft3d/%brand%:%release_type%-%release_version%")
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
            name = "Tag generic image"
            commandType = other {
                subCommand = "tag"
                commandArgs = "%source_image% %destination_image_generic%"
            }
        }
        dockerCommand {
            name = "Tag specific image"
            commandType = other {
                subCommand = "tag"
                commandArgs = "%source_image% %destination_image_specific%"
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
        dockerCommand {
            name = "Push generic and specific images"
            commandType = push {
                namesAndTags = """
                    "%destination_image_generic%"
                    "%destination_image_specific%"
                """.trimIndent()
            }
        }
        script {
            name = "Replace default image in run_docker.sh scripts"
            workingDir = "examples/dflowfm"
            scriptContent = """
                for file in */run_docker.sh; do
                    sed -i 's@^image=[^ ]*@image=%destination_image_specific%@' ${'$'}file
                done
            """.trimIndent()
        }
        script {
            name = "Replace branding delft3dfm->dhydro"
            conditions {
                equals("brand", "dhydro")
            }
            scriptContent = """
                sed -i 's@delft3dfm@dhydro@' ci/teamcity/Delft3D/linux/docker/readme.txt
                sed -i 's@Delft3D FM@D-HYDRO@' ci/teamcity/Delft3D/linux/docker/readme.txt
            """.trimIndent()
        }
        script {
            name = "Copy to teamcity/Testdata"
            scriptContent = """
                ZIP_FILE=/opt/Testdata/DIMR/DIMR_collectors/DIMRset_lnx64_Docker/%brand%_%release_version%-%commit_id_short%.zip
                pushd examples/dflowfm
                    rm -f *.*
                    mkdir examples
                    shopt -s extglob
                    mv !(examples) examples
                    zip -r ${'$'}{ZIP_FILE} examples
                popd
                pushd ci/teamcity/Delft3D/linux/docker
                    zip -g ${'$'}{ZIP_FILE} readme.txt
                popd

                zip -g ${'$'}{ZIP_FILE} %brand%_*.tar
            """.trimIndent()
        }
    }
})