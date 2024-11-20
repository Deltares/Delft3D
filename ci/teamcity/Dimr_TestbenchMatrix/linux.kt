package testbenchMatrix

import java.io.File
import jetbrains.buildServer.configs.kotlin.*
import jetbrains.buildServer.configs.kotlin.buildFeatures.*
import jetbrains.buildServer.configs.kotlin.buildSteps.*
import jetbrains.buildServer.configs.kotlin.triggers.*

import testbenchMatrix.Trigger

object Linux : BuildType({

    name = "Linux"
    buildNumberPattern = "%dep.${DockerLinuxBuild.id}.build.revisions.short%"

    artifactRules = """
        delft3d\test\deltares_testbench\data\cases\**\*.pdf      => pdf
        delft3d\test\deltares_testbench\data\cases\**\*.dia      => logging
        delft3d\test\deltares_testbench\data\cases\**\*.log      => logging
        delft3d\test\deltares_testbench\logs                     => logging
        delft3d\test\deltares_testbench\copy_cases               => copy_cases.zip
    """.trimIndent()

    val filePath = "${DslContext.baseDir}/vars/dimr_testbench_table.csv"
    val lines = File(filePath).readLines()
    val linuxLines = lines.filter { line -> line.contains("lnx64")}
    val configs = linuxLines.map { line ->
        line.split(",")[1]
    }
    val linesForAll = linuxLines.filter { line -> line.split(",")[2] == "TRUE" }
    val selectedConfigs = linesForAll.map { line -> line.split(",")[1] }

    vcs {
        root(DslContext.settingsRoot)
    }

    params {
        select("configfile", selectedConfigs.joinToString(","),
            allowMultiple = true,
            options = configs,
            display = ParameterDisplay.PROMPT
        )
        param("s3_dsctestbench_accesskey", "j3WxZe0x3LB6cHgg5vp9")
        password("s3_dsctestbench_secret", "credentialsJSON:7e8a3aa7-76e9-4211-a72e-a3825ad1a159")
    }

    features {
        matrix {
            id = "matrix"
            param("configfile", selectedConfigs.map { config ->
                value(config)
            })
        }
        pullRequests {
            id = "merge_request"
            provider = gitlab {
                authType = token {
                    token = "%gitlab_private_access_token%"
                }
                filterSourceBranch = "+:*"
                ignoreDrafts = true
            }
        }
        dockerSupport {
            id = "DockerSupport"
            cleanupPushedImages = true
            loginToRegistry = on {
                dockerRegistryId = "PROJECT_EXT_133,PROJECT_EXT_81"
            }
        }
        commitStatusPublisher {
            id = "Delft3D_gitlab"
            enabled = true
            vcsRootExtId = "${DslContext.settingsRoot.id}"
            publisher = gitlab {
                authType = vcsRoot()
            }
        }
        perfmon {
            id = "perfmon"
        }
    }

    steps {
        script {
            name = "Merge main into branch"
            id = "Merge_main_into_branch"

            conditions {
                contains("teamcity.build.branch", "merge-requests")
            }
            workingDir = "."
            scriptContent = """
                git remote add temporary 'https://svc_teamcity_gitdsc:%gitlab_private_access_token%@git.deltares.nl/oss/delft3d.git'
                git fetch temporary refs/merge-requests/*:refs/remotes/temporary/merge-requests/* --quiet
                git checkout temporary/%teamcity.build.branch%/merge
                python3 -c "import subprocess; commit_id=subprocess.check_output(['git','rev-parse', 'HEAD'], universal_newlines=True).strip(); print('##teamcity[addBuildTag \'merge commit ID: '+commit_id+'\']')"
                git remote remove temporary
            """.trimIndent()
        }

        python {
            name = "Run TestBench.py"
            id = "Run_Testbench"
            workingDir = "test/deltares_testbench/"
            pythonVersion = customPython {
                executable = "python3.9"
            }
            command = file {
                filename = "TestBench.py"
                scriptArguments = """
                    --username "%s3_dsctestbench_accesskey%"
                    --password "%s3_dsctestbench_secret%"
                    --compare
                    --config "configs/%configfile%"
                    --log-level DEBUG
                    --parallel
                    --teamcity
                    --override-paths "from[local]=/dimrset,root[local]=/opt,from[engines_to_compare]=/dimrset,root[engines_to_compare]=/opt"
                """.trimIndent()
            }
            dockerImage = "containers.deltares.nl/delft3d/test/delft3dfm:alma8-%dep.${DockerLinuxBuild.id}.build.revisions.short%"
            dockerImagePlatform = PythonBuildStep.ImagePlatform.Linux
            dockerPull = true
            dockerRunParameters = """
                --rm
                --pull always
                --shm-size 8G
            """.trimIndent()
        }
        dockerCommand {
            name = "Remove container"
            id = "Remove_Container"
            executionMode = BuildStep.ExecutionMode.ALWAYS
            commandType = other {
                subCommand = "rmi"
                commandArgs = "containers.deltares.nl/delft3d/test/delft3dfm:alma8-%dep.${DockerLinuxBuild.id}.build.revisions.short%"
            }
        }
        dockerCommand {
            name = "Prune"
            id = "Prune"
            executionMode = BuildStep.ExecutionMode.ALWAYS
            commandType = other {
                subCommand = "system"
                commandArgs = "prune -f"
            }
        }
    }

    dependencies {
        dependency(Trigger) {
            snapshot {
                onDependencyFailure = FailureAction.FAIL_TO_START
            }
        }
        dependency(DockerLinuxBuild) {
            snapshot {
                onDependencyFailure = FailureAction.FAIL_TO_START
            }
        }
    }

    requirements {
        equals("teamcity.agent.jvm.os.name", "Linux")
    }
})