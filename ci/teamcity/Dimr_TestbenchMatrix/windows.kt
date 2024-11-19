package testbenchMatrix

import java.io.File
import jetbrains.buildServer.configs.kotlin.*
import jetbrains.buildServer.configs.kotlin.buildFeatures.*
import jetbrains.buildServer.configs.kotlin.buildSteps.*
import jetbrains.buildServer.configs.kotlin.triggers.*

import testbenchMatrix.Trigger

object Windows : BuildType({

    name = "Windows"
    buildNumberPattern = "%dep.${Trigger.id}.build.revisions.short%"

    artifactRules = """
        delft3d\test\deltares_testbench\data\cases\**\*.pdf      => pdf
        delft3d\test\deltares_testbench\data\cases\**\*.dia      => logging
        delft3d\test\deltares_testbench\data\cases\**\*.log      => logging
        delft3d\test\deltares_testbench\logs                     => logging
        delft3d\test\deltares_testbench\copy_cases               => copy_cases.zip
    """.trimIndent()

    val filePath = "${DslContext.baseDir}/vars/dimr_testbench_table.csv"
    val lines = File(filePath).readLines()
    val windowsLines = lines.filter { line -> line.contains("win64")}
    val configs = windowsLines.map { line ->
        line.split(",")[1]
    }
    val linesForAll = windowsLines.filter { line -> line.split(",")[2] == "TRUE" }
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

    dependencies {
        dependency(Trigger) {
            snapshot {
                onDependencyFailure = FailureAction.FAIL_TO_START
            }
        }
    }

    requirements {
        startsWith("teamcity.agent.jvm.os.name", "Windows 1")
    }
})