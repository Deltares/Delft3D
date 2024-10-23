package testbenchMatrix

import java.io.File
import jetbrains.buildServer.configs.kotlin.*
import jetbrains.buildServer.configs.kotlin.buildFeatures.commitStatusPublisher
import jetbrains.buildServer.configs.kotlin.buildFeatures.pullRequests
import jetbrains.buildServer.configs.kotlin.buildSteps.python
import jetbrains.buildServer.configs.kotlin.triggers.VcsTrigger
import jetbrains.buildServer.configs.kotlin.triggers.vcs
import jetbrains.buildServer.configs.kotlin.buildSteps.script

import testbenchMatrix.Trigger

object Windows : BuildType({

    name = "Windows"
    buildNumberPattern = "%dep.${Trigger.id}.build.revisions.short%"

    val filePath = "${DslContext.baseDir}/dimr_testbench_table.csv"
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