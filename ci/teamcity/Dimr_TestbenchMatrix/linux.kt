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

object Linux : BuildType({
    name = "Linux"

    val filePath = "${DslContext.baseDir}/dimr_testbench_table.csv"
    val lines = File(filePath).readLines()
    val linuxLines = lines.filter({ line -> line.contains("lnx64")})
    val configs = linuxLines.drop(1).map { line ->
        line.split(",")[1]
    }

    vcs {
        root(DslContext.settingsRoot)
    }

    params {
        select("configfile", configs.joinToString(","),
            allowMultiple = true,
            options = configs
        )
    }

    dependencies {
        snapshot(Trigger) {
            onDependencyFailure = FailureAction.CANCEL
            onDependencyCancel = FailureAction.CANCEL
        }
    }

    features {
        matrix {
            id = "matrix"
            param("configfile", configs.map { config ->
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

    requirements {
        equals("teamcity.agent.jvm.os.name", "Linux")
    }
})