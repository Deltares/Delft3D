import java.io.File
import jetbrains.buildServer.configs.kotlin.*
import jetbrains.buildServer.configs.kotlin.buildFeatures.commitStatusPublisher
import jetbrains.buildServer.configs.kotlin.buildFeatures.pullRequests
import jetbrains.buildServer.configs.kotlin.buildSteps.python
import jetbrains.buildServer.configs.kotlin.triggers.VcsTrigger
import jetbrains.buildServer.configs.kotlin.triggers.vcs
import jetbrains.buildServer.configs.kotlin.buildSteps.script

version = "2024.03"

project {
    description = "contact: BlackOps"

    buildType(TriggerMatrix)
    buildType(Windows)
    buildType(Linux)

    buildTypesOrder = arrayListOf(TriggerMatrix, Linux, Windows)
}

object TriggerMatrix : BuildType({
    name = "Trigger Matrix"

    vcs {
        root(DslContext.settingsRoot)
    }

    params {
        param("teamcity_user", "svc_dimr_trigger")
        password("teamcity_pass", "credentialsJSON:15cc6665-e900-4360-8942-00e654f6acfe")
        param("matrix_list", "first,second")
    }

    steps {
        script {
            id = "Start Linux Testbench"
            scriptContent = """
                curl -u %teamcity_user%:%teamcity_pass% \
                     -X POST \
                     -H "Content-Type: application/xml" \
                     -d '<build branchName="%teamcity.build.branch%">
                            <buildType id="Dimr_TestbenchMatrix_Linux"/>
                            <revisions>
                                <revision version="%build.vcs.number%" vcsBranchName="refs/%teamcity.build.branch%/head">
                                    <vcs-root-instance vcs-root-id="Delft3dGitlab"/>
                                </revision>
                            </revisions>
                            <properties>
                                <property name="configfile" value="%matrix_list%"/>
                            </properties>
                         </build>' \
                     "https://build.avi.directory.intra/app/rest/buildQueue"
                if (test $? -ne 0)
                then
                    echo Start Linux Testbench through TC API failed.
                    exit 1
                fi
            """.trimIndent()
        }
    }

    features{
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
})

object Windows : BuildType({
    name = "Windows"
    description = "WIP"
})

object Linux : BuildType({
    name = "Linux"

    val filePath = "${DslContext.baseDir}/dimr_testbench_table.csv"
    val lines = File(filePath).readLines()
    val configs = lines.drop(1).map { line ->
        line.split(",")[0]
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