package testbenchMatrix

import jetbrains.buildServer.configs.kotlin.*
import jetbrains.buildServer.configs.kotlin.buildSteps.script
import jetbrains.buildServer.configs.kotlin.triggers.finishBuildTrigger

object LinuxApprove : BuildType({

    name = "Linux Approve"
    buildNumberPattern = "%dep.${Trigger.id}.build.revisions.short%"

    params {
        param("teamcity_user", "svc_dimr_approve_linux")
        password("teamcity_pass", "credentialsJSON:c134631a-649e-4a00-87b2-460768430fbe")
    }

    vcs {
        root(DslContext.settingsRoot)
        branchFilter = """
            +:<default>
            +:all/release/*
        """.trimIndent()
    }

    steps {
        script {
            name = "Approve Release"

            conditions {
                endsWith("dep.${Trigger.id}.teamcity.build.triggeredBy", "Release")
            }
            scriptContent = """
                curl --fail --verbose --silent --show-error \
                     -u %teamcity_user%:%teamcity_pass% \
                     -X POST \
                     -H "Content-Type: application/xml" \
                     -d '' \
                     "https://build.avi.directory.intra/app/rest/buildQueue/buildType:${Release.id},defaultFilter:false,state:queued/approve"
                if (test ${'$'}? -ne 0)
                then
                    echo Approving Linux Testbench through TC API failed.
                    exit 1
                fi
            """.trimIndent()
        }
    }

    triggers {
        finishBuildTrigger {
            buildType = "${Linux.id}"
            successfulOnly = true
        }
    }

    dependencies {
        snapshot(Linux) {
        }
    }

    requirements {
        equals("teamcity.agent.jvm.os.name", "Linux")
    }
})
