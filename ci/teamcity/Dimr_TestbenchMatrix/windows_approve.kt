package testbenchMatrix

import jetbrains.buildServer.configs.kotlin.*
import jetbrains.buildServer.configs.kotlin.buildSteps.script
import jetbrains.buildServer.configs.kotlin.triggers.finishBuildTrigger

object WindowsApprove : BuildType({

    name = "Windows Approve"
    buildNumberPattern = "%dep.${Trigger.id}.build.revisions.short%"

    params {
        param("teamcity_user", "svc_dimr_approve_windows")
        password("teamcity_pass", "credentialsJSON:8a625bb9-5f1a-4479-b3e3-0cc362784a75")
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
                    echo Approving Windows Testbench through TC API failed.
                    exit 1
                fi
            """.trimIndent()
        }
    }

    triggers {
        finishBuildTrigger {
            buildType = "${Windows.id}"
            successfulOnly = true
        }
    }

    dependencies {
        snapshot(Windows) {
        }
    }

    requirements {
        equals("teamcity.agent.jvm.os.name", "Linux")
    }
})