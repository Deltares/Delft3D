package _Self.buildTypes

import jetbrains.buildServer.configs.kotlin.*
import jetbrains.buildServer.configs.kotlin.buildFeatures.PullRequests
import jetbrains.buildServer.configs.kotlin.buildFeatures.commitStatusPublisher
import jetbrains.buildServer.configs.kotlin.buildFeatures.pullRequests
import jetbrains.buildServer.configs.kotlin.buildSteps.script
import jetbrains.buildServer.configs.kotlin.triggers.finishBuildTrigger

object PublishAggregateStatus : BuildType({
    name = "Publish Aggregate Status"

    params {
        password("svc_teamcity_github_delft3d_access_token", "******")
    }

    vcs {
        root(Delft3DGitPleaseReplaceWithProjectSpecificAccessAccount)
    }

    steps {
        script {
            id = "simpleRunner"
            scriptContent = """
                chmod +x ./ci/github/get_aggregate_teamcity_build_status.sh
                ./ci/github/get_aggregate_teamcity_build_status.sh  \
                  --teamcity-token "%svc_teamcity_github_delft3d_access_token%" \
                  --project-id "${DslContext.projectId}"
                  --branch-name "%teamcity.build.branch%" \
                  --commit-sha "%build.vcs.number%" \
                  --poll-interval 10
            """.trimIndent()
        }
    }

    triggers {
        finishBuildTrigger {
            buildType = "${Trigger_1.id}"
            branchFilter = """
                +:<default>
                +:pull/*
            """.trimIndent()
        }
    }

    features {
        commitStatusPublisher {
            publisher = github {
                githubUrl = "https://api.github.com"
                authType = vcsRoot()
            }
        }
        pullRequests {
            provider = github {
                authType = vcsRoot()
                filterSourceBranch = "+:*"
                filterAuthorRole = PullRequests.GitHubRoleFilter.MEMBER
                ignoreDrafts = true
            }
        }
    }

    dependencies {
        snapshot(Trigger_1) {
        }
    }

    requirements {
        equals("teamcity.agent.jvm.os.name", "Linux")
    }
})
