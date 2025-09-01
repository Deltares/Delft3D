import jetbrains.buildServer.configs.kotlin.*
import jetbrains.buildServer.configs.kotlin.buildFeatures.PullRequests
import jetbrains.buildServer.configs.kotlin.buildFeatures.commitStatusPublisher
import jetbrains.buildServer.configs.kotlin.buildFeatures.pullRequests
import jetbrains.buildServer.configs.kotlin.buildSteps.script
import jetbrains.buildServer.configs.kotlin.triggers.finishBuildTrigger

import Trigger
import Delft3D.linux.*
import Delft3D.linux.containers.*
import Delft3D.windows.*

object PublishAggregateStatus : BuildType({
    name = "Publish Aggregate Status"

    params {
        // token with permissions restricted to the Delft3D project
        // password("svc_teamcity_github_delft3d_access_token", "credentialsJSON:3655888b-678a-425a-8c21-8b533c33bc2d")
        // token with more permissions that can be used in sandbox projects (DEV-ONLY, remove before merging and use token above)
        password("svc_teamcity_github_delft3d_access_token", "credentialsJSON:4493718c-625a-46a7-9c1d-ec75e0bf7c34")
    }

    vcs {
        root(DslContext.settingsRoot)
    }

    steps {
        script {
            id = "simpleRunner"
            scriptContent = """
                #!/usr/bin/env bash
                chmod +x ./ci/github/get_aggregate_teamcity_build_status.sh
                ./ci/github/get_aggregate_teamcity_build_status.sh  \
                  --teamcity-token "%svc_teamcity_github_delft3d_access_token%" \
                  --project-id "${DslContext.projectId}" \
                  --branch-name "%teamcity.build.branch%" \
                  --commit-sha "%build.vcs.number%" \
                  --poll-interval 10
            """.trimIndent()
        }
    }

    triggers {
        finishBuildTrigger {
            buildType = "${Trigger.id}"
            branchFilter = """
                -:*
                +:pull/*
            """.trimIndent()
        }
        finishBuildTrigger {
            buildType = "${LinuxBuild.id}"
            branchFilter = """
                -:*
                +:pull/*
            """.trimIndent()
        }
        finishBuildTrigger {
            buildType = "${LinuxTest.id}"
            branchFilter = """
                -:*
                +:pull/*
            """.trimIndent()
        }
        finishBuildTrigger {
            buildType = "${LinuxCollect.id}"
            branchFilter = """
                -:*
                +:pull/*
            """.trimIndent()
        }
        finishBuildTrigger {
            buildType = "${LinuxUnitTest.id}"
            branchFilter = """
                -:*
                +:pull/*
            """.trimIndent()
        }
        finishBuildTrigger {
            buildType = "${LinuxBuildTools.id}"
            branchFilter = """
                -:*
                +:pull/*
            """.trimIndent()
        }
        finishBuildTrigger {
            buildType = "${LinuxThirdPartyLibs.id}"
            branchFilter = """
                -:*
                +:pull/*
            """.trimIndent()
        }
        finishBuildTrigger {
            buildType = "${WindowsBuild.id}"
            branchFilter = """
                -:*
                +:pull/*
            """.trimIndent()
        }
        finishBuildTrigger {
            buildType = "${WindowsTest.id}"
            branchFilter = """
                -:*
                +:pull/*
            """.trimIndent()
        }
        finishBuildTrigger {
            buildType = "${WindowsUnitTest.id}"
            branchFilter = """
                -:*
                +:pull/*
            """.trimIndent()
        }
        finishBuildTrigger {
            buildType = "${WindowsCollect.id}"
            branchFilter = """
                -:*
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
        snapshot(Trigger) {
        }
    }

    requirements {
        equals("teamcity.agent.jvm.os.name", "Linux")
    }
})
