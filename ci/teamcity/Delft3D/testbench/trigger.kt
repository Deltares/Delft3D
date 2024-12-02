package testbench

import jetbrains.buildServer.configs.kotlin.*
import jetbrains.buildServer.configs.kotlin.buildFeatures.*
import jetbrains.buildServer.configs.kotlin.buildSteps.*
import jetbrains.buildServer.configs.kotlin.triggers.*

object TestbenchTrigger : BuildType({

    name = "Trigger"
    buildNumberPattern = "%build.revisions.short%"

    vcs {
        root(DslContext.settingsRoot)
    }

    params {
        param("testbench_table", "ci/teamcity/Delft3D/testbench/vars/dimr_testbench_table.csv")
    
        param("teamcity_user", "svc_dimr_trigger")
        password("teamcity_pass", "credentialsJSON:15cc6665-e900-4360-8942-00e654f6acfe")

        param("matrix_list_lnx64",   "dummy_value")
        param("matrix_list_win64", "dummy_value")
        param("branch_name",         "dummy_value")
    }

    steps {

        python {
            name = "Determine components by branch"
            command = script {
                content="""
                if "merge-request" in "%teamcity.build.branch%":
                    branch_name = "%teamcity.pullRequest.source.branch%".split("/")[0]
                    print(f"##teamcity[setParameter name='branch_name' value='{branch_name}']")
                else:
                    branch_name = "%teamcity.build.branch%".split("/")[0]
                    print(f"##teamcity[setParameter name='branch_name' value='{branch_name}']")
                """.trimIndent()
            }
        }

        // Enable this buildstep to check for code changes instead of branch (disable previous buildstep)
        python {
            name = "Determine components by code changes"
            enabled = false

            // The code changes can only be requested through the gitlab api on merge-requests.
            conditions {
                contains("teamcity.build.branch", "merge-requests")
            }
            command = file {
                filename = "ci/teamcity/scripts/check_scope.py"
                scriptArguments = """
                    -b "%teamcity.build.branch%"
                    -t "%gitlab_private_access_token%"
                    -f "ci/teamcity/Delft3D/testbench/vars/repo_index.json"
                """.trimIndent()
            }
        }

        python {
            name = "Retrieve Linux Testbench XMLs from CSV"
            command = file {
                filename = "ci/teamcity/scripts/testbench_filter.py"
                scriptArguments = "-n %branch_name% -f %testbench_table% -v lnx64"
            }
        }

        python {
            name = "Retrieve Windows Testbench XMLs from CSV"
            command = file {
                filename = "ci/teamcity/scripts/testbench_filter.py"
                scriptArguments = "-n %branch_name% -f %testbench_table% -v win64"
            }
        }

        script {
            name = "Start Linux Testbench"

            conditions {
                doesNotContain("teamcity.build.triggeredBy", "Snapshot dependency")
                doesNotEqual("branch_name", "none")
                doesNotEqual("branch_name", "qp")
                doesNotEqual("branch_name", "d3d4")
            }

            scriptContent = """
                curl --fail --silent --show-error \
                     -u %teamcity_user%:%teamcity_pass% \
                     -X POST \
                     -H "Content-Type: application/xml" \
                     -d '<build branchName="%teamcity.build.branch%" replace="true">
                            <buildType id="${TestbenchLinux.id}"/>
                            <revisions>
                                <revision version="%build.vcs.number%" vcsBranchName="%teamcity.build.branch%">
                                    <vcs-root-instance vcs-root-id="DslContext.settingsRoot"/>
                                </revision>
                            </revisions>
                            <properties>
                                <property name="configfile" value="%matrix_list_lnx64%"/>
                            </properties>
                            <snapshot-dependencies>
                                <build id="%teamcity.build.id%" buildTypeId="%system.teamcity.buildType.id%"/>
                            </snapshot-dependencies>
                         </build>' \
                     "%teamcity.serverUrl%/app/rest/buildQueue"
                if (test $? -ne 0)
                then
                    echo Start Linux Testbench through TC API failed.
                    exit 1
                fi
            """.trimIndent()
        }

        script {
            name = "Start Windows Testbench"

            conditions {
                doesNotContain("teamcity.build.triggeredBy", "Snapshot dependency")
                doesNotEqual("branch_name", "none")
                doesNotEqual("branch_name", "qp")
                doesNotEqual("branch_name", "d3d4")
            }
            
            scriptContent = """
                curl --fail --silent --show-error \
                     -u %teamcity_user%:%teamcity_pass% \
                     -X POST \
                     -H "Content-Type: application/xml" \
                     -d '<build branchName="%teamcity.build.branch%" replace="true">
                            <buildType id="${TestbenchWindows.id}"/>
                            <revisions>
                                <revision version="%build.vcs.number%" vcsBranchName="%teamcity.build.branch%">
                                    <vcs-root-instance vcs-root-id="DslContext.settingsRoot"/>
                                </revision>
                            </revisions>
                            <properties>
                                <property name="configfile" value="%matrix_list_win64%"/>
                            </properties>
                            <snapshot-dependencies>
                                <build id="%teamcity.build.id%" buildTypeId="%system.teamcity.buildType.id%"/>
                            </snapshot-dependencies>
                         </build>' \
                     "%teamcity.serverUrl%/app/rest/buildQueue"
                if (test $? -ne 0)
                then
                    echo Start Windows Testbench through TC API failed.
                    exit 1
                fi
            """.trimIndent()
        }
    }

    if (DslContext.getParameter("environment") == "production") {
        triggers {
            schedule {
                schedulingPolicy = daily {
                    hour = 20
                }
                branchFilter = ""
                triggerBuild = always()
                withPendingChangesOnly = false
                param("revisionRuleBuildBranch", "<default>")
            }
            vcs {
                quietPeriodMode = VcsTrigger.QuietPeriodMode.USE_CUSTOM
                quietPeriod = 60
                branchFilter = "+:merge-requests/*"
            }
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
        if (DslContext.getParameter("environment") == "production") {
            commitStatusPublisher {
                id = "Delft3D_gitlab"
                enabled = true
                vcsRootExtId = "${DslContext.settingsRoot.id}"
                publisher = gitlab {
                    authType = vcsRoot()
                }
            }
        }
    }

    requirements {
        equals("teamcity.agent.jvm.os.name", "Linux")
    }

})