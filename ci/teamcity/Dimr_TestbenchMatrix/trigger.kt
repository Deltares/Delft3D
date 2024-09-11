package testbenchMatrix

import jetbrains.buildServer.configs.kotlin.*
import jetbrains.buildServer.configs.kotlin.buildFeatures.commitStatusPublisher
import jetbrains.buildServer.configs.kotlin.buildFeatures.pullRequests
import jetbrains.buildServer.configs.kotlin.buildSteps.python
import jetbrains.buildServer.configs.kotlin.triggers.VcsTrigger
import jetbrains.buildServer.configs.kotlin.triggers.vcs
import jetbrains.buildServer.configs.kotlin.buildSteps.script

object Trigger : BuildType({

    name = "Trigger"
    buildNumberPattern = "%build.revisions.revision%"

    vcs {
        root(DslContext.settingsRoot)
    }

    params {
        param("testbench_table", "ci/teamcity/Dimr_TestbenchMatrix/dimr_testbench_table.csv")
    
        param("teamcity_user", "svc_dimr_trigger")
        password("teamcity_pass", "credentialsJSON:15cc6665-e900-4360-8942-00e654f6acfe")
    
        param("matrix_list",         "dummy_value")
        param("matrix_list_linux",   "dummy_value")
        param("matrix_list_windows", "dummy_value")
        param("git_head",            "dummy_value")
        param("branch_name",         "dummy_value")
    }

    steps {

        python {
            name = "Determine Git head"
            command = script {
                content="""
                if "merge-request" in "%teamcity.build.branch%":
                    branch_name = "%teamcity.pullRequest.source.branch%".split("/")[0]
                    print("##teamcity[setParameter name='git_head' value='refs/%teamcity.build.branch%/head']")
                    print(f"##teamcity[setParameter name='branch_name' value='{branch_name}']")
                else:
                    branch_name = "%teamcity.build.branch%".split("/")[0]
                    print("##teamcity[setParameter name='git_head' value='refs/heads/%teamcity.build.branch%']")
                    print(f"##teamcity[setParameter name='branch_name' value='{branch_name}']")
                """.trimIndent()
            }
        }

        python {
            name = "Retrieve Testbench XMLs from CSV"
            command = file {
                filename = "ci/teamcity/scripts/testbench_filter.py"
                scriptArguments = "%branch_name% %testbench_table% matrix_list"
            }
        }

        python {
            name = "Filter Testbench xmls"
            command = script {
                content="""
                    matrix_list = "%matrix_list%".split(',')
                    matrix_list_linux   = ",".join([xml for xml in matrix_list if "lnx64" in xml])
                    matrix_list_windows = ",".join([xml for xml in matrix_list if "win64" in xml])

                    print(f"##teamcity[setParameter name='matrix_list_linux' value='{matrix_list_linux}']")
                    print(f"##teamcity[setParameter name='matrix_list_windows' value='{matrix_list_windows}']")
                """.trimIndent()
            }
        }

        script {
            name = "Start Linux Testbench"

            scriptContent = """
                curl -sS \
                     -u %teamcity_user%:%teamcity_pass% \
                     -X POST \
                     -H "Content-Type: application/xml" \
                     -d '<build branchName="%teamcity.build.branch%">
                            <buildType id="Dimr_TestbenchMatrix_Linux"/>
                            <revisions>
                                <revision version="%build.vcs.number%" vcsBranchName="%git_head%">
                                    <vcs-root-instance vcs-root-id="Delft3dGitlab"/>
                                </revision>
                            </revisions>
                            <properties>
                                <property name="configfile" value="%matrix_list_linux%"/>
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

        script {
            name = "Start Windows Testbench"

            scriptContent = """
                curl --fail --verbose --silent --show-error \
                     -u %teamcity_user%:%teamcity_pass% \
                     -X POST \
                     -H "Content-Type: application/xml" \
                     -d '<build branchName="%teamcity.build.branch%">
                            <buildType id="Dimr_TestbenchMatrix_Windows"/>
                            <revisions>
                                <revision version="%build.vcs.number%" vcsBranchName="%git_head%">
                                    <vcs-root-instance vcs-root-id="Delft3dGitlab"/>
                                </revision>
                            </revisions>
                            <properties>
                                <property name="configfile" value="%matrix_list_windows%"/>
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
                filterSourceBranch = """
                    -:refs/heads/none/*
                    +:*
                """.trimIndent()
                ignoreDrafts = true
            }
        }
    }

    requirements {
        equals("teamcity.agent.jvm.os.name", "Linux")
    }

})