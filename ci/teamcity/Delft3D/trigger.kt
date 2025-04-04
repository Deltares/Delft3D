import jetbrains.buildServer.configs.kotlin.*
import jetbrains.buildServer.configs.kotlin.buildFeatures.*
import jetbrains.buildServer.configs.kotlin.buildSteps.*
import jetbrains.buildServer.configs.kotlin.triggers.*
import Delft3D.template.*
import Delft3D.step.*
import Delft3D.linux.*
import Delft3D.windows.*

object Trigger : BuildType({

    description = "This is triggered for merge-requests and will schedule the appropriate testbenches."

    templates(
        TemplateMergeRequest,
        TemplateDetermineProduct,
        TemplatePublishStatus,
        TemplateMonitorPerformance
    )

    name = "Trigger"
    buildNumberPattern = "%build.vcs.number%"

    vcs {
        root(DslContext.settingsRoot)
    }

    params {
        param("testbench_table", "ci/teamcity/Delft3D/vars/dimr_testbench_table.csv")
    
        param("teamcity_user", "svc_dimr_trigger")
        password("teamcity_pass", "credentialsJSON:15cc6665-e900-4360-8942-00e654f6acfe")

        param("matrix_list_lnx64", "dummy_value")
        param("matrix_list_win64", "dummy_value")
        param("product", "auto-select")
    }

    steps {
        mergeTargetBranch {}
        python {
            name = "Retrieve Linux Testbench XMLs from CSV"
            command = file {
                filename = "ci/python/ci_tools/trigger/testbench_filter.py"
                scriptArguments = "-n %product% -f %testbench_table% -v lnx64"
            }
        }

        python {
            name = "Retrieve Windows Testbench XMLs from CSV"
            command = file {
                filename = "ci/python/ci_tools/trigger/testbench_filter.py"
                scriptArguments = "-n %product% -f %testbench_table% -v win64"
            }
        }

        script {
            name = "Start Linux Testbench"

            conditions {
                doesNotContain("teamcity.build.triggeredBy", "Snapshot dependency")
                doesNotEqual("product", "none-testbench")
                doesNotEqual("product", "qp-testbench")
            }

            scriptContent = """
                curl --fail --silent --show-error \
                     -u %teamcity_user%:%teamcity_pass% \
                     -X POST \
                     -H "Content-Type: application/xml" \
                     -d '<build branchName="%teamcity.build.branch%" replace="true">
                            <buildType id="${LinuxTest.id}"/>
                            <revisions>
                                <revision version="%build.vcs.number%" vcsBranchName="%teamcity.build.branch%">
                                    <vcs-root-instance vcs-root-id="DslContext.settingsRoot"/>
                                </revision>
                            </revisions>
                            <properties>
                                <property name="product" value="%product%"/>
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
                doesNotEqual("product", "none-testbench")
                doesNotEqual("product", "qp-testbench")
            }
            
            scriptContent = """
                curl --fail --silent --show-error \
                     -u %teamcity_user%:%teamcity_pass% \
                     -X POST \
                     -H "Content-Type: application/xml" \
                     -d '<build branchName="%teamcity.build.branch%" replace="true">
                            <buildType id="${WindowsTest.id}"/>
                            <revisions>
                                <revision version="%build.vcs.number%" vcsBranchName="%teamcity.build.branch%">
                                    <vcs-root-instance vcs-root-id="DslContext.settingsRoot"/>
                                </revision>
                            </revisions>
                            <properties>
                                <property name="product" value="%product%"/>
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

    requirements {
        equals("teamcity.agent.jvm.os.name", "Linux")
    }

})