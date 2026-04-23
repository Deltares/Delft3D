package Delft3D.template

import jetbrains.buildServer.configs.kotlin.*
import jetbrains.buildServer.configs.kotlin.buildSteps.*
import jetbrains.buildServer.configs.kotlin.buildFeatures.*
import jetbrains.buildServer.configs.kotlin.triggers.*


object TemplateFunctionalityDocumentation : Template({
    name = "Generate functionality report"
    description = "This build configuration generates functionality reports for the Delft3D engine."
    buildNumberPattern = "%build.vcs.number%"

    artifactRules = """
        %engine_dir%/*.log=>logging
        %engine_dir%/doc/functionalities/*.pdf=>pdf
        %engine_dir%/doc/functionalities/*.log=>logging
        %engine_dir%/*/doc/*.pdf=>pdf/functionality
        %engine_dir%/*/doc/*.log=>logging/functionality
    """.trimIndent()

    params {
        param("s3_dsctestbench_accesskey", DslContext.getParameter("s3_dsctestbench_accesskey"))
        password("s3_dsctestbench_secret", "credentialsJSON:7e8a3aa7-76e9-4211-a72e-a3825ad1a160")
    }

    vcs {
        root(DslContext.settingsRoot)
        cleanCheckout = true
    }

    steps {
        python {
            environment = venv {
                requirementsFile = ""
                pipArgs = "--editable ./ci/python"
            }
            name = "Generate functionality report"
            id = "GENERATE_FUNCTIONALITY_REPORT"
            command = module {
                module = "ci_tools.documentation.generate_functionality_report"
                scriptArguments = """
                    --engine-dir=%engine_dir%
                    --max-workers=8
                    --teamcity
                """.trimIndent()
            }
        }
    }

    requirements {
        startsWith("teamcity.agent.jvm.os.name", "Windows", "RQ_2470")
    }

    triggers {
        if (DslContext.getParameter("enable_documentation_trigger").lowercase() == "true") {
            schedule {
                schedulingPolicy = weekly {
                    dayOfWeek = ScheduleTrigger.DAY.Sunday
                    hour = 0
                    minute = 0
                }
                branchFilter = "+:<default>"
                triggerBuild = always()
                withPendingChangesOnly = false
            }
        }
    }    
    
    failureConditions {
        executionTimeoutMin = 180
    }

    features {
        perfmon {}
    }
})
