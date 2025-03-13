package Delft3D.template

import jetbrains.buildServer.configs.kotlin.*
import jetbrains.buildServer.configs.kotlin.buildSteps.*
import jetbrains.buildServer.configs.kotlin.buildFeatures.*
import jetbrains.buildServer.configs.kotlin.triggers.*


object TemplateValidationDocumentation : Template({
    name = "Generate validation report"
    description = "This build configuration generates validation reports for the Delft3D engine."
    buildNumberPattern = "%build.vcs.number%"

    artifactRules = """
        %engine_dir%/*.log=>logging
        %engine_dir%/doc/validation/*.pdf=>pdf
        %engine_dir%/doc/validation/*.log=>logging
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
            name = "Generate report"
            id = "GENERATE_REPORT"
            environment = venv {
                requirementsFile = "ci/teamcity/Delft3D/documentation/scripts/requirements.txt"
            }
            command = file {
                filename = "ci/teamcity/Delft3D/documentation/scripts/generate_report.py"
                scriptArguments = "--texfile %engine_dir%/doc/validation/%engine_name%_validation_doc.tex"
            }
        }
    }

    requirements {
        startsWith("teamcity.agent.jvm.os.name", "Windows", "RQ_2470")
    }

    triggers {
        finishBuildTrigger {
            buildType = "Dimr_DimrCollector"
            branchFilter = "+:<default>"
        }
    }    
    
    failureConditions {
        executionTimeoutMin = 180
    }
})
