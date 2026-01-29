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
                requirementsFile = ""
                pipArgs = "--editable ./ci/python"
            }
            command = module {
                module = "ci_tools.documentation.generate_validation_report"
                scriptArguments = """
                    --tex-file %engine_dir%/doc/validation/%engine_name%_validation_doc.tex
                    --teamcity
                """.trimIndent()
            }
        }
    }

    requirements {
        startsWith("teamcity.agent.jvm.os.name", "Windows", "RQ_2470")
    }

    if (DslContext.getParameter("enable_documentation_trigger").lowercase() == "true") {
        triggers {
            finishBuildTrigger {
                buildType = "Dimr_DimrCollector"
                branchFilter = "+:<default>"
            }
        }
    }

    features {
        perfmon {}
    }
    
    failureConditions {
        executionTimeoutMin = 180
    }
})
