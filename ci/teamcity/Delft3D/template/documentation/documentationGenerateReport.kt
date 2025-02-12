package Delft3D.template

import jetbrains.buildServer.configs.kotlin.*
import jetbrains.buildServer.configs.kotlin.buildSteps.*
import jetbrains.buildServer.configs.kotlin.buildFeatures.*
import jetbrains.buildServer.configs.kotlin.triggers.*


object TemplateDocumentationGenerateReport : Template({
    name = "Generate report step."

    steps {
        python {
            name = "Generate report"
            id = "GENERATE_REPORT"
            environment = venv {
                requirementsFile = "test/deltares_testbench/thirdparty/scripts/requirements.txt"
            }
            command = file {
                filename = "test/deltares_testbench/thirdparty/scripts/generate_report.py"
                scriptArguments = "--texfile %engine_dir%/doc/validation/%engine_name%_validation_doc.tex"
            }
        } 
    }
})
