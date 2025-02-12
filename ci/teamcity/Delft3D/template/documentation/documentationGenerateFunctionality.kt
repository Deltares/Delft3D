package Delft3D.template

import jetbrains.buildServer.configs.kotlin.*
import jetbrains.buildServer.configs.kotlin.buildSteps.*
import jetbrains.buildServer.configs.kotlin.buildFeatures.*
import jetbrains.buildServer.configs.kotlin.triggers.*


object TemplateDocumentationGenerateFunctionality : Template({
    name = "Generate functionality report step."

    steps {
        python {
            environment = venv {
                requirementsFile = "test/deltares_testbench/thirdparty/scripts/requirements.txt"
            }
            name = "Generate functionality report"
            id = "GENERATE_FUNCTIONALITY_REPORT"
            command = file {
                filename = "test/deltares_testbench/thirdparty/scripts/generate_functionality_report.py"
                scriptArguments = "--engine_dir_name %engine_dir%"
            }
        } 
    }
})
