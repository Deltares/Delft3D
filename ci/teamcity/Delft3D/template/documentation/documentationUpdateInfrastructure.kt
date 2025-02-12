package Delft3D.template

import jetbrains.buildServer.configs.kotlin.*
import jetbrains.buildServer.configs.kotlin.buildSteps.*
import jetbrains.buildServer.configs.kotlin.buildFeatures.*
import jetbrains.buildServer.configs.kotlin.triggers.*


object TemplateDocumentationUpdateInfrastructure : Template({
    name = "Update functionality report step."

    steps {
        python {
            name = "Update infrastructure for functionality report"
            id = "UPDATE_INFRASTRUCTURE_FUNCTIONALITY_REPORT"
            environment = venv {
                requirementsFile = "test/deltares_testbench/thirdparty/scripts/requirements.txt"
            }
            command = file {
                filename = "test/deltares_testbench/thirdparty/scripts/update_functionality_report.py"
                scriptArguments = "--reldir ./%engine_dir%"
            }
        }
    }
})
