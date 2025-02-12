package Delft3D.template

import jetbrains.buildServer.configs.kotlin.*
import jetbrains.buildServer.configs.kotlin.buildSteps.*
import jetbrains.buildServer.configs.kotlin.buildFeatures.*
import jetbrains.buildServer.configs.kotlin.triggers.*


object TemplateDocumentationCheckout : Template({
    name = "Documentation Checkout step."

    steps {
        python {
            name = "Checkout Testbench cases from MinIO"
            id = "CHECKOUT_TESTBENCH_CASES_FROM_MINIO"
            environment = venv {
                requirementsFile = "test/deltares_testbench/thirdparty/scripts/requirements.txt"
            }
            command = file {
                filename = "test/deltares_testbench/thirdparty/scripts/download_from_s3.py"
                scriptArguments = "--access_key %s3_dsctestbench_accesskey% --secret_key %s3_dsctestbench_secret% --engine_dir %engine_dir% --iso_time %TIME_ISO_8601%"
            }
        }
    }
})
