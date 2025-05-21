package Delft3D.template

import jetbrains.buildServer.configs.kotlin.*
import jetbrains.buildServer.configs.kotlin.buildSteps.*

object TemplateDownloadExamples : Template({

    name = "Download dflowfm examples cases"
    description = "Download dflowfm examples cases and move them to the right location."

    steps {
        python {
            name = "Download examples using TestBench.py"
            workingDir = "test/deltares_testbench/"
            environment = venv {
                requirementsFile = "pip/win-requirements.txt"
            }
            command = file {
                filename = "TestBench.py"
                scriptArguments = """
                    --username "%s3_dsctestbench_accesskey%"
                    --password "%s3_dsctestbench_secret%"
                    --reference
                    --config "configs/dimr/dimr_smoke_test_cases_win64.xml"
                    --filter "testcase=e100_f00_c00,e109_f01_c010_weir_timeseries"
                    --skip-run
                    --skip-post-processing
                    --log-level DEBUG
                    --parallel
                    --teamcity
                """.trimIndent()
            }
        }
        script {
            name = "Move examples to the right location and rename"
            scriptContent = """
                mv -v test/deltares_testbench/data/cases/e100_f00_c00/* ./examples/dflowfm/08_dflowfm_sequential_dwaves
                mv -v test/deltares_testbench/data/cases/e109_f01_c010_weir_timeseries/* ./examples/dflowfm/09_dflowfm_parallel_dwaves
            """.trimIndent()
        }
    }
})
