import jetbrains.buildServer.configs.kotlin.*
import jetbrains.buildServer.configs.kotlin.buildFeatures.*
import jetbrains.buildServer.configs.kotlin.buildSteps.*
import jetbrains.buildServer.configs.kotlin.triggers.*
import Delft3D.template.*
import Delft3D.step.*
import Delft3D.linux.*
import Delft3D.windows.*

object CopyExamples : BuildType({
    id("DHydro_ExampleCases_CopyDelft3dfmExampleCases")
    name = "Copy delft3dfm example cases"
    description = "Copy example files to P drive"
    buildNumberPattern = "%build.vcs.number%"

    templates(
        TemplateMonitorPerformance
    )

    vcs {
        root(DslContext.settingsRoot)
    }

    params {
        param("DEST_DIR", """\\directory.intra\PROJECT\d-hydro\dimrset\examples""")
    }

    steps {
        python {
            name = "Copy example files to P drive"
            command = file {
                filename = "ci/python/ci_tools/example_utils/copy_examples.py"
                scriptArguments = "%DEST_DIR% --tc_logging"
            }
        }
    }

    if (DslContext.getParameter("environment") == "production") {
        triggers {
            vcs {
                branchFilter = "+:<default>"
            }
        }
    }
    requirements {
        contains("teamcity.agent.jvm.os.name", "Windows")
    }
})