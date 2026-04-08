package Delft3D.ciUtilities

import jetbrains.buildServer.configs.kotlin.*
import jetbrains.buildServer.configs.kotlin.triggers.*
import jetbrains.buildServer.configs.kotlin.buildFeatures.dockerRegistryConnections

object SigCi : BuildType({
    name = "Sig Ci"
    buildNumberPattern = "%build.vcs.number%"
    
    vcs {
        root(DslContext.settingsRoot)
    }

    steps {
        step {
            name = "Upload to sigrid using recipe"
            type = "SigridCiUploadTemplate2"
            param("sourceDir", ".")
            param("system", "dflow-flexible")
            param("targetquality", "3.5")
            param("publish", "--publish")
            param("showupload", "--showupload")
            param("customer", "deltares")
            param(
                "include",
                """
                src/cmake,
                src/engines_gpl,
                src/plugins_lgpl,
                src/scripts_lgpl,
                src/test,
                src/tools_gpl,
                src/tools_lgpl,
                src/utils_gpl/flow1d,
                src/utils_gpl/morphology,
                src/utils_gpl/trachytopes,
                src/utils_lgpl/deltares_common,
                src/utils_lgpl/ec_module,
                src/utils_lgpl/ftnunit,
                src/utils_lgpl/gridgeom,
                src/utils_lgpl/io_netcdf,
                src/utils_lgpl/kdtree_wrapper,
                src/utils_lgpl/metistools,
                src/utils_lgpl/unittests,
                src/version_includes
                """.trimIndent()
            )
        }
    }

    features {
        dockerRegistryConnections {
            loginToRegistry = on {
                dockerRegistryId = "PROJECT_EXT_304"
            }
        }
    }

    if (DslContext.getParameter("enable_sigrid_trigger").lowercase() == "true") {
        triggers {
            vcs {
                branchFilter = "+:<default>"
                perCheckinTriggering = false
            }
        }
    }
})
