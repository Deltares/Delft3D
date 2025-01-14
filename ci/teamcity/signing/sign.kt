import jetbrains.buildServer.configs.kotlin.*
import jetbrains.buildServer.configs.kotlin.buildSteps.*
import jetbrains.buildServer.configs.kotlin.buildFeatures.*

object Sign : BuildType({

    name = "Sign"
    buildNumberPattern = "%build.vcs.number%"
    artifactRules = "to_sign => oss_artifacts_x64_%build.vcs.number%.zip!x64"

    vcs {
        root(DslContext.settingsRoot)
    }

    steps {
        powerShell {
            name = "Sign"
            platform = PowerShellStep.Platform.x64
            workingDir = "to_sign"
            scriptMode = script {
                content = """
                    Get-ChildItem -Path . -Recurse -Include *.exe,*.dll | Foreach { signtool sign /v /debug /fd SHA256 /tr "http://timestamp.acs.microsoft.com" /td SHA256 /dlib "C:\ProgramData\Microsoft\MicrosoftTrustedSigningClientTools\Azure.CodeSigning.Dlib.dll" /dmdf "C:\ProgramData\Microsoft\MicrosoftTrustedSigningClientTools\metadata.json" ${'$'}_.fullname }
                """.trimIndent()
            }
        }
    }

    dependencies {
        dependency(AbsoluteId("${DslContext.getParameter("delft3d_project_root")}_WindowsBuild")) {
            snapshot {
                onDependencyFailure = FailureAction.FAIL_TO_START
                onDependencyCancel = FailureAction.CANCEL
            }
            artifacts {
                cleanDestination = true
                artifactRules = """
                    ?:*_x64_*.zip!/x64/**/delpar.exe => to_sign
                    ?:*_x64_*.zip!/x64/**/delwaq.dll => to_sign
                    ?:*_x64_*.zip!/x64/**/delwaq.exe => to_sign
                    ?:*_x64_*.zip!/x64/**/dflowfm.dll => to_sign
                    ?:*_x64_*.zip!/x64/**/dflowfm-cli.exe => to_sign
                    ?:*_x64_*.zip!/x64/**/dfmoutput.exe => to_sign
                    ?:*_x64_*.zip!/x64/**/dimr.exe => to_sign
                    ?:*_x64_*.zip!/x64/**/dimr.dll => to_sign
                    ?:*_x64_*.zip!/x64/**/ESMF_RegridWeightGen.exe => to_sign
                    ?:*_x64_*.zip!/x64/**/FBCTools_BMI.dll => to_sign
                    ?:*_x64_*.zip!/x64/**/gridgeom.dll => to_sign
                    ?:*_x64_*.zip!/x64/**/mormerge.exe => to_sign
                    ?:*_x64_*.zip!/x64/**/rr_dll.dll => to_sign
                    ?:*_x64_*.zip!/x64/**/swan_omp.exe => to_sign
                    ?:*_x64_*.zip!/x64/**/waq_plugin_wasteload.dll => to_sign
                    ?:*_x64_*.zip!/x64/**/wave.dll => to_sign
                    ?:*_x64_*.zip!/x64/**/wave.exe => to_sign
                    ?:*_x64_*.zip!/x64/**/ec_module.dll => to_sign
                    ?:*_x64_*.zip!/x64/**/plugin_culvert.dll => to_sign
                    ?:*_x64_*.zip!/x64/**/dfm_volume_tool.exe => to_sign
                    ?:*_x64_*.zip!/x64/**/maptonetcdf.exe => to_sign
                    ?:*_x64_*.zip!/x64/**/waqmerge.exe => to_sign
                    ?:*_x64_*.zip!/x64/**/waqpb_export.exe => to_sign
                    ?:*_x64_*.zip!/x64/**/waqpb_import.exe => to_sign
                    ?:*_x64_*.zip!/x64/**/cosumo_bmi.dll => to_sign
                    ?:*_x64_*.zip!/x64/**/io_netcdf.dll => to_sign
                    ?:*_x64_*.zip!/x64/**/plugin_delftflow_traform.dll => to_sign
                    ?:*_x64_*.zip!/x64/**/agrhyd.exe => to_sign
                    ?:*_x64_*.zip!/x64/**/ddcouple.exe => to_sign
                    ?:*_x64_*.zip!/x64/**/swan_mpi.exe => to_sign
                    ?:*_x64_*.zip!/x64/**/cosumo_bmi.dll => to_sign
                """.trimIndent()
            }
        }
    }

    features {
        if (DslContext.getParameter("environment") == "production") {
            commitStatusPublisher {
                enabled = true
                vcsRootExtId = "${DslContext.settingsRoot.id}"
                publisher = gitlab {
                    authType = vcsRoot()
                }
            }
        }
    }

})
