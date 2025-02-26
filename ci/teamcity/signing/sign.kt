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
            name = "Move already signed binaries"
            platform = PowerShellStep.Platform.x64
            scriptMode = file {
                path = "ci/teamcity/signing/move_signed_bins.ps1"
            }
            scriptArgs = "-source to_sign -destination signed"
        }
        powerShell {
            name = "Move exception binary tclkitsh852.exe"
            platform = PowerShellStep.Platform.x64
            scriptMode = script {
                // We cannot sing this binary, see: https://wiki.tcl-lang.org/page/SDX+under+Windows
                content = """
                    if (-Not (Test-Path -Path dont_sign\\bin)) {
                        New-Item -ItemType Directory -Path dont_sign\\bin
                    }
                    Move-Item -Path to_sign\\bin\\tclkitsh852.exe -Destination dont_sign\\bin\\tclkitsh852.exe -Force
                """.trimIndent()
            }
        }
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
        powerShell {
            name = "Move back signed binaries"
            platform = PowerShellStep.Platform.x64
            scriptMode = file {
                path = "ci/teamcity/signing/move_signed_bins.ps1"
            }
            scriptArgs = "-source signed -destination to_sign"
        }
        powerShell {
            name = "Move back exception binary tclkitsh852.exe"
            platform = PowerShellStep.Platform.x64
            scriptMode = script {
                content = """Move-Item -Path dont_sign\\bin\\tclkitsh852.exe -Destination to_sign\\bin\\tclkitsh852.exe -Force""".trimIndent()
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
                    ?:*_x64_*.zip!/x64/bin/** => to_sign/bin
                    ?:*_x64_*.zip!/x64/lib/** => to_sign/lib
                    ?:*_x64_*.zip!/x64/share/** => to_sign/share
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
