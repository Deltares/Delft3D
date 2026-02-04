import jetbrains.buildServer.configs.kotlin.*
import jetbrains.buildServer.configs.kotlin.buildSteps.*
import jetbrains.buildServer.configs.kotlin.buildFeatures.*

import Delft3D.linux.*
import Delft3D.windows.*

object PublishToGui : BuildType({
    name = "Publish to GUI"
    description = "Push latest DIMR release to NuGet for GUI pipeline"

    buildNumberPattern = "%build.vcs.number%"

    features {
        approval {
            approvalRules = "group:DIMR_BAKKERS:1"
        }
    }

    params {
        param("DIMR_nuget_version", "%release_version%")
        param("grid_geom_version", "1.0.0")
        param("ec_module_version", "1.0.0")
    }

    vcs {
        root(DslContext.settingsRoot)
        checkoutMode = CheckoutMode.ON_AGENT
        cleanCheckout = true
        checkoutDir = "publish_dimr_to_gui"
    }

    steps {
        nuGetPack {
            name = "Pack DIMR NuGet"
            toolPath = "%teamcity.tool.NuGet.CommandLine.DEFAULT%"
            paths = "ci/nuget/Dimr.Libs/Dimr.Libs.nuspec"
            version = "%DIMR_nuget_version%"
            outputDir = "target"
            cleanOutputDir = false
            publishPackages = true
        }
        powerShell {
            name = "Set ECModule version"
            formatStderrAsError = true
            scriptMode = script {
                content = """
                    ${'$'}pathToDll = "source\x64\lib\ec_module.dll"
                    ${'$'}fileVersion = (Get-Item ${'$'}pathToDll).VersionInfo.FileVersionRaw
                    
                    if (${'$'}fileVersion -ne ${'$'}null) {
                    	Write-Output "##teamcity[setParameter name='ec_module_version' value='${'$'}fileVersion']"
                    } else {
                        Write-Output "Unable to retrieve ECModule version."
                        exit 1
                    }
                """.trimIndent()
            }
        }
        nuGetPack {
            name = "Pack EC Module NuGet"
            toolPath = "%teamcity.tool.NuGet.CommandLine.DEFAULT%"
            paths = "ci/nuget/ECModule.Native/ECModule.Native.nuspec"
            version = "%ec_module_version%"
            outputDir = "target"
            cleanOutputDir = false
            publishPackages = true
        }
        powerShell {
            name = "Set GridGeom version"
            formatStderrAsError = true
            scriptMode = script {
                content = """
                    ${'$'}pathToDll = "source\x64\lib\gridgeom.dll"
                    ${'$'}fileVersion = (Get-Item ${'$'}pathToDll).VersionInfo.FileVersionRaw
                    
                    if (${'$'}fileVersion -ne ${'$'}null) {
                    	Write-Output "##teamcity[setParameter name='grid_geom_version' value='${'$'}fileVersion']"
                    } else {
                        Write-Output "Unable to retrieve GridGeom version."
                        exit 1
                    }
                """.trimIndent()
            }
        }
        nuGetPack {
            name = "Pack GridGeom NuGet"
            toolPath = "%teamcity.tool.NuGet.CommandLine.DEFAULT%"
            paths = "ci/nuget/GridGeom.Native/GridGeom.Native.nuspec"
            version = "%grid_geom_version%"
            outputDir = "target"
            cleanOutputDir = false
            publishPackages = true
        }
    }

    if (DslContext.getParameter("enable_release_publisher").lowercase() == "true") {
        dependencies {
            dependency(LinuxCollect) {
                snapshot {
                }
                
                artifacts {
                    artifactRules = """
                        dimrset_lnx64_*.tar.gz!lnx64/bin/** => source/bin
                        dimrset_lnx64_*.tar.gz!lnx64/lib/** => source/lib
                        ?:dimrset_lnx64_*.tar.gz!lnx64/share/** => source/share
                    """.trimIndent()
                }
            }
            dependency(WindowsCollect) {
                snapshot {
                }
                artifacts {
                    artifactRules = "dimrset_x64_*.zip!**=>source"
                }
            }
            dependency(LinuxTest) {
                snapshot {
                    onDependencyFailure = FailureAction.FAIL_TO_START
                    onDependencyCancel = FailureAction.CANCEL
                }
            }
            dependency(WindowsTest) {
                snapshot {
                    onDependencyFailure = FailureAction.FAIL_TO_START
                    onDependencyCancel = FailureAction.CANCEL
                }
            }
            dependency(LinuxUnitTest) {
                snapshot {
                    onDependencyFailure = FailureAction.FAIL_TO_START
                    onDependencyCancel = FailureAction.CANCEL
                }
            }
            dependency(WindowsUnitTest) {
                snapshot {
                    onDependencyFailure = FailureAction.FAIL_TO_START
                    onDependencyCancel = FailureAction.CANCEL
                }
            }
            dependency(LinuxRunAllContainerExamples) {
                snapshot {
                    onDependencyFailure = FailureAction.FAIL_TO_START
                    onDependencyCancel = FailureAction.CANCEL
                }
            }
        }
    }

    cleanup {
        keepRule {
            id = "RetainAll"
            keepAtLeast = allBuilds()
            dataToKeep = everything()
            applyPerEachBranch = true
            preserveArtifactsDependencies = true
        }
    }

    requirements {
        startsWith("teamcity.agent.jvm.os.name", "Windows 1")
    }
})
