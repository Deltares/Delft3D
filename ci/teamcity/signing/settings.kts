import jetbrains.buildServer.configs.kotlin.*
import jetbrains.buildServer.configs.kotlin.projectFeatures.*
import Sign

version = "2026.2"

project {
    description = "contact: BlackOps (black-ops@deltares.nl)"

    cleanup {
        baseRule {
            artifacts(builds = 5, artifactPatterns = "+:**/*")
            // Expire old build records but retain their chart statistics, such that storage is freed.
            history(days = 90, builds = 5)
            // preventDependencyCleanup does not affect teamcity logs apparently, only artifacts.
            preventDependencyCleanup = false
        }
        keepRule {
            id = "KeepMainAndReleaseLogs"
            keepAtLeast = allBuilds()
            dataToKeep = logs()
            applyToBuilds {
                inBranches {
                    branchFilter = patterns("+:main", "+:all/release/*")
                }
            }
        }
    }

    buildType(Sign)
}
