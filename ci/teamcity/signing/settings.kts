import jetbrains.buildServer.configs.kotlin.*
import jetbrains.buildServer.configs.kotlin.projectFeatures.*
import Sign

version = "2025.11"

project {

    description = "contact: BlackOps (black-ops@deltares.nl)"

    cleanup {
        baseRule {
            artifacts(builds = 5, artifactPatterns = "+:**/*")
            // Count days from the last successful build; protect the artifacts of the latest five successes.
            history(days = 90, builds = 5)
            preventDependencyCleanup = false
        }
        // Preserve history and statistics after the logs and artifacts expire.
        keepRule {
            id = "KeepHistoryAndStatistics"
            keepAtLeast = allBuilds()
            dataToKeep = historyAndStatistics()
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
