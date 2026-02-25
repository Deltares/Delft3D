package Delft3D.ciUtilities

import jetbrains.buildServer.configs.kotlin.*
import jetbrains.buildServer.configs.kotlin.buildSteps.*
import Delft3D.template.*
import Trigger

object DvcDiffComment : BuildType({

    name = "Dvc Diff"
    description = "Place a comment with the dvc diff in the PR"

    templates(
        TemplateMergeRequest,
    )
    
    vcs {
        root(DslContext.settingsRoot)
        cleanCheckout = true
    }

    steps {
        script {
            name = "place a comment on the PR"
            scriptContent = """
            cd ci/teamcity/Delft3D/ciUtilities/
            uv sync 
            source .venv/bin/activate
            ./scripts/postDvcDiffReport.sh "%teamcity.pullRequest.target.branch%" "%teamcity.pullRequest.number%" "%github_deltares-service-account_access_token%" 
            """
        }
        
    }

    dependencies {
        dependency(Trigger) {
            snapshot {
                onDependencyFailure = FailureAction.FAIL_TO_START
            }
        }
    }

    requirements {
        equals("teamcity.agent.jvm.os.name", "Linux")
    }
})
