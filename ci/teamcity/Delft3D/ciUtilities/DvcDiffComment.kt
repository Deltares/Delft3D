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

    params {
        param("minio_testbench_access_key_id", DslContext.getParameter("minio_testbench_access_key_id"))
        password("minio_testbench_access_secret", DslContext.getParameter("minio_testbench_access_secret"))
    }
    
    vcs {
        root(DslContext.settingsRoot)
        cleanCheckout = true
    }

    steps {
        script {
            name = "place a comment on the PR"
            scriptContent = """
            UV_PROJECT="ci/teamcity/Delft3D/ciUtilities/" uv sync 
            source ci/teamcity/Delft3D/ciUtilities/.venv/bin/activate
            AWS_ACCESS_KEY_ID="%minio_testbench_access_key_id%" AWS_SECRET_ACCESS_KEY="%minio_testbench_access_secret%" ./ci/teamcity/Delft3D/ciUtilities/scripts/postDvcDiffReport.sh "%teamcity.pullRequest.target.branch%" "%teamcity.pullRequest.number%" "%github_deltares-service-account_access_token%" 
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
