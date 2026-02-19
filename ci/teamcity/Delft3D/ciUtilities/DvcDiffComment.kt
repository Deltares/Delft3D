package Delft3D.linux

import java.io.File
import jetbrains.buildServer.configs.kotlin.*
import jetbrains.buildServer.configs.kotlin.buildSteps.*
import jetbrains.buildServer.configs.kotlin.triggers.*
import Delft3D.template.*
import Delft3D.linux.*
import Delft3D.step.*
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
            set -eo pipefail
            uv venv --python=3.12 .venv
            uv pip sync test/deltares_testbench/pip/lnx-dev-requirements.txt
            source .venv/bin/activate
            uv pip install jinja2-cli
            dvc diff "$(git merge-base main HEAD)" "$(git rev-parse HEAD)" --json > diff.json
            jinja2 ci/teamcity/Delft3D/ciUtilities/diff-report-template.jinja diff.json --lstrip-blocks --trim-blocks -o report.md
            if [ -s report.md ]; then
                PAYLOAD="$(jq -c -n --rawfile body report.md '${'$'}ARGS.named')"
                curl -L \
                    -X POST \
                    -H "Accept: application/vnd.github+json" \
                    -H "Authorization: Bearer %github_deltares-service-account_access_token%" \
                    -H "X-GitHub-Api-Version: 2022-11-28" \
                    https://api.github.com/repos/deltares/delft3d/issues/%teamcity.pullRequest.number%/comments \
                    -d "${'$'}PAYLOAD"
            fi
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
