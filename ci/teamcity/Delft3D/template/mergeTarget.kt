package Delft3D.template

import jetbrains.buildServer.configs.kotlin.*
import jetbrains.buildServer.configs.kotlin.buildSteps.*

object TemplateMergeTarget : Template({

    name = "Merge Target Branch"
    description = "Merge target branch into branch for merge-requests."

    steps {
        script {
            name = "Merge target into branch"
            conditions {
                contains("teamcity.build.branch", "merge-requests")
            }
            workingDir = "."
            scriptContent = """
                git remote add temporary 'https://svc_teamcity_gitdsc:%gitlab_private_access_token%@git.deltares.nl/oss/delft3d.git'
                git fetch temporary refs/merge-requests/*:refs/remotes/temporary/merge-requests/* --quiet
                git checkout temporary/%teamcity.build.branch%/merge
                python3 -c "import subprocess; commit_id=subprocess.check_output(['git','rev-parse', 'HEAD'], universal_newlines=True).strip(); print('##teamcity[addBuildTag \'merge commit ID: '+commit_id+'\']')"
                git remote remove temporary
            """.trimIndent()
        }
    }
})