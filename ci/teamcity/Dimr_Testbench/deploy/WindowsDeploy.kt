package deploy

import jetbrains.buildServer.configs.kotlin.*

object WindowsDeploy : BuildType({

    name = "Windows"
    type = BuildTypeSettings.Type.DEPLOYMENT
    enablePersonalBuilds = false

    vcs {
        root(DslContext.settingsRoot)
        branchFilter = """
            +:<default>
        """.trimIndent()
    }

    dependencies {
        snapshot(test.WindowsAll) {
            reuseBuilds = ReuseBuilds.NO
            onDependencyFailure = FailureAction.CANCEL
            onDependencyCancel = FailureAction.CANCEL
        }
    }

})