package deploy

import jetbrains.buildServer.configs.kotlin.*

object LinuxDeploy : BuildType({

    name = "Linux"
    type = BuildTypeSettings.Type.DEPLOYMENT
    enablePersonalBuilds = false

    vcs {
        root(DslContext.settingsRoot)
        branchFilter = """
            +:<default>
        """.trimIndent()
    }

    dependencies {
        snapshot(test.LinuxAll) {
            reuseBuilds = ReuseBuilds.NO
            onDependencyFailure = FailureAction.CANCEL
            onDependencyCancel = FailureAction.CANCEL
        }
    }

})