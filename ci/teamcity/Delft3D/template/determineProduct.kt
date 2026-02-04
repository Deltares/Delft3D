package Delft3D.template

import jetbrains.buildServer.configs.kotlin.*
import jetbrains.buildServer.configs.kotlin.buildSteps.*

object TemplateDetermineProduct : Template({

    name = "Determine product to build and test."
    description = "Determine branch prefix for selecting testbenches to run."

    params {
        param("product", "auto-select")
    }

    steps {
        python {
            name = "Determine product by branch prefix"
            command = script {
                content="""
                    if "%product%" == "auto-select":
                        if "pull" in "%teamcity.build.branch%":
                            source_branch = "%teamcity.pullRequest.source.branch%"
                            if source_branch.startswith("revert-") or source_branch.startswith("dependabot/"):
                                product = "tc"
                            else:
                                product = source_branch.split("/")[0]
                        else:
                            product = "%teamcity.build.branch%".split("/")[0]
                        if "%teamcity.build.branch.is_default%" == "true":
                            product = "all"
                        print(f"##teamcity[setParameter name='product' value='{product}-testbench']")
                        print(f"##teamcity[buildNumber '{product}-testbench: %build.vcs.number%']")
                """.trimIndent()
            }
        }
    }
})
