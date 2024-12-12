package Delft3D.template

import jetbrains.buildServer.configs.kotlin.*
import jetbrains.buildServer.configs.kotlin.buildSteps.*

object TemplateDetermineProduct : Template({

    name = "Determine product to build and test."
    description = "Determine branch prefix for selecting testbenches to run."

    params {
        param("product", "dummy_value")
    }

    steps {
        python {
            name = "Determine product by branch prefix"
            command = script {
                content="""
                    if "%product%" == "dummy_value":
                        if "merge-request" in "%teamcity.build.branch%":
                            product = "%teamcity.pullRequest.source.branch%".split("/")[0]
                            print(f"##teamcity[setParameter name='product' value='{product}']")
                        else:
                            product = "%teamcity.build.branch%".split("/")[0]
                            print(f"##teamcity[setParameter name='product' value='{product}']")
                """.trimIndent()
            }
        }
    }
})
