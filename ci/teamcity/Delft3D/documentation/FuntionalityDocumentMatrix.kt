package Delft3D.windows

import jetbrains.buildServer.configs.kotlin.*
import jetbrains.buildServer.configs.kotlin.buildFeatures.*
import jetbrains.buildServer.configs.kotlin.buildSteps.*
import jetbrains.buildServer.configs.kotlin.failureConditions.*

import Delft3D.template.*

object FunctionalityDocumentMatrix : BuildType({
    templates(
        TemplateDownloadFromS3, 
        TemplateFunctionalityDocumentation
    )

    name = "Functionality document (Latex/PDF)"

    val engineOptions = listOf(
        "dflowfm:e02_dflowfm",
        "dflowfm-dwaves:e100_dflowfm-dwaves",
        "dmorphology:e02_dflowfm",
        "dwaq:e03_waq"
    )

    params {
        select("engine_name_and_dir", "",
            allowMultiple = true,
            options = engineOptions,
            display = ParameterDisplay.PROMPT
        )

        param("engine_name", "")
        param("engine_dir", "")
    }

    features {
        matrix {
            id = "matrix"
            param("engine_name_and_dir", engineOptions.map { MatrixFeature.Value(it) })
        }
    }
})