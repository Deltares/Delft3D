package Delft3D.windows

import jetbrains.buildServer.configs.kotlin.*
import jetbrains.buildServer.configs.kotlin.buildFeatures.*
import jetbrains.buildServer.configs.kotlin.buildSteps.*
import jetbrains.buildServer.configs.kotlin.failureConditions.*

import Delft3D.template.*

object ValidationDocumentDMorphology : BuildType({
    templates(
        TemplateValidationDocumentation,
        TemplateSetTimeVariable,
        TemplateDocumentationCheckout,
        TemplateDocumentationGenerateValidationReport)

    name = "D-Morphology - Validation document (Latex/PDF)"

    params {
        param("engine_dir", "e02_dflowfm")
        param("engine_name", "dmorphology")
    }

    steps {
        stepsOrder = arrayListOf("SET_TIME_VARIABLE", "CHECKOUT_TESTBENCH_CASES_FROM_MINIO", "GENERATE_REPORT")
    }
})
