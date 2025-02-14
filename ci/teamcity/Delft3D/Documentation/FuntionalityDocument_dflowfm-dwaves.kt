package Delft3D.windows

import jetbrains.buildServer.configs.kotlin.*
import jetbrains.buildServer.configs.kotlin.buildFeatures.*
import jetbrains.buildServer.configs.kotlin.buildSteps.*
import jetbrains.buildServer.configs.kotlin.failureConditions.*

import Delft3D.template.*

object FunctionalityDocumentDflowfmDwaves : BuildType({
    templates(
        TemplateFunctionalityDocumentation,
        TemplateSetTimeVariable,
        TemplateDocumentationCheckout,
        TemplateDocumentationUpdateInfrastructure,
        TemplateDocumentationGenerateFunctionality)

    name = "D-Flow FM, D-Waves - Functionality document (Latex/PDF)"



    params {
        param("engine_dir", "e100_dflowfm-dwaves")
        param("engine_name", "dflowfm-dwaves")
    }

    steps {
        stepsOrder = arrayListOf("SET_TIME_VARIABLE", "CHECKOUT_TESTBENCH_CASES_FROM_MINIO", "UPDATE_INFRASTRUCTURE_FUNCTIONALITY_REPORT", "GENERATE_FUNCTIONALITY_REPORT")
    }
})
