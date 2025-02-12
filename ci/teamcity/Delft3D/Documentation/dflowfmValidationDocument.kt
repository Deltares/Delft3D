package Delft3D.windows

import jetbrains.buildServer.configs.kotlin.*
import jetbrains.buildServer.configs.kotlin.buildFeatures.*
import jetbrains.buildServer.configs.kotlin.buildSteps.*
import jetbrains.buildServer.configs.kotlin.failureConditions.*

import Delft3D.template.*

object DflowfmValidationDocument : BuildType({
    templates(
        TemplateDocumentationBuild,
        TemplateSetTimeVariable,
        TemplateDocumentationCheckout,
        TemplateDocumentationUpdateInfrastructure,
        TemplateDocumentationGenerateFunctionality,
        TemplateDocumentationGenerateReport)

    name = "D-Flow FM - Validation and Functionality document (Latex/PDF)"

    params {
        param("engine_dir", "e02_dflowfm")
        param("engine_name", "dflowfm")
    }
    
    steps {
        stepsOrder = arrayListOf("SET_TIME_VARIABLE", "CHECKOUT_TESTBENCH_CASES_FROM_MINIO", "UPDATE_INFRASTRUCTURE_FUNCTIONALITY_REPORT", "GENERATE_FUNCTIONALITY_REPORT", "GENERATE_REPORT")
    }
})
