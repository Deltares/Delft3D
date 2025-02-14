package Delft3D.windows

import jetbrains.buildServer.configs.kotlin.*
import jetbrains.buildServer.configs.kotlin.buildFeatures.*
import jetbrains.buildServer.configs.kotlin.buildSteps.*
import jetbrains.buildServer.configs.kotlin.failureConditions.*

import Delft3D.template.*

object FunctionalityDocumentDWaq : BuildType({
    templates(
        TemplateFunctionalityDocumentation,
        TemplateSetTimeVariable,
        TemplateDocumentationCheckout,
        TemplateDocumentationUpdateInfrastructure,
        TemplateDocumentationGenerateFunctionality)

    name = "D-Water Quality- Functionality document (Latex/PDF)"

    params {
        param("engine_dir", "e03_waq")
        param("engine_name", "dwaq")
    }
    
    steps {
        stepsOrder = arrayListOf("SET_TIME_VARIABLE", "CHECKOUT_TESTBENCH_CASES_FROM_MINIO", "UPDATE_INFRASTRUCTURE_FUNCTIONALITY_REPORT", "GENERATE_FUNCTIONALITY_REPORT")
    }
})
