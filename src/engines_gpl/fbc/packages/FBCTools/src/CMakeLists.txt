set(FBCTools_static_SRC
            piDiagInterface.cpp
            rtcToolsRuntime.cpp
            rtcToolsSimulator.cpp
)
set(dataBindingSRC
            dataBinding/pi_diag.cxx
            dataBinding/pi_modelparameters.cxx
            dataBinding/pi_run.cxx
            dataBinding/pi_sharedtypes.cxx
            dataBinding/pi_state.cxx
            dataBinding/pi_timeseries.cxx
            dataBinding/rtcDataConfig.cxx
            dataBinding/rtcObjectiveConfig.cxx
            dataBinding/rtcRuntimeConfig.cxx
            dataBinding/rtcSharedTypes.cxx
            dataBinding/rtcToolsConfig.cxx
            dataBinding/treeVector.cxx
)
set(schematizationSRC
            schematization/element.cpp
            schematization/expression.cpp
            schematization/lookup2DTable.cpp
            schematization/lookupTable.cpp
            schematization/mergerSplitter.cpp
            schematization/parameterInterface.cpp
            schematization/rtcMerger.cpp
            schematization/schematisation.cpp
            schematization/unitDelay.cpp
)
set(schematization_componentsSRC
            schematization/components/component.cpp
)
set(schematization_rulesSRC
            schematization/rules/absoluteTimeController.cpp
            schematization/rules/constantRule.cpp
            schematization/rules/dateLookupTableRule.cpp
            schematization/rules/deadBandValueRule.cpp
            schematization/rules/guideBandRule.cpp
            schematization/rules/intervalController.cpp
            schematization/rules/limiterRule.cpp
            schematization/rules/pidController.cpp
            schematization/rules/pidControllerSobek2.cpp
            schematization/rules/pidControllerPositional.cpp
            schematization/rules/pidControllerVelocity.cpp
            schematization/rules/relativeTimeController.cpp
            schematization/rules/rule.cpp
)
set(schematization_triggersSRC
            schematization/triggers/condition.cpp
            schematization/triggers/deadbandTimeTrigger.cpp
            schematization/triggers/deadbandTrigger.cpp
            schematization/triggers/polygon.cpp
            schematization/triggers/polygonTrigger.cpp
            schematization/triggers/ruleReferenceTrigger.cpp
            schematization/triggers/setTrigger.cpp
            schematization/triggers/standardTrigger.cpp
            schematization/triggers/trigger.cpp
)
set(timeseriesSRC
            timeseries/csvInterface.cpp
            timeseries/openMIExchangeItem.cpp
            timeseries/openMIInterface.cpp
            timeseries/piTimeSeries.cpp
            timeseries/piTimeSeriesSAX2Handler.cpp
            timeseries/scenarioTreeGenerator.cpp
            timeseries/scenarioTree_binary.cpp
            timeseries/scenarioTree_oneToN.cpp
            timeseries/sparseTimeSeries.cpp
            timeseries/stateInterface.cpp
            timeseries/stringContainer.cpp
            timeseries/timeSeriesBasics.cpp
            timeseries/timeSeriesInterface.cpp
            timeseries/timeSeriesMatrix.cpp
            timeseries/timeSeriesModel.cpp
            timeseries/timeSeriesSparseMatrix.cpp
            timeseries/timeSeriesSparseTensor.cpp
            timeseries/timeSeriesTensor.cpp
)
set(utilitiesSRC
            utilities/dateLookupTableConverter.cpp
            utilities/equidistantAggregation.cpp
            utilities/linearEquationConverter.cpp
            utilities/linearEquationSolvers.cpp
            utilities/lookup2DTableConverter.cpp
            utilities/lookupTableConverter.cpp
            utilities/monotonLookupTableConverter.cpp
            utilities/powerEquationConverter.cpp
            utilities/sigmoidLogisticEquationConverter.cpp
            utilities/tansigEquationConverter.cpp
            utilities/utils.cpp
)
source_group(src FILES ${FBCTools_static_SRC})
source_group(src/dataBinding FILES ${dataBindingSRC})
source_group(src/optimizationProblem FILES ${optimizationProblemSRC})
source_group(src/postprocess FILES ${postprocessSRC})
source_group(src/schematization FILES ${schematizationSRC})
source_group(src/schematization/components FILES ${schematization_componentsSRC})
source_group(src/schematization/rules FILES ${schematization_rulesSRC})
source_group(src/schematization/triggers FILES ${schematization_triggersSRC})
source_group(src/timeseries FILES ${timeseriesSRC})
source_group(src/utilities FILES ${utilitiesSRC})

add_library(FBCTools_static STATIC 
            ${FBCTools_static_SRC}
            ${dataBindingSRC}
            ${optimizationProblemSRC}
            ${postprocessSRC}
            ${schematizationSRC}
            ${schematization_componentsSRC}
            ${schematization_rulesSRC}
            ${schematization_triggersSRC}
            ${timeseriesSRC}
            ${utilitiesSRC}
            ${PROJECT_SOURCE_DIR}/../../version/fbc_version.h
)
set_target_properties (FBCTools_static PROPERTIES FOLDER engines_gpl/fbc)
target_link_libraries(FBCTools_static PRIVATE ${LINK_LIBRARIES} ${Boost_LIBRARIES})
target_include_directories(FBCTools_static PRIVATE ${Boost_INCLUDE_DIR})
