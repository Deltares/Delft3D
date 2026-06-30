set(CSHARP_PROJECT_NAME DFlowFM.IO.NET)
set(CSHARP_SOLUTION_FILE ${CMAKE_CURRENT_SOURCE_DIR}/DFlowFM.IO.NET.slnx)
set(CSHARP_DOTNET_CONFIG $<IF:$<CONFIG:Debug>,Debug,Release>)
set(CSHARP_OUTPUT_DIR ${CMAKE_CURRENT_BINARY_DIR}/bin/${CSHARP_DOTNET_CONFIG})

add_custom_target(${CSHARP_PROJECT_NAME} ALL
    COMMAND dotnet build
        ${CSHARP_SOLUTION_FILE}
        --configuration ${CSHARP_DOTNET_CONFIG}
        -p:Platform="Any CPU"
        -p:OutputPath=${CSHARP_OUTPUT_DIR}
        -p:DFlowFM_IO_NativeLibrary=$<TARGET_FILE:dflowfm_io_api>
    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
    COMMENT "Building C# solution ${CSHARP_PROJECT_NAME}"
)

add_dependencies(${CSHARP_PROJECT_NAME} dflowfm_io_api)

add_test(NAME ${CSHARP_PROJECT_NAME}.Tests
    COMMAND dotnet test
        ${CSHARP_SOLUTION_FILE}
        --configuration ${CSHARP_DOTNET_CONFIG}
        -p:Platform="Any CPU"
        -p:OutputPath=${CSHARP_OUTPUT_DIR}
        --no-build
    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
)