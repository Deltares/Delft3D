set(CSHARP_PROJECT_NAME DFlowFM.IO.NET)
set(CSHARP_SOLUTION_FILE ${CMAKE_CURRENT_SOURCE_DIR}/DFlowFM.IO.NET.slnx)

add_custom_target(${CSHARP_PROJECT_NAME} ALL
    COMMAND dotnet build
        ${CSHARP_SOLUTION_FILE}
        --configuration $<IF:$<CONFIG:Debug>,Debug,Release>
        -p:Platform="Any CPU"
        -p:OutputPath=${CMAKE_BINARY_DIR}/bin/${CSHARP_PROJECT_NAME}
        -p:DFlowFM_IO_NativeLibrary=$<TARGET_FILE:dflowfm_io_api>
    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
    COMMENT "Building C# solution ${CSHARP_PROJECT_NAME}"
)

add_dependencies(${CSHARP_PROJECT_NAME} dflowfm_io_api)

add_test(NAME ${CSHARP_PROJECT_NAME}.Tests
    COMMAND dotnet test
        ${CSHARP_SOLUTION_FILE}
        --configuration $<IF:$<CONFIG:Debug>,Debug,Release>
        -p:Platform="Any CPU"
        --no-build
    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
)