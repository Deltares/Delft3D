add_library(dflowfm_io_api SHARED
    include/dflowfm_io_api/dflowfm_io_api.h
    dflowfm_io_api.cpp
)

target_link_libraries(dflowfm_io_api
    PRIVATE dflowfm_io
)

include(GenerateExportHeader)
generate_export_header(dflowfm_io_api
    BASE_NAME dflowfm_io_api
    EXPORT_FILE_NAME ${CMAKE_CURRENT_BINARY_DIR}/dflowfm_io_api/dflowfm_io_api_export.h
)

target_include_directories(dflowfm_io_api
    PUBLIC
        ${CMAKE_CURRENT_SOURCE_DIR}/include
        ${CMAKE_CURRENT_BINARY_DIR} # For the generated export header
)

install(TARGETS dflowfm_io_api
    RUNTIME DESTINATION bin
    LIBRARY DESTINATION lib
    ARCHIVE DESTINATION lib
)

# Stage the freshly built native library into the Python wheel package (_lib/) so `pip wheel`
# bundles it. The library is a build artifact: it is git-ignored there and only ever produced by
# the build, never committed.
set(DFLOWFM_IO_PY_WHEEL_LIB_DIR
	"${CMAKE_CURRENT_SOURCE_DIR}/wrappers/python/src/dflowfm_io/_lib")

add_custom_command(TARGET dflowfm_io_api POST_BUILD
	COMMAND ${CMAKE_COMMAND} -E make_directory "${DFLOWFM_IO_PY_WHEEL_LIB_DIR}"
	COMMAND ${CMAKE_COMMAND} -E copy_if_different
	"$<TARGET_FILE:dflowfm_io_api>" "${DFLOWFM_IO_PY_WHEEL_LIB_DIR}"
	COMMENT "Staging dflowfm_io_api into the Python wheel package (_lib/)"
	VERBATIM
)

include(CheckLanguage)
check_language(Fortran)
if(CMAKE_Fortran_COMPILER)
    enable_language(Fortran)
    add_subdirectory(wrappers/fortran)
endif()

find_program(DOTNET_EXECUTABLE dotnet)
if(DOTNET_EXECUTABLE)
    execute_process(
        COMMAND ${DOTNET_EXECUTABLE} --version
        RESULT_VARIABLE DOTNET_SDK_RESULT
        OUTPUT_QUIET ERROR_QUIET
    )
    if(DOTNET_SDK_RESULT EQUAL 0)
        add_subdirectory(wrappers/csharp)
    else()
        message(STATUS "No .NET SDK found. Skipping C# wrappers.")
    endif()
endif()

add_subdirectory(test)
