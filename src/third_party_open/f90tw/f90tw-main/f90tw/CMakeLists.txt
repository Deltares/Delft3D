# preprocess a fortran source file.
# INFILE, OUTFILE: the input and the output files
# EXE, SWITCH: the preprocessor executable and arguments switch
# OPTIONS, INCLUDES DEFINITIONS: the preprocessor options, include directories and definitions
function(F90TWFPP INFILE OUTFILE)
    set(prefix FPP)
    set(flags)
    set(singleValues EXE SWITCH DEPENDS)
    set(multiValues OPTIONS INCLUDES DEFINITIONS)

    include(CMakeParseArguments)
    cmake_parse_arguments(PARSE_ARGV 2 ${prefix}
                     "${flags}"
                     "${singleValues}"
                     "${multiValues}"
    )

    foreach(dir ${FPP_INCLUDES})
        string(PREPEND dir ${FPP_SWITCH} I)
        list(APPEND INCL ${dir})
    endforeach()
    foreach(def ${FPP_DEFINITIONS})
        string(PREPEND def ${FPP_SWITCH} D)
        list(APPEND DEFS ${def})
    endforeach()

    set(FPPOPTS "${FPP_OPTIONS}")
    set(FPPDEFS "${DEFS}")
    set(FPPINCL "${INCL}")

    if(WIN32)
        string(REPLACE ";" " " FPPINCL "${INCL}")
        find_program(POWERSHELL_EXE NAMES "powershell.exe" REQUIRED)
        add_custom_command(
            OUTPUT ${OUTFILE}
            COMMAND ${POWERSHELL_EXE} -Command "(& \"${FPP_EXE}\" ${FPPOPTS} ${FPPDEFS} ${FPPINCL} \"${INFILE}\") -replace '$<SEMICOLON>'$<COMMA>\"\`r\`n\" | ForEach-Object { $_.Trim() -split \"`r`n\" | ForEach-Object { if ($_ -notmatch \"^#\" -and $_.Trim() -ne '') { $_.Trim() } } } | Set-Content -Path \"${OUTFILE}\""
            DEPENDS ${INFILE} ${FPP_DEPENDS}
            COMMENT "precompile ${INFILE}"
            VERBATIM
        )
    else()
        find_program(SED_EXE NAMES "sed" REQUIRED)
        find_program(TR_EXE NAMES "tr" REQUIRED)
        add_custom_command(
            OUTPUT "${OUTFILE}"
            COMMAND "${FPP_EXE}" ${FPPOPTS} ${FPPDEFS} ${FPPINCL} "${INFILE}" | ${SED_EXE} "/^[ \\t]*#/d" | ${TR_EXE} "\\;" "\\n" | ${SED_EXE} "s/^[ \\t]*//;s/[ \\t]*$//;/^$/d" > "${OUTFILE}"
            DEPENDS "${INFILE}" ${FPP_DEPENDS}
            COMMENT "precompile ${INFILE}"
            VERBATIM
        )
    endif()
endfunction()

# extract the f90tw directive from the given fortran files
# INFILES: f90 files with f90tw directives to be extracted
# OUTFILE: header file to place the extracted directives
function(F90TWF2H INFILES OUTFILE)
    set(prefix F2H)
    set(flags)
    set(singleValues OUTFILE)
    set(multiValues INFILES)

    include(CMakeParseArguments)
    cmake_parse_arguments(PARSE_ARGV 0 ${prefix}
                    "${flags}"
                    "${singleValues}"
                    "${multiValues}"
    )

    if(WIN32)
        find_program(POWERSHELL_EXE NAMES "powershell.exe" REQUIRED)
        add_custom_command(
            OUTPUT ${F2H_OUTFILE}
            COMMAND ${POWERSHELL_EXE} -Command " \" ( ( cat ${F2H_INFILES} ) | select-string -pattern '^[ \\t]*[!cC][\\$$][fF]90[tT][wW]' ) -replace '^[ \\t]*[!cC][\\$$][fF]90[tT][wW][ \\t]*', '' | Set-Content -Path ${F2H_OUTFILE} \" "
            DEPENDS ${F2H_INFILES}
            COMMENT "process ${F2H_INFILES}"
        )
    else()
        find_program(SED_EXE NAMES "sed" REQUIRED)
        add_custom_command(
            OUTPUT ${F2H_OUTFILE}
            COMMAND ${SED_EXE} '/^[ \\t]*[!cC]\$$[fF]90[tT][wW]/!d \; s/.*[!cC]\$$[fF]90[tT][wW][ \\t]*\\\(.*\\\)$$/\\1/' ${F2H_INFILES} > ${F2H_OUTFILE}
            DEPENDS ${F2H_INFILES}
            COMMENT "process ${F2H_INFILES}"
        )
    endif()
endfunction()

# setup a F90TWTEST
# CFILES : list of c/c++ files
# F90FILES : list of fortran files
# F2HFILES : list of fortran files to be processed for extracting f90tw directives
# the rest of the arguments are the arguments of F90TWFPP and F90TWFPPFILES functions.
function(F90TWTEST NAME)
    set(prefix FTW)
    set(flags)
    set(singleValues OUTDIR VISUAL_STUDIO_PATH)
    set(multiValues CFILES F90FILES F2HFILES LIBRARIES INCLUDES DEFINITIONS)

    include(CMakeParseArguments)
    cmake_parse_arguments(PARSE_ARGV 1 ${prefix}
                    "${flags}"
                    "${singleValues}"
                    "${multiValues}"
    )

    list(APPEND FTW_INCLUDES ${f90tw_INCLUDE_DIR})

    set(FLIBNAME "${NAME}_f90")
    set(F2HTRGNAME "${NAME}_f2h")

    # create the fortran header files
    if (NOT "${FTW_F2HFILES}" STREQUAL "")
        list(GET FTW_F2HFILES 0 firstfile)
        get_filename_component(firstfilename "${firstfile}" NAME)
        set(F2HNAME "${firstfilename}.h")
        set(F2HFILE "${CMAKE_CURRENT_BINARY_DIR}/${F2HNAME}")
        F90TWF2H(INFILES ${FTW_F2HFILES} OUTFILE ${F2HFILE})
        add_custom_target(${F2HTRGNAME} ALL DEPENDS ${F2HFILE})
        set_target_properties(${F2HTRGNAME} PROPERTIES FOLDER ${FTW_VISUAL_STUDIO_PATH}/f2h)
    else()
        set(F2HFILE)
    endif()

    # create the fortran library
    add_library(${FLIBNAME} STATIC ${F2HFILE} ${FTW_F90FILES})
    set_property(TARGET ${FLIBNAME} PROPERTY POSITION_INDEPENDENT_CODE ON)
    target_include_directories(${FLIBNAME} PRIVATE ${CMAKE_CURRENT_BINARY_DIR} ${FTW_INCLUDES})
    set_property(TARGET ${FLIBNAME}
        APPEND PROPERTY ADDITIONAL_CLEAN_FILES ${XF90FILES}
    )
    set_target_properties(${FLIBNAME} PROPERTIES FOLDER ${FTW_VISUAL_STUDIO_PATH})

    if("${CMAKE_Fortran_COMPILER_ID}" MATCHES "GNU")
        target_compile_options(${FLIBNAME} PRIVATE "-ffree-line-length-none")
    endif()
    target_link_libraries( ${FLIBNAME} PRIVATE ${FTW_LIBRARIES} f90tw_gtest)
    if(NOT "${FTW_F2HFILES}" STREQUAL "")
        add_dependencies(${FLIBNAME} ${F2HTRGNAME})
    endif()

    # compile c and create the executable
    add_executable(${NAME} ${FTW_CFILES} ${F2HFILE})
    target_include_directories(${NAME} PRIVATE ${FPPINCLDS} ${CMAKE_CURRENT_BINARY_DIR} ${FTW_INCLUDES})
    target_link_libraries(${NAME} PRIVATE ${FLIBNAME} f90tw_gtest)
    target_compile_definitions(${NAME} PUBLIC ${FTW_DEFINITIONS})
    add_dependencies(${NAME} ${FLIBNAME})
    set_target_properties(${NAME} PROPERTIES FOLDER ${FTW_VISUAL_STUDIO_PATH}/cpp)

    if(WIN32)
        # Set separate output directories for each test to avoid parallel build conflicts
        set_target_properties(${NAME} PROPERTIES
            RUNTIME_OUTPUT_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/${NAME}"
            ARCHIVE_OUTPUT_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/${NAME}"
            LIBRARY_OUTPUT_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/${NAME}"
        )
        set_target_properties(${FLIBNAME} PROPERTIES ARCHIVE_OUTPUT_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/${NAME}")
        add_custom_command(TARGET ${NAME} POST_BUILD
            COMMAND ${CMAKE_COMMAND} -E copy -t $<TARGET_FILE_DIR:${NAME}> $<TARGET_RUNTIME_DLLS:${NAME}>
            COMMAND_EXPAND_LISTS
        )
        
    endif()
    gtest_discover_tests(${NAME} WORKING_DIRECTORY $<TARGET_FILE_DIR:${NAME}>)
endfunction()

# resolve the preprocessor to be used. it is assumed that on unix-based systems
# cpp or clang will be found in the path and in addition, on windows-based
# systems, cl.exe may be available. On mac os cpp is an interface to clang with
# "-traditional-cpp" locked. To avoid this we use directly the clang front end.
if(WIN32)
    set(FPP_EXE "${CMAKE_C_COMPILER}")
else()
    find_program(FPP_EXE NAMES clang cpp REQUIRED)
endif()

if(NOT FPP_EXE)
    message (FATAL_ERROR "C preprocessor was not found")
endif()

# resolve preprocessor options
if(WIN32)
    set(FPP_OPTIONS /E)
    set(FPP_SWITCH /)
else()
    set(FPP_OPTIONS -E)
    set(FPP_SWITCH -)
endif()

# expose the preprocessor and the options through the FPP_OPTIONS variables
set(FPP_OPTIONS ${FPP_OPTIONS} CACHE STRING "preprocessor options")
set(FPP_SWITCH ${FPP_SWITCH} CACHE STRING "preprocessor switch")

set(file2fpp "${CMAKE_CURRENT_SOURCE_DIR}/assertions_gtest.fpp")
set(file2 "${CMAKE_CURRENT_BINARY_DIR}/assertions_gtest.f90")

F90TWFPP("${file2fpp}" "${file2}"
    EXE ${FPP_EXE}
    OPTIONS ${FPP_OPTIONS}
    SWITCH ${FPP_SWITCH}
)

# Create a custom target to ensure the file generation completes before library compilation
add_custom_target(generate_assertions_gtest_f90 DEPENDS "${file2}")

add_library(f90twi_gtest OBJECT "${file2}")
add_dependencies(f90twi_gtest generate_assertions_gtest_f90)

set_property(TARGET f90twi_gtest
        APPEND PROPERTY ADDITIONAL_CLEAN_FILES "${file2}"
)
set_target_properties(generate_assertions_gtest_f90 PROPERTIES FOLDER third_party_open/googletest)

add_library(f90tw_gtest STATIC gtest_assertions.cpp)
set_property(TARGET f90tw_gtest PROPERTY POSITION_INDEPENDENT_CODE ON)
target_include_directories(f90tw_gtest PUBLIC ${gtest_INCLUDE_DIR})
target_link_libraries(f90tw_gtest PUBLIC f90twi_gtest gtest)

# make available the include and module paths
set(f90tw_INCLUDE_DIR ${CMAKE_CURRENT_SOURCE_DIR} CACHE STRING "f90tw include directory")

include(GoogleTest)
