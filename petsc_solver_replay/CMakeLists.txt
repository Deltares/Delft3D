cmake_minimum_required(VERSION 3.25)

project(petsc_solver_replay VERSION 0.1.0 LANGUAGES CXX)

set(CMAKE_COMPILE_WARNING_AS_ERROR ON)

find_package(PETSc CONFIG REQUIRED)

add_library(replay-mpi INTERFACE)
if(WIN32)
    if(NOT DEFINED ENV{ONEAPI_ROOT})
        message(FATAL_ERROR "ONEAPI_ROOT must be set to locate Intel MPI")
    endif()
    target_include_directories(replay-mpi INTERFACE "$ENV{ONEAPI_ROOT}/mpi/latest/include")
    target_link_directories(replay-mpi INTERFACE "$ENV{ONEAPI_ROOT}/mpi/latest/lib")
    target_link_libraries(replay-mpi INTERFACE impi.lib impicxx.lib)
else()
    find_package(MPI REQUIRED COMPONENTS CXX)
    target_link_libraries(replay-mpi INTERFACE MPI::MPI_CXX)
endif()

add_executable(petsc-solver-replay src/main.cpp)
set_property(TARGET petsc-solver-replay PROPERTY CXX_STANDARD 23)
set_property(TARGET petsc-solver-replay PROPERTY CXX_STANDARD_REQUIRED ON)
set_property(TARGET petsc-solver-replay PROPERTY CXX_EXTENSIONS OFF)
set_property(TARGET petsc-solver-replay PROPERTY INSTALL_RPATH "$ORIGIN/../lib")
target_link_libraries(petsc-solver-replay PRIVATE PETSc::PETSc replay-mpi)

install(TARGETS petsc-solver-replay RUNTIME DESTINATION bin)
foreach(config IN ITEMS Debug Release RelWithDebInfo MinSizeRel)
    string(TOUPPER "${config}" config_upper)
    if(TARGET CONAN_LIB::petsc_libpetsc_${config_upper})
        install(IMPORTED_RUNTIME_ARTIFACTS CONAN_LIB::petsc_libpetsc_${config_upper}
            CONFIGURATIONS ${config}
            RUNTIME DESTINATION bin
            LIBRARY DESTINATION lib
        )
    endif()
endforeach()

if(MSVC)
    target_compile_options(petsc-solver-replay PRIVATE /W4 /permissive- /Zc:preprocessor)
else()
    target_compile_options(petsc-solver-replay PRIVATE -Wall -Wextra -Wpedantic)
endif()

include(CTest)
if(BUILD_TESTING)
    add_executable(petsc-replay-create-test-dump tests/create_test_dump.cpp)
    set_property(TARGET petsc-replay-create-test-dump PROPERTY CXX_STANDARD 23)
    set_property(TARGET petsc-replay-create-test-dump PROPERTY CXX_STANDARD_REQUIRED ON)
    set_property(TARGET petsc-replay-create-test-dump PROPERTY CXX_EXTENSIONS OFF)
    target_link_libraries(petsc-replay-create-test-dump PRIVATE PETSc::PETSc replay-mpi)
    if(MSVC)
        target_compile_options(petsc-replay-create-test-dump PRIVATE /Zc:preprocessor)
    endif()

    set(test_dump_1 "${CMAKE_CURRENT_BINARY_DIR}/petsc_replay_test_00000001.bin")
    set(test_dump_2 "${CMAKE_CURRENT_BINARY_DIR}/petsc_replay_test_00000002.bin")
    if(WIN32)
        set(replay_mpiexec "$ENV{ONEAPI_ROOT}/mpi/latest/bin/mpiexec.exe")
    else()
        set(replay_mpiexec "${MPIEXEC_EXECUTABLE}")
    endif()
    if(EXISTS "${replay_mpiexec}")
        set(create_metadata_dump_command ${replay_mpiexec} -n 2 $<TARGET_FILE:petsc-replay-create-test-dump>)
    else()
        set(create_metadata_dump_command petsc-replay-create-test-dump)
    endif()
    add_test(
        NAME petsc-replay-create-test-dump-1
        COMMAND ${create_metadata_dump_command}
            -output_file ${test_dump_1}
            -metadata_prefix ${CMAKE_CURRENT_BINARY_DIR}/petsc_replay_test
    )
    add_test(
        NAME petsc-replay-create-test-dump-2
        COMMAND petsc-replay-create-test-dump -output_file ${test_dump_2}
    )
    set_tests_properties(petsc-replay-create-test-dump-2 PROPERTIES DEPENDS petsc-replay-create-test-dump-1)
    add_test(
        NAME petsc-solver-replay-smoke
        COMMAND petsc-solver-replay
                -replay_file ${CMAKE_CURRENT_BINARY_DIR}/petsc_replay_test
                -replay_rebuild_preconditioner 0
                -replay_fail_on_reference_error true
                -replay_reference_rtol 1e-12
                -pc_type jacobi
    )
    set_tests_properties(
        petsc-solver-replay-smoke
        PROPERTIES
            DEPENDS "petsc-replay-create-test-dump-1;petsc-replay-create-test-dump-2"
            PASS_REGULAR_EXPRESSION "Replaying 2 recorded linear solve\\(s\\)"
    )

    if(EXISTS "${replay_mpiexec}")
        add_test(
            NAME petsc-solver-replay-repartition
            COMMAND ${replay_mpiexec} -n 2 $<TARGET_FILE:petsc-solver-replay>
                    -replay_file ${CMAKE_CURRENT_BINARY_DIR}/petsc_replay_test
                    -replay_node_owners ${CMAKE_CURRENT_BINARY_DIR}/petsc_replay_test_owner_ranks.bin
                    -replay_fail_on_reference_error true
                    -replay_reference_rtol 1e-12
                    -pc_type jacobi
        )
        set_tests_properties(
            petsc-solver-replay-repartition
            PROPERTIES
                DEPENDS "petsc-replay-create-test-dump-1;petsc-replay-create-test-dump-2"
                PASS_REGULAR_EXPRESSION "maximum relative reference error:"
        )
        if(WIN32)
            set_property(
                TEST petsc-solver-replay-repartition
                PROPERTY ENVIRONMENT_MODIFICATION
                    "PATH=path_list_prepend:$<$<CONFIG:Debug>:${petsc_BIN_DIRS_DEBUG}>$<$<CONFIG:Release>:${petsc_BIN_DIRS_RELEASE}>"
                    "PATH=path_list_prepend:$ENV{ONEAPI_ROOT}/mpi/latest/bin"
            )
        endif()
    endif()
    if(WIN32)
        set_property(
            TEST petsc-replay-create-test-dump-1 petsc-replay-create-test-dump-2 petsc-solver-replay-smoke
            PROPERTY ENVIRONMENT_MODIFICATION
                "PATH=path_list_prepend:$<$<CONFIG:Debug>:${petsc_BIN_DIRS_DEBUG}>$<$<CONFIG:Release>:${petsc_BIN_DIRS_RELEASE}>"
                "PATH=path_list_prepend:$ENV{ONEAPI_ROOT}/mpi/latest/bin"
        )
    endif()
endif()
