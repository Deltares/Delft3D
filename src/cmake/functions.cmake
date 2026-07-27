# strip_boost_header_only_deps
# Boost's CMake config conservatively declares compile-time header
# dependencies (e.g. thread -> atomic, chrono, container) as
# INTERFACE_LINK_LIBRARIES. These are not actual runtime DLL dependencies
# (verified via dumpbin /dependents), but CMake's TARGET_RUNTIME_DLLS
# walks INTERFACE_LINK_LIBRARIES and picks them up anyway. This function
# strips those header-only transitive deps from the given Boost targets
# so the corresponding DLLs don't get installed.
#
# Arguments
#   TARGETS       : List of Boost targets to clean (e.g. Boost::thread Boost::log)
#   EXCLUDE_DEPS  : List of Boost targets to remove from INTERFACE_LINK_LIBRARIES
function(strip_boost_header_only_deps)
    cmake_parse_arguments("" "" "" "TARGETS;EXCLUDE_DEPS" ${ARGN})
    foreach(_target IN LISTS _TARGETS)
        if(TARGET ${_target})
            get_target_property(_libs ${_target} INTERFACE_LINK_LIBRARIES)
            if(_libs)
                foreach(_dep IN LISTS _EXCLUDE_DEPS)
                    list(REMOVE_ITEM _libs ${_dep})
                endforeach()
                set_target_properties(${_target} PROPERTIES INTERFACE_LINK_LIBRARIES "${_libs}")
            endif()
        endif()
    endforeach()
endfunction()

# create_target
# Creates a target (library or executable) of a certain module
#
# Argument
# target_name : The name of the target to be created.
# source_group_name : The name of the root folder to group the source files in.
# src_dir: [Optional] directory where the source files exist. One of the two parameters (src_dir/src_files) should be
#                     given  otherwise an error will be raised. If both parameters (src_dir/src_files) are given they
#                     will be considered.
# target_type: [Optional] library/executable if not given a library will be created
# language: [Optional] language name, use this parameter when you need to ensure the linkage of the files to a
#           certain language. On Windows this is often not needed since the compiler will detect the language
#           from the file extensions, but on Linux sometimes the language has to be defined explicitly.
#           possible values are : C, CXX, Fortran
# shared : [Optional] True if the library is SHARED library, False if not.
# recursive: [Optional]  TRUE/FALSE to search for source file recursively or not, Default is False.
# src_files: [Optional]  List of source files to be added to the target.
# resource_files: [Optional] list of the version and the icon files. resource_files are different than the other
#                 src_files in that they are grouped in a separate folder in Visual Studio IDE.
#
#
# Example:
# set(src kernel/calculations)
# create_target(${target_name} ${CMAKE_CURRENT_SOURCE_DIR} src_dir src target_type "library" recursive True)
# create_target(${target_name} ${CMAKE_CURRENT_SOURCE_DIR} src_dir src)
function(create_target target_name source_group_name)
    # Set default values for optional parameters
    set(options shared recursive) # For options without values
    set(one_value_args src_dir target_type language) # For options with one value
    set(multi_value_args resource_files src_files) # For options with multiple values

    # Parse the arguments
    cmake_parse_arguments("op" "${options}" "${one_value_args}" "${multi_value_args}" ${ARGN})

    # Set default values if not provided
    if(NOT DEFINED op_target_type)
        set(op_target_type "library")
    endif()

    if(NOT DEFINED op_src_dir AND NOT DEFINED op_src_files )
        message(FATAL_ERROR "Error: Either src_dir or src_files must be defined")
    endif()

    if (op_src_dir)
        # if src_dir is defined
        if (op_recursive)
            # if src_dir is defined and the recursive argument is given
            get_fortran_source_files_recursive(${op_src_dir} source)
        else()
            # if src_dir is defined and the recursive argument is not given
            get_fortran_source_files(${op_src_dir} source)
        endif()
    endif()

    # combine all the given files (if any of the parameters is given)
    # Separate .rc files from other resource files (e.g. .F90 version files).
    # RC files are compiled in a separate object library to avoid resource
    # compiler command line length issues with include directories.
    set(rc_sources "")
    set(non_rc_resources "")
    if(DEFINED op_resource_files)
        foreach(f IN LISTS op_resource_files)
            if(f MATCHES "\\.rc$")
                list(APPEND rc_sources ${f})
            else()
                list(APPEND non_rc_resources ${f})
            endif()
        endforeach()
    endif()

    set(all_source ${op_src_files} ${non_rc_resources} ${source})

    if(${op_target_type} STREQUAL "library")
        if (op_shared)
            add_library(${target_name} SHARED ${all_source})
        else()
            add_library(${target_name} ${all_source})
        endif()
    else()
        # executable
        add_executable(${target_name} ${all_source})
    endif()

    if(rc_sources)
        add_rc_object_library(${target_name} "${rc_sources}" "${version_include_dir}")
        target_link_libraries(${target_name} PRIVATE ${target_name}_rc)
    endif()
    # Set the language of the target.
    if(UNIX)
        if(op_language STREQUAL "C")
            set_property(TARGET ${target_name} PROPERTY LINKER_LANGUAGE C)
        elseif(op_language STREQUAL "CXX")
            set_property(TARGET ${target_name} PROPERTY LINKER_LANGUAGE CXX)
        else()
            # if the language is not defined, set it to Fortran
            set_property(TARGET ${target_name} PROPERTY LINKER_LANGUAGE Fortran)
        endif()
    endif(UNIX)

    # Create the folder structure in visual studio IDE
    if (DEFINED op_resource_files)
        source_group(Resources FILES ${op_resource_files})
    endif()

    if (DEFINED op_src_dir OR DEFINED op_src_files)
        source_group(TREE ${source_group_name} FILES ${source} ${op_src_files})
    endif()

endfunction()

# add_rc_object_library
# Creates a separate OBJECT library for .rc resource files, compiled as C with
# only the specified include directories. This prevents the resource compiler
# command line from exceeding the Windows length limit when many transitive
# include paths (e.g. from Conan packages) are inherited by the main target.
# The resulting object library should be linked into the main target.
#
# Arguments:
#   target_name      : The name of the main target (used to derive the object library name).
#   rc_files         : List of .rc files to compile.
#   include_dirs     : Include directories the .rc files actually need.
#
# Usage:
#   add_rc_object_library(my_target "${rc_version_file}" "${version_include_dir}")
#   target_link_libraries(my_target PRIVATE my_target_rc)
function(add_rc_object_library target_name rc_files include_dirs)
    set(rc_lib_name ${target_name}_rc)
    if(WIN32)
        add_library(${rc_lib_name} OBJECT ${rc_files})
        set_target_properties(${rc_lib_name} PROPERTIES LINKER_LANGUAGE C)
        target_include_directories(${rc_lib_name} PRIVATE ${include_dirs})
        set_target_properties(${rc_lib_name} PROPERTIES FOLDER "rc_objects")
    else()
        add_library(${rc_lib_name} INTERFACE)
    endif()
endfunction()

# Create template for Visual Studio environment paths for debugging on Windows
function(create_vs_user_files)
    cmake_path(CONVERT "${CMAKE_INSTALL_PREFIX}/bin/$(TargetName).exe" TO_NATIVE_PATH_LIST debugcommand)
    cmake_path(CONVERT "${CMAKE_INSTALL_PREFIX}/share/" TO_NATIVE_PATH_LIST path_prefix)
    set(envpath "PATH=${path_prefix};%PATH%")
    set(userfilename "${CMAKE_BINARY_DIR}/template.vfproj.user")
    file(
        WRITE "${userfilename}"
"<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<VisualStudioUserFile>
	<Configurations>
		<Configuration Name=\"Debug|x64\" Command=\"${debugcommand}\" Environment=\"${envpath}\"/>
		<Configuration Name=\"Release|x64\" Command=\"${debugcommand}\" Environment=\"${envpath}\"/>
		<Configuration Name=\"RelWithDebInfo|x64\" Command=\"${debugcommand}\" Environment=\"${envpath}\"/>
    </Configurations>
</VisualStudioUserFile>"
)
	set (userfilename "${CMAKE_BINARY_DIR}/template.vcxproj.user")
    file(
        WRITE "${userfilename}"
"<?xml version=\"1.0\" encoding=\"utf-8\"?>
<Project ToolsVersion=\"15.0\" xmlns=\"http://schemas.microsoft.com/developer/msbuild/2003\">
    <PropertyGroup>
        <LocalDebuggerEnvironment>${envpath}</LocalDebuggerEnvironment>
        <LocalDebuggerCommand>${debugcommand}</LocalDebuggerCommand>
    </PropertyGroup>
</Project>"
    )
endfunction()


# Set environment paths for Visual Studio debugger on Windows
function(configure_visual_studio_user_file executable_name)
    if (CMAKE_GENERATOR MATCHES "Visual Studio" AND NOT EXISTS "${CMAKE_CURRENT_BINARY_DIR}/${executable_name}.vfproj.$ENV{USERNAME}.user")
        configure_file(
            "${CMAKE_BINARY_DIR}/template.vfproj.user"
            "${CMAKE_CURRENT_BINARY_DIR}/${executable_name}.vfproj.$ENV{USERNAME}.user"
            @ONLY
        )
    endif()
endfunction()

# get_fortran_source_files
# Gathers Fortran *.f or *.f90 files from a given directory.
#
# Argument
# source_directory : The directory to gather the source files from.
#
# Return
# source_files : The source files that were gathered.
function(get_fortran_source_files source_directory source_files)
    file(GLOB source    ${source_directory}/*.f90
                        ${source_directory}/*.F90
                        ${source_directory}/*.for
                        ${source_directory}/*.f
                        ${source_directory}/*.F
                        ${source_directory}/*.inc)
    set(${source_files} ${source} PARENT_SCOPE)
endfunction()
# get_fortran_source_files_recursive
# Gathers Fortran *.f or *.f90 files from a given directory recurcivly.
#
# Argument
# source_directory : The directory to gather the source files from.
#
# Return
# source_files : The source files that were gathered.
function(get_fortran_source_files_recursive source_directory source_files)
    file(GLOB_RECURSE source ${source_directory}/*.f90
                        ${source_directory}/*.F90
                        ${source_directory}/*.for
                        ${source_directory}/*.f
                        ${source_directory}/*.F)
    set(${source_files} ${source} PARENT_SCOPE)
endfunction()

# get_module_include_path
# Gets the include directory of a module. Will throw an exception if there is no value for the property public_include_path.
#
# Argument
# module_path           : The path of the module to retrieve the public_include_path property for.
# library_name          : The name of the library to retrieve the include directory of a module for.
#
# Return
# return_include_path   : The value of the include_path property for the module_path.
function(get_module_include_path module_path library_name return_include_path)
    get_directory_property(public_include_path  DIRECTORY ${module_path}
                                                DEFINITION public_include_path)

    if(NOT public_include_path)
        message(FATAL_ERROR "Parameter 'public_include_path' not defined for the module in ${module_path}: Path should define a value for property 'public_include_path'.")
    endif()

    set(${return_include_path} ${public_include_path} PARENT_SCOPE)
endfunction()

# configure_package_installer
# Configures a package for installing.
#
# Argument
# name              : The name of the package.
# description_file  : The file containing the description of the package.
# major             : The major version nr.
# minor             : The minor version nr.
# build             : The build version nr.
# generator         : The generators to be used to build the package, seperated by ';'.
function(configure_package_installer name description_file  major minor build generator)
  set(CPACK_VERBATIM_VARIABLES YES)
  set(CPACK_INCLUDE_TOPLEVEL_DIRECTORY OFF)
  set(CPACK_PACKAGE_DESCRIPTION_SUMMARY "${name}")
  set(CPACK_PACKAGE_VENDOR "Deltares 2026")
  set(CPACK_PACKAGE_DESCRIPTION_FILE "${description_file}")
  set(CPACK_RESOURCE_FILE_LICENSE "${checkout_src_root}/Copyright.txt")
  set(CPACK_PACKAGE_VERSION_MAJOR "${major}")
  set(CPACK_PACKAGE_VERSION_MINOR "${minor}")
  set(CPACK_PACKAGE_VERSION_PATCH "${build}")
  set(CPACK_GENERATOR "${generator}")
  include(CPack)
endfunction(configure_package_installer)

# set_rpath
# Find all binaries in "targetDir" and set rpath to "rpathValue" in these binaries
# This function is called from the "install_and_bundle.cmake" files
#
# Arguments
# targetDir         : Name of the directory to search for binaries whose rpath needs to be set
# rpathValue        : Value to which rpath needs to be set
function(set_rpath targetDir rpathValue)
  execute_process(COMMAND find "${targetDir}" -type f -exec bash -c "patchelf --set-rpath '${rpathValue}' $1" _ {} \; -exec echo "patched rpath of: " {} \;)
endfunction(set_rpath)

# Use the `create_test` cmake function to create a unit test by providing the following arguments.
# test_name:
#    The test name
# dependencies: [separate multiple values/ list]
#    The dependencies of the test
# visual_studio_folder: [one value/optional]
#    argument defines the folder in which the test will be located in the visual studio solution.
# test_files: [separate multiple values/ list]
#    argument defines the source files that will be compiled to create the test.
# include_dir: [separate multiple values/ list]
#    argument defines the directory that contains the files that are needed for the test.
#    The `include_dir` argument is optional, if the test does not depend on external data, do not provide the argument.
# test_list: [separate multiple values/ list]
#   if you have one fortran file that contains multiple tests, and you want to execute each test separately, you have to
#    implement
#
#   >>>   if (iargc > 0) then
#   >>>     call get_command_argument(1, cmd_arg)
#   >>>
#   >>>      select case (trim(cmd_arg))
#   >>>      case('test_1_name')
#   >>>           write(*,*) "Running test_1_name"
#   >>>           call runtests(test_1_subroutine)
#   >>>       case ('test_2_name')
#   >>>           write(*,*) "Running test_2_subroutine"
#   >>>           call runtests(test_2_subroutine)
#   >>>       case ('test_3_name')
#   >>>            write(*,*) "Running test_3_subroutine"
#   >>>            call runtests(test_3_subroutine)
#   >>>        end select
#   >>>   else
#   >>>        write(*,*) "No test specified, running all tests"
#   >>>        call runtests(test_1_subroutine)
#   >>>        call runtests(test_2_subroutine)
#   >>>        call runtests(test_3_subroutine)
#   >>>   end if
#   >>>   ! then write the test_1_subroutine, test_2_subroutine, test_3_subroutine
# labels: [separate multiple values/ list]
#    argument defines the labels that will be added to the test.
#    >>> labels "test_1:fast" "test_2:medium" "test_3:e2e"
# Examples:
# create_test(
#   test_name
#   dependencies ftnunit
#   visual_studio_folder "tests"
#   test_files test_file_1.f90 test_file_2.f90
#   include_dir ${CMAKE_CURRENT_SOURCE_DIR}/test_data
#   test_list test_1 test_2 test_3
#   labels "test_1:fast" "test_2:medium" "test_3:e2e"
#)
function(create_test test_name)

    # For options without values
    set(options)
    # For options with one value
    set(one_value_args visual_studio_folder)
    # For options with multiple values
    set(multi_value_args dependencies test_files include_dir test_list labels)

    # set expression to check for failed tests
    set(fail_reg_ex "Condition.*failed;Values not comparable;[A|a]ssertion.*failed")

    # Parse the arguments
    cmake_parse_arguments("op" "${options}" "${one_value_args}" "${multi_value_args}" ${ARGN})

    create_target(
            ${test_name}
            ${CMAKE_CURRENT_SOURCE_DIR}
            src_files ${op_test_files}
            target_type "executable"
    )
    # Set environment paths to find *.so/*.dll files Make sure DLL is found by adding its directory to PATH
    if (UNIX)
        set(lib_path "LD_LIBRARY_PATH=${CMAKE_INSTALL_PREFIX}/lib:$ENV{LD_LIBRARY_PATH}")
    endif (UNIX)
    if (WIN32)
        set(lib_path "PATH=${CMAKE_INSTALL_PREFIX}/bin\;$ENV{PATH}")
    endif (WIN32)


    # Link libraries, include ftnunit in dependencies
    set(op_dependencies ftnunit ${op_dependencies})
    target_link_libraries(${test_name} PRIVATE ${op_dependencies})

    # Other link libraries
    if (WIN32)
        target_link_directories(
                ${test_name} PRIVATE
                ${mpi_library_path}
                ${checkout_src_root}/third_party_open/pthreads/bin/x64
        )
        set_target_properties(${test_name} PROPERTIES FOLDER ${op_visual_studio_folder})
    endif(WIN32)

    # Obtain name of test, irrespective of whether a single test or a list is given
    set(tests_to_set ${test_name})
    if(DEFINED op_test_list)
        set(tests_to_set ${op_test_list})
    endif()

    # add labels to tests
    if (DEFINED op_labels)
        set(labels "")
        # convert the labels list to a dictionary
        list(LENGTH op_labels labels_len)

        foreach(pair IN LISTS op_labels)
            string(REPLACE ":" ";" pair_list ${pair})
            list(GET pair_list 0 test_i)
                list(GET pair_list 1 label)
                list(APPEND labels ${label})
        endforeach()
    endif()

    set(TEST_DATA_PATH ${CMAKE_CURRENT_BINARY_DIR}/test_data)

    foreach(test_i IN LISTS tests_to_set)
        if(tests_to_set STREQUAL ${test_name})
            add_test(NAME ${test_i} COMMAND ${test_name})
        else()
            add_test(NAME ${test_i} COMMAND ${test_name} ${test_i})
        endif()

        set_property(TEST ${test_i} PROPERTY FAIL_REGULAR_EXPRESSION ${fail_reg_ex})

        if (DEFINED op_include_dir)
            # Copy an entire directory
            file(COPY ${op_include_dir} DESTINATION ${TEST_DATA_PATH})
        endif()
        # Set data path environmental variable
        set(data_path "DATA_PATH=${TEST_DATA_PATH}")

        set_tests_properties(${test_i} PROPERTIES
            ENVIRONMENT "${lib_path};${data_path}"
            LABELS "${labels}"
        )
    endforeach()

endfunction()
