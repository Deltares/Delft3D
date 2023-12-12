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
    set(all_source ${op_src_files} ${op_resource_files} ${source})

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


# oss_include_libraries
# Adds oss dependencies to the specified library.
#
# Note that it is assumed that the dependency is located in the PROJECT_BINARY_DIR in a subdirectory with the same dependency name.
#
# Argument
# library_name : The name of the library where dependencies should be added.
# dependencies : A list of dependencies to set for the library_name.
function(oss_include_libraries library_name dependencies)

    foreach(dependency IN LISTS ${dependencies})
        add_dependencies(${library_name} ${dependency})

        if (NOT CMAKE_GENERATOR MATCHES "Visual Studio")
            include_directories( ${PROJECT_BINARY_DIR}/${dependency} )
        endif()
    endforeach()

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
                        ${source_directory}/*.F)
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
    file(GLOB_RECURSE source ${source_directory} *.f90
                        ${source_directory} *.F90
                        ${source_directory} *.for
                        ${source_directory} *.f
                        ${source_directory} *.F)
    set(${source_files} ${source} PARENT_SCOPE)
endfunction()

# add_postbuild_event
# Adds a postbuild event to the target.
#
# Argument
# target_name : The name of the target to add this postbuild event to.
function(add_postbuild_event target_name)
    # Perform a precheck to determine if the post build event script is defined.
    IF(NOT DEFINED postbuild_event_path)
        message(FATAL_ERROR "Variable 'postbuild_event_path' is undefined.")
    ENDIF()

    message(STATUS "Adding postbuild event step for ${target_name}")
    if (UNIX)
       execute_process(COMMAND /bin/bash ${postbuild_event_path})
    endif(UNIX)

    if (WIN32)
       IF(DEFINED ARGV5 AND ARGV5)
          add_custom_command( TARGET ${target_name}
                              POST_BUILD
                              COMMAND  call ${postbuild_event_path})
        ELSE()
           add_custom_command( TARGET ${target_name}
                               POST_BUILD
                               COMMAND  call ${postbuild_event_path})
        ENDIF()
    endif()
endfunction()



# Executes the post_build steps for a given target
#
# Arguments
# target_name         : The name of the target
# install_dir         : The directory where to copy the binaries
# build_dir           : The directory where to copy the binaries
# checkout_src_root   : The checkout directory
# build_project       : The name of the project
function(post_build_target target_name install_dir build_dir checkout_src_root build_project)

   if (CMAKE_GENERATOR MATCHES "Visual Studio")

    # compiler_redist_dir : Compiler dlls (Windows only)
    # mkl_redist_dir      : mkl dlls (Windows only)

      if (DEFINED ENV{ONEAPI_ROOT})
         set(oneapi_root $ENV{ONEAPI_ROOT})
         set(compiler_redist_dir "${oneapi_root}/compiler/latest/windows/redist/intel64_win/compiler/")
         set(mkl_redist_dir   "${oneapi_root}/mkl/latest/redist/intel64/")
         set(mpi_redist_dir "${oneapi_root}/mpi/latest/")
      else()
         set(compiler_redist_dir "C:/Program Files (x86)/IntelSWTools/compilers_and_libraries/windows/redist/intel64_win/compiler/")
         set(mkl_redist_dir   "C:/Program Files (x86)/IntelSWTools/compilers_and_libraries/windows/redist/intel64_win/mkl/")
      endif()

      set(build_config $<CONFIG>)

      add_custom_command(TARGET ${target_name}
                         POST_BUILD
                         COMMAND call "${checkout_src_root}/scripts_lgpl/win64/oss-post_build.cmd"
                         ${install_dir}
                         ${build_dir}
                         ${checkout_src_root}
                         ${build_config}
                         ${build_project}
                         ${compiler_redist_dir}
                         ${mkl_redist_dir}
                         ${mpi_redist_dir})
   endif(CMAKE_GENERATOR MATCHES "Visual Studio")

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
# mayor             : The mayor version nr.
# minor             : The minor version nr.
# build             : The build version nr.
# generator         : The generators to be used to build the package, seperated by ';'.
function(configure_package_installer name description_file  mayor minor build generator)
  set(CPACK_VERBATIM_VARIABLES YES)
  set(CPACK_INCLUDE_TOPLEVEL_DIRECTORY OFF)
  set(CPACK_PACKAGE_DESCRIPTION_SUMMARY "${name}")
  set(CPACK_PACKAGE_VENDOR "Deltares 2021")
  set(CPACK_PACKAGE_DESCRIPTION_FILE "${description_file}")
  set(CPACK_RESOURCE_FILE_LICENSE "${checkout_src_root}/Copyright.txt")
  set(CPACK_PACKAGE_VERSION_MAJOR "${mayor}")
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
#           The test name
# dependencies: [separate multiple values/ list]
#           The dependencies of the test
# visual_studio_folder: [one value/optional]
#           argument defines the folder in which the test will be located in the visual studio solution.
# test_files: [separate multiple values/ list]
#            argument defines the source files that will be compiled to create the test.
# include_dir: [separate multiple values/ list]
#           argument defines the directory that contains the files that are needed for the test. 
#           The `include_dir` argument is optional, if the test does not depend on external data, do not provide the argument. 
function(create_test test_name)

    set(options) # For options without values
    set(one_value_args visual_studio_folder) # For options with one value
    set(multi_value_args dependencies test_files include_dir) # For options with multiple values

    # Parse the arguments
    cmake_parse_arguments("op" "${options}" "${one_value_args}" "${multi_value_args}" ${ARGN})

    create_target(
            ${test_name}
            ${CMAKE_CURRENT_SOURCE_DIR}
            src_files ${op_test_files}
            target_type "executable"
    )

    # add the ftnunit to the dependencies.
    set(op_dependencies ftnunit ${op_dependencies})

    # Link libraries
    target_link_libraries(${test_name} ${op_dependencies})
    set_property(TARGET ${test_name} PROPERTY LINKER_LANGUAGE Fortran)

    # Other link libraries
    if (WIN32)
        target_link_directories(
                ${test_name} PRIVATE
                ${mpi_library_path}
                ${checkout_src_root}/third_party_open/pthreads/bin/x64
        )
    endif(WIN32)

    set_target_properties(${test_name} PROPERTIES FOLDER ${op_visual_studio_folder})

    add_test(NAME ${test_name} COMMAND ${test_name})

    if (DEFINED op_include_dir)
        # Copy an entire directory
        file(COPY ${op_include_dir} DESTINATION ${CMAKE_BINARY_DIR}/test_data/${test_name})
        set_tests_properties(${test_name} PROPERTIES ENVIRONMENT DATA_PATH=${CMAKE_BINARY_DIR}/test_data/${test_name}
        )        
    endif()
endfunction()
