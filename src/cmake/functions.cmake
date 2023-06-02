# create_library
# Creates a library of a certain module with the assumption that all the Fortran source files are located within /src.
#
# Argument
# library_name : The name of the library to create.
# source_group_name : The name of the root folder to group the source files in.
function(create_library library_name source_group_name)
    file(GLOB source    src/*.f90
                        src/*.f
                        src/*.F90)
    add_library(${library_name} ${source})

    # Create the folder structure in vfproj
    source_group(${source_group_name} FILES ${source})
endfunction()

# generate_version_files
# Generates files that are related to the version numbering as a preconfiguration process of cmake.
#
# Argument
# fortran_version_file : The name of the Fortran version file it needs to generate.
# version_file : The .ini file which contains the version information format.
# --onlyifmissing : Optional boolean flag to determine if the version file should be generated. (default: OFF)
function(generate_version_files fortran_version_file version_file) 
    IF(NOT DEFINED update_version_script_path)
        message(FATAL_ERROR "Variable 'update_version_script_path' is undefined.")
    ENDIF()

    if (UNIX)
        message(STATUS "generate_version_files IN UNIX  ${update_version_script_path} ${fortran_version_file} ${checkout_src_root} ${version_file} ${checkout_src_root} ${CMAKE_BINARY_DIR}")
        execute_process(COMMAND /bin/bash ${update_version_script_path} ${fortran_version_file} ${checkout_src_root} ${version_file} ${checkout_src_root} ${CMAKE_BINARY_DIR})
    endif(UNIX)
    if (WIN32)
        message(STATUS "generate_version_files IN WINDOWS")
        IF(DEFINED ARGV3 AND ARGV3)
            execute_process(COMMAND ${update_version_script_path} ${fortran_version_file} ${checkout_src_root} ${version_file} --onlyifmissing)
        ELSE()
            execute_process(COMMAND ${update_version_script_path} ${fortran_version_file} ${checkout_src_root} ${version_file})
        ENDIF()
    endif(WIN32)

endfunction()

# prebuild_version_number
# Adds a version number as part of a prebuild event to the target library
#
# Argument
# library_name : The name of the library to add this prebuild event to.
# svn_version_path : Path to check for the svn version.
# fortran_version_file : The name of the Fortran version file it needs to generate.
# version_file : The .ini file which contains the version information format.
# --onlyifmissing : Optional boolean flag to determine if the version file should be generated. (default: OFF)
function(prebuild_version_number library_name fortran_version_file svn_version_path version_file)
    IF(NOT DEFINED update_version_script_path)
        message(FATAL_ERROR "Variable 'update_version_script_path' is undefined.")
    ENDIF()

    message(STATUS "Adding prebuild event step for ${library_name}")

    if (UNIX)
        #       if(EXISTS ${fortran_version_file})
        #           file(READ ${fortran_version_file} version_file_content)
        #           file(READ ${version_file} version_content)
        #           string(FIND "${version_file_content}" "${version_content}" versions_found)
        #
        #           message(STATUS ${versions_found})
        #
        #           if(${versions_found} GREATER_EQUAL 0)
        #               message(STATUS "file and version match found, returning because no update is needed")
        #               return()
        #           endif()
        #       endif()
        execute_process(COMMAND /bin/bash ${update_version_script_path} ${fortran_version_file} ${svn_version_path} ${version_file} ${checkout_src_root} ${CMAKE_BINARY_DIR})
    endif(UNIX)
    if (WIN32)
       IF(DEFINED ARGV4 AND ARGV4)
            add_custom_command( TARGET ${library_name}
                              PRE_BUILD
                              COMMAND  call ${update_version_script_path} ${fortran_version_file} ${svn_version_path} ${version_file} --onlyifmissing)
        ELSE()
           add_custom_command( TARGET ${library_name}
                               PRE_BUILD
                               COMMAND  call ${update_version_script_path} ${fortran_version_file} ${svn_version_path} ${version_file})
        ENDIF()
    endif(WIN32)
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
# UNST-6985: temporary fix to force Intel21 on machines that have multiple versions of Intel Fortran installed:
         set(compiler_redist_dir "${oneapi_root}/compiler/2021.2.0/windows/redist/intel64_win/compiler/")
         set(mkl_redist_dir   "${oneapi_root}/mkl/2021.2.0/redist/intel64/")   
         set(mpi_redist_dir "${oneapi_root}/mpi/2021.2.0/")
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

