# create_library
# Creates a library of a certain module
#
# Argument
# library_name : The name of the library to create.
# source_group_name : The name of the root folder to group the source files in.
# source_directory: directory where the source files exist.
function(create_library library_name source_group_name source_directory)
    get_fortran_source_files(${source_directory} source)
    add_library(${library_name} ${source})
    # Create the folder structure in visual studio ide
    source_group(TREE ${source_group_name} FILES ${source})
endfunction()


# create_library_recursive
# Creates a library of a certain module by finding the source files recursively in a given source_directory.
#
# Argument
# library_name : The name of the library to create.
# source_group_name : The name of the root folder to group the source files in.
# source_directory: directory where the source files exist.
function(create_library_recursive library_name source_group_name source_directory)
    get_fortran_source_files_recursive(${source_directory} source)
    add_library(${library_name} ${source})
    # Create the folder structure in visual studio ide
    source_group(TREE ${source_group_name} FILES ${source})
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
