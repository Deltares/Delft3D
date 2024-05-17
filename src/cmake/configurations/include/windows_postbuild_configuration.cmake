project(windows_postbuild)

if(WIN32)
    #intel MPI & MKL
    if(NOT TARGET intelredist)
        add_subdirectory(${CMAKE_SOURCE_DIR}/src/${intelredist_module} intelredist)
    endif()
    #TECPLOT
    if(NOT TARGET Tecplot)
        add_subdirectory(${CMAKE_SOURCE_DIR}/src/${Tecplot_module} Tecplot)
    endif()

    if(NOT TARGET GISInternals)
        add_subdirectory(${CMAKE_SOURCE_DIR}/src/${GISInternals_module} GISInternals)
    endif()

    if(NOT TARGET pthreads)
        add_subdirectory(${CMAKE_SOURCE_DIR}/src/${pthreads_module} pthreads)
    endif()
endif(WIN32)