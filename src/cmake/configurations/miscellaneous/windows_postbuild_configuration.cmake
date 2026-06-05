project(windows_postbuild)

if(WIN32)
    if(NOT TARGET pthreads)
        add_subdirectory(${checkout_src_root}/${pthreads_module} pthreads)
    endif()
endif(WIN32)

if(WIN32)
   if (NOT TARGET microsoftredist)
      add_subdirectory(${checkout_src_root}/${microsoftredist_module} microsoftredist)
   endif()
endif(WIN32)