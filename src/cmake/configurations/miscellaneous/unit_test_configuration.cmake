set(f90tw_module third_party_open/f90tw)

if (NOT TARGET f90tw_gtest)
    add_subdirectory(${checkout_src_root}/${f90tw_module} f90tw)
endif()
