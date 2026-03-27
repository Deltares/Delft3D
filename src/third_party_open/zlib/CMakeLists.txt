if (WIN32)
    # Register zlib as a proper SHARED IMPORTED target so CMake's
    # TARGET_RUNTIME_DLLS machinery propagates it automatically to dependents,
    # exactly as is done for PETSc in petsc/cmake_deltares/CMakeLists.txt.
    if(NOT TARGET zlib)
        add_library(zlib SHARED IMPORTED GLOBAL)
        set_target_properties(zlib PROPERTIES
            IMPORTED_LOCATION "${checkout_src_root}/${zlib_module}/bin/zlib.dll"
            IMPORTED_IMPLIB   "${checkout_src_root}/${zlib_module}/lib/zlib.lib"
            INTERFACE_INCLUDE_DIRECTORIES "${checkout_src_root}/${zlib_module}/include"
        )
    endif()

    install(FILES "${checkout_src_root}/${zlib_module}/bin/zlib.dll" DESTINATION lib)
else()
    add_custom_target(zlib)
    set_target_properties(zlib PROPERTIES FOLDER third_party_open)
endif()
