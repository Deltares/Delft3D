if (WIN32)
    if(NOT TARGET hdf5)
        add_library(hdf5 SHARED IMPORTED GLOBAL)
        set_target_properties(hdf5 PROPERTIES
            IMPORTED_LOCATION "${checkout_src_root}/${hdf5_module}/bin/hdf5.dll"
            IMPORTED_IMPLIB   "${checkout_src_root}/${hdf5_module}/lib/hdf5.lib"
            INTERFACE_INCLUDE_DIRECTORIES "${checkout_src_root}/${hdf5_module}/include"
        )
    endif()

    if(NOT TARGET hdf5_hl)
        add_library(hdf5_hl SHARED IMPORTED GLOBAL)
        set_target_properties(hdf5_hl PROPERTIES
            IMPORTED_LOCATION "${checkout_src_root}/${hdf5_module}/bin/hdf5_hl.dll"
            IMPORTED_IMPLIB   "${checkout_src_root}/${hdf5_module}/lib/hdf5_hl.lib"
            INTERFACE_INCLUDE_DIRECTORIES "${checkout_src_root}/${hdf5_module}/include"
        )
    endif()

    install(FILES
        "${checkout_src_root}/${hdf5_module}/bin/hdf5.dll"
        "${checkout_src_root}/${hdf5_module}/bin/hdf5_hl.dll"
        DESTINATION lib
    )
else()
    add_custom_target(hdf5)
    set_target_properties(hdf5 PROPERTIES FOLDER third_party_open)
endif()
