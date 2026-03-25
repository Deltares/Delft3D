# ---------------------------------------------------------------------------
# interacter_utils_c  –  C layer: IAT patch for SendMessageA
# Interacter is Windows-only; provide an empty interface target on Linux.
# ---------------------------------------------------------------------------
set(c_library_name interacter_utils_c)

if(WIN32)
    add_library(${c_library_name} STATIC
        src/patch_user32_dll_sendmessage_for_interacter.c
    )
    target_link_libraries(${c_library_name} PRIVATE no_compiler_warnings)
else()
    add_library(${c_library_name} INTERFACE)
endif()

set_target_properties(${c_library_name} PROPERTIES FOLDER utils_lgpl/interacter_utils)

# ---------------------------------------------------------------------------
# interacter_utils  –  Fortran interface (iso_c_binding, no header needed)
# ---------------------------------------------------------------------------
set(library_name interacter_utils)

if(WIN32)
    add_library(${library_name} STATIC
        src/interacter_utils.f90
    )
    target_link_libraries(${library_name} PUBLIC ${c_library_name})
else()
    add_library(${library_name} INTERFACE)
    target_link_libraries(${library_name} INTERFACE ${c_library_name})
endif()

set_target_properties(${library_name} PROPERTIES FOLDER utils_lgpl/interacter_utils)

source_group(TREE ${CMAKE_CURRENT_SOURCE_DIR} FILES
    src/patch_sendmessage.c
    src/interacter_utils.f90
)