add_custom_target(hdf5)
set_target_properties(hdf5 PROPERTIES FOLDER third_party_open)
if (WIN32)
   install(FILES
   ${CMAKE_CURRENT_SOURCE_DIR}/bin/hdf5.dll
   ${CMAKE_CURRENT_SOURCE_DIR}/bin/hdf5_hl.dll
   DESTINATION lib)
endif(WIN32)
