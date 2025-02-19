if(NOT DEFINED COMMIT_VERSION)
        set(COMMIT_VERSION "0")
endif()
find_program(ESMF_REGRIDWEIGHTGEN_EXECUTABLE ESMF_RegridWeightGen REQUIRED)

install(PROGRAMS ${ESMF_REGRIDWEIGHTGEN_EXECUTABLE} DESTINATION bin)
install(SCRIPT "${checkout_src_root}/cmake/install_wave/install_and_bundle.cmake")
configure_package_installer("DWAVES" "${checkout_src_root}/cmake/install_wave/ReadMe.txt" "1" "0" "${COMMIT_VERSION}" "STGZ")
