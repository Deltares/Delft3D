# Builds the dflowfm_io Python wheel.
#
# The native library is staged into src/dflowfm_io/_lib by the dflowfm_io_api POST_BUILD step, so the
# wheel produced here bundles it and is self-contained (`pip install` needs nothing else).
#
# The wheel is NOT part of the default build (it is slow and only needed when publishing). Build it
# on demand:
#     cmake --build <build-dir> --target dflowfm_io_wheel
# or include it in the default build by configuring with:
#     -D DFLOWFM_IO_BUILD_PYTHON_WHEEL=ON

option(
	DFLOWFM_IO_BUILD_PYTHON_WHEEL
	"Build the dflowfm_io Python wheel as part of the default build"
	OFF
)

set(DFLOWFM_IO_WHEEL_DIR "${CMAKE_CURRENT_BINARY_DIR}/wheel")

if(DFLOWFM_IO_BUILD_PYTHON_WHEEL)
	set(_dflowfm_io_wheel_in_all ALL)
else()
	set(_dflowfm_io_wheel_in_all "")
endif()

add_custom_target(dflowfm_io_wheel ${_dflowfm_io_wheel_in_all}
	COMMAND ${CMAKE_COMMAND} -E make_directory "${DFLOWFM_IO_WHEEL_DIR}"
	COMMAND ${Python3_EXECUTABLE} -m pip wheel "${CMAKE_CURRENT_SOURCE_DIR}"
	--no-deps --wheel-dir "${DFLOWFM_IO_WHEEL_DIR}"
	COMMENT "Building the dflowfm_io Python wheel into ${DFLOWFM_IO_WHEEL_DIR}"
	VERBATIM
)

# The wheel bundles the native library, so it must be built (and staged into _lib) first.
add_dependencies(dflowfm_io_wheel dflowfm_io_api)
