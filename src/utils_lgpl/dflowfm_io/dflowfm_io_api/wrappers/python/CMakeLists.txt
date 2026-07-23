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

# Regenerate the ctypes binding layer (src/dflowfm_io/base/bindings.py) from the C header, so the
# Python bindings can never drift from the dflowfm_io_api ABI. The generated file is committed, but
# rebuilt here whenever the header or the generator changes.
cmake_path(SET DFLOWFM_IO_HEADER NORMALIZE
	"${CMAKE_CURRENT_SOURCE_DIR}/../../include/dflowfm_io_api/dflowfm_io_api.h")
set(DFLOWFM_IO_BINDINGS_GENERATOR "${CMAKE_CURRENT_SOURCE_DIR}/codegen/generate_bindings.py")
set(DFLOWFM_IO_BINDINGS "${CMAKE_CURRENT_SOURCE_DIR}/src/dflowfm_io/base/bindings.py")

add_custom_command(
	OUTPUT "${DFLOWFM_IO_BINDINGS}"
	COMMAND ${Python3_EXECUTABLE} "${DFLOWFM_IO_BINDINGS_GENERATOR}"
	DEPENDS "${DFLOWFM_IO_BINDINGS_GENERATOR}" "${DFLOWFM_IO_HEADER}"
	COMMENT "Generating dflowfm_io Python ctypes bindings from dflowfm_io_api.h"
	VERBATIM
)
add_custom_target(dflowfm_io_bindings DEPENDS "${DFLOWFM_IO_BINDINGS}")

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

# The wheel bundles the native library (built + staged into _lib by dflowfm_io_api) and the
# generated bindings, so both must be up to date first.
add_dependencies(dflowfm_io_wheel dflowfm_io_api dflowfm_io_bindings)
