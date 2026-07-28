# Builds the dflowfm_io Python wheel.
#
# Targets (build order when the wheel is built):
#   dflowfm_io_bindings -> runs the generator scripts (regenerates the committed generated code)
#   dflowfm_io_api      -> compiles the DLL; its POST_BUILD stages it into _lib/
#   dflowfm_io_wheel    -> `pip wheel` packages src/ + _lib/ into a .whl (depends on the two above)
#
# The wheel is NOT part of the default build (slow; only needed when publishing). Build on demand:
#     cmake --build <build-dir> --target dflowfm_io_wheel
# or add it to the default build with: -D DFLOWFM_IO_BUILD_PYTHON_WHEEL=ON

option(
	DFLOWFM_IO_BUILD_PYTHON_WHEEL
	"Build the dflowfm_io Python wheel as part of the default build"
	OFF
)

# Generated (committed) Python, regenerated here so it can never drift from its source:
#   base/bindings.py + mdu/model.py  <- generate_bindings.py, from the C header
#   mdu/schema.py                    <- generate_schema.py, from mdu.json
cmake_path(SET DFLOWFM_IO_HEADER NORMALIZE
	"${CMAKE_CURRENT_SOURCE_DIR}/../../include/dflowfm_io_api/dflowfm_io_api.h")
set(DFLOWFM_IO_GENERATOR_SUPPORT "${CMAKE_CURRENT_SOURCE_DIR}/scripts/generator_base.py")
set(DFLOWFM_IO_BINDINGS_GENERATOR "${CMAKE_CURRENT_SOURCE_DIR}/scripts/generate_bindings.py")
set(DFLOWFM_IO_GENERATED
	"${CMAKE_CURRENT_SOURCE_DIR}/src/dflowfm_io/base/bindings.py"
	"${CMAKE_CURRENT_SOURCE_DIR}/src/dflowfm_io/mdu/model.py")

# Runs generate_bindings.py (re-runs only when a DEPENDS input changes).
add_custom_command(
	OUTPUT ${DFLOWFM_IO_GENERATED}
	COMMAND ${Python3_EXECUTABLE} "${DFLOWFM_IO_BINDINGS_GENERATOR}"
	DEPENDS "${DFLOWFM_IO_BINDINGS_GENERATOR}" "${DFLOWFM_IO_GENERATOR_SUPPORT}" "${DFLOWFM_IO_HEADER}"
	COMMENT "Generating dflowfm_io Python bindings and typed model from dflowfm_io_api.h"
	VERBATIM
)

set(DFLOWFM_IO_SCHEMA_GENERATOR "${CMAKE_CURRENT_SOURCE_DIR}/scripts/generate_schema.py")
cmake_path(SET DFLOWFM_IO_MDU_JSON NORMALIZE "${CMAKE_CURRENT_SOURCE_DIR}/../../../json/mdu.json")
set(DFLOWFM_IO_SCHEMA "${CMAKE_CURRENT_SOURCE_DIR}/src/dflowfm_io/mdu/schema.py")

# Runs generate_schema.py (re-runs only when mdu.json or the generator changes).
add_custom_command(
	OUTPUT "${DFLOWFM_IO_SCHEMA}"
	COMMAND ${Python3_EXECUTABLE} "${DFLOWFM_IO_SCHEMA_GENERATOR}"
	DEPENDS "${DFLOWFM_IO_SCHEMA_GENERATOR}" "${DFLOWFM_IO_GENERATOR_SUPPORT}" "${DFLOWFM_IO_MDU_JSON}"
	COMMENT "Generating dflowfm_io typed MDU schema (Layer 2) from mdu.json"
	VERBATIM
)

# Build this target to (re)generate the Python code on its own.
add_custom_target(dflowfm_io_bindings DEPENDS ${DFLOWFM_IO_GENERATED} "${DFLOWFM_IO_SCHEMA}")

set(DFLOWFM_IO_WHEEL_DIR "${CMAKE_CURRENT_BINARY_DIR}/wheel")

if(DFLOWFM_IO_BUILD_PYTHON_WHEEL)
	set(_dflowfm_io_wheel_in_all ALL)
else()
	set(_dflowfm_io_wheel_in_all "")
endif()

# The wheel itself: pip wheel packages src/ (generated code + bundled _lib/ DLL) into DFLOWFM_IO_WHEEL_DIR.
add_custom_target(dflowfm_io_wheel ${_dflowfm_io_wheel_in_all}
	COMMAND ${CMAKE_COMMAND} -E make_directory "${DFLOWFM_IO_WHEEL_DIR}"
	COMMAND ${Python3_EXECUTABLE} -m pip wheel "${CMAKE_CURRENT_SOURCE_DIR}"
	--no-deps --wheel-dir "${DFLOWFM_IO_WHEEL_DIR}"
	COMMENT "Building the dflowfm_io Python wheel into ${DFLOWFM_IO_WHEEL_DIR}"
	VERBATIM
)

# Guarantees the DLL is staged and the code regenerated before pip wheel runs.
add_dependencies(dflowfm_io_wheel dflowfm_io_api dflowfm_io_bindings)
