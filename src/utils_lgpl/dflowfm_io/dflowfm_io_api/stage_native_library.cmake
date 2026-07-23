# Stage a freshly built native library into the Python wheel package's _lib/ directory.
#
# Run at build time via `cmake -DLIB_DIR=... -DLIB_FILE=... -P stage_native_library.cmake`.
#
# It first removes any stale libraries from LIB_DIR — matching exactly the patterns the wheel bundles
# (pyproject.toml package-data) — so a renamed backup or a library from a previous build config can
# never leak into the wheel. The tracked .gitignore in LIB_DIR is left untouched. Then it copies the
# just-built library in.

file(MAKE_DIRECTORY "${LIB_DIR}")

file(GLOB _stale_libraries
    "${LIB_DIR}/*.dll"
    "${LIB_DIR}/*.so"
    "${LIB_DIR}/*.dylib"
    "${LIB_DIR}/*.pyd"
)
if(_stale_libraries)
    file(REMOVE ${_stale_libraries})
endif()

file(COPY "${LIB_FILE}" DESTINATION "${LIB_DIR}")
