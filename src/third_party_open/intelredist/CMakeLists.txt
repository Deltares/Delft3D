message(STATUS "Installing Intel Redistributable Libraries")
# Create a dummy target, such that if(NOT TARGET intelredist) works
add_custom_target(intelredist)

if(WIN32)
  include(intel_redist_windows.cmake)
endif(WIN32)

if(UNIX)
  include(intel_redist_linux.cmake)
endif(UNIX)
