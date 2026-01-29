message(STATUS "Installing Intel Redistributable Libraries")
# Create a dummy target, such that if(NOT TARGET intelredist) works
add_custom_target(intelredist)
set_target_properties(intelredist PROPERTIES FOLDER third_party_open)

if(WIN32)
  include(intel_redist_windows.cmake)
endif(WIN32)

if(UNIX)
  include(intel_redist_linux.cmake)
endif(UNIX)
