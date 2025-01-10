cmake_minimum_required(VERSION 3.10)

# all of this works only
# if (MSVC)
project(SplashScreen)

# compilation settings
add_executable(d3d_qp src/SplashScreenStarter.cpp
                      src/SplashScreen.cpp
                      src/FileImageLoader.cpp
                      src/ResourceImageLoader.cpp
                      src/ImageLoader.cpp
                      src/stdafx.cpp)

target_sources(d3d_qp PRIVATE resource/SplashScreenStarter.rc)

target_compile_definitions(d3d_qp PUBLIC UNICODE _UNICODE)
target_include_directories(d3d_qp PUBLIC include)

# link settings
set_target_properties(d3d_qp PROPERTIES LINK_FLAGS "/SUBSYSTEM:WINDOWS")
target_link_libraries(d3d_qp shlwapi windowscodecs)

# endif()
