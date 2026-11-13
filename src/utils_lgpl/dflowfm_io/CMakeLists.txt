cmake_minimum_required(VERSION 3.21)
project(dflowfm_io_solution LANGUAGES CXX)

# enable project files grouping by folders
set_property(GLOBAL PROPERTY USE_FOLDERS ON)

enable_testing()

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)

set(DFLOWFM_IO_MDU_JSON "${CMAKE_CURRENT_SOURCE_DIR}/json/mdu.json" CACHE INTERNAL "Path to mdu.json schema")

option(DFLOWFM_IO_BUILD_WRAPPER_PACKAGES
	"Build the redistributable wrapper packages (Python wheel, C# NuGet) as part of the default build"
	OFF
)

add_subdirectory(dflowfm_ini_parser)
add_subdirectory(dflowfm_io)
add_subdirectory(dflowfm_io_api)
add_subdirectory(dflowfm_io_tool)
