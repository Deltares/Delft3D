cmake_minimum_required(VERSION 3.10)
enable_language( Fortran )

project(testbmifortranmodel VERSION 1.0.0 DESCRIPTION "BMI Fortran Testing Model Shared Library")
add_subdirectory(./iso_c_fortran_bmi ${CMAKE_BINARY_DIR}/iso_c_bmi)
#Get the iso_c_fortran binding module to build as part of this build
#add_subdirectory(../iso_c_fortran_bmi ${CMAKE_BINARY_DIR}/iso_c_bmi)


#set (CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -fPIC -Wl")
#set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fPIC -Wl")

# https://www.intel.com/content/www/us/en/develop/documentation/oneapi-dpcpp-cpp-compiler-dev-guide-and-reference/top/compiler-setup/use-the-command-line/use-cmake-with-the-compiler.html
if (INTEL_DPCPP)
    cmake_minimum_required(VERSION 3.20)
    find_package(IntelDPCPP REQUIRED)
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -g -traceback -check all -debug all -mcmodel=medium -shared-intel")
else()
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -g -fbacktrace -fbounds-check -Wall")
endif()


#### Find D-Flow FM required external dependencies ######
find_package(Threads REQUIRED)
find_package(PkgConfig REQUIRED)

#MPI
find_package(MPI REQUIRED)
add_definitions(-DHAVE_MPI)

#NetCDF
pkg_check_modules(NETCDF     REQUIRED IMPORTED_TARGET netcdf)
pkg_check_modules(NETCDF_FTN REQUIRED IMPORTED_TARGET netcdf-fortran)

# Petsc
pkg_check_modules(PETSC REQUIRED PETSc)

# Proj
pkg_check_modules(PROJ REQUIRED proj)

# gdal
pkg_check_modules(GDAL REQUIRED gdal)

# link required libraries
add_library(metis SHARED IMPORTED)
set_target_properties(metis PROPERTIES IMPORTED_LOCATION "/scratch2/STI/coastal/save/COASTAL_ACT_NWC/Libs/intel/metis/lib/libmetis.so" INTERFACE_INCLUDE_DIRECTORIES "/scratch2/STI/coastal/save/COASTAL_ACT_NWC/Libs/intel/metis/include/metis.h")

include_directories(/scratch2/STI/coastal/save/COASTAL_ACT_NWC/Libs/intel/metis/include)
link_directories(/scratch2/STI/coastal/save/COASTAL_ACT_NWC/Libs/intel/metis/lib/libmetis.so)

# Uncomment this and rebuild artifacts to enable debugging
set(CMAKE_BUILD_TYPE Debug)

set( CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/fortran)

file(GLOB SOURCES BMI/*.f90)

if(WIN32)
    add_library(testbmifortranmodel ${SOURCES})
else()
    add_library(testbmifortranmodel SHARED ${SOURCES} )
endif()


#### Now set D-Flow FM library and include paths #####
set(DFLOWFM_LIB /scratch2/NCEPDEV/ohd/Jason.Ducker/DFLOWFM_BMI_BRANCH_COASTAL_ACT_METHOD/DFlowFM_NOAA_BMI_Branch/BMI_build/dflowfm_install/lib/libdflowfm.so)

include_directories(/scratch2/NCEPDEV/ohd/Jason.Ducker/DFLOWFM_BMI_BRANCH_COASTAL_ACT_METHOD/DFlowFM_NOAA_BMI_Branch/BMI_build/dflowfm_install/include)
link_directories(/scratch2/NCEPDEV/ohd/Jason.Ducker/DFLOWFM_BMI_BRANCH_COASTAL_ACT_METHOD/DFlowFM_NOAA_BMI_Branch/BMI_build/dflowfm_install/lib)


target_link_libraries(testbmifortranmodel PUBLIC iso_c_bmi ${DFLOWFM_LIB} /scratch2/STI/coastal/save/COASTAL_ACT_NWC/Libs/intel/metis/lib/libmetis.so) #${metis})
target_compile_options(testbmifortranmodel PUBLIC -cpp -DNGEN_ACTIVE)

set_target_properties(testbmifortranmodel PROPERTIES VERSION ${PROJECT_VERSION})

include(GNUInstallDirs)

install(TARGETS testbmifortranmodel
        LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
        PUBLIC_HEADER DESTINATION ${CMAKE_INSTALL_INCLUDEDIR})

set(BMI_INCLUDE ./build/fortran)
set(BMI_LIB ./build/libtestbmifortranmodel.so)

include_directories(${BMI_INCLUDE})
link_directories(${BMI_LIB})


add_executable(dflowfm_driver src/dflowfm_bmi_driver_test.f90)

target_link_libraries(dflowfm_driver PUBLIC ${BMI_LIB} ${DFLOWFM_LIB} /scratch2/STI/coastal/save/COASTAL_ACT_NWC/Libs/intel/metis/lib/libmetis.so)

install(TARGETS dflowfm_driver DESTINATION ${CMAKE_BINARY_DIR})                                                     
