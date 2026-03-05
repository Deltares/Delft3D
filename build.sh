#!/bin/bash

# ================
# === Usage    ===
# ================
function print_usage_info {
    echo
    echo
    echo "Usage: ${0##*/} <CONFIG> [OPTIONS]"
    echo "- Create directory 'build_<CONFIG>'"
    echo "  Delete it when it already existed"
    echo "- Execute CMake step to create makefile inside 'build_<CONFIG>'"
    echo "- Execute CMake step to build inside 'build_<CONFIG>' and install inside 'install_<CONFIG>'"
    echo
    echo "<CONFIG>:"
    echo "- all: All CMaked products that are in fm-suite and d3d4-suite combined"
    echo "- fm-suite"
    echo "- d3d4-suite"
    echo "- dwaq"
    echo "- dwaves"
    echo "- dimr"
    echo "- tests"
    echo "- swan"
    echo "- flow2d3d"
    echo
    echo "Options:"
    echo "-h, --help"
    echo "       Show this help page."
    echo "--build"
    echo "       Run build and install steps after running cmake."
    echo "--build_dir <DIR_NAME>"
    echo "       Absolute path to build directory equal to"
    echo "       '${PWD}/build_<CONFIG>' for Release builds and"
    echo "       '${PWD}/build_<CONFIG>_debug' for Debug builds."
    echo "--build_type <BUILD_TYPE>"
    echo "       Build optimization level with <BUILD_TYPE> equal to 'Release' or 'Debug'."
    echo "--install_dir <DIR_NAME>"
    echo "       Absolute path to build directory equal to"
    echo "       '${PWD}/install_<CONFIG>' for Release builds and"
    echo "       '${PWD}/install_<CONFIG>_debug' for Debug builds."
    echo "--keep_build"
    echo "       Do not delete the 'build_<CONFIG>' and 'install_<CONFIG>' folders."
    echo
    echo "More info  : https://github.com/Deltares/Delft3D"
    echo
    exit 1
}

# =========================
# === CheckUtils        ===
# =========================
function CheckUtils () {
    if ! command -v patchelf &> /dev/null; then
       echo "'patchelf' is not found."
    fi
}



# =========================
# === CreateCMakedir    ===
# =========================
function CreateCMakedir () {
    echo
    echo "Create build dir for $1 ..."
    
    cd $root
    if [ -z "keep_build" ]; then
       rm -rf $build_dir
       rm -rf $install_dir
    fi
    if [ ! -d "$build_dir" ]; then
       mkdir  $build_dir
    fi
    if [ ! -d "$install_dir" ]; then
       mkdir  $install_dir
    fi

    return
}



# ==================
# === DoCMake    ===
# ==================
function DoCMake () {
    echo
    echo "Executing CMake for $1 ..."
    
    cd $root
    echo 'cmake ./src/cmake -G "$generator" -B $build_dir \\'
    echo '    -D CONFIGURATION_TYPE="$1" \\'
    echo '    -D CMAKE_BUILD_TYPE=$build_type \\'
    echo '    -D CMAKE_INSTALL_PREFIX="$install_dir"'
    
    cmake ./src/cmake -G "$generator" -B $build_dir \
        -D CONFIGURATION_TYPE="$1" \
        -D CMAKE_BUILD_TYPE=$build_type \
        -D CMAKE_INSTALL_PREFIX="$install_dir"
        
    if [ $? -ne 0 ]; then
        echo "CMake configure resulted in an error. Check log files."
        exit 1
    fi

    return
}



# =====================
# === BuildCMake    ===
# =====================
function BuildCMake () {
    echo
    echo "Building $1 ..."
    
    cd $build_dir
    echo "cmake --build . --parallel --target install --config $build_type"
          cmake --build . --parallel --target install --config $build_type
    
    if [ $? -ne 0 ]; then
        echo "CMake build resulted in an error. Check log files."
        exit 1
    fi

    return
}



# ============
# === MAIN ===
# ============

#
## Defaults
mode=quiet
config=
generator="Unix Makefiles"
compiler=ifx
build_type=Release
build_dir_postfix=""
build=0
build_dir=""
install_dir=""

#
## Start processing command line options:

while [[ $# -ge 1 ]]
do
key="$1"

case $key in
    -c|--compiler)
    shift
    compiler="$1"
    shift
    ;;
    --build)
    build=1
    shift
    ;;
    --build_dir)
    shift
    build_dir="$1"
    shift
    ;;
    --build_type)
    shift
    build_type="$1"
    shift
    ;;
    -h|--help)
    print_usage_info
    ;;
    --install_dir)
    shift
    install_dir="$1"
    shift
    ;;
    all)
    config="all"
    shift
    ;;
    dflowfm)
    config="dflowfm"
    shift
    ;;
    dwaq)
    config="dwaq"
    shift
    ;;
    dwaves)
    config="dwaves"
    shift
    ;;
    dimr)
    config="dimr"
    shift
    ;;
    tests)
    config="tests"
    shift
    ;;
    swan)
    config="swan"
    shift
    ;;
    delft3d4)
    config="delft3d4"
    shift
    ;;
    flow2d3d)
    config="flow2d3d"
    shift
    ;;
    *)
    echo ERROR: Unknown command line argument $key
    exit 1
    ;;
esac
done

# Check config parameter
if [ -z "$config" ]; then
    print_usage_info
fi

# Check build_type
if [ "$build_type" == "Debug" ]; then
    build_dir_postfix="_debug"
elif [ "$build_type" == "Release" ]; then
    build_dir_postfix=""
else
    echo ERROR: Unknown build type ${build_type}; should be "Release" or "Debug".
fi

scriptdirname=`readlink \-f \$0`
scriptdir=`dirname $scriptdirname`
root=$scriptdir

# set directories for build and install
if [ -z "$build_dir" ]; then
    build_dir=$root/build_${config}${build_dir_postfix}
fi
if [ -z "$install_dir" ]; then
    install_dir=$root/install_${config}${build_dir_postfix}
fi

echo
echo "    config          : $config"
echo "    generator       : $generator"
echo "    build_type      : $build_type"
echo "    build           : $build"
echo "    keep_build      : $keep_build"
echo "    build_dir       : $build_dir"
echo "    install_dir     : $install_dir"
echo

CreateCMakedir ${config}

DoCMake ${config}

if [ "$build"=="1" ]; then
    BuildCMake ${config}
fi

echo Finished
