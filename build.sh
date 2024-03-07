#!/bin/bash


# ================
# === Usage    ===
# ================
function print_usage_info {
    echo
    echo
    echo "Usage: ${0##*/} <CONFIG> [OPTIONS]"
    echo "- Only when <CONFIG>=all: Compile all engines that are not CMaked yet in the traditional way"
    echo "- Create directory 'build_<CONFIG>'"
    echo "  Delete it when it already existed"
    echo "- Execute '. src/setenv.sh' to load modules"
    echo "- Execute 'CMake <CONFIG>' to create makefile inside 'build_<CONFIG>'"
    echo "- Execute 'make VERBOSE=1 install'"
    echo "- Only when <CONFIG>=all: Combine all binaries in 'build_<CONFIG>\lnx64'"
    echo
    echo "<CONFIG>:"
    echo "- If <CONFIG> is missing, this usage will be print"
    echo "- all: All CMaked projects, currently D-Flow FM, DWAQ and DIMR"
    echo "- dflowfm"
    echo "- dwaq"
    echo "- dwaves"
    echo "- dimr"
    echo "- tests"
    echo "- swan"
    echo "- delft3d4"
    echo "- flow2d3d"
    echo
    echo "Options:"
    echo "-p, --prepareonly"
    echo "       Only CMake, no make"
    echo
    echo "--debug"
    echo "      Compile in debug mode"
    echo
    echo "More info  : https://oss.deltares.nl/web/delft3d/source-code"
    echo "About CMake: https://git.deltares.nl/oss/delft3d/-/tree/main/src/cmake/README"
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

    if ! command -v svnversion &> /dev/null; then
       echo "'svnversion' is not found."
    fi
}



# =========================
# === CreateCMakedir    ===
# =========================
function CreateCMakedir () {
    echo
    echo "Create CMake dir for $1$2 ..."
    cd     $root
    rm -rf $root/build_$1$2
    mkdir  $root/build_$1$2

    return
}



# ==================
# === DoCMake    ===
# ==================
function DoCMake () {
    echo
    echo "Executing CMake for $1 ..."
    cd    $root/build_$1$2
    echo "cmake ../src/cmake -G "$generator" -B "." -D CONFIGURATION_TYPE="$1" -D CMAKE_BUILD_TYPE=${buildtype}"
          cmake ../src/cmake -G "$generator" -B "." -D CONFIGURATION_TYPE="$1" -D CMAKE_BUILD_TYPE=${buildtype} -D CMAKE_INSTALL_PREFIX=../build_$1$2/install/
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
    echo "Building (make) based on CMake preparations for $1 ..."
    cd    $root/build_$1$2
    echo "make -j VERBOSE=1 install"
          make -j VERBOSE=1 install
    if [ $? -ne 0 ]; then
        echo "CMake build resulted in an error. Check log files."
        exit 1
    fi

    return
}



# =========================
# === InstallAll        ===
# =========================
function InstallAll () {
    if [ ${1} = "all"  ]; then
        echo
        echo "Installing in build_$1$2 ..."
        cd     $root
        rm -rf $root/build_$1$2/lnx64
        mkdir -p $root/build_$1$2/lnx64/bin
        mkdir -p $root/build_$1$2/lnx64/lib
        mkdir -p $root/build_$1$2/lnx64/share/delft3d/esmf/lnx64/bin
        mkdir -p $root/build_$1$2/lnx64/share/delft3d/esmf/lnx64/bin_COS7


        # CMaked stuff
        cp -rf $root/build_$1$2/install/* $root/build_$1$2/lnx64/ &>/dev/null

        # Additional step to copy ESMF stuff needed by D-WAVES
        cp -rf $root/src/third_party_open/esmf/lnx64/bin/ESMF_RegridWeightGen                          $root/build_$1$2/lnx64/bin                               &>/dev/null
        cp -rf $root/src/third_party_open/esmf/lnx64/scripts/ESMF_RegridWeightGen_in_Delft3D-WAVE.sh   $root/build_$1$2/lnx64/bin                               &>/dev/null
        cp -rf $root/src/third_party_open/esmf/lnx64/bin/lib*                                          $root/build_$1$2/lnx64/share/delft3d/esmf/lnx64/bin      &>/dev/null
        cp -rf $root/src/third_party_open/esmf/lnx64/bin_COS7/lib*                                     $root/build_$1$2/lnx64/share/delft3d/esmf/lnx64/bin_COS7 &>/dev/null
    fi

    return
}



# =========================
# === InstallDwaq       ===
# =========================
function InstallDwaq () {
    if [ ${1} = "dwaq"  ]; then
        echo
        echo "Installing in build_$1$2 ..."
        cd     $root
        rm -rf $root/build_$1$2/lnx64
        mkdir -p $root/build_$1$2/lnx64/bin
        mkdir -p $root/build_$1$2/lnx64/lib


        # CMaked stuff
        cp -rf $root/build_$1$2/install/* $root/build_$1$2/lnx64/ &>/dev/null
    fi

    return
}


# =========================
# === InstallDimr       ===
# =========================
function InstallDimr () {
    if [ ${1} = "dimr"  ]; then
        echo
        echo "Installing in build_$1$2 ..."
        cd     $root
        rm -rf $root/build_$1$2/lnx64
        mkdir -p $root/build_$1$2/lnx64/bin
        mkdir -p $root/build_$1$2/lnx64/lib


        # CMaked stuff
        cp -rf $root/build_$1$2/install/* $root/build_$1$2/lnx64/ &>/dev/null
    fi

    return
}

# ============
# === MAIN ===
# ============

#
## Defaults
prepareonly=0
mode=quiet
config=
generator="Unix Makefiles"
compiler=intel23
buildtype=Release
buildDirExtension=""

## check if Deltares system
isdeltares=$([ -f "/opt/apps/deltares/.nl" ] && echo "yes" || echo "no")

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
    -p|--prepareonly)
    prepareonly=1
    shift
    ;;
    -h|--help)
    print_usage_info
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
    --debug)
    buildtype=Debug
    buildDirExtension="_debug"
    shift
    ;;
    *)
    echo ERROR: Unknown command line argument $key
    exit 1
    ;;
esac
done

#
# Check config parameter
if [ -z $config ]; then
    print_usage_info
fi

scriptdirname=`readlink \-f \$0`
scriptdir=`dirname $scriptdirname`
root=$scriptdir


if [ "$isdeltares" = "yes" ]; then
    # On Deltares systems only
    echo
    echo "    config          : $config" "${buildtype}"
    echo "    deltares system : $isdeltares"
    echo "    compiler        : $compiler"
    echo "    prepareonly     : $prepareonly"
    echo

    # Check if modules exist
    module list > /dev/null
    if [ $? -ne 0 ]; then
        # No, modules do not exist: "Dot" setenv.sh version without modules
        echo ". $root/src/setenv_no_modules.sh $compiler"
              . $root/src/setenv_no_modules.sh $compiler
    else
        # Yes, modules do exist: "Dot" setenv.sh to load the modules needed
        echo ". $root/src/setenv.sh $compiler"
              . $root/src/setenv.sh $compiler
    fi
    if [ $? -ne 0 ]; then
        echo "Setenv.sh resulted in an error. Check log files."
        exit 1
    fi

else
    # On other systems
    echo
    echo "    config          : $config" "${buildtype}"
    echo "    prepareonly     : $prepareonly"
    echo
fi

# check required utilities
chkutils=$(CheckUtils)
if [ ! -z "$chkutils" ]; then
    echo "$chkutils"
    echo "Install missing programs and retry."
    exit 1
fi

CreateCMakedir ${config} ${buildDirExtension}

DoCMake ${config} ${buildDirExtension}

if [ "$prepareonly" = "1" ]; then
    echo Finished with preparations only
    exit 0
fi

BuildCMake ${config} ${buildDirExtension}

InstallAll ${config} ${buildDirExtension}
InstallDwaq ${config} ${buildDirExtension}
InstallDimr ${config} ${buildDirExtension}

echo Finished
