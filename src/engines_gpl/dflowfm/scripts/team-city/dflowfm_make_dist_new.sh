#!/bin/bash
#BELOW IS THE LINE TO MAKE SURE THAT MODULES ARE INITIALIZED
#In case you use different shell replace/add below
. /usr/share/Modules/init/sh
. /usr/share/Modules/init/bash

# (intended to be run directly from svn checkout dir of D-Flow FM)
# Creates a source dist for D-Flow FM through `make dist`
# and tests it in several steps:
# 1. Loading necessary modules (automake, autoconf, libtool, netcdf, mpich)
# 2. Make source dist from D-Flow FM checkout dir.
# 3. Unpack the created tar.gz, and test configure+make phase.
# 4. Create the tar.gz with binaries and linked libraries.


# First choose either GNU Fortran or not (not implies: Intel Fortan)
if [ -n "$1" ]; then
    if [ -n "$PS1" ]; then
        echo Using arg \#1 as GNU-compiler selection: $1
    fi
    DOGNU=$1
else
    DOGNU=0
fi

######################################

function make_dir {
    if [ ! -d "$1" ]; then
        mkdir $1
    fi
}

function prepare_fc {
    if [ "$DOGNU" = "1" ]; then
        prepare_gfortran
    else
        prepare_ifort
    fi
}

function prepare_gfortran {
    echo "Preparing GNU Fortran compiler..."
	module load gcc/4.9.1
    export FC=gfortran
    export CC=gcc
}

function prepare_ifort {
    echo "Preparing Intel Fortran compiler..."
    module load intel/14.0.3
    export FC=ifort
    export CC=icc
}

function prepare_netcdf {
    if [ "$DOGNU" = "1" ]; then
    	echo Loading GNU NetCDF libraries...
        module load netcdf/v4.3.2_v4.4.0_gcc_4.9.1
    else
    	echo Loading Intel NetCDF libraries...
        module load netcdf/v4.3.2_v4.4.0_intel_14.0.3
    fi
}

function prepare_mpi {
    if [ "$DOGNU" = "1" ]; then
		echo Loading GNU mpich 3.1.4 library
        module load mpich2/3.1.4_gcc_4.9.1
    else
		echo Loading Intel mpich 3.1.4 library
        module load mpich2/3.1.4_intel_14.0.3
    fi
}

function prepare_petsc {
    if [ "$DOGNU" = "1" ]; then
		echo Loading GNU Petsc-3.4.0 mpich library
        module load petsc/3.4.0_gcc4.9.1_mpich_3.1.4
    else
		echo Loading Intel Petsc-3.4.0 mpich library
        module load petsc/3.4.0_intel14.0.3_mpich_3.1.4
    fi
}

function prepare_metis {
    if [ "$DOGNU" = "1" ]; then
		echo Loading GNU Metis library
        module load metis/5.1.0_gcc4.9.1
    else
		echo Loading Intel Metis library
        module load metis/5.1.0_intel14.0.3
    fi
}

function prepare_autotools {
	module load libtool/2.4.3
	module load autoconf/2.69
	module load automake/1.14.1
}

# FUNCTION NOT USED, subversion shared
function prepare_svn {
    PATH=/opt/subversion/bin:$PATH
    export PATH
}

# Subsitute the current version in the AC_INIT line in configure.ac
function patch_version_number {
    topbuilddir=`pwd`

    major=`head -4 $topbuilddir/res/version_number.ini | grep --color=no major | sed -e 's/^.*=[ \t]*\([0-9][0-9]*\).*$/\1/'`
    minor=`head -4 $topbuilddir/res/version_number.ini | grep --color=no minor | sed -e 's/^.*=[ \t]*\([0-9][0-9]*\).*$/\1/'`
    rev=`head -4 $topbuilddir/res/version_number.ini | grep --color=no revision | sed -e 's/^.*=[ \t]*\([0-9][0-9]*\).*$/\1/'`

    if [[ -n "$major" && -n "$minor" && -n "$rev" ]]; then 
        sed -i -e 's/^\(AC_INIT[^,]*\),[^,]*,\(.*\)$/\1, '$major.$minor.$rev',\2/' $topbuilddir/configure.ac
    fi
}

function configure_fm {
    if [ "$DOGNU" = "1" ]; then
	    ./configure --prefix=$PWD --with-netcdf --with-mpi --with-metis --with-petsc 2>&1
    else
	    ./configure --prefix=$PWD --with-netcdf --with-mpi --with-metis --with-petsc 2>&1
    fi
}

function make_dist {
    echo Making D-Flow FM distribution...
    # Remove any existing tar gz
    rm -f dflowfm-[0-9]*.tar.gz

    # Make sure autogen is executable (not the case in TeamCity job)
    chmod u+x ./autogen.sh
    ./autogen.sh 2>&1
    if [ $? -ne 0 ]; then
        echo Error in automake+conf of D-Flow FM.
        return $?
    fi

    chmod u+x third_party_open/kdtree2/autogen.sh
    (cd third_party_open/kdtree2/; ./autogen.sh) 2>&1

    configure_fm
	
    # TEMP: because for dist building, version_number isn't automatically built yet.
    make -C third_party_open/version_number/

    make dist 2>&1
    if [ $? -ne 0 ]; then
        echo Error in make dist of D-Flow FM.
        return $?
    fi

    fmdistfile=`ls --color=no dflowfm-[0-9]*.tar.gz`
}

function test_dist {
    echo Testing D-Flow FM distribution...

    curdir=`pwd`

    if [ ! -e "$fmdistfile" ]; then
        echo No D-Flow distribution .tar.gz was found.
        return 1
    fi

    tar zxvf $fmdistfile
    fmdistdir=`basename $fmdistfile .tar.gz`
    cd $fmdistdir

    configure_fm

    make 2>&1 && make install 2>&1
    result=$?
    cd $curdir
    return $?
}

function save_bin {
	 echo Trying to save executable for linux TestBench
	 curdir=`pwd`
	 bindir="$fmdistdir/bin"
	 libdir="$fmdistdir/lib"
 	 t_dir="$PWD/bin_TB"
	 # check that there is no old folder
	 if [ -d "$t_dir" ]; then 
	 	 rm -rf $t_dir
	 fi
	 # check that there is no old targz binaries
	 if [ -f "bin_TB.tar.gz" ]; then
	 	 rm bin_TB.tar.gz
	 fi

	 #prepare files
	 mkdir "$PWD/bin_TB"
	 mkdir "$PWD/bin_TB/bin"
	 mkdir "$PWD/bin_TB/lib"
	 mkdir "$PWD/bin_TB/scripts"
	 cp $bindir/* "$t_dir/bin/"
	 #cp "$libdir/libdflowfm.so" "$t_dir/lib/"
	 #cp "$libdir/libchannel_flow.so" "$t_dir/lib/"
	 cp "$libdir/lib"*so* "$t_dir/lib/"
	 #ls "$curdir/scripts/team-city/"
	 #cp "$curdir/scripts/team-city/run_dflow_TB.sh" "$t_dir/"
	 cp "$curdir/scripts/team-city/run_dflowfm.bat" "$t_dir/scripts/"
	 cp "$curdir/scripts/team-city/run_dflowfm_parallel.bat" "$t_dir/scripts/"
	 cp "$curdir/scripts/team-city/run_dflowfm.sh" "$t_dir/scripts/"
	 chmod a+x $curdir/scripts/team-city/run_flowfm*
	 #mv "$t_dir/run_dflow_TB.sh" "$t_dir/run_dflow.sh"
	 # checks the linked libraries to dflowfm and puts them to the right folder
	 ldd "$bindir/dflowfm" | grep "=> /" | awk '{print $3}' | xargs -I '{}' cp -v '{}' "$t_dir/lib/"
	 #ldd "$libdir/libdflowfm.so" | grep "=> /" | awk '{print $3}' | xargs -I '{}' cp -v '{}' "$t_dir/lib/"
	 #ldd "$libdir/libchannel_flow.so" | grep "=> /" | awk '{print $3}' | xargs -I '{}' cp -v '{}' "$t_dir/lib/"
	 #prepare package
	 cd $t_dir
	 tar -zcvf bin_TB.tar.gz *
	 echo $curdir
	 #move to main folder
	 mv bin_TB.tar.gz "$curdir/"
	 #remove temp directory with collected files
	 rm -rf $t_dir
}	


function check_error {
    if [ $1 -ne 0 ]; then
        echo D-Flow FM dist creation failed.
        exit $1
    fi
}

#-Main part:------------------------

patch_version_number

echo "##teamcity[blockOpened name='Setup prerequisites']"
echo "##teamcity[blockOpened name='Prerequisites: Compilers']"
prepare_fc
check_error $?
echo "##teamcity[blockClosed name='Prerequisites: Compilers']"

echo "##teamcity[blockOpened name='Prerequisites: NetCDF libraries']"
prepare_netcdf 
check_error $?
echo "##teamcity[blockClosed name='Prerequisites: NetCDF libraries']"

echo "##teamcity[blockOpened name='Prerequisites: MPI']"
prepare_mpi
check_error $?
echo "##teamcity[blockClosed name='Prerequisites: MPI']"

echo "##teamcity[blockOpened name='Prerequisites: PETSc']"
prepare_petsc
check_error $?
echo "##teamcity[blockClosed name='Prerequisites: PETSc']"

echo "##teamcity[blockOpened name='Prerequisites: Metis']"
prepare_metis
check_error $?
echo "##teamcity[blockClosed name='Prerequisites: Metis']"

echo "##teamcity[blockOpened name='Prerequisites: Autotools']"
prepare_autotools
check_error $?
#prepare_svn
#check_error $?
echo "##teamcity[blockClosed name='Prerequisites: Autotools']"
echo "##teamcity[blockClosed name='Setup prerequisites']"

echo "##teamcity[blockOpened name='Make dist']"
make_dist
check_error $?
echo "##teamcity[blockClosed name='Make dist']"

echo "##teamcity[blockOpened name='Test dist']"
test_dist
result=$?
check_error $result
echo "##teamcity[blockClosed name='Test dist']"

echo "##teamcity[blockOpened name='Saving binaries']"
save_bin
echo "##teamcity[blockClosed name='Saving binaries']"

if [ $result -eq 0 ]; then
    echo D-Flow FM dist created successfully: $fmdistfile
fi

exit $result
