#!/bin/bash

# (intended to be run directly from svn checkout dir of D-Flow FM)
# Creates a source dist for D-Flow FM through `make dist`
# and tests it in several steps:
# 1. Download+build+install NetCDF C and Fortran libs (from unidata website)
# 2. Make source dist from D-Flow FM checkout dir.
# 3. Unpack the created tar.gz, and test configure+make phase.


# First choose either GNU Fortran or not (not implies: Intel Fortan)
if [ -n "$1" ]; then
    if [ -n "$PS1" ]; then
        echo Using arg \#1 as GNU-compiler selection: $1
    fi
    DOGNU=$1
else
    DOGNU=0
fi

# Note: possibly different versions for netcdf (C) and netcdff (Fortran)
netcdfversion=4.2.1
netcdffversion=4.2
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
    export PATH=/opt/gcc-4.6.2/bin:$PATH
    export LD_LIBRARY_PATH=/opt/gcc-4.6.2/lib64:$LD_LIBRARY_PATH
    export FC=gfortran
    export CC=gcc

    compiler=gcc462
}

function prepare_ifort {
    echo "Preparing Intel Fortran compiler..."

    compiler=ifort
    export FC=ifort
    export CC=icc

#prepare_ifort /opt/intel/Compiler/11.1/072/bin/intel64/ifort

    if [ -z "$1" ]; then
        fcpath=`which ifort`
    else
        fcpath=$1
    fi

    if [ -n "$fcpath" ]; then
        fcvarspath=`dirname $fcpath`/../compilervars.sh
        echo Checking Intel path: $fcvarspath
        if [ -e $fcvarspath ]; then
            source $fcvarspath intel64
        else
            echo Could not set Intel Fortran environment variables.
            return 1
        fi
    else
        echo Intel Fortran compiler 'ifort' not found.
        return 1
    fi
}

function prepare_netcdf {
    echo Preparing NetCDF installation...

    curdir=`pwd`
    if [ -z "$1" ]; then
        targetdir=`pwd`
    else
        make_dir $1
	targetdir=$(cd $1; pwd)
    fi
    builddir=$targetdir/build
    make_dir $builddir

    if [ ! -e $targetdir/lib/libnetcdf.so ]; then
        cd $builddir
        if [ ! -e netcdf-$netcdfversion.tar.gz ]; then
            wget http://www.unidata.ucar.edu/downloads/netcdf/ftp/netcdf-$netcdfversion.tar.gz
        fi
        tar zxvf netcdf-$netcdfversion.tar.gz
        cd netcdf-$netcdfversion

        ./configure --prefix=$targetdir  --disable-netcdf-4 --disable-doxygen 2>&1
        make 2>&1 && make install 2>&1
        if [ $? -ne 0 ]; then
            echo Error in build of NetCDF C library.
            return $?
        fi
    fi

    if [ ! -e $targetdir/lib/libnetcdff.so ]; then
        cd $builddir
        if [ ! -e netcdf-fortran-$netcdffversion.tar.gz ]; then
            wget http://www.unidata.ucar.edu/downloads/netcdf/ftp/netcdf-fortran-$netcdffversion.tar.gz
            tar zxvf netcdf-fortran-$netcdffversion.tar.gz
        fi
        cd netcdf-fortran-$netcdffversion
#        FC=ifort 
        CPPFLAGS=-I$targetdir/include LDFLAGS=-L$targetdir/lib \
        LD_LIBRARY_PATH=$targetdir/lib:$LD_LIBRARY_PATH ./configure --prefix=$targetdir 2>&1
        make 2>&1 && make install 2>&1
        if [ $? -ne 0 ]; then
           echo Error in build of NetCDF Fortran library.
           return $?
        fi
    fi

    LD_LIBRARY_PATH=$targetdir/lib:$LD_LIBRARY_PATH; export LD_LIBRARY_PATH

    NETCDFROOT=$targetdir

    cd $curdir
}

function prepare_mpi {
    if [ "$DOGNU" = "1" ]; then
        MPIROOT=/opt/mpich2-1.4.1-gcc-4.6.2
    else
        MPIROOT=/opt/mpich2-1.0.8-intel64-PIC
    fi

    LD_LIBRARY_PATH=${MPIROOT}/lib:$LD_LIBRARY_PATH
    PATH=${MPIROOT}/bin:$PATH
}

function prepare_petsc {
# TODO
return 0
}

function prepare_autotools {
    PATH=/opt/libtool-2.4.2/bin:$PATH
    PATH=/opt/autoconf-2.68/bin:$PATH
    PATH=/opt/automake-1.11.1/bin:$PATH
    export PATH
}

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
    NETCDF_FORTRAN_CFLAGS=-I${NETCDFROOT}/include \
    NETCDF_FORTRAN_LIBS="-L${NETCDFROOT}/lib -lnetcdf -lnetcdff" \
    PKG_CONFIG_PATH=${NETCDFROOT}/lib/pkgconfig \
    ./configure --prefix=$PWD --with-netcdf=${NETCDFROOT} 2>&1
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

#    FC=ifort
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
	 cp "${NETCDFROOT}/lib/libnetcdf.so" "$t_dir/lib/"
	 cp "${NETCDFROOT}/lib/libnetcdff.so" "$t_dir/lib/"
	 cp "$bindir/dflowfm" "$t_dir/bin/"
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

#PATH="/opt/gcc-4.6.2/bin:$PATH"
#LD_LIBRARY_PATH="/opt/gcc-4.6.2/lib64:$LD_LIBRARY_PATH"

#prepare_ifort /opt/intel/Compiler/11.1/072/bin/intel64/ifort
#prepare_ifort /opt/intel/composerxe/bin/ifort

echo "##teamcity[blockOpened name='Setup prerequisites']"
echo "##teamcity[blockOpened name='Prerequisites: Compilers']"
prepare_fc
check_error $?
echo "##teamcity[blockClosed name='Prerequisites: Compilers']"

echo "##teamcity[blockOpened name='Prerequisites: NetCDF libraries']"
prepare_netcdf $PWD/third_party_open/netcdf/linux
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

echo "##teamcity[blockOpened name='Prerequisites: Autotools']"
prepare_autotools
check_error $?

prepare_svn
check_error $?
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
