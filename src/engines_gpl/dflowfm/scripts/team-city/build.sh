source /opt/intel/Compiler/11.1/072/bin/ifortvars.sh intel64

#export PATH=/opt/openmpi-1.4.3-intel64/bin:$PATH
export PATH=/opt/mpich2-1.0.8-intel64-PIC/bin:$PATH

netcdfroot=/opt/netcdf
export PATH=$netcdfroot/bin:$PATH

export PATH=$netcdfroot/bin:$PATH
export LD_LIBRARY_PATH=$netcdfroot/lib:$LD_LIBRARY_PATH

export PATH=/opt/autoconf/bin:$PATH
export PATH=/opt/libtool/bin:$PATH
export PATH=/opt/automake/bin:$PATH
export LD_LIBRARY_PATH=/opt/autoconf/current/lib:$LD_LIBRARY_PATH
export LD_LIBRARY_PATH=/opt/automake/current/lib:$LD_LIBRARY_PATH

export CPPFLAGS=-I$netcdfroot/include
export LDFLAGS=-L$netcdfroot/lib

export NETCDF_CFLAGS=-I$netcdfroot/include
export NETCDF_LIBS="-L$netcdfroot/lib -lnetcdf"
export NETCDF_FORTRAN_CFLAGS="-g -I$netcdfroot/include"
export NETCDF_FORTRAN_LIBS="-L$netcdfroot/lib -lnetcdff -lnetcdf"


export CC=icc
export CXX=icpc
export CFLAGS='-O3 -xHost -ip -no-prec-div -static-intel'
export CXXFLAGS='-O3 -xHost -ip -no-prec-div -static-intel'
export F77=ifort
export FC=ifort
export F90=ifort
export FFLAGS='-O3 -xHost -ip -no-prec-div -static-intel'
export CPP='icc -E'
export CXXCPP='icpc -E'

#make distclean


# make the configure script
./autogen.sh

# make a build directory per build type
#rm -rf ./build-$FC
#mkdir build-$FC
#cd build-$FC

# do an out of source build
#../configure --prefix=$(pwd)/opt
./configure --prefix=$(pwd)/opt

make 

# install into prefix
make install
make dist
