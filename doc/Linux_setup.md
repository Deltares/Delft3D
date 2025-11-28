# Setting up your own Linux environment

To set up your own Linux environment several steps must be taken. This can be done either on a full Linux system, or in WSL (Windows Subsystem for Linux).
The description below is for a Ubuntu based system, the steps are similar for different distributions. Package names might vary between distributions.
Using the Intel compiler toolchain (oneAPI) is recommended.


#### Intel compilers
Install the Intel compilers using the desciption on: https://www.intel.com/content/www/us/en/docs/oneapi/installation-guide-linux/. Recommended is using "Install with Package Managers" method.

Install both the intel-basekit and intel-hpckit. To load the compilers you need to source the setvars.sh shell script, using: "source /opt/intel/oneapi/setvars.sh"


#### Packages through package manager
Several packages are needed. Recommended is using the system package manager (e.g. apt, yum, dnf, etc.).
The following packages must be installed:
- cmake
- ninja-build
- petsc-dev
- libnetcdf-dev
- hdf5-tools
- hdf5-helpers
- libhdf5-dev
- libhdf5-doc
- libhdf5-serial-dev
- netcdf-bin
- metis
- libgdal-dev
- uuid-dev
- sqlite3
- libtiff-dev


#### Downloading and compiling NetCDF-Fortran
NetCDF-Fortran has to be compiled with the same compiler as you plan using on your system. Hence it has to be downloaded and compiled from source. Installing it through the package manager will NOT work.

Steps:
- check if the netcdf package installation was succesful: "nc-config --all"
- download the netcdf-fortran source files: https://downloads.unidata.ucar.edu/netcdf/
- extract the netcdf-fortran-*.*.*.tar.gz
- create a file config-intel.sh inside the netcdf-fortran-*.*.* folder:
      export PATH="$PATH:/opt/intel/oneapi/compiler/latest/linux/bin/intel64/"

      export CDFROOT="/usr"
      export LD_LIBRARY_PATH="${CDFROOT}/lib:${LD_LIBRARY_PATH}"
      export LDFLAGS="-L${CDFROOT}/lib -I${CDFROOT}/include":
      export OPTIM="-O3 -mcmodel=large -fPIC ${LDFLAGS}"

      export CC=icx
      export CXX=icx
      export FC=ifort
      export F77=ifort
      export F90=ifort
      export CPP='icx -E -mcmodel=large'
      export CXXCPP='icx -E -mcmodel=large'
      export CPPFLAGS="-DNDEBUG -DpgiFortran ${LDFLAGS}"

      export CFLAGS=" ${OPTIM}"
      export CXXFLAGS=" ${OPTIM}"
      export FCFLAGS=" ${OPTIM}"
      export F77FLAGS=" ${OPTIM}"
      export F90FLAGS=" ${OPTIM}"

      ./configure --prefix=/usr/local/netcdf-ifort/4.6.1 --enable-large-file-tests --with-pic
- Give the file config-intel.sh execution rights ("chmod a+x config-intel.sh").
- Execute:
  - ./config-intel.sh
  - make
  - make check
  - sudo make install
- Check if installation was succesful: "nf-config --all"



#### Add pkgconfig files
Several packages need to have a pkgconfig file setup to be able to find the package. These include netcdf-fortran, proj and sqlite3. To do this, make a folder pkgconfig, and place there the following files:

netcdf-fortran.pc


    prefix=/usr/local/netcdf-ifort/4.6.1/
    exec_prefix=${prefix}
    libdir=${exec_prefix}/lib
    includedir=${prefix}/include
    fmoddir=${includedir}
    ccompiler=icc
    fcompiler=ifort
    Name: netcdf-fortran23
    Description: NetCDF Client Library for Fortran
    URL: https://www.unidata.ucar.edu/netcdf
    Version: 4.6.1
    Requires.private: netcdf > 4.1.1
    Libs: -L${libdir} -lnetcdff
    Libs.private: -L${libdir} -lnetcdff -lnetcdf
    Cflags: -I${includedir} -I${fmoddir}



proj.pc


    prefix=/usr
    libdir=${prefix}/lib64
    includedir=${prefix}/include
    datarootdir=${prefix}/share
    datadir=${datarootdir}/proj
    Name: PROJ
    Description: Coordinate transformation software library
    Requires:
    Version: 9.2.0
    Libs: -L${libdir} -lproj
    Libs.private: -lpthread -lstdc++ -lm -ldl
    Requires.private: sqlite3 libtiff-4 libcurl
    Cflags: -I${includedir}



sqlite3.pc


    prefix=/usr
    exec_prefix=${prefix}
    libdir=${exec_prefix}/lib
    includedir=${prefix}/include
    Name: SQLite
    Description: SQL database engine
    Version: 3.41.2
    Libs: -L${libdir} -lsqlite3
    Libs.private: -lz -lpthread
    Cflags: -I${includedir}


#### Compile the code!
Example below is for compiling D-Water Quality in Release mode.
- Load the intel compiler: "source /opt/intel/oneapi/setvars.sh"
- Export the pkgconfig files: "export PKG_CONFIG_PATH=/home/<change-to-your-user-name>/pkgconfig"
- Export the compilers:
  - export FC=mpiifort
  - export CXX=mpiicpc
  - export CC=mpiicc
- make a build_dir: "mkdir build"
- cmake configure and build the code:
  - cmake ./src/cmake -G "Unix Makefiles" -B build -D CONFIGURATION_TYPE=dwaq -D CMAKE_BUILD_TYPE=Release -D CMAKE_INSTALL_PREFIX=install
  - cmake --build . -j --target install --config Release

Of course these steps can be put into a shell script for convenience.

