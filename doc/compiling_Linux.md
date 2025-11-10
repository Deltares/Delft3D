# Compiling Delft3D on Linux
For the latest overview of build steps see the Docker files located in `ci\dockerfiles\linux\`.

## Prerequisites
- Various asic Linux utilities
```
dnf install --assumeyes \
    which binutils patchelf diffutils procps m4 make gcc gcc-c++ \
    openssl openssl-devel wget perl python3 xz curl-devel
```

- [Intel oneAPI Fortran Compiler](https://www.intel.com/content/www/us/en/developer/tools/oneapi/fortran-compiler-download.html)
- [Intel oneAPI MPI Library](https://www.intel.com/content/www/us/en/developer/tools/oneapi/mpi-library.html)
- [Intel oneAPI Math Kernal Library](https://www.intel.com/content/www/us/en/developer/tools/oneapi/onemkl-download.html)
```
INTEL_ONEAPI_VERSION=2024
    COMMON_VARS_VERSION="2024.2.1"
    COMPILER_DPCPP_CPP_VERSION="2024.2.1"
    COMPILER_FORTRAN_VERSION="2024.2.1"
    MKL_DEVEL_VERSION="2024.2.2"
    MPI_DEVEL_VERSION="2021.13.1"

cat <<EOT > /etc/yum.repos.d/oneAPI.repo
[oneAPI]
name=Intel® oneAPI repository
baseurl=https://yum.repos.intel.com/oneapi
enabled=1
gpgcheck=1
repo_gpgcheck=1
gpgkey=https://yum.repos.intel.com/intel-gpg-keys/GPG-PUB-KEY-INTEL-SW-PRODUCTS.PUB
EOT


```
- [CMake](https://cmake.org/download/) version 3.30 or later
- [Git](https://gitforwindows.org/)
- For more advanced development steps, such as running test benches, you will need a Python installation.

## Build steps
- build.sh
  Execute "./build.sh --help" to show the usage
  Currently used as default build process: "./build.sh fm-suite --compiler intel21"
  This will execute "src/setenv.sh" on Deltares systems. On other systems, the environment must be prepared upfront.
  For instructions, see [Setup your own Linux environment](Linux_setup.md).

## Alternative: without build-script
Refer to the [README](src\cmake\README) file in the CMake folder.
WARNING: When building without our build-script, the collection of the resulting binaries will need attention
