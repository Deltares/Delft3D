#!/bin/bash
#. /usr/share/Modules/init/bash

source /opt/intel/oneapi/setvars.sh

cmake ./src/cmake -DCMAKE_Fortran_COMPILER=mpiifort -D CONFIGURATION_TYPE:STRING=DflowFM -D CMAKE_BUILD_TYPE=Debug -B build_all

cd build_all
cmake --build . -j --target install --config Debug