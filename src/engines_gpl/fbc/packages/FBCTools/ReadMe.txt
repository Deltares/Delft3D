========================================================================
    C++ Library : "D-Feedback Control" Project Overview
========================================================================

The Deltares can-do-everything Wizard has created this 
"D-Feedback Control" project for you as a starting point.

This file contains a summary of D-FBC project.

It used to be RTC. 

/////////////////////////////////////////////////////////////////////////////
Other notes:

## Build

The requirements are:
- CMake
- The Boost libraries

On windows precompiled boost binaries (with MSVC compiler) can be downloaded here:

https://sourceforge.net/projects/boost/files/boost-binaries/ 

Once installed, modify boost environmental variables accordingly. If boost 1.65 is installed in C:\Apps\boost_1_65_0, set the following environmental variables:

BOOST_INCLUDEDIR=C:\Apps\boost_1_65_0
BOOST_LIBRARYDIR=C:\Apps\boost_1_65_0\lib64-msvc-14.1

### IDE
To use an IDE, such as Visual Studio:

cmake -S . -B build -G"Visual Studio 16 2019"
cmake --open build

/////////////////////////////////////////////////////////////////////////////