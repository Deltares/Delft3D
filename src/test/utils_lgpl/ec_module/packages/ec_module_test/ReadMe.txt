========================================================================
    Fortran Console Application : "ec_module_test" Project Overview
========================================================================

This is a temporary stand-alone executable that can be used to test EC-module (calling static library)
in the same fashion as the computational core. Initially intended to debug while making EC-module modifications,
it is modified to be used from Teamcity directly.

ec_module_test.vfproj
    Main application, stand-alone console.
    Arguments:
         -v, --verbose   : verbose (optional)
         -c <configname> : name of the test configuration file (MDU-style)
         --no-compare    : skip comparison with a reference, just produce output (optional)
        -r <reference>   : specify reference file explicitly (optional)
        -l <listfile>    : specify a file with a list of tests to be run
        -t <testname>    : specify a single test to be run

    if neither -t nor -l: run all tests in the config file

ec_module_test.f90
    Program main

cmdlargs.f90
    Simple routines handling arguments from the command line

/////////////////////////////////////////////////////////////////////////////
Other notes:

/////////////////////////////////////////////////////////////////////////////
