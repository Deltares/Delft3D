#!/bin/bash
module load intel/14.0.3
module load netcdf/v4.3.2_v4.4.0_intel_14.0.3
module load anaconda

python ../run_all_tests.py

../src/ec_module_test --verbose -c internal

