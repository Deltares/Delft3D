/* set the fpp where the tests are implemented using f90tw "directives" in the fortran. */
#define CURRENTTESTFILE "test_dflowfm_kernel_example.f90.h"

extern "C" void reset_dflowfm_after_test();
#define F90TW_TEST_TEARDOWN() reset_dflowfm_after_test()

#include "f90tw_gtest.h"
