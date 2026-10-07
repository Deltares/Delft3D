#if defined(CURRENTTESTFILE)

#include "gtest/gtest.h"

#include "f90tw_defs_gtest.h"

/* get the test defined in the corresponding file
// and create the definitions of the fortran methods
// to be used */
#define TESTCODE HCODE
#include CURRENTTESTFILE

/* get the test defined in the corresponding file
// and create implementation of the c++ methods for
// calling the paired fortran code */
#undef TESTCODE
#define TESTCODE CCODE
#include CURRENTTESTFILE

#if defined(F90TW_TEST_TEARDOWN)
class F90TWTestCleanup : public testing::EmptyTestEventListener {
public:
   void OnTestEnd(const testing::TestInfo&) override { F90TW_TEST_TEARDOWN(); }
};
#endif

int main(int argc, char** argv) {
   ::testing::InitGoogleTest(&argc, argv);
#if defined(F90TW_TEST_TEARDOWN)
   testing::UnitTest::GetInstance()->listeners().Append(new F90TWTestCleanup);
#endif
   return RUN_ALL_TESTS();
}

#endif
