#include <gtest/gtest.h>

#include <dflowfm_io/MduModel.h>

TEST(MduModelTest, GetDummyValueReturns42)
{
    dflowfm_io::MduModel model;
    EXPECT_EQ(model.GetDummyValue(), 42);
}
