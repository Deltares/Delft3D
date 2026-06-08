#include <gtest/gtest.h>

#include <dflowfm_io/MduData.h>

TEST(MduDataTest, GetDummyValueReturns42)
{
    dflowfm_io::MduData model;
    EXPECT_EQ(model.GetDummyValue(), 42);
}
