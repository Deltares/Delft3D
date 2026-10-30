#include <gtest/gtest.h>
#include <gtest/gtest-spi.h>
#include <set>
#include <string>

extern "C" {
void passing_assertions();
void failing_assertions();
void passing_status_assertions();
void guarded_assertions(int scenario);
void passing_status_after_failure();
void passing_string_edges();
void failing_array_edges();
void failing_message_edges();
void passing_documented_surface();
void failing_documented_surface();
extern int assertion_continued;
extern int assertion_cleanup_completed;
}

int main(int argc, char** argv) {
    testing::InitGoogleTest(&argc, argv);
    return RUN_ALL_TESTS();
}

TEST(FortranAssertions, PassingExpectations) {
    passing_assertions();
}

TEST(FortranAssertions, PassingStatusAssertions) {
    passing_status_assertions();
}

TEST(FortranAssertions, ExplicitReturnsStopCallerAndPropagateHelperFailure) {
    for (int scenario = 0; scenario < 5; ++scenario) {
        SCOPED_TRACE(scenario);
        testing::TestPartResultArray failures;
        {
            testing::ScopedFakeTestPartResultReporter reporter(
                testing::ScopedFakeTestPartResultReporter::INTERCEPT_ONLY_CURRENT_THREAD, &failures);
            guarded_assertions(scenario);
        }
        ASSERT_EQ(failures.size(), 1);
        EXPECT_TRUE(failures.GetTestPartResult(0).fatally_failed());
        EXPECT_EQ(assertion_continued, 0);
        EXPECT_EQ(assertion_cleanup_completed, scenario == 4 ? 1 : 0);
    }
}

TEST(FortranAssertions, StatusIsIndependentOfEarlierFailures) {
    testing::TestPartResultArray failures;
    {
        testing::ScopedFakeTestPartResultReporter reporter(
            testing::ScopedFakeTestPartResultReporter::INTERCEPT_ONLY_CURRENT_THREAD, &failures);
        passing_status_after_failure();
    }
    ASSERT_EQ(failures.size(), 1);
    EXPECT_TRUE(failures.GetTestPartResult(0).fatally_failed());
    EXPECT_EQ(assertion_continued, 1);
}

TEST(FortranAssertions, NonfatalFailureMessagesAndContinuation) {
    testing::TestPartResultArray failures;
    {
        testing::ScopedFakeTestPartResultReporter reporter(
            testing::ScopedFakeTestPartResultReporter::INTERCEPT_ONLY_CURRENT_THREAD, &failures);
        failing_assertions();
    }

    ASSERT_EQ(failures.size(), 7);
    const char* messages[] = {
        "expect_eq", "expect_true", "expect_false", "expect_near", "expect_streq",
        "expect_gt", "expect_double_eq"
    };
    for (int i = 0; i < failures.size(); ++i) {
        const auto& failure = failures.GetTestPartResult(i);
        EXPECT_FALSE(failure.fatally_failed());
        EXPECT_NE(std::string(failure.message()).find(messages[i]), std::string::npos);
    }
    EXPECT_EQ(assertion_continued, 1) << "Nonfatal expectations must continue in their calling Fortran procedure";
    EXPECT_NE(std::string(failures.GetTestPartResult(0).message()).find("7"), std::string::npos);
    EXPECT_NE(std::string(failures.GetTestPartResult(0).message()).find("8"), std::string::npos);
    EXPECT_NE(std::string(failures.GetTestPartResult(4).message()).find("left"), std::string::npos);
    EXPECT_NE(std::string(failures.GetTestPartResult(4).message()).find("right"), std::string::npos);
}

TEST(FortranAssertions, FortranStringsAndLegacyNuls) {
    passing_string_edges();
}

TEST(FortranAssertions, ArrayIndicesAndShapeFailures) {
    testing::TestPartResultArray failures;
    {
        testing::ScopedFakeTestPartResultReporter reporter(
            testing::ScopedFakeTestPartResultReporter::INTERCEPT_ONLY_CURRENT_THREAD, &failures);
        failing_array_edges();
    }
    ASSERT_EQ(failures.size(), 6);
    const char* messages[] = {
        "vector : (2) INDX", "matrix : (1,2) INDX", "logical vector : (2) INDX",
        "logical matrix : (1,2) INDX", "bad vector : arrays of different shape",
        "bad matrix : arrays of different shape"
    };
    for (int i = 0; i < failures.size(); ++i) {
        const auto& failure = failures.GetTestPartResult(i);
        EXPECT_FALSE(failure.fatally_failed());
        EXPECT_NE(std::string(failure.message()).find(messages[i]), std::string::npos);
    }
}

TEST(FortranAssertions, LongEmptyAndNulTerminatedMessages) {
    testing::TestPartResultArray failures;
    {
        testing::ScopedFakeTestPartResultReporter reporter(
            testing::ScopedFakeTestPartResultReporter::INTERCEPT_ONLY_CURRENT_THREAD, &failures);
        failing_message_edges();
    }
    ASSERT_EQ(failures.size(), 3);
    EXPECT_NE(std::string(failures.GetTestPartResult(0).message()).find(std::string(4096, 'x') + ":end"),
              std::string::npos);
    const std::string nul_message = failures.GetTestPartResult(1).message();
    EXPECT_NE(nul_message.find("visible"), std::string::npos);
    EXPECT_EQ(nul_message.find("not-visible"), std::string::npos);
    EXPECT_FALSE(failures.GetTestPartResult(2).fatally_failed());
}

TEST(FortranAssertions, CompleteDocumentedSurfacePasses) {
    passing_documented_surface();
}

TEST(FortranAssertions, CompleteDocumentedSurfaceReportsFailures) {
    testing::TestPartResultArray failures;
    {
        testing::ScopedFakeTestPartResultReporter reporter(
            testing::ScopedFakeTestPartResultReporter::INTERCEPT_ONLY_CURRENT_THREAD, &failures);
        failing_documented_surface();
    }
    ASSERT_EQ(failures.size(), 56);
    std::set<std::string> cases;
    for (int i = 0; i < failures.size(); ++i) {
        const auto& failure = failures.GetTestPartResult(i);
        const std::string message = failure.message();
        const auto position = message.find("case=");
        ASSERT_NE(position, std::string::npos) << message;
        const auto name = message.substr(position);
        EXPECT_FALSE(failure.fatally_failed()) << name;
        EXPECT_TRUE(cases.insert(name).second) << "Duplicate failure: " << name;
    }
}
