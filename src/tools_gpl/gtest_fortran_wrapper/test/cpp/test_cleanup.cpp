#include <gtest/gtest.h>

extern "C" {
extern int wrapper_registration_count;
}

namespace {
int cleanup_count = 0;

class CheckFortranCleanup : public testing::EmptyTestEventListener {
    int completed = 0;

    void OnTestStart(const testing::TestInfo&) override {
        EXPECT_EQ(cleanup_count, completed);
    }

    void OnTestEnd(const testing::TestInfo&) override {
        // End callbacks run in reverse order, after the generated cleanup listener.
        ++completed;
        EXPECT_EQ(cleanup_count, completed);
        EXPECT_EQ(wrapper_registration_count, completed) << "Fortran test body was not called";
    }
};

struct InstallCleanupCheck {
    InstallCleanupCheck() {
        testing::UnitTest::GetInstance()->listeners().Append(new CheckFortranCleanup);
    }
} install_cleanup_check;
}

extern "C" void wrapper_test_cleanup() {
    ++cleanup_count;
}
