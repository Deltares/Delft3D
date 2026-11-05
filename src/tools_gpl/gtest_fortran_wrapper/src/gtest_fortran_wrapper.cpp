#include "gtest_fortran_wrapper.h"
#include <gtest/gtest.h>
#include <string>

static_assert(sizeof(int32_t) == 4 && sizeof(bridge_bool) == 1, "Unsupported integer sizes");
static_assert(sizeof(float) == 4 && sizeof(double) == 8, "Unsupported real sizes");

void bridge_eq_i32(int32_t val1, int32_t val2, bridge_bool fatal, const char* message, size_t message_length) noexcept {
    const std::string diagnostic(message, message_length);
    if (fatal != 0) {
        ASSERT_EQ(val1, val2) << diagnostic;
    } else {
        EXPECT_EQ(val1, val2) << diagnostic;
    }
}

void bridge_eq_f32(float val1, float val2, const char* message, size_t message_length) noexcept {
    const std::string diagnostic(message, message_length);
    EXPECT_EQ(val1, val2) << diagnostic;
}

void bridge_eq_f64(double val1, double val2, const char* message, size_t message_length) noexcept {
    const std::string diagnostic(message, message_length);
    EXPECT_EQ(val1, val2) << diagnostic;
}

void bridge_eq_bool(bridge_bool val1, bridge_bool val2, const char* message, size_t message_length) noexcept {
    const std::string diagnostic(message, message_length);
    EXPECT_EQ(val1, val2) << diagnostic;
}

void bridge_ne_i32(int32_t val1, int32_t val2, const char* message, size_t message_length) noexcept {
    const std::string diagnostic(message, message_length);
    EXPECT_NE(val1, val2) << diagnostic;
}

void bridge_ne_f32(float val1, float val2, const char* message, size_t message_length) noexcept {
    const std::string diagnostic(message, message_length);
    EXPECT_NE(val1, val2) << diagnostic;
}

void bridge_ne_f64(double val1, double val2, const char* message, size_t message_length) noexcept {
    const std::string diagnostic(message, message_length);
    EXPECT_NE(val1, val2) << diagnostic;
}

void bridge_ne_bool(bridge_bool val1, bridge_bool val2, const char* message, size_t message_length) noexcept {
    const std::string diagnostic(message, message_length);
    EXPECT_NE(val1, val2) << diagnostic;
}

void bridge_lt_i32(int32_t val1, int32_t val2, const char* message, size_t message_length) noexcept {
    const std::string diagnostic(message, message_length);
    EXPECT_LT(val1, val2) << diagnostic;
}

void bridge_lt_f32(float val1, float val2, const char* message, size_t message_length) noexcept {
    const std::string diagnostic(message, message_length);
    EXPECT_LT(val1, val2) << diagnostic;
}

void bridge_lt_f64(double val1, double val2, const char* message, size_t message_length) noexcept {
    const std::string diagnostic(message, message_length);
    EXPECT_LT(val1, val2) << diagnostic;
}

void bridge_le_i32(int32_t val1, int32_t val2, const char* message, size_t message_length) noexcept {
    const std::string diagnostic(message, message_length);
    EXPECT_LE(val1, val2) << diagnostic;
}

void bridge_le_f32(float val1, float val2, const char* message, size_t message_length) noexcept {
    const std::string diagnostic(message, message_length);
    EXPECT_LE(val1, val2) << diagnostic;
}

void bridge_le_f64(double val1, double val2, const char* message, size_t message_length) noexcept {
    const std::string diagnostic(message, message_length);
    EXPECT_LE(val1, val2) << diagnostic;
}

void bridge_gt_i32(int32_t val1, int32_t val2, const char* message, size_t message_length) noexcept {
    const std::string diagnostic(message, message_length);
    EXPECT_GT(val1, val2) << diagnostic;
}

void bridge_gt_f32(float val1, float val2, const char* message, size_t message_length) noexcept {
    const std::string diagnostic(message, message_length);
    EXPECT_GT(val1, val2) << diagnostic;
}

void bridge_gt_f64(double val1, double val2, const char* message, size_t message_length) noexcept {
    const std::string diagnostic(message, message_length);
    EXPECT_GT(val1, val2) << diagnostic;
}

void bridge_ge_i32(int32_t val1, int32_t val2, const char* message, size_t message_length) noexcept {
    const std::string diagnostic(message, message_length);
    EXPECT_GE(val1, val2) << diagnostic;
}

void bridge_ge_f32(float val1, float val2, const char* message, size_t message_length) noexcept {
    const std::string diagnostic(message, message_length);
    EXPECT_GE(val1, val2) << diagnostic;
}

void bridge_ge_f64(double val1, double val2, const char* message, size_t message_length) noexcept {
    const std::string diagnostic(message, message_length);
    EXPECT_GE(val1, val2) << diagnostic;
}

void bridge_float_eq_f32(float val1, float val2, const char* message, size_t message_length) noexcept {
    const std::string diagnostic(message, message_length);
    EXPECT_FLOAT_EQ(val1, val2) << diagnostic;
}

void bridge_double_eq_f64(double val1, double val2, const char* message, size_t message_length) noexcept {
    const std::string diagnostic(message, message_length);
    EXPECT_DOUBLE_EQ(val1, val2) << diagnostic;
}

void bridge_near_f32(float val1, float val2, float abs_error, const char* message, size_t message_length) noexcept {
    const std::string diagnostic(message, message_length);
    EXPECT_NEAR(val1, val2, abs_error) << diagnostic;
}

void bridge_near_f64(double val1, double val2, double abs_error, const char* message, size_t message_length) noexcept {
    const std::string diagnostic(message, message_length);
    EXPECT_NEAR(val1, val2, abs_error) << diagnostic;
}

void bridge_true_i32(int32_t condition, bridge_bool fatal, const char* message, size_t message_length) noexcept {
    const std::string diagnostic(message, message_length);
    if (fatal != 0) {
        ASSERT_TRUE(condition != 0) << diagnostic;
    } else {
        EXPECT_TRUE(condition != 0) << diagnostic;
    }
}

void bridge_true_bool(bridge_bool condition, bridge_bool fatal, const char* message, size_t message_length) noexcept {
    const std::string diagnostic(message, message_length);
    if (fatal != 0) {
        ASSERT_TRUE(condition != 0) << diagnostic;
    } else {
        EXPECT_TRUE(condition != 0) << diagnostic;
    }
}

void bridge_false_i32(int32_t condition, const char* message, size_t message_length) noexcept {
    const std::string diagnostic(message, message_length);
    EXPECT_FALSE(condition != 0) << diagnostic;
}

void bridge_false_bool(bridge_bool condition, const char* message, size_t message_length) noexcept {
    const std::string diagnostic(message, message_length);
    EXPECT_FALSE(condition != 0) << diagnostic;
}

void bridge_streq_string(const char* str1, size_t length1, const char* str2, size_t length2, const char* message, size_t message_length) noexcept {
    const std::string diagnostic(message, message_length);
    const std::string left(str1, length1);
    const std::string right(str2, length2);
    EXPECT_STREQ(left.c_str(), right.c_str()) << diagnostic;
}

void bridge_strne_string(const char* str1, size_t length1, const char* str2, size_t length2, const char* message, size_t message_length) noexcept {
    const std::string diagnostic(message, message_length);
    const std::string left(str1, length1);
    const std::string right(str2, length2);
    EXPECT_STRNE(left.c_str(), right.c_str()) << diagnostic;
}

void bridge_strcaseeq_string(const char* str1, size_t length1, const char* str2, size_t length2, const char* message, size_t message_length) noexcept {
    const std::string diagnostic(message, message_length);
    const std::string left(str1, length1);
    const std::string right(str2, length2);
    EXPECT_STRCASEEQ(left.c_str(), right.c_str()) << diagnostic;
}

void bridge_strcasene_string(const char* str1, size_t length1, const char* str2, size_t length2, const char* message, size_t message_length) noexcept {
    const std::string diagnostic(message, message_length);
    const std::string left(str1, length1);
    const std::string right(str2, length2);
    EXPECT_STRCASENE(left.c_str(), right.c_str()) << diagnostic;
}

void bridge_failure(const char* message, size_t message_length) noexcept {
    const std::string diagnostic(message, message_length);
    ADD_FAILURE() << diagnostic;
}
