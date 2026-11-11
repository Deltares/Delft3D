#include "gtest_fortran_wrapper.h"
#include <cassert>
#include <gtest/gtest.h>
#include <string>

static_assert(sizeof(int32_t) == 4 && sizeof(gtest_wrapper_bool_t) == 1, "Unsupported integer sizes");
static_assert(sizeof(float) == 4 && sizeof(double) == 8, "Unsupported real sizes");

void gtest_wrapper_eq_int32(int32_t val1, int32_t val2, gtest_wrapper_bool_t fatal, const char* message, size_t message_length) noexcept {
    assert(message != nullptr);
    const std::string diagnostic(message, message_length);
    if (fatal != 0) {
        ASSERT_EQ(val1, val2) << diagnostic;
    } else {
        EXPECT_EQ(val1, val2) << diagnostic;
    }
}

void gtest_wrapper_eq_float(float val1, float val2, const char* message, size_t message_length) noexcept {
    assert(message != nullptr);
    const std::string diagnostic(message, message_length);
    EXPECT_EQ(val1, val2) << diagnostic;
}

void gtest_wrapper_eq_double(double val1, double val2, const char* message, size_t message_length) noexcept {
    assert(message != nullptr);
    const std::string diagnostic(message, message_length);
    EXPECT_EQ(val1, val2) << diagnostic;
}

void gtest_wrapper_eq_bool(gtest_wrapper_bool_t val1, gtest_wrapper_bool_t val2, const char* message, size_t message_length) noexcept {
    assert(message != nullptr);
    const std::string diagnostic(message, message_length);
    EXPECT_EQ(val1, val2) << diagnostic;
}

void gtest_wrapper_ne_int32(int32_t val1, int32_t val2, const char* message, size_t message_length) noexcept {
    assert(message != nullptr);
    const std::string diagnostic(message, message_length);
    EXPECT_NE(val1, val2) << diagnostic;
}

void gtest_wrapper_ne_float(float val1, float val2, const char* message, size_t message_length) noexcept {
    assert(message != nullptr);
    const std::string diagnostic(message, message_length);
    EXPECT_NE(val1, val2) << diagnostic;
}

void gtest_wrapper_ne_double(double val1, double val2, const char* message, size_t message_length) noexcept {
    assert(message != nullptr);
    const std::string diagnostic(message, message_length);
    EXPECT_NE(val1, val2) << diagnostic;
}

void gtest_wrapper_ne_bool(gtest_wrapper_bool_t val1, gtest_wrapper_bool_t val2, const char* message, size_t message_length) noexcept {
    assert(message != nullptr);
    const std::string diagnostic(message, message_length);
    EXPECT_NE(val1, val2) << diagnostic;
}

void gtest_wrapper_lt_int32(int32_t val1, int32_t val2, const char* message, size_t message_length) noexcept {
    assert(message != nullptr);
    const std::string diagnostic(message, message_length);
    EXPECT_LT(val1, val2) << diagnostic;
}

void gtest_wrapper_lt_float(float val1, float val2, const char* message, size_t message_length) noexcept {
    assert(message != nullptr);
    const std::string diagnostic(message, message_length);
    EXPECT_LT(val1, val2) << diagnostic;
}

void gtest_wrapper_lt_double(double val1, double val2, const char* message, size_t message_length) noexcept {
    assert(message != nullptr);
    const std::string diagnostic(message, message_length);
    EXPECT_LT(val1, val2) << diagnostic;
}

void gtest_wrapper_le_int32(int32_t val1, int32_t val2, const char* message, size_t message_length) noexcept {
    assert(message != nullptr);
    const std::string diagnostic(message, message_length);
    EXPECT_LE(val1, val2) << diagnostic;
}

void gtest_wrapper_le_float(float val1, float val2, const char* message, size_t message_length) noexcept {
    assert(message != nullptr);
    const std::string diagnostic(message, message_length);
    EXPECT_LE(val1, val2) << diagnostic;
}

void gtest_wrapper_le_double(double val1, double val2, const char* message, size_t message_length) noexcept {
    assert(message != nullptr);
    const std::string diagnostic(message, message_length);
    EXPECT_LE(val1, val2) << diagnostic;
}

void gtest_wrapper_gt_int32(int32_t val1, int32_t val2, const char* message, size_t message_length) noexcept {
    assert(message != nullptr);
    const std::string diagnostic(message, message_length);
    EXPECT_GT(val1, val2) << diagnostic;
}

void gtest_wrapper_gt_float(float val1, float val2, const char* message, size_t message_length) noexcept {
    assert(message != nullptr);
    const std::string diagnostic(message, message_length);
    EXPECT_GT(val1, val2) << diagnostic;
}

void gtest_wrapper_gt_double(double val1, double val2, const char* message, size_t message_length) noexcept {
    assert(message != nullptr);
    const std::string diagnostic(message, message_length);
    EXPECT_GT(val1, val2) << diagnostic;
}

void gtest_wrapper_ge_int32(int32_t val1, int32_t val2, const char* message, size_t message_length) noexcept {
    assert(message != nullptr);
    const std::string diagnostic(message, message_length);
    EXPECT_GE(val1, val2) << diagnostic;
}

void gtest_wrapper_ge_float(float val1, float val2, const char* message, size_t message_length) noexcept {
    assert(message != nullptr);
    const std::string diagnostic(message, message_length);
    EXPECT_GE(val1, val2) << diagnostic;
}

void gtest_wrapper_ge_double(double val1, double val2, const char* message, size_t message_length) noexcept {
    assert(message != nullptr);
    const std::string diagnostic(message, message_length);
    EXPECT_GE(val1, val2) << diagnostic;
}

void gtest_wrapper_float_eq_float(float val1, float val2, const char* message, size_t message_length) noexcept {
    assert(message != nullptr);
    const std::string diagnostic(message, message_length);
    EXPECT_FLOAT_EQ(val1, val2) << diagnostic;
}

void gtest_wrapper_double_eq_double(double val1, double val2, const char* message, size_t message_length) noexcept {
    assert(message != nullptr);
    const std::string diagnostic(message, message_length);
    EXPECT_DOUBLE_EQ(val1, val2) << diagnostic;
}

void gtest_wrapper_near_float(float val1, float val2, float abs_error, const char* message, size_t message_length) noexcept {
    assert(message != nullptr);
    const std::string diagnostic(message, message_length);
    EXPECT_NEAR(val1, val2, abs_error) << diagnostic;
}

void gtest_wrapper_near_double(double val1, double val2, double abs_error, const char* message, size_t message_length) noexcept {
    assert(message != nullptr);
    const std::string diagnostic(message, message_length);
    EXPECT_NEAR(val1, val2, abs_error) << diagnostic;
}

void gtest_wrapper_true_int32(int32_t condition, gtest_wrapper_bool_t fatal, const char* message, size_t message_length) noexcept {
    assert(message != nullptr);
    const std::string diagnostic(message, message_length);
    if (fatal != 0) {
        ASSERT_TRUE(condition != 0) << diagnostic;
    } else {
        EXPECT_TRUE(condition != 0) << diagnostic;
    }
}

void gtest_wrapper_true_bool(gtest_wrapper_bool_t condition, gtest_wrapper_bool_t fatal, const char* message, size_t message_length) noexcept {
    assert(message != nullptr);
    const std::string diagnostic(message, message_length);
    if (fatal != 0) {
        ASSERT_TRUE(condition != 0) << diagnostic;
    } else {
        EXPECT_TRUE(condition != 0) << diagnostic;
    }
}

void gtest_wrapper_false_int32(int32_t condition, const char* message, size_t message_length) noexcept {
    assert(message != nullptr);
    const std::string diagnostic(message, message_length);
    EXPECT_FALSE(condition != 0) << diagnostic;
}

void gtest_wrapper_false_bool(gtest_wrapper_bool_t condition, const char* message, size_t message_length) noexcept {
    assert(message != nullptr);
    const std::string diagnostic(message, message_length);
    EXPECT_FALSE(condition != 0) << diagnostic;
}

void gtest_wrapper_streq_string(const char* str1, size_t length1, const char* str2, size_t length2, const char* message, size_t message_length) noexcept {
    assert(str1 != nullptr);
    assert(str2 != nullptr);
    assert(message != nullptr);
    const std::string diagnostic(message, message_length);
    const std::string left(str1, length1);
    const std::string right(str2, length2);
    EXPECT_STREQ(left.c_str(), right.c_str()) << diagnostic;
}

void gtest_wrapper_strne_string(const char* str1, size_t length1, const char* str2, size_t length2, const char* message, size_t message_length) noexcept {
    assert(str1 != nullptr);
    assert(str2 != nullptr);
    assert(message != nullptr);
    const std::string diagnostic(message, message_length);
    const std::string left(str1, length1);
    const std::string right(str2, length2);
    EXPECT_STRNE(left.c_str(), right.c_str()) << diagnostic;
}

void gtest_wrapper_strcaseeq_string(const char* str1, size_t length1, const char* str2, size_t length2, const char* message, size_t message_length) noexcept {
    assert(str1 != nullptr);
    assert(str2 != nullptr);
    assert(message != nullptr);
    const std::string diagnostic(message, message_length);
    const std::string left(str1, length1);
    const std::string right(str2, length2);
    EXPECT_STRCASEEQ(left.c_str(), right.c_str()) << diagnostic;
}

void gtest_wrapper_strcasene_string(const char* str1, size_t length1, const char* str2, size_t length2, const char* message, size_t message_length) noexcept {
    assert(str1 != nullptr);
    assert(str2 != nullptr);
    assert(message != nullptr);
    const std::string diagnostic(message, message_length);
    const std::string left(str1, length1);
    const std::string right(str2, length2);
    EXPECT_STRCASENE(left.c_str(), right.c_str()) << diagnostic;
}

void gtest_wrapper_failure(const char* message, size_t message_length) noexcept {
    assert(message != nullptr);
    const std::string diagnostic(message, message_length);
    ADD_FAILURE() << diagnostic;
}
