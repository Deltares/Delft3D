#ifndef GTEST_FORTRAN_WRAPPER_H
#define GTEST_FORTRAN_WRAPPER_H

#include <stddef.h>
#include <stdint.h>

// ABI boolean: Fortran integer(gtest_wrapper_bool_t), using the c_int8_t kind.
// Values are canonical 0 (false) or 1 (true).
typedef int8_t gtest_wrapper_bool_t;

// Values are passed by value.
// Every character argument has an explicit byte length; no C++ types cross this ABI.
// Character pointers must be non-null, even when their byte length is zero.
#ifdef __cplusplus
#define GTEST_WRAPPER_NOEXCEPT noexcept
extern "C" {
#else
#define GTEST_WRAPPER_NOEXCEPT
#endif

void gtest_wrapper_eq_int32(int32_t val1, int32_t val2, gtest_wrapper_bool_t fatal, const char* message, size_t message_length) GTEST_WRAPPER_NOEXCEPT;
void gtest_wrapper_eq_float(float val1, float val2, const char* message, size_t message_length) GTEST_WRAPPER_NOEXCEPT;
void gtest_wrapper_eq_double(double val1, double val2, const char* message, size_t message_length) GTEST_WRAPPER_NOEXCEPT;
void gtest_wrapper_eq_bool(gtest_wrapper_bool_t val1, gtest_wrapper_bool_t val2, const char* message, size_t message_length) GTEST_WRAPPER_NOEXCEPT;
void gtest_wrapper_ne_int32(int32_t val1, int32_t val2, const char* message, size_t message_length) GTEST_WRAPPER_NOEXCEPT;
void gtest_wrapper_ne_float(float val1, float val2, const char* message, size_t message_length) GTEST_WRAPPER_NOEXCEPT;
void gtest_wrapper_ne_double(double val1, double val2, const char* message, size_t message_length) GTEST_WRAPPER_NOEXCEPT;
void gtest_wrapper_ne_bool(gtest_wrapper_bool_t val1, gtest_wrapper_bool_t val2, const char* message, size_t message_length) GTEST_WRAPPER_NOEXCEPT;
void gtest_wrapper_lt_int32(int32_t val1, int32_t val2, const char* message, size_t message_length) GTEST_WRAPPER_NOEXCEPT;
void gtest_wrapper_lt_float(float val1, float val2, const char* message, size_t message_length) GTEST_WRAPPER_NOEXCEPT;
void gtest_wrapper_lt_double(double val1, double val2, const char* message, size_t message_length) GTEST_WRAPPER_NOEXCEPT;
void gtest_wrapper_le_int32(int32_t val1, int32_t val2, const char* message, size_t message_length) GTEST_WRAPPER_NOEXCEPT;
void gtest_wrapper_le_float(float val1, float val2, const char* message, size_t message_length) GTEST_WRAPPER_NOEXCEPT;
void gtest_wrapper_le_double(double val1, double val2, const char* message, size_t message_length) GTEST_WRAPPER_NOEXCEPT;
void gtest_wrapper_gt_int32(int32_t val1, int32_t val2, const char* message, size_t message_length) GTEST_WRAPPER_NOEXCEPT;
void gtest_wrapper_gt_float(float val1, float val2, const char* message, size_t message_length) GTEST_WRAPPER_NOEXCEPT;
void gtest_wrapper_gt_double(double val1, double val2, const char* message, size_t message_length) GTEST_WRAPPER_NOEXCEPT;
void gtest_wrapper_ge_int32(int32_t val1, int32_t val2, const char* message, size_t message_length) GTEST_WRAPPER_NOEXCEPT;
void gtest_wrapper_ge_float(float val1, float val2, const char* message, size_t message_length) GTEST_WRAPPER_NOEXCEPT;
void gtest_wrapper_ge_double(double val1, double val2, const char* message, size_t message_length) GTEST_WRAPPER_NOEXCEPT;
void gtest_wrapper_float_eq_float(float val1, float val2, const char* message, size_t message_length) GTEST_WRAPPER_NOEXCEPT;
void gtest_wrapper_double_eq_double(double val1, double val2, const char* message, size_t message_length) GTEST_WRAPPER_NOEXCEPT;
void gtest_wrapper_near_float(float val1, float val2, float abs_error, const char* message, size_t message_length) GTEST_WRAPPER_NOEXCEPT;
void gtest_wrapper_near_double(double val1, double val2, double abs_error, const char* message, size_t message_length) GTEST_WRAPPER_NOEXCEPT;
void gtest_wrapper_true_int32(int32_t condition, gtest_wrapper_bool_t fatal, const char* message, size_t message_length) GTEST_WRAPPER_NOEXCEPT;
void gtest_wrapper_true_bool(gtest_wrapper_bool_t condition, gtest_wrapper_bool_t fatal, const char* message, size_t message_length) GTEST_WRAPPER_NOEXCEPT;
void gtest_wrapper_false_int32(int32_t condition, const char* message, size_t message_length) GTEST_WRAPPER_NOEXCEPT;
void gtest_wrapper_false_bool(gtest_wrapper_bool_t condition, const char* message, size_t message_length) GTEST_WRAPPER_NOEXCEPT;
void gtest_wrapper_streq_string(const char* str1, size_t length1, const char* str2, size_t length2, const char* message, size_t message_length) GTEST_WRAPPER_NOEXCEPT;
void gtest_wrapper_strne_string(const char* str1, size_t length1, const char* str2, size_t length2, const char* message, size_t message_length) GTEST_WRAPPER_NOEXCEPT;
void gtest_wrapper_strcaseeq_string(const char* str1, size_t length1, const char* str2, size_t length2, const char* message, size_t message_length) GTEST_WRAPPER_NOEXCEPT;
void gtest_wrapper_strcasene_string(const char* str1, size_t length1, const char* str2, size_t length2, const char* message, size_t message_length) GTEST_WRAPPER_NOEXCEPT;
void gtest_wrapper_failure(const char* message, size_t message_length) GTEST_WRAPPER_NOEXCEPT;

#ifdef __cplusplus
}
#endif
#undef GTEST_WRAPPER_NOEXCEPT
#endif
