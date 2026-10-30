#ifndef GTEST_FORTRAN_WRAPPER_H
#define GTEST_FORTRAN_WRAPPER_H

#include <stddef.h>
#include <stdint.h>

// Values are passed by value. Logical and fatal flags are canonical 0 or 1.
// Every character argument has an explicit byte length; no C++ types cross this ABI.
#ifdef __cplusplus
#define GFW_NOEXCEPT noexcept
extern "C" {
#else
#define GFW_NOEXCEPT
#endif

void gfw_eq_i32(int32_t val1, int32_t val2, int8_t fatal, const char* message, size_t message_length) GFW_NOEXCEPT;
void gfw_eq_f32(float val1, float val2, const char* message, size_t message_length) GFW_NOEXCEPT;
void gfw_eq_f64(double val1, double val2, const char* message, size_t message_length) GFW_NOEXCEPT;
void gfw_eq_bool(int8_t val1, int8_t val2, const char* message, size_t message_length) GFW_NOEXCEPT;
void gfw_ne_i32(int32_t val1, int32_t val2, const char* message, size_t message_length) GFW_NOEXCEPT;
void gfw_ne_f32(float val1, float val2, const char* message, size_t message_length) GFW_NOEXCEPT;
void gfw_ne_f64(double val1, double val2, const char* message, size_t message_length) GFW_NOEXCEPT;
void gfw_ne_bool(int8_t val1, int8_t val2, const char* message, size_t message_length) GFW_NOEXCEPT;
void gfw_lt_i32(int32_t val1, int32_t val2, const char* message, size_t message_length) GFW_NOEXCEPT;
void gfw_lt_f32(float val1, float val2, const char* message, size_t message_length) GFW_NOEXCEPT;
void gfw_lt_f64(double val1, double val2, const char* message, size_t message_length) GFW_NOEXCEPT;
void gfw_le_i32(int32_t val1, int32_t val2, const char* message, size_t message_length) GFW_NOEXCEPT;
void gfw_le_f32(float val1, float val2, const char* message, size_t message_length) GFW_NOEXCEPT;
void gfw_le_f64(double val1, double val2, const char* message, size_t message_length) GFW_NOEXCEPT;
void gfw_gt_i32(int32_t val1, int32_t val2, const char* message, size_t message_length) GFW_NOEXCEPT;
void gfw_gt_f32(float val1, float val2, const char* message, size_t message_length) GFW_NOEXCEPT;
void gfw_gt_f64(double val1, double val2, const char* message, size_t message_length) GFW_NOEXCEPT;
void gfw_ge_i32(int32_t val1, int32_t val2, const char* message, size_t message_length) GFW_NOEXCEPT;
void gfw_ge_f32(float val1, float val2, const char* message, size_t message_length) GFW_NOEXCEPT;
void gfw_ge_f64(double val1, double val2, const char* message, size_t message_length) GFW_NOEXCEPT;
void gfw_float_eq_f32(float val1, float val2, const char* message, size_t message_length) GFW_NOEXCEPT;
void gfw_double_eq_f64(double val1, double val2, const char* message, size_t message_length) GFW_NOEXCEPT;
void gfw_near_f32(float val1, float val2, float abs_error, const char* message, size_t message_length) GFW_NOEXCEPT;
void gfw_near_f64(double val1, double val2, double abs_error, const char* message, size_t message_length) GFW_NOEXCEPT;
void gfw_true_i32(int32_t condition, int8_t fatal, const char* message, size_t message_length) GFW_NOEXCEPT;
void gfw_true_bool(int8_t condition, int8_t fatal, const char* message, size_t message_length) GFW_NOEXCEPT;
void gfw_false_i32(int32_t condition, const char* message, size_t message_length) GFW_NOEXCEPT;
void gfw_false_bool(int8_t condition, const char* message, size_t message_length) GFW_NOEXCEPT;
void gfw_streq_string(const char* str1, size_t length1, const char* str2, size_t length2, const char* message, size_t message_length) GFW_NOEXCEPT;
void gfw_strne_string(const char* str1, size_t length1, const char* str2, size_t length2, const char* message, size_t message_length) GFW_NOEXCEPT;
void gfw_strcaseeq_string(const char* str1, size_t length1, const char* str2, size_t length2, const char* message, size_t message_length) GFW_NOEXCEPT;
void gfw_strcasene_string(const char* str1, size_t length1, const char* str2, size_t length2, const char* message, size_t message_length) GFW_NOEXCEPT;
void gfw_failure(const char* message, size_t message_length) GFW_NOEXCEPT;

#ifdef __cplusplus
}
#endif
#undef GFW_NOEXCEPT
#endif
