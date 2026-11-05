#ifndef GTEST_FORTRAN_WRAPPER_H
#define GTEST_FORTRAN_WRAPPER_H

#include <stddef.h>
#include <stdint.h>

// ABI boolean: Fortran integer(bridge_bool), using the c_int8_t kind.
// Values are canonical 0 (false) or 1 (true).
typedef int8_t bridge_bool;

// Values are passed by value.
// Every character argument has an explicit byte length; no C++ types cross this ABI.
#ifdef __cplusplus
#define BRIDGE_NOEXCEPT noexcept
extern "C" {
#else
#define BRIDGE_NOEXCEPT
#endif

void bridge_eq_i32(int32_t val1, int32_t val2, bridge_bool fatal, const char* message, size_t message_length) BRIDGE_NOEXCEPT;
void bridge_eq_f32(float val1, float val2, const char* message, size_t message_length) BRIDGE_NOEXCEPT;
void bridge_eq_f64(double val1, double val2, const char* message, size_t message_length) BRIDGE_NOEXCEPT;
void bridge_eq_bool(bridge_bool val1, bridge_bool val2, const char* message, size_t message_length) BRIDGE_NOEXCEPT;
void bridge_ne_i32(int32_t val1, int32_t val2, const char* message, size_t message_length) BRIDGE_NOEXCEPT;
void bridge_ne_f32(float val1, float val2, const char* message, size_t message_length) BRIDGE_NOEXCEPT;
void bridge_ne_f64(double val1, double val2, const char* message, size_t message_length) BRIDGE_NOEXCEPT;
void bridge_ne_bool(bridge_bool val1, bridge_bool val2, const char* message, size_t message_length) BRIDGE_NOEXCEPT;
void bridge_lt_i32(int32_t val1, int32_t val2, const char* message, size_t message_length) BRIDGE_NOEXCEPT;
void bridge_lt_f32(float val1, float val2, const char* message, size_t message_length) BRIDGE_NOEXCEPT;
void bridge_lt_f64(double val1, double val2, const char* message, size_t message_length) BRIDGE_NOEXCEPT;
void bridge_le_i32(int32_t val1, int32_t val2, const char* message, size_t message_length) BRIDGE_NOEXCEPT;
void bridge_le_f32(float val1, float val2, const char* message, size_t message_length) BRIDGE_NOEXCEPT;
void bridge_le_f64(double val1, double val2, const char* message, size_t message_length) BRIDGE_NOEXCEPT;
void bridge_gt_i32(int32_t val1, int32_t val2, const char* message, size_t message_length) BRIDGE_NOEXCEPT;
void bridge_gt_f32(float val1, float val2, const char* message, size_t message_length) BRIDGE_NOEXCEPT;
void bridge_gt_f64(double val1, double val2, const char* message, size_t message_length) BRIDGE_NOEXCEPT;
void bridge_ge_i32(int32_t val1, int32_t val2, const char* message, size_t message_length) BRIDGE_NOEXCEPT;
void bridge_ge_f32(float val1, float val2, const char* message, size_t message_length) BRIDGE_NOEXCEPT;
void bridge_ge_f64(double val1, double val2, const char* message, size_t message_length) BRIDGE_NOEXCEPT;
void bridge_float_eq_f32(float val1, float val2, const char* message, size_t message_length) BRIDGE_NOEXCEPT;
void bridge_double_eq_f64(double val1, double val2, const char* message, size_t message_length) BRIDGE_NOEXCEPT;
void bridge_near_f32(float val1, float val2, float abs_error, const char* message, size_t message_length) BRIDGE_NOEXCEPT;
void bridge_near_f64(double val1, double val2, double abs_error, const char* message, size_t message_length) BRIDGE_NOEXCEPT;
void bridge_true_i32(int32_t condition, bridge_bool fatal, const char* message, size_t message_length) BRIDGE_NOEXCEPT;
void bridge_true_bool(bridge_bool condition, bridge_bool fatal, const char* message, size_t message_length) BRIDGE_NOEXCEPT;
void bridge_false_i32(int32_t condition, const char* message, size_t message_length) BRIDGE_NOEXCEPT;
void bridge_false_bool(bridge_bool condition, const char* message, size_t message_length) BRIDGE_NOEXCEPT;
void bridge_streq_string(const char* str1, size_t length1, const char* str2, size_t length2, const char* message, size_t message_length) BRIDGE_NOEXCEPT;
void bridge_strne_string(const char* str1, size_t length1, const char* str2, size_t length2, const char* message, size_t message_length) BRIDGE_NOEXCEPT;
void bridge_strcaseeq_string(const char* str1, size_t length1, const char* str2, size_t length2, const char* message, size_t message_length) BRIDGE_NOEXCEPT;
void bridge_strcasene_string(const char* str1, size_t length1, const char* str2, size_t length2, const char* message, size_t message_length) BRIDGE_NOEXCEPT;
void bridge_failure(const char* message, size_t message_length) BRIDGE_NOEXCEPT;

#ifdef __cplusplus
}
#endif
#undef BRIDGE_NOEXCEPT
#endif
