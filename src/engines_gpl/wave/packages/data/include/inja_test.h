#ifndef INJA_TEST_H
#define INJA_TEST_H

#ifdef __cplusplus
extern "C" {
#endif

/// Test hook that renders an inja template, callable from Fortran/C.
///
/// The template text and the value of the template variable `name` are passed
/// in as NUL-terminated strings. The rendered text is copied into `result`,
/// truncated to `result_size` - 1 characters and NUL-terminated.
///
/// Returns the number of characters written to `result`, or -1 on failure.
int inja_render_test(const char* template_text, const char* name, char* result, int result_size);

#ifdef __cplusplus
}
#endif

#endif /* INJA_TEST_H */
