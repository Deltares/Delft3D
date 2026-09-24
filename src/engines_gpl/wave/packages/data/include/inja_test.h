#ifndef INJA_TEST_H
#define INJA_TEST_H

#ifdef __cplusplus
extern "C" {
#endif

typedef struct inja_context inja_context;

/// Creates an empty persistent context for inja template rendering.
inja_context* inja_create_context(void);

/// Adds or replaces a string value in the context. Returns 0 on success.
int inja_add_string(inja_context* context, const char* key, const char* value);

/// Destroys a context created by inja_create_context.
void inja_destroy_context(inja_context* context);

/// Renders template_file with the context and writes the result to dest_file.
/// Returns 0 on success, or -1 on invalid arguments, I/O, or rendering errors.
int inja_render_file(inja_context* context, const char* template_file,
					 const char* dest_file);

/// Copies the context's last error message to result and NUL-terminates it.
/// Returns the number of characters copied, or -1 for invalid arguments.
int inja_get_last_error(const inja_context* context, char* result, int result_size);

#ifdef __cplusplus
}
#endif

#endif /* INJA_TEST_H */
