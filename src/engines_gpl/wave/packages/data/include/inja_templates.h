#ifndef INJA_TEST_H
#define INJA_TEST_H

#ifdef __cplusplus
extern "C" {
#endif

typedef struct inja_context inja_context;

/**
 * Creates a new Inja context.
 * Returns a pointer to the newly created context, or nullptr if creation fails.
 */
inja_context* inja_create_context(void);

/**
 * Adds or replaces a string value in the context. Returns 0 on success.
 */
int inja_add_string(inja_context* context, const char* key, const char* value);

/**
 * Adds or replaces a string value at the context array. Creates the array if it does not exist.
 * Returns 0 on success.
 */
int inja_add_string_to_array(inja_context* context, const char* key, const char* value);

/**
 * Adds or replaces a string value in an object within the context. Creates the object if it does not exist.
 * Returns 0 on success.
 */
int inja_add_string_to_object(inja_context* context, const char* key, const char* object_key, const char* value);

/**
 * Destroys a context created by inja_create_context.
 */
void inja_destroy_context(inja_context* context);

/**
 * Renders template_file with the context and writes the result to dest_file.
 * Returns 0 on success, or -1 on invalid arguments, I/O, or rendering errors.
 */
int inja_render_file(inja_context* context, const char* template_file,
					 const char* dest_file);

/**
 * Retrieves the last error message from the specified Inja context.
 * The error message is copied into the provided result buffer, which must have a size of at least result_size.
 * Returns the number of characters copied, or -1 if an error occurs.
 */
int inja_get_last_error(const inja_context* context, char* result, int result_size);

#ifdef __cplusplus
}
#endif

#endif /* INJA_TEST_H */
