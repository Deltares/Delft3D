/* -*- c-file-style: "stroustrup" -*- */
/* Please use the stroustrup coding standard: */
namespace BMI
{
#pragma once

#ifndef BMI_API_H
#define BMI_API_H

	typedef enum {
		ALL,
		DEBUG,
		INFO,
		WARNING,
		ERRORS,
		FATAL,
		NONE
	} Level;

#define BMI_API_VERSION_MAJOR 1
#define BMI_API_VERSION_MINOR 0

#if defined _WIN32
#define BMI_API __declspec(dllexport)
	/* Calling convention, stdcall in windows, cdecl in the rest of the world */
#define BMI_CALLCONV __stdcall
#else
#define BMI_API
#define BMI_CALLCONV
#endif


#define MAXSTRINGLEN 1024
#define MAXDIMS 6

#ifdef __cplusplus
	extern "C" {
#endif

		/* control functions. These return an error code. */
		BMI_API int initialize(const char *config_file);

		BMI_API int update(double dt);

		BMI_API int finalize();

		/* time control functions */
		BMI_API void get_start_time(double *t);

		BMI_API void get_end_time(double *t);

		BMI_API void get_current_time(double *t);

		BMI_API void get_time_step(double *dt);

		/* variable info */
		BMI_API void get_var_shape(const char *name, int shape[MAXDIMS]);

		BMI_API void get_var_rank(const char *name, int *rank);

		BMI_API void get_var_type(const char *name, char *type);

		BMI_API void get_var_count(int *count);

		BMI_API void get_var_name(int index, char *name);

		/* get a pointer pointer - a reference to a multidimensional array */
		BMI_API void get_var(const char *name, void **ptr);

		/* Set the variable from contiguous memory referenced to by ptr */
		BMI_API void set_var(const char *name, const void *ptr);

		/* Set a slice of the variable from contiguous memory using start / count multi-dimensional indices */
		BMI_API void set_var_slice(const char *name, const int *start, const int *count, const void *ptr);

		/* logger to be set from outside so we can log messages */
		typedef void (BMI_CALLCONV *BMILogger)(Level level, const char *msg);

		/* set logger by setting a pointer to the log function */
		BMI_API void set_logger(BMILogger logger);
#ifdef __cplusplus
	}
#endif

#endif
}