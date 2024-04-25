//---- LGPL --------------------------------------------------------------------
//
// Copyright (C)  Stichting Deltares, 2011-2024.
//
// This library is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation version 2.1.
//
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public
// License along with this library; if not, see <http://www.gnu.org/licenses/>.
//
// contact: delft3d.support@deltares.nl
// Stichting Deltares
// P.O. Box 177
// 2600 MH Delft, The Netherlands
//
// All indications and logos of, and references to, "Delft3D" and "Deltares"
// are registered trademarks of Stichting Deltares, and remain the property of
// Stichting Deltares. All rights reserved.
//
//------------------------------------------------------------------------------
// $Id$
// $HeadURL$
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>

#include "so_fortran_api.h"

#ifndef min
#  define min(a,b) (a)<(b) ? (a) : (b)
#  define max(a,b) (a)>(b) ? (a) : (b)
#endif

#if defined(WIN32)
#  include <windows.h>
#elif defined(salford32)
#  include <windows.h>
#elif defined(linux)
#  include <dlfcn.h>
#endif

#if defined(WIN32)
#  define BMI_INITIALIZE        BMI_INITIALIZE
#  define BMI_UPDATE            BMI_UPDATE
#  define BMI_FINALIZE          BMI_FINALIZE
#  define BMI_GET_START_TIME    BMI_GET_START_TIME
#  define BMI_GET_END_TIME      BMI_GET_END_TIME
#  define BMI_GET_CURRENT_TIME  BMI_GET_CURRENT_TIME
#  define BMI_GET_TIME_STEP     BMI_GET_TIME_STEP
#  define BMI_GET_VAR_TYPE      BMI_GET_VAR_TYPE
#  define BMI_GET_VAR_RANK      BMI_GET_VAR_RANK
#  define BMI_GET_VAR_SHAPE     BMI_GET_VAR_SHAPE
#  define BMI_SET_VAR           BMI_SET_VAR
#  define BMI_GET_VAR           BMI_GET_VAR
#  define BMI_GET_VAR_POINTER   BMI_GET_VAR_POINTER
#  define STDCALL
#elif defined(linux)
#  include "config.h"
#  define BMI_INITIALIZE        FC_FUNC(bmi_initialize,BMI_INITIALIZE)
#  define BMI_UPDATE            FC_FUNC(bmi_update,BMI_UPDATE)
#  define BMI_FINALIZE          FC_FUNC(bmi_finalize,BMI_FINALIZE)
#  define BMI_GET_START_TIME    FC_FUNC(bmi_get_start_time,BMI_GET_START_TIME)
#  define BMI_GET_END_TIME      FC_FUNC(bmi_get_end_time,BMI_GET_END_TIME)
#  define BMI_GET_CURRENT_TIME  FC_FUNC(bmi_get_current_time,BMI_GET_CURRENT_TIME)
#  define BMI_GET_TIME_STEP     FC_FUNC(bmi_get_time_step,BMI_GET_TIME_STEP)
#  define BMI_GET_VAR_TYPE      FC_FUNC(bmi_get_var_type,BMI_GET_VAR_TYPE)
#  define BMI_GET_VAR_RANK      FC_FUNC(bmi_get_var_rank,BMI_GET_VAR_RANK)
#  define BMI_GET_VAR_SHAPE     FC_FUNC(bmi_get_var_shape,BMI_GET_VAR_SHAPE)
#  define BMI_SET_VAR           FC_FUNC(bmi_set_var,BMI_SET_VAR)
#  define BMI_GET_VAR           FC_FUNC(bmi_get_var,BMI_GET_VAR)
#  define BMI_GET_VAR_POINTER   FC_FUNC(bmi_get_var_pointer,BMI_GET_VAR_POINTER)
#  define STDCALL
#endif

/*
*
* Connection routine between F90 (main) -> C (interface) -> F90 (DLL).
* Special attention to the WINAPI define, which is needed if the DLL is written in F90
* Support methods are implemented in shared_library_fortran_api:
* .  strFcpy
* .  RemoveTrailingBlanks_dll
*
*/

#if defined(WIN32)
typedef HMODULE DllHandle;
typedef WINBASEAPI FARPROC WINAPI DllProcedureAddress;
#elif defined(linux)
typedef void * DllHandle;
typedef void * DllProcedureAddress;
#endif

typedef struct {
	DllHandle   dllHandle;
} SharedDLL;


DllProcedureAddress GetDllProcedure(
	int64_t * sharedDLLHandle,
	char * fun_name)
{
	SharedDLL * sharedDLL = (SharedDLL *)(*sharedDLLHandle);

	DllProcedureAddress procedure;
#if defined(WIN32)
	procedure = GetProcAddress(sharedDLL->dllHandle, fun_name);
#elif defined(linux)
	procedure = (DllProcedureAddress)dlsym(sharedDLL->dllHandle, fun_name);
#endif
	return procedure;
}

double DllGetBmiTime(int64_t * sharedDLLHandle, char * function_name)
{
	typedef void * (STDCALL * MyProc)(double *);
	MyProc proc = (MyProc)GetDllProcedure(sharedDLLHandle, function_name);

	double time = -1;

	if (proc != NULL)
	{
		(void *)(*proc)(&time);
	}
	return time;
}

long STDCALL BMI_INITIALIZE(int64_t * sharedDLLHandle,
	char   * config_file,
	int      config_file_len)
{
	typedef void * (STDCALL * MyProc)(char *);
	MyProc proc = (MyProc)GetDllProcedure(sharedDLLHandle, "initialize");

	long error = -1;

	char * c_config_file = strFcpy(config_file, config_file_len);
	RemoveTrailingBlanks_dll(c_config_file);

	if (proc != NULL)
	{
		error = 0;
		(void *)(*proc)(c_config_file);
	}

	free(c_config_file); c_config_file = NULL;

	return error;
}

void STDCALL BMI_GET_START_TIME(int64_t * sharedDLLHandle,
	double * start_time)
{
	*start_time = DllGetBmiTime(sharedDLLHandle, "get_start_time");
}

void STDCALL BMI_GET_END_TIME(int64_t * sharedDLLHandle,
	double * end_time)
{
	*end_time = DllGetBmiTime(sharedDLLHandle, "get_end_time");
}

void STDCALL BMI_GET_CURRENT_TIME(int64_t * sharedDLLHandle,
	double * current_time)
{
	*current_time = DllGetBmiTime(sharedDLLHandle, "get_current_time");
}

void STDCALL BMI_GET_TIME_STEP(int64_t * sharedDLLHandle,
	double * time_step)
{
	*time_step = DllGetBmiTime(sharedDLLHandle, "get_time_step");
}

void STDCALL BMI_GET_VAR(int64_t * sharedDLLHandle,
   char   * var_name,
   double * values,
   int    * num_values,
   int      var_name_len)
{
   typedef void * (STDCALL * MyProc)(char *, double**);
   MyProc proc = (MyProc)GetDllProcedure(sharedDLLHandle, "get_var");

   int  i;					// vs2102 and lower do not support typedefs in combination
   double * bmi_values;    // with local variable declaration, hence declare at start of function

   char * c_var_name = strFcpy(var_name, var_name_len);
   RemoveTrailingBlanks_dll(c_var_name);

   if (proc != NULL)
   {
      (void *)(*proc)(c_var_name, &bmi_values);
      for (i = 0; i < *num_values; i++) {
         values[i] = bmi_values[i];
      }
   }

   free(c_var_name); c_var_name = NULL;
}

void STDCALL BMI_GET_VAR_POINTER(int64_t* sharedDLLHandle, char* var_name, void* xptr)
{
	typedef void* (STDCALL* MyProc)(char*, void*);
	MyProc proc = (MyProc)GetDllProcedure(sharedDLLHandle, "get_var");


	if (proc != NULL)
	{
		(void*)(*proc)(var_name, xptr);
	}
}

void STDCALL BMI_GET_VAR_SHAPE(int64_t * sharedDLLHandle,
   char   * var_name,
   int    * values,
   int      var_name_len)
{
   typedef void * (STDCALL * MyProc)(char *, double**);
   MyProc proc = (MyProc)GetDllProcedure(sharedDLLHandle, "get_var_shape");

   int  i;					// vs2102 and lower do not support typedefs in combination
   int * bmi_values;    // with local variable declaration, hence declare at start of function
   char * c_var_name = strFcpy(var_name, var_name_len);
   if (proc != NULL)
   {
      (void *)(*proc)(var_name, values);
   }

   free(c_var_name); c_var_name = NULL;
}


void STDCALL BMI_SET_VAR(int64_t * sharedDLLHandle,
	char   * var_name,
	double * values,
	int    * num_values,
	int      var_name_len)
{
	typedef void * (STDCALL * MyProc)(char*, double*);
	MyProc proc = (MyProc)GetDllProcedure(sharedDLLHandle, "set_var");

	char * c_var_name = strFcpy(var_name, var_name_len);
	RemoveTrailingBlanks_dll(c_var_name);

	if (proc != NULL)
	{
		(void *)(*proc)(c_var_name, values);
	}

	free(c_var_name); c_var_name = NULL;
}

long STDCALL BMI_UPDATE(int64_t * sharedDLLHandle,
	double * dt)
{
	typedef void * (STDCALL * MyProc)(double);
	MyProc proc = (MyProc)GetDllProcedure(sharedDLLHandle, "update");

	if (proc != NULL)
	{
		(void *)(*proc)(*dt);
		return 0;
	}
	return -1;
}

long STDCALL BMI_FINALIZE(int64_t * sharedDLLHandle)
{
	typedef void * (STDCALL * MyProc)();
	MyProc proc = (MyProc)GetDllProcedure(sharedDLLHandle, "finalize");

	if (proc != NULL)
	{
		(void *)(*proc)();
		return 0;
	}
	return -1;
}
