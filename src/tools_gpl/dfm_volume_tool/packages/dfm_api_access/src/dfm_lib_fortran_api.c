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
#  define DFM_GENERATE_VOLUME_TABLES  DFM_GENERATE_VOLUME_TABLES
#  define WRITE_1D_FLOWGEOM_UGRID WRITE_1D_FLOWGEOM_UGRID
#  define STDCALL
#elif defined(linux)
#  include "config.h"
#  define DFM_GENERATE_VOLUME_TABLES FC_FUNC(dfm_generate_volume_tables,DFM_GENERATE_VOLUME_TABLES)
#  define WRITE_1D_FLOWGEOM_UGRID FC_FUNC(write_1d_flowgeom_ugrid,WRITE_1D_FLOWGEOM_UGRID)
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
	int64_t* sharedDLLHandle,
	char* fun_name);

long STDCALL DFM_GENERATE_VOLUME_TABLES(int64_t* sharedDLLHandle, double* increment)
{
	typedef void* (STDCALL* MyProc)(double);
	MyProc proc = (MyProc)GetDllProcedure(sharedDLLHandle, "dfm_generate_volume_tables");


	if (proc != NULL)
	{
		(void*)(*proc)(*increment);
        return 0;
	} 
	return -1;
}

long STDCALL WRITE_1D_FLOWGEOM_UGRID(int64_t* sharedDLLHandle, int64_t* ncid)
{
	typedef void* (STDCALL* MyProc)(int64_t*);
	MyProc proc = (MyProc)GetDllProcedure(sharedDLLHandle, "write_1D_flowgeom_ugrid");


	if (proc != NULL)
	{
		(void*)(*proc)(ncid);
		return 0;
	}
	return -1;
}


