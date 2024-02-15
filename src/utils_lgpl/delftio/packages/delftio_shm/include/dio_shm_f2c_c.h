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
//
//  dio_shm_f2c_c.h: DelftIO Shared Memory F90 to C interface, C part.
//
//  local include file for C functions to be called by Fortran
//
//  stef.hummel@deltares.nl
//
//  (c) Deltares, Jan 2000
//


#include <string.h>


//
// Function names for FORTRAN-C interface.
//
#if HAVE_CONFIG_H
#   include "config.h"
#   define STDCALL  /* nothing */
    //
    // DioShmDataset functions
    //
#   define DIO_SHM_F2C_DS_DEFINE_C        FC_FUNC(dio_shm_f2c_ds_define_c,DIO_SHM_F2C_DS_DEFINE_C)
#   define DIO_SHM_F2C_DS_GETINFO_C       FC_FUNC(dio_shm_f2c_ds_getinfo_c,DIO_SHM_F2C_DS_GETINFO_C)
#   define DIO_SHM_F2C_DS_SETSIZE_C       FC_FUNC(dio_shm_f2c_ds_setsize_c,DIO_SHM_F2C_DS_SETSIZE_C)
#   define DIO_SHM_F2C_DS_SETSIZEPART_C   FC_FUNC(dio_shm_f2c_ds_setsizepart_c,DIO_SHM_F2C_DS_SETSIZEPART_C)
#   define DIO_SHM_F2C_DS_DESTROY_C       FC_FUNC(dio_shm_f2c_ds_destroy_c,DIO_SHM_F2C_DS_DESTROY_C)
#   define DIO_SHM_F2C_START_WRITE_C      FC_FUNC(dio_shm_f2c_start_write_c,DIO_SHM_F2C_START_WRITE_C)
#   define DIO_SHM_F2C_END_WRITE_C        FC_FUNC(dio_shm_f2c_end_write_c,DIO_SHM_F2C_END_WRITE_C)
#   define DIO_SHM_F2C_START_READ_C       FC_FUNC(dio_shm_f2c_start_read_c,DIO_SHM_F2C_START_READ_C)
#   define DIO_SHM_F2C_END_READ_C         FC_FUNC(dio_shm_f2c_end_read_c,DIO_SHM_F2C_END_READ_C)
#   define DIO_SHM_F2C_DS_WRITE_REALS_C   FC_FUNC(dio_shm_f2c_ds_write_reals_c,DIO_SHM_F2C_DS_WRITE_REALS_C)
#   define DIO_SHM_F2C_DS_WRITE_DOUBLES_C FC_FUNC(dio_shm_f2c_ds_write_doubles_c,DIO_SHM_F2C_DS_WRITE_DOUBLES_C)
#   define DIO_SHM_F2C_DS_WRITE_INTS_C    FC_FUNC(dio_shm_f2c_ds_write_ints_c,DIO_SHM_F2C_DS_WRITE_INTS_C)
#   define DIO_SHM_F2C_DS_WRITE_INT_C     FC_FUNC(dio_shm_f2c_ds_write_int_c,DIO_SHM_F2C_DS_WRITE_INT_C)
#   define DIO_SHM_F2C_DS_WRITE_CHARS_C   FC_FUNC(dio_shm_f2c_ds_write_chars_c,DIO_SHM_F2C_DS_WRITE_CHARS_C)
#   define DIO_SHM_F2C_DS_READ_REALS_C    FC_FUNC(dio_shm_f2c_ds_read_reals_c,DIO_SHM_F2C_DS_READ_REALS_C)
#   define DIO_SHM_F2C_DS_READ_DOUBLES_C  FC_FUNC(dio_shm_f2c_ds_read_doubles_c,DIO_SHM_F2C_DS_READ_DOUBLES_C)
#   define DIO_SHM_F2C_DS_READ_INTS_C     FC_FUNC(dio_shm_f2c_ds_read_ints_c,DIO_SHM_F2C_DS_READ_INTS_C)
#   define DIO_SHM_F2C_DS_READ_INT_C      FC_FUNC(dio_shm_f2c_ds_read_int_c,DIO_SHM_F2C_DS_READ_INT_C)
#   define DIO_SHM_F2C_DS_READ_CHARS_C    FC_FUNC(dio_shm_f2c_ds_read_chars_c,DIO_SHM_F2C_DS_READ_CHARS_C)
    //
    // Dio Shared Mem DataBlock functions
    //
#   define DIO_SHM_F2C_PUTDB_DOUBLES_C    FC_FUNC(dio_shm_f2c_putdb_doubles_c,DIO_SHM_F2C_PUTDB_DOUBLES_C)
#   define DIO_SHM_F2C_GETDB_DOUBLES_C    FC_FUNC(dio_shm_f2c_getdb_doubles_c,DIO_SHM_F2C_GETDB_DOUBLES_C)
#   define DIO_SHM_F2C_FREEDB_C           FC_FUNC(dio_shm_f2c_freedb_c,DIO_SHM_F2C_FREEDB_C)
#   define DIO_SHM_F2C_DBCLEANUP_C        FC_FUNC(dio_shm_f2c_dbcleanup_c,DIO_SHM_F2C_DBCLEANUP_C)
#else
// WIN32
#   define STDCALL  /* nothing */
    //
    // DioShmDataset functions
    //
#   define DIO_SHM_F2C_DS_DEFINE_C        DIO_SHM_F2C_DS_DEFINE_C
#   define DIO_SHM_F2C_DS_GETINFO_C       DIO_SHM_F2C_DS_GETINFO_C
#   define DIO_SHM_F2C_DS_SETSIZE_C       DIO_SHM_F2C_DS_SETSIZE_C
#   define DIO_SHM_F2C_DS_SETSIZEPART_C   DIO_SHM_F2C_DS_SETSIZEPART_C
#   define DIO_SHM_F2C_DS_DESTROY_C       DIO_SHM_F2C_DS_DESTROY_C
#   define DIO_SHM_F2C_START_WRITE_C      DIO_SHM_F2C_START_WRITE_C
#   define DIO_SHM_F2C_END_WRITE_C        DIO_SHM_F2C_END_WRITE_C
#   define DIO_SHM_F2C_START_READ_C       DIO_SHM_F2C_START_READ_C
#   define DIO_SHM_F2C_END_READ_C         DIO_SHM_F2C_END_READ_C
#   define DIO_SHM_F2C_DS_WRITE_REALS_C   DIO_SHM_F2C_DS_WRITE_REALS_C
#   define DIO_SHM_F2C_DS_WRITE_DOUBLES_C DIO_SHM_F2C_DS_WRITE_DOUBLES_C
#   define DIO_SHM_F2C_DS_WRITE_INTS_C    DIO_SHM_F2C_DS_WRITE_INTS_C
#   define DIO_SHM_F2C_DS_WRITE_INT_C     DIO_SHM_F2C_DS_WRITE_INT_C
#   define DIO_SHM_F2C_DS_WRITE_CHARS_C   DIO_SHM_F2C_DS_WRITE_CHARS_C
#   define DIO_SHM_F2C_DS_READ_REALS_C    DIO_SHM_F2C_DS_READ_REALS_C
#   define DIO_SHM_F2C_DS_READ_DOUBLES_C  DIO_SHM_F2C_DS_READ_DOUBLES_C
#   define DIO_SHM_F2C_DS_READ_INTS_C     DIO_SHM_F2C_DS_READ_INTS_C
#   define DIO_SHM_F2C_DS_READ_INT_C      DIO_SHM_F2C_DS_READ_INT_C
#   define DIO_SHM_F2C_DS_READ_CHARS_C    DIO_SHM_F2C_DS_READ_CHARS_C
    //
    // Dio Shared Mem DataBlock functions
    //
#   define DIO_SHM_F2C_PUTDB_DOUBLES_C    DIO_SHM_F2C_PUTDB_DOUBLES_C
#   define DIO_SHM_F2C_GETDB_DOUBLES_C    DIO_SHM_F2C_GETDB_DOUBLES_C
#   define DIO_SHM_F2C_FREEDB_C           DIO_SHM_F2C_FREEDB_C
#   define DIO_SHM_F2C_DBCLEANUP_C        DIO_SHM_F2C_DBCLEANUP_C
#endif


/*------------------------------------------------------------------------------
 *    Function definitions
 */

#if ( defined(__cplusplus) || defined(salford32) )
extern "C" {
#endif

//
// DioShmDataset functions
//

void STDCALL DIO_SHM_F2C_DS_DEFINE_C(int *retVal, int *headerSize, int *dataSize, int *memType, long long int *cHandle, char * name, int nameLen);
void STDCALL DIO_SHM_F2C_DS_GETINFO_C(int *retVal, int *memType, long long int *cHandle, char * name, int nameLen);
void STDCALL DIO_SHM_F2C_DS_SETSIZE_C(int *retVal, long long int *cHandle, int *hSize, int * dSize);
void STDCALL DIO_SHM_F2C_DS_SETSIZEPART_C(int *retVal, long long int *cHandle, int *part, int * dSize);

void STDCALL DIO_SHM_F2C_DS_DESTROY_C(long long int *cHandle);

void STDCALL DIO_SHM_F2C_START_WRITE_C(long long int *cHandle, int *part, int * retVal);
void STDCALL DIO_SHM_F2C_END_WRITE_C(long long int *cHandle, int *part);
void STDCALL DIO_SHM_F2C_START_READ_C(long long int *cHandle, int *part, int * retVal);
void STDCALL DIO_SHM_F2C_END_READ_C(long long int *cHandle, int *part);

void STDCALL DIO_SHM_F2C_DS_WRITE_REALS_C  (long long int *, int *, int *, float *);
void STDCALL DIO_SHM_F2C_DS_WRITE_DOUBLES_C(long long int *, int *, int *, double *);
void STDCALL DIO_SHM_F2C_DS_WRITE_INTS_C   (long long int *, int *, int *, int *);
void STDCALL DIO_SHM_F2C_DS_WRITE_INT_C    (long long int *, int *, int *);
void STDCALL DIO_SHM_F2C_DS_WRITE_CHARS_C  (long long int *, int *, int *, char *, int);

int  STDCALL DIO_SHM_F2C_DS_READ_REALS_C   (long long int *, int *, int *, float *);
int  STDCALL DIO_SHM_F2C_DS_READ_DOUBLES_C (long long int *, int *, int *, double *);
int  STDCALL DIO_SHM_F2C_DS_READ_INTS_C    (long long int *, int *, int *, int *);
int  STDCALL DIO_SHM_F2C_DS_READ_INT_C     (long long int *, int *, int *);
int  STDCALL DIO_SHM_F2C_DS_READ_CHARS_C   (long long int *, int *, int *, char *, int);

//
// Dio Shared Mem DataBlock functions
//

void STDCALL DIO_SHM_F2C_PUTDB_DOUBLES_C(int *, double *, char *, int);
void STDCALL DIO_SHM_F2C_GETDB_DOUBLES_C(int *, int *, double *, char *, int);
void STDCALL DIO_SHM_F2C_FREEDB_C(char *, int);
void STDCALL DIO_SHM_F2C_DBCLEANUP_C(void);

#if ( defined(__cplusplus) || defined(salford32) )
}
#endif

