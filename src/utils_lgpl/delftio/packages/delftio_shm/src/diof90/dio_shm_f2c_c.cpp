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
//  dio_shm_f2c_c.cpp: C-part of F90 to C(++) interface for DioSharedMem
//
//  stef.hummel@deltares.nl
//
//  (c) Deltares, july 2002
//



#include <ctype.h>   // for isspace, tolower, etc.


#include "dio_shm.h"
#include "dio_shm_datablock.h"
#include "dio_shm_f2c_c.h"


///
/// All Interface functions are called by dio_shm_f2c_c.F90.
/// The Fortran code keeps track of an integer, which is the 'handle'
/// (i.e. pointer) to the C++ object.
/// In this source, this handle is called cHandle, in the F90 source: cppHandle
///

///
/// Basic functions
///

//
// Convert Fortran (header|data) part flag to C++ enumeration
//

static DioShmPart fPart2Cpp(
    int *fDataPart
    )
{
    return ( (*fDataPart == 0) ? DioShmHeaderPart : DioShmDataPart);
}

//
// Convert Fortran name string to C string
// Remove all characters in the file name that are not accepted by
// the Windows Memory mapped file modules.
//

static void textF2C(
    char * name,
    int nameLen,
    char * locName
    )
{
    char *p;
    int locNameLen = (nameLen < MAX_DS_NAME_LEN) ? nameLen : MAX_DS_NAME_LEN;

    strncpy(locName, name, locNameLen);
    locName[locNameLen] = '\0';
    p = locName+locNameLen-1;
    while(isspace(*p)) p--;
    *(p+1) = '\0';

    for (p = locName ; *p!='\0' ; p++)
    {
        if ( *p==':' ) *p = '-';
        if ( *p=='\\') *p = '-';
        if ( *p=='/' ) *p = '-';
        if ( *p=='.' ) *p = '_';
    }
}


//
// Convert name string to lower case
//
static void nameToLower(
    char * name
    )
{
    char *p;
    for ( p = name; *p != '\0' ; p++ )
        if ( isupper(*p) ) *p = tolower(*p);
}


///
/// INTERFACE functions
///

//
// Define a shared memory data set (called by 'Putter')
//
void STDCALL DIO_SHM_F2C_DS_DEFINE_C(
    int *retVal,
    int *headerSize,
    int *dataSize,
    int *memType,
    long long int *cHandle,
    char * name,
    int nameLen
    )
{
    char locName[MAX_DS_NAME_LEN+1];
    DioShmMemType mType = DioShmMemUnknown;
    *retVal = -1;

    textF2C(name, nameLen, locName);
    nameToLower(locName);
    mType = (DioShmMemType) *memType;

    *cHandle = (long long int) new DioShmDs(*headerSize, *dataSize, mType, locName);

    if (*cHandle != (long long int) NULL)
    {
        if (( (DioShmDs *) *cHandle )->InfoIsValid() )
        {
            *retVal = 0;
        }
    }
}


//
// Attach to a shared memory data set (called by 'Getter')
//
void STDCALL DIO_SHM_F2C_DS_GETINFO_C(
    int *retVal,
    int *memType,
    long long int *cHandle,
    char * name,
    int nameLen
    )
{
    char locName[MAX_DS_NAME_LEN+1];
    DioShmMemType mType = DioShmMemUnknown;
    *retVal = -1;

    textF2C(name, nameLen, locName);
    nameToLower(locName);
    mType = (DioShmMemType) *memType;

    *cHandle = (long long int) new DioShmDs(mType, locName);

    if (*cHandle != (long long int) NULL)
    {
        if (( (DioShmDs *) *cHandle )->InfoIsValid() )
        {
            *retVal = 0;
        }
    }
}


//
// Destroy a shared memory data set (called by Putter and Getter)
//
void STDCALL DIO_SHM_F2C_DS_DESTROY_C(
    long long int *cHandle
    )
{
    delete ( (DioShmDs *) *cHandle );
}



//
// Set dataset sizes for both header and data part
//
void STDCALL DIO_SHM_F2C_DS_SETSIZE_C(
    int *retVal,
    long long int *cHandle,
    int *hSize,
    int *dSize
    )
{
    *retVal = ( (DioShmDs *) *cHandle )->SetSize(*hSize, *dSize);
}


//
// Set dataset sizes for header or data part
//
void STDCALL DIO_SHM_F2C_DS_SETSIZEPART_C(
    int *retVal,
    long long int *cHandle,
    int *dataPart,
    int * size
    )
{
    *retVal = ( (DioShmDs *) *cHandle )->SetSizeForPart(fPart2Cpp(dataPart), *size);
}


//
// Synchronization functions (TODO: Rewind)
//
void STDCALL DIO_SHM_F2C_START_WRITE_C(
    long long int *cHandle,
    int *dataPart,
    int * retVal
    )
{
    *retVal = ( (DioShmDs *) *cHandle )->StartWrite(fPart2Cpp(dataPart));
}

void STDCALL DIO_SHM_F2C_END_WRITE_C(
    long long int *cHandle,
    int *dataPart
    )
{
    ( (DioShmDs *) *cHandle )->EndWrite(fPart2Cpp(dataPart));
}

void STDCALL DIO_SHM_F2C_START_READ_C(
    long long int *cHandle,
    int *dataPart,
    int * retVal
    )
{
    *retVal = ( (DioShmDs *) *cHandle )->StartRead(fPart2Cpp(dataPart));
}

void STDCALL DIO_SHM_F2C_END_READ_C(
    long long int *cHandle,
    int *dataPart
    )
{
    ( (DioShmDs *) *cHandle )->EndRead(fPart2Cpp(dataPart));
}


//
// Write functions for various (arrays of) data primitives
//
void STDCALL DIO_SHM_F2C_DS_WRITE_REALS_C(
    long long int *cHandle,
    int *dataPart,
    int *nReals,
    float *reals
    )
{
    ( (DioShmDs *) *cHandle )->Write(fPart2Cpp(dataPart), (*nReals)*sizeof(float), (char *) reals);
}

void STDCALL DIO_SHM_F2C_DS_WRITE_DOUBLES_C(
    long long int *cHandle,
    int *dataPart,
    int *nDoubles,
    double *doubles
    )
{
    ( (DioShmDs *) *cHandle )->Write(fPart2Cpp(dataPart), (*nDoubles)*sizeof(double), (char *) doubles);
}

void STDCALL DIO_SHM_F2C_DS_WRITE_INTS_C(
    long long int *cHandle,
    int *dataPart,
    int *nInts,
    int *ints
    )
{
    ( (DioShmDs *) *cHandle )->Write(fPart2Cpp(dataPart), (*nInts)*sizeof(int), (char *) ints);
}

void STDCALL DIO_SHM_F2C_DS_WRITE_INT_C(
    long long int *cHandle,
    int *dataPart,
    int *integer
    )
{
    ( (DioShmDs *) *cHandle )->Write(fPart2Cpp(dataPart), sizeof(int), (char *) integer);
}

void STDCALL DIO_SHM_F2C_DS_WRITE_CHARS_C(
    long long int *cHandle,
    int *dataPart,
    int *nChars,
    char *chars,
    int charsLen
    )
{
    ( (DioShmDs *) *cHandle )->Write(fPart2Cpp(dataPart), (*nChars)*sizeof(char), (char *) chars);
}


//
// Read functions for various (arrays of) data primitives
//
int STDCALL DIO_SHM_F2C_DS_READ_REALS_C(
    long long int *cHandle,
    int *dataPart,
    int *nReals,
    float *reals
    )
{
    return ( (DioShmDs *) *cHandle )->Read(fPart2Cpp(dataPart), (*nReals)*sizeof(float), (char *) reals);
}

int STDCALL DIO_SHM_F2C_DS_READ_DOUBLES_C(
    long long int *cHandle,
    int *dataPart,
    int *nDoubles,
    double *doubles
    )
{
    return ( (DioShmDs *) *cHandle )->Read(fPart2Cpp(dataPart), (*nDoubles)*sizeof(double), (char *) doubles);
}

int STDCALL DIO_SHM_F2C_DS_READ_INTS_C(
    long long int *cHandle,
    int *dataPart,
    int *nInts,
    int *ints
    )
{
    return ( (DioShmDs *) *cHandle )->Read(fPart2Cpp(dataPart), (*nInts)*sizeof(int), (char *) ints);
}

int STDCALL DIO_SHM_F2C_DS_READ_INT_C(
    long long int *cHandle,
    int *dataPart,
    int *integer
    )
{
    return ( (DioShmDs *) *cHandle )->Read(fPart2Cpp(dataPart), sizeof(int), (char *) integer);
}

int STDCALL DIO_SHM_F2C_DS_READ_CHARS_C(
    long long int *cHandle,
    int *dataPart,
    int *nChars,
    char *chars,
    int charsLen
    )
{
    return ( (DioShmDs *) *cHandle )->Read(fPart2Cpp(dataPart), (*nChars)*sizeof(char), (char *) chars);
}


///
/// Named Datablock Interface functions
///

//
// Put/Get (array of) double(s) to/from a Named Datablock
//

void STDCALL DIO_SHM_F2C_PUTDB_DOUBLES_C(
    int *nDoubles,
    double *doubles,
    char * name,
    int nameLen
    )
{
    char locName[MAX_DS_NAME_LEN+1];

    textF2C(name, nameLen, locName);
    nameToLower(locName);

    DioShmPutDataBlock(locName, (*nDoubles)*sizeof(double), (char *) doubles);
}


void STDCALL DIO_SHM_F2C_GETDB_DOUBLES_C(
    int *retVal,
    int *nDoubles,
    double *doubles,
    char * name,
    int nameLen
    )
{
    char locName[MAX_DS_NAME_LEN+1];

    textF2C(name, nameLen, locName);
    nameToLower(locName);

    *retVal = DioShmGetDataBlock(locName, (*nDoubles)*sizeof(double), (char *) doubles);
}


void STDCALL DIO_SHM_F2C_FREEDB_C(
    char * name,
    int nameLen
    )
{
    char locName[MAX_DS_NAME_LEN+1];

    textF2C(name, nameLen, locName);
    nameToLower(locName);

    DioShmFreeDataBlock(locName);
}


//
// Cleanup the Named Datablock administration
//

void STDCALL DIO_SHM_F2C_DBCLEANUP_C(void)
{
    DioShmDataBlockCleanup();
}

