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
//  dio_shm_datablock.cpp: DelftIO SharedMem Named Datablocks
//
//  stef.hummel@deltares.nl
//
//  (c) Deltares, july 2002
//


#include <string.h>

#include "dio_shm.h"
#include "dio_shm_datablock.h"


//
// Max length of datablock name
// Max num datablocks that can be created
//
#define MAX_DB_NAME_LEN 256
#define MAX_NUM_DATABLOCKS 50


//
// Store for created Datablocks (index in shmDb = DatablockID)
//

typedef struct ShmDatablock_STR {
    char name[MAX_DB_NAME_LEN+1];  // datablock name
    DioShmDs * shmHandle;          // datablock's Shared Mem Dataset
} DioShmDatablock;

DioShmDatablock shmDb[MAX_NUM_DATABLOCKS]; // SharedMem Named Datablocks
static int numShmDs = 0;                   // #SharedMem Named Datablocks


///
/// Functions to create/store/find created Datablocks
///

//
// Find a datablock by it's name
//
static int DioShmFindDatablockID(
    char * name
    )
{
    int retVal = -1;
    int ds;

    for ( ds = 0 ; ds < numShmDs ; ds ++ )
    {
        if (strcmp(shmDb[ds].name, name) == 0 )
        {
            retVal = ds;
            break;
        }
    }
    return retVal;
}

//
// Add a new datablock to store
//
static int DioShmNewDatablockID(
    char * name,
    int dSize
    )
{
    int retVal = -1;

	for ( int i = 0 ; i < numShmDs ; i++ )
	{
		if ( shmDb[i].shmHandle == NULL )
		{
			retVal = i;
		}
	}

	if ( retVal < 0 )
	{
		if (numShmDs < MAX_NUM_DATABLOCKS-1)
		{
			if ( strlen(name) > MAX_DB_NAME_LEN)
			{
				retVal = -2;
			}
			else
			{
				retVal = numShmDs;
				numShmDs++;
			}
		}
	}

	if ( retVal >= 0 )
	{
		strcpy(shmDb[retVal].name, name);
		if ( dSize > 0 )
		{
			shmDb[retVal].shmHandle = new DioShmDs(0, dSize, DioShmSharedMem, name);
		}
		else
		{
			shmDb[retVal].shmHandle = new DioShmDs(DioShmSharedMem, name);
		}
	}

    return retVal;
}


///
/// Public functions: Put bytes in Shared Mem Named datablock
///

void DioShmPutDataBlock(
    char * name,        // data block name
    int numBytes,       // #bytes to be written
    char * bytes        // pointer to bytes
    )
{
    int dsId = DioShmFindDatablockID(name);

    if ( dsId < 0 )
    {
        dsId = DioShmNewDatablockID(name,numBytes);
    }
    if ( dsId >= 0 )
    {
        shmDb[dsId].shmHandle->Rewind(DioShmDataPart);
        shmDb[dsId].shmHandle->Write(DioShmDataPart, numBytes, bytes);
    }
}


///
/// Public functions: Put bytes in Shared Mem Named datablock
///

int DioShmGetDataBlock(
    char * name,       // data block name
    int numBytes,      // #bytes to be written
    char * bytes       // pointer to bytes
    )
{
    int retVal = false;
    int dsId = DioShmFindDatablockID(name);

    if ( dsId < 0 )
    {
        dsId = DioShmNewDatablockID(name, 0);
    }
    if ( dsId >= 0 )
    {
        shmDb[dsId].shmHandle->Rewind(DioShmDataPart);
		if ( shmDb[dsId].shmHandle->InfoIsValid() ) {
			shmDb[dsId].shmHandle->Read(DioShmDataPart, numBytes, bytes);
			retVal = true;
		}
    }
    return retVal;
}


///
/// Public functions: Free Shared Mem Named datablock
///

void DioShmFreeDataBlock(
    char * name        // data block name
    )
{
    int dsId = DioShmFindDatablockID(name);

    if ( dsId >= 0 )
    {
        delete shmDb[dsId].shmHandle;
		shmDb[dsId].shmHandle = NULL;
		strcpy(shmDb[dsId].name, "");
    }
}


void DioShmDataBlockCleanup(void)
{
    int ds;
    for ( ds = 0 ; ds < numShmDs ; ds ++ )
    {
        delete shmDb[ds].shmHandle;
		shmDb[ds].shmHandle = NULL;
    }
}

