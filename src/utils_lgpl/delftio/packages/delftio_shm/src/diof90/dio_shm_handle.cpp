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
//  dio_shm_handle.cpp: DelftIO SharedMemory data handles
//
//  stef.hummel@deltares.nl
//
//  (c) Deltares, july 2002
//


#include <stdio.h>
#include <stdlib.h>    // for getenv

#include "dio_shm_handle.h"




//
// On Unix: ESM for shared mem blocks
//

#ifdef HAVE_CONFIG_H

#include "dio-sync-ux.h"
#include "esm.h"

static int esmContext=-1;   // esm Context (-1: not connected)

int DioShmEsmGetContext(void)
{

    if ( esmContext == -1 )
    {
        char * dioShmEnv = getenv("DIO_SHM_ESM");
        if ( dioShmEnv == 0 )
        {
            fprintf(stderr, "COULD NOT GET env. var. DIO_SHM_ESM\n");
        }
        else
        {
            int id, numRead;
            numRead = sscanf(dioShmEnv, "%d", &id);
            if ( numRead != 1 )
            {
                fprintf(stderr,
                    "COULD NOT READ ContextID from env. var. DIO_SHM_ESM\n");
            }
            else
            {
                if (ESM_Init(0) != ESM_OK )
                {
                    fprintf(stderr,
                       "COULD not initialize ESM (Context %d)\n", id);

                }
                else
                {
                    esmContext = id;
                }
            }
        }
    }
    return( esmContext );
}

#endif


//
// CONSTRUCTOR for WINDOWS (use memory mapped file as shared mem block)
//

#if (!defined(HAVE_CONFIG_H))

DioShmHandle::DioShmHandle(
    int iSize,
    char * name
    )
{
    this->mmfHandle = NULL;
    this->shmBlock = NULL;

    // TODO Allow only one view per process (search name)
    if (this->mmfHandle == NULL)
    {
        this->mmfHandle =
            CreateFileMapping(  INVALID_HANDLE_VALUE,
                                NULL,
                                PAGE_READWRITE,
                                0,
                                iSize,
                                name
                                );

		DWORD dwError_CFM = GetLastError();
		if (dwError_CFM != 0 && dwError_CFM != ERROR_ALREADY_EXISTS )
		{
			fprintf(stderr,
                       "Error creating Memory Mapped File: %lu\n", dwError_CFM);
		}

        if (this->mmfHandle != NULL)
        {
            this->shmBlock = MapViewOfFile (  this->mmfHandle,
                                    FILE_MAP_WRITE, 0, 0, 0);
        }
    }
}

#else

//
// CONSTRUCTOR for UNIX (attach to shared mem block in ESM)
//

DioShmHandle::DioShmHandle(
    int iSize,
    char * name
    )
{
    this->mmfHandle = NULL;
    this->shmBlock = NULL;

    // TODO Allow only one view per process (search name)
    if (this->mmfHandle == NULL)
    {
        int cId = DioShmEsmGetContext();

        if ( cId >= 0 )
        {
            if ( iSize == 0 )
            {
                while( this->mmfHandle == NULL )
                {
                    int sleepTime = 100;
                    DIOSYNCcSLEEP(&sleepTime);
                    this->mmfHandle = ESM_Alloc(cId, name, (size_t)(0));
                }
            }
            else
            {
                this->mmfHandle = ESM_Alloc(cId, name, (size_t)(iSize));
            }

            if (this->mmfHandle != NULL)
            {
                this->shmBlock = this->mmfHandle;
            }
            else
            {
                fprintf(stderr, "Could not attach to ESM (dataset %s)\n", name);
            }
        }
    }
}

#endif


#if (!defined(HAVE_CONFIG_H))

//
// DESTRUCTOR for WINDOWS (release memory mapped file)
//

DioShmHandle::~DioShmHandle(void)
{
    if (this->shmBlock != NULL)
    {
        UnmapViewOfFile (this->shmBlock);
    }
    if (this->mmfHandle != NULL)
    {
        CloseHandle (this->mmfHandle);
    }
}

#else

//
// DESTRUCTOR for UNIX (detach from shared mem block in ESM)
//

DioShmHandle::~DioShmHandle(void)
{
    if (this->shmBlock != NULL)
    {
        fprintf(stderr, "DIO_SHM_ESM: detaching from context not yet implemented\n");
    }
}

#endif

