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
//  dio_shm.cpp: DelftIO (Shared|In)Memory Datasets
//
//  stef.hummel@deltares.nl
//
//  (c) Deltares, july 2002
//


#include <string.h>
#include <stdarg.h>
#include <assert.h>

#include <stdlib.h>
#if defined (HAVE_CONFIG_H)
#include "config.h"
#ifdef HAVE_MALLOC_H
#include <malloc.h>
#endif
#else
#include <malloc.h>
#endif

#include "dio_shm.h"


//
// include esm and synch.support functions for unix platforms
//

#if (defined(HAVE_CONFIG_H))
#include "dio-sync-ux.h"
#endif


//
// Data structure for InMem storage.
//

typedef struct InMemDs_STR {
    DioShmInfo info;
    char * data[DIO_NUM_PARTS];
    DioShmDs * putter;
} InMemDs;


//
// Max num datasets that can be stored InMemory
// Data for InMem storage.
//

#define MAX_NUM_DATASETS 50

static InMemDs inMemDs[MAX_NUM_DATASETS];   // InMem datasets
static int nInMemDs=0;                      // #InMem datasets


///
/// Suppport function for Error Messages.
///

void DioShmError(
    const char *format,   // I: 'fprintf-format' for print of log
    ...                   // I: arguments of log message (should be
                          //    terminated with NULL)
    )
{
    va_list    arguments;    // var-arg list

    va_start ( arguments, format );

    vfprintf ( stderr, format, arguments );
    fflush(stderr);

    va_end ( arguments );

}


///
/// Suppport functions InMemStorage:
///

//
// - Add Dataset to InMemStore
//   (info and header/data part, header/data part may be NULL)
//
static void addInMemSet(
    DioShmInfo info,  // handle to dataset info
    char * hdrData,   // handle to header part
    char * dtaData,   // handle to data part
    bool putter,      // putter?
    DioShmDs * ds     // calling ds
    )
{
    inMemDs[nInMemDs].putter = NULL;
    if (putter) inMemDs[nInMemDs].putter = ds;
    inMemDs[nInMemDs].info = info;
    inMemDs[nInMemDs].data[DioShmHeaderPart]   = hdrData;
    inMemDs[nInMemDs++].data[DioShmDataPart  ] = dtaData;
}


//
// - Set header/data part of a Dataset that is allready InMemStore
//   (find dataset 'info', store header/data part)
//
static void setInMemSetData(
    DioShmPart part,  // header or data part?
    DioShmInfo info,  // handle to dataset info
    char * data       // handle to header or data
    )
{
    int i;
    for ( i = 0 ; i < nInMemDs ; i++ )
    {
        if ( inMemDs[i].info == info )
        {
            inMemDs[i].data[part] = data;
            break;
        }
    }
}


//
// - Find info part a Dataset that is allready InMemStore
//   (find dataset name 'name', return info)
//
static DioShmInfo findInMemSetInfo(
    char * name         // dataset name
    )
{
    int i;
    DioShmInfo retVal = NULL;
    for ( i = 0 ; i < nInMemDs ; i++ )
    {
        if ( strcmp(inMemDs[i].info->name, name) == 0 )
        {
            retVal = inMemDs[i].info;
            break;
        }
    }
    return retVal;
}


//
// - Find header/data part of a Dataset that is allready InMemStore
//   (find dataset containing 'info', return header/data part)
//
static char * findInMemSetData(
    DioShmPart part,    // header or data part?
    DioShmInfo info     // handle to dataset info
    )
{
    int i;
    char * retVal = NULL;
    for ( i = 0 ; i < nInMemDs ; i++ )
    {
        if ( inMemDs[i].info == info )
        {
            retVal = inMemDs[i].data[part];
            break;
        }
    }
    return retVal;
}


//
// - Find the handle to the putting Dataset
//
DioShmDs * findInMemSetPutter(
    DioShmInfo info     // handle to dataset info
    )
{
    int i;
    DioShmDs * retVal = NULL;
    for ( i = 0 ; i < nInMemDs ; i++ )
    {
        if ( inMemDs[i].info == info )
        {
            retVal = inMemDs[i].putter;
            break;
        }
    }
    return retVal;
}


///
/// Class DioShmDs, PRIVATE functions
///

//
// (Free and) Nullify all elements, if something goes wrong
// Remark: in case of InMem, the Putter has allocated, so only
//         the putter frees.
//
void DioShmDs::Reset(void)
{
    int part;       // loop counter header/data part

    //
    // Free mem. in case of putter
    //
    if (this->info)
    {
        if (this->info->memType == DioShmInMem && this->putter)
        {
            for ( part = 0 ; part < DIO_NUM_PARTS ; part++ )
            {
                if (this->data[part] ) free(this->data[part]);
            }
            if (this->info ) free(this->info);
        }
    }

    //
    // Release Shared mem handles (if created), and nullify all elements
    //
    for ( part = 0 ; part < DIO_NUM_PARTS ; part++ )
    {
        this->data[part] = NULL;
        if (this->dataHandle[part]) delete this->dataHandle[part];
        this->dataHandle[part] = NULL;
    }

    this->sync = NULL;
    this->info = NULL;

    if (this->infoHandle) delete this->infoHandle;
    this->infoHandle = NULL;

}


//
// Initialize the Info block
//
void DioShmDs::InitInfo(
    char * name,           // dataset name
    DioShmMemType memType  // InMem or Shared Mem
    )
{
    int part;                              // loop counter header/data part
    int infoSize = sizeof(DioShmInfo_STR); // size of info structure
    char infoName[MAX_DS_NAME_LEN+1];      // name for info part of dataset

    //
    // Determine Shared Memory name for info part,
    // Nullify all elements.
    //
    sprintf(infoName, "%s.info", name);

    this->sync = NULL;

    this->info = NULL;
    this->infoHandle = NULL;

    for ( part = 0 ; part < DIO_NUM_PARTS ; part++ )
    {
        this->data[part]       = NULL;
        this->dataHandle[part] = NULL;
        this->curSize[part]  = 0;
    }

    switch (memType)
    {
        case DioShmInMem:
                //
                // InMemory. Putter: - allocate and nullify info block
                //                   - store info in InMem store
                //           Getter: - find info in InMem store
                //
                if (this->putter)
                {
                    char *p; // TODO: new gebruiken
                    this->info = (DioShmInfo) malloc(infoSize);
                        for ( p = (char *) this->info;
                            p < ((char *) this->info + infoSize) ; p++ ) *p='\0';
                    addInMemSet(this->info, NULL, NULL, true, this);
                }
                else
                {
                    this->info = findInMemSetInfo(name);
                }
            break;

        case DioShmSharedMem:
                //
                // Shared Memory. Create/Attach Shared Mem Block for info
                //                (On Ux, Getter should specify zero,
                //                 to be able to attach to ESM dataset)
                //
#if (defined(HAVE_CONFIG_H))
                if (! this->putter) infoSize = 0;
#endif
                this->infoHandle = new DioShmHandle(infoSize , infoName);
                this->info = (DioShmInfo) this->infoHandle->shmBlock;
            break;

        default:
            assert(0);
            break;
    }

    if (this->info)
    {
        //
        // New info-handle created.
        // - Init. info in case of putter, and store mem-type and name
        // - Let sync-info point to sync-part of info-handle.
        //
        if ( this->putter )
        {
            for ( part = 0 ; part < DIO_NUM_PARTS ; part++ )
            {
                this->info->dataSize[part] = 0;
            }
            this->info->memType  = memType;
            strncpy(this->info->name, name, MAX_DS_NAME_LEN);
            this->info->name[MAX_DS_NAME_LEN] ='\0';
        }
        this->sync = &(this->info->sync);

        //
        // Indicate nothing read/written yet
        //
        for ( part = 0 ; part < DIO_NUM_PARTS ; part++ )
        {
            this->curSize[part]  = 0;
        }
        this->sync->putterDone = false;
    }
}


//
// Initialize a datablock (header or data part)
//
void DioShmDs::InitData(
    DioShmPart part,   // header or data part?
    int dataSize       // #bytes for header/data part
    )
{
    if (this->info)
    {
        //
        // Determine name for data block (name.data.0 or "name.data.1")
        // Store size for header/data part
        //
        char dataName[MAX_DS_NAME_LEN+1]; // name for data part of dataset
        sprintf(dataName, "%s.data.%d", this->info->name, part);

        if (this->putter)
        {
            this->info->dataSize[part] = dataSize;
        }

        switch (this->info->memType)
        {
            case DioShmInMem:
                //
                // InMemory. Putter: - allocate and nullify header/data block
                //                   - store header/data in InMem store
                //           Getter: - find header/data in InMem store
                //
                if (this->putter)
                {
                    char *p;
                    this->data[part] = (char *) malloc(dataSize);
                    for ( p = this->data[part] ;
                        p < (this->data[part] + dataSize) ; p++ ) *p='\0';
                    setInMemSetData(part, this->info, this->data[part]);
                }
                else
                {
                    this->data[part] = findInMemSetData(part, this->info);
                }
                break;

            case DioShmSharedMem:
                //
                // Shared Memory. Create/Attach Shared Mem Block
                //                (On Ux, Getter should specify zero,
                //                 to be able to attach to ESM dataset)
                //
#if (defined(HAVE_CONFIG_H))
                if (! putter) dataSize = 0; // ESM requires size 0 to attach
#endif
                this->dataHandle[part] = new DioShmHandle(dataSize, dataName);
                this->data[part] = (char *) this->dataHandle[part]->shmBlock;
                break;

            default:
                assert(0);
                break;
        }
    }
}


///
/// Class DioShmDs, PUBLIC functions, CONSTRUCTORS
///

//
// General Constructor for derived classes. Nullify all.
//
DioShmDs::DioShmDs(void)
{
    int part;

    this->sync = NULL;

    this->info = NULL;
    this->infoHandle = NULL;

    this->putter = false;

    for ( part = 0 ; part < DIO_NUM_PARTS ; part++ )
    {
        this->data[part]       = NULL;
        this->dataHandle[part] = NULL;
        this->curSize[part]    = 0;
    }
}


//
// Constructor for Putter
//
DioShmDs::DioShmDs(
    int headerSize,         // header size (can be 0, to be set later on)
    int dataSize,           // data size (can be 0, to be set later on)
    DioShmMemType memType,  // InMem or Shared Mem
    char * name             // dataset name
    )
{
    this->putter = true;

    this->InitInfo(name, memType);

    if ( this->info == NULL )
    {
        this->Reset();
    }
    else
    {
        this->SetSize(headerSize, dataSize);
    }
}


//
// Constructor for Getter
//
DioShmDs::DioShmDs(
    DioShmMemType memType, // InMem or Shared Mem
    char * name            // dataset name
    )
{
    this->putter = false;

    this->InitInfo(name, memType);

    if (DioShmInfoAvailable(this->sync))
    {
		if ( strcmp(name, this->info->name) != 0 )
		{
			DioShmError("Could not attach to data set %s\n", name);
		}
		if ( this->info == NULL )
		{
			this->Reset();
		}
    }
	else
	{
		this->Reset();
		DioShmError("Time out on attaching to data set %s\n", name);
	}
}


//
// Destructor
//
DioShmDs::~DioShmDs(void)
{
    int part;

    if (this->info)
    {
        if (this->putter)
        {
            // Indicate done
            if (this->sync) this->sync->putterDone = true;
        }
        if (this->info->memType == DioShmInMem)
        {
            // For in mem exchange, getter cleans up
            if (! this->putter)
            {
                DioShmDs * puttingDs = findInMemSetPutter(this->info);

                for ( part = 0 ; part < DIO_NUM_PARTS ; part++ )
                {
                    if (this->data[part]) free(this->data[part]);
                    this->data[part] = NULL;
                }
                free(this->info);
                this->info=NULL;
                this->sync=NULL;

                if ( puttingDs )
                {
                    for ( part = 0 ; part < DIO_NUM_PARTS ; part++ )
                        puttingDs->data[part] = NULL;
                    puttingDs->info=NULL;
                    puttingDs->sync=NULL;
                }
            }
        }
    }

    for ( part = 0 ; part < DIO_NUM_PARTS ; part++ )
    {
        if (this->dataHandle[part]) delete this->dataHandle[part];
        this->dataHandle[part] = NULL;
    }
    if (this->infoHandle) delete this->infoHandle;

}


///
/// Class DioShmDs, PUBLIC functions, GENERAL
///

//
// Check if a dataset is valid (by checking if it's info is valid)
// Remark: used by F90/C interface, to check if constructor worked well
//
int DioShmDs::InfoIsValid(void)
{
    return ( this->info != NULL );
}


//
// Set size for the header OR for the data part
// Called when a Putter:
// - has created a dataset with size 0 for header and/or data part.
// - sets the actual size later on.
//
int DioShmDs::SetSizeForPart(
    DioShmPart part,       // header or data part?
    int dSize              // #bytes for header/data part
    )
{
    int retVal = 0;

    //
    // Result is only valid if:
    // - #bytes > 0
    // - the caller is a putter
    // - the header/data block is not set yet
    //
    if ( dSize > 0 )
    {
        if ( this->putter )
        {
            if ( part == DioShmHeaderPart ) this->info->headerUsed = true;

            if ( this->data[part] == NULL )
            {
                this->InitData(part, dSize);

                if ( this->data[part] != NULL ) retVal = 1;

                //
                // Dataset info is available for getter if both parts are initialized,
                // or if the datapart is initialized and the header part will not be used
                //
                if ( ( ( this->data[DioShmHeaderPart] != NULL ) &&
                       ( this->data[DioShmDataPart  ] != NULL )    ) ||
                     ( ( this->data[DioShmDataPart]   != NULL ) &&
                       ( this->info->headerUsed       == false)    )    )
                {
                    this->sync->infoAvailable = true;
                }
            }
        }
    }
    return retVal;
}


//
// Set size for header AND data part
// Called when a Putter:
// - has created a dataset with size 0 for header and data part.
// - sets both actual sizes later on.
//

int DioShmDs::SetSize(
    int headerSize,
    int dataSize
    )
{
    int retVal = 0;

    assert(this->info);

    if ( ( this->data[DioShmHeaderPart] == NULL ) &&
         ( this->data[DioShmDataPart  ] == NULL )    )
    {
        this->info->headerUsed = (headerSize > 0 && dataSize > 0) ? true : false;
        //
        // Initialize header part and datapart.
        //
        if ( headerSize > 0 ) this->InitData(DioShmHeaderPart, headerSize);
        if ( dataSize   > 0 ) this->InitData(DioShmDataPart, dataSize);

        if ( ( headerSize > 0 && this->data[DioShmHeaderPart] == NULL ) ||
             ( dataSize   > 0 && this->data[DioShmDataPart  ] == NULL )    )
        {
            this->Reset();
        }
        //
        // Dataset info is available for getter if both parts are initialized,
        // or if the datapart is initialized and the header part will not be used
        //
        if ( ( ( this->data[DioShmHeaderPart] != NULL ) &&
               ( this->data[DioShmDataPart  ] != NULL )    ) ||
             ( ( this->data[DioShmDataPart]   != NULL ) &&
               ( this->info->headerUsed       == false)    )    )
        {
            retVal = 1;
            this->sync->infoAvailable = true;
        }
    }
    return retVal;
}


//
// Get size of header or data part
//
int DioShmDs::GetSize(
    DioShmPart part       // header or data part?
    )
{
    int retVal = 0;

    if ( this->data[part] != NULL )
    {
        retVal = this->info->dataSize[part];
    }
    return retVal;
}


//
// Get remaining size of header or data part available for reading/writing
//
int DioShmDs::GetRemainingSize(
    DioShmPart part       // header or data part?
    )
{
    int retVal = 0;

    if ( this->data[part] != NULL )
    {
        retVal = this->info->dataSize[part] - this->curSize[part];
    }
    return retVal;
}


//
// Get Dataset name
//
char * DioShmDs::GetName(void)
{
    char * retVal = NULL;

    if ( this->info != NULL )
    {
        retVal = this->info->name;
    }
    return retVal;
}


///
/// Class DioShmDs, PUBLIC functions, WRITE and READ
///

//
// Basic function: Write a number of bytes to header or data part
//
void DioShmDs::Write(
    DioShmPart part, // header or data part?
    int numBytes,    // #bytes to be written
    char * bytes     // pointer to bytes
    )
{
    assert(this->info);
    if ( (this->curSize[part] + numBytes) > this->info->dataSize[part] )
    {
        DioShmError("data space too small to write\n");
    }
    else
    {
        assert(this->data[part]);
        memcpy( this->data[part] + this->curSize[part], bytes, numBytes);
        this->curSize[part] += numBytes;
    }
}


//
// Basic function: Read a number of bytes from header or data part
//
int DioShmDs::Read(
    DioShmPart part, // header or data part?
    int numBytes,    // #bytes to be read
    char * bytes     // pointer to bytes
    )
{
    int retVal = false;
    assert(this->info);
    if ( (this->curSize[part] + numBytes) > this->info->dataSize[part] )
    {
        DioShmError("data space too small to read\n");
    }
    else
    {
        if (this->data[part] == NULL)
        {
            this->InitData(part, this->info->dataSize[part]);
        }
        if ( this->data[part] == NULL )
        {
            DioShmError("could not attach data space\n");
        }
        else
        {
            memcpy(bytes, this->data[part] + this->curSize[part], numBytes);
            this->curSize[part] += numBytes;
            retVal = true;
        }
    }
    return retVal;
}


//
// Functions for writing/reading primitive data types to/from header or data part
// - Write/Read (array of) float(s)
// - Write/Read (array of) ints(s)
//

void DioShmDs::Write(DioShmPart part, int numItems, float * bytes)
{
    this->Write(part, numItems*sizeof(float), (char *) bytes);
}

int DioShmDs::Read(DioShmPart part, int numItems,float * bytes)
{
    return this->Read(part, numItems*sizeof(float), (char *) bytes);
}

void DioShmDs::Write(DioShmPart part,int numItems,int * bytes)
{
    this->Write(part, numItems*sizeof(int), (char *) bytes);
}

int DioShmDs::Read(DioShmPart part, int numItems, int * bytes)
{
    return this->Read(part, numItems*sizeof(int), (char *) bytes);
}


///
/// Class DioShmDs, PUBLIC functions, START/END Write and Read
///

//
// Restart writing/reading the header block or data block
//
void DioShmDs::Rewind(
    DioShmPart part
    )
{
    this->curSize[part] = 0;
}


//
// Start/End writing the header block or data block (including synchronisation)
//

int DioShmDs::StartWrite(
    DioShmPart part
    )
{
    int retVal = DioShmDataConsumed(this->sync, part, DsCheck);
    if ( retVal ) this->Rewind(part);
    return retVal;
}

void DioShmDs::EndWrite(
    DioShmPart part
    )
{
    (void) DioShmDataStored(this->sync, part, DsConfirm);
}


//
// Start/End reading the header block or data block (including synchronisation)
//

int DioShmDs::StartRead(
    DioShmPart part
    )
{
    int retVal = false;

    if ( this->data[part] == NULL )
    {
        this->InitData(part, this->info->dataSize[part]);
    }

    if ( this->data[part] == NULL )
    {
        DioShmError("could not attach data space\n");
    }
    else
    {
        retVal = DioShmDataStored(this->sync, part, DsCheck);
    }

    if ( retVal ) this->Rewind(part);

    return retVal;
}

void DioShmDs::EndRead(
    DioShmPart part
    )
{
    (void) DioShmDataConsumed(this->sync, part, DsConfirm);
}


