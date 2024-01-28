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
//  dio_shm.h: DelftIO Shared Memory
//
//  Class DioShm
//
//  stef.hummel@deltares.nl
//
//  (c) Deltares, july 2002
//


#if (!defined(DIOSHM_H))
#define DIOSHM_H


#include <stdio.h>
#include "dio_shm_handle.h"
#include "dio_shm_sync.h"


//
// Max length of a dataset name
//

#define MAX_DS_NAME_LEN 256


//
// Data exchange type enumeration.
// Remark: 'Unknown' must be zero, because after the inititalisation
//         of the info block, this whole block is set to zero.
//
typedef enum _DioShmMemType{
    DioShmMemUnknown,   // not set yet
    DioShmInMem,        // In Memory (i.e. in the same process)
    DioShmSharedMem,    // Shared Memory (between processes)
    DIO_NUM_MEM_TYPES
} DioShmMemType;


//
// Dataset info structure
//
typedef struct _DioShmInfo_STR {
    char name [MAX_DS_NAME_LEN+1];  // dataset name
    DioShmMemType memType;          // InMem / SharedMem
    int dataSize[DIO_NUM_PARTS];    // size of header part and data part
    int headerUsed;                 // headerPart used? (or datapart only?)
    DioShmSync_STR sync;            // synch. info (see dio_shm_sync.h)
}DioShmInfo_STR;
typedef DioShmInfo_STR * DioShmInfo;


//
// DioShmDs InMem or SharedMem dataset
//
class DioShmDs {

private:

    //
    // Handle to shared memory blocks
    // (only used in case of SharedMem dataset; NULL in case of InMem dataset)
    //
    DioShmHandle * infoHandle;                // handle info block
    DioShmHandle * dataHandle[DIO_NUM_PARTS]; // handle header and data block

    //
    // Actual memory blocks, shared between putter and getter
    // - allocated for InMem
    // - pointer to (info|data)Handle.shmBlock for Shared Mem.
    //
    DioShmInfo info;                          // dataset info
    char * data[DIO_NUM_PARTS];               // dataset header and data part

    //
    // Fields for putter and getter
    //
    int putter;                               // Putter or Getter?
    int curSize[DIO_NUM_PARTS];               // #bytes currently written/read

    DioShmSync sync;                          // pointer to info.sync
                                              // (for convenience reasons)

public:

    //
    // Constructors/Destructors
    //
    DioShmDs(void);
    DioShmDs(int headerSize, int dataSize, DioShmMemType memType, char * name);
    DioShmDs(DioShmMemType memType, char * name);

    ~DioShmDs();

private:

    //
    // Private Initialization functions
    //
    void InitInfo(char * name, DioShmMemType memType);
    void InitData(DioShmPart part, int dataSize);
    void Reset(void);

public:

    //
    // General public functions
    //
    char * GetName(void);
    int InfoIsValid(void);

    int SetSize(int hSize, int dSize);
    int SetSizeForPart(DioShmPart part, int dSize);
    int GetSize(DioShmPart part);
    int GetRemainingSize(DioShmPart part);
    char * GetData(DioShmPart part) { return(this->data[part]); };

    int StartWrite(DioShmPart part);
    void EndWrite(DioShmPart part);

    int StartRead(DioShmPart part);
    void EndRead(DioShmPart part);

    void Rewind(DioShmPart part);

    //
    // public functions for Writing/Reading to/from header/data part.
    // - basic write/read function (numBytes, bytes)
    // - write/read functions per (array of) primitive types
    //   (only partly implemented)
    //

    void Write(DioShmPart part, int numBytes, char * bytes);
    int  Read (DioShmPart part, int numBytes, char * bytes);

    void Write(DioShmPart part, int intVal);
    void Write(DioShmPart part, float floatVal);
    void Write(DioShmPart part, char * string);
    void Write(DioShmPart part, int numItems, int * ints);
    void Write(DioShmPart part, int numItems, float * floats);
    void Write(DioShmPart part, int numItems, char ** strings);

    int  Read (DioShmPart part, int *intVal);
    int  Read (DioShmPart part, float *floatVal);
    int  Read (DioShmPart part, char *string);
    int  Read (DioShmPart part, int numItems, int * ints);
    int  Read (DioShmPart part, int numItems, float * floats);
    int  Read (DioShmPart part, int numItems, char ** strings);

};

void DioShmError(const char *format, ...);


#endif
