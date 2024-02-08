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
//  dio_shm_sync.h: DelftIO Shared Memory synchronisation functions
//
//  stef.hummel@deltares.nl
//
//  (c) Deltares, Jan 2000
//


#if (!defined(DIOSHM_SYNC_H))
#define DIOSHM_SYNC_H


typedef enum {	// enum for synchr., Not Yet Used Yet
    DioSyncOK,              // data received / consumed
    DioSyncNotAvailableYet, // data not available yet, time out
    DioSyncNotConsumedYet,  // data not consumed yet, time out
    DioSyncNoCounterPart,   // counterpart has quit
    DioSyncError,           // error, invalid situation
    DIO_SYNC_NUM_RETVALS
} DioShmSyncRetVal;


//
// Enumeration for Dataset part to synchronize
//

typedef enum  _DioShmPart {
    DioShmHeaderPart, // dataset's header data
    DioShmDataPart,   // dataset's 'timestep' data
    DIO_NUM_PARTS
} DioShmPart;


//
// Dataset Synchronisation information
//

typedef struct _DioShmSync_STR {

    int infoAvailable;                // is info complete
    int dataAvailable[DIO_NUM_PARTS]; // has header/data been provided?
    int putterDone;                   // Putter cleans up

}DioShmSync_STR;
typedef DioShmSync_STR * DioShmSync;


//
// Enumeration for DataStored/DataConsumed functions
//

typedef enum _DsStoreFlag{
    DsConfirm,       // Confirm Storage/Consumation of header or data part
    DsCheck,         // Check if header or data part has been stored/consumed
    NR_STORE_TYPES
} DsStoreFlag;


///
/// PUBLIC Functions
///

//
// Set sleep time / Sleep
//
void DioSetSyncTimeOut  (int timeOutTime);
void DioSetSyncSleepTime(int sleepTime);
void DioSleep(int sleepTime);

//
// Confirm/Check Storage/Consumation of header or data part
// In case of Check flag, function synchronizes.
//

int DioShmDataConsumed(DioShmSync sync, DioShmPart part, DsStoreFlag flag);
int DioShmDataStored(DioShmSync sync, DioShmPart part, DsStoreFlag flag);

//
// Wait until dataset info is available
//
int DioShmInfoAvailable(DioShmSync sync);


//
// Function names for FORTRAN-C interface.
//
#if HAVE_CONFIG_H
#   include "config.h"
#   define STDCALL  /* nothing */
#   define DIOSETSYNCTIMEOUT_C   FC_FUNC(diosetsynctimeout_c,DIOSETSYNCTIMEOUT_C)
#   define DIOSETSYNCSLEEPTIME_C FC_FUNC(diosetsyncsleeptime_c,DIOSETSYNCSLEEPTIME_C)
#else
// WIN32
#   define STDCALL  /* nothing */
#   define DIOSETSYNCTIMEOUT_C   DIOSETSYNCTIMEOUT_C
#   define DIOSETSYNCSLEEPTIME_C DIOSETSYNCSLEEPTIME_C
#endif

/*------------------------------------------------------------------------------
 *    Function definitions
 */

#if ( defined(__cplusplus) || defined(salford32) )
extern "C" {
#endif

void STDCALL DIOSETSYNCTIMEOUT_C  (int * timeOutTime);
void STDCALL DIOSETSYNCSLEEPTIME_C(int * sleepTime  );

#if ( defined(__cplusplus) || defined(salford32) )
}
#endif



#endif

