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
//  dio_shm_sync.cpp: DelftIO SharedMem Dataset Synchronisation
//
//  stef.hummel@deltares.nl
//
//  (c) Deltares, july 2002
//


#include "dio_shm_sync.h"


#if (defined(salford32))
#include <bool.h>          // false
#include <stddef.h>        // salford
#include <dbos\lib.h>      // sleep for salford
#elif (defined(WIN32))
#include <windows.h>      // sleep for Windows
#else
#include "dio-sync-ux.h"  // sleep for UX
#endif


//
// Sleep time for synchronisation functions
//

static int DioSyncSleepTime   = 100     ;
static int DioSyncTimeOutTime = 86400000;


//
// General Sleep function
//

void DioSleep(
    int sleepTime       // sleep time in milliseconds
    )
{
#if (defined(salford32))
        float &fSleepTime = (float) sleepTime; //  / 1000.0 // TODO check
        sleep(fSleepTime);
#elif (defined(WIN32))
        Sleep(sleepTime);
#else
        DIOSYNCcSLEEP(&sleepTime);
#endif
}


//
// Set sleep time for synchronisation functions
//

void DioSetSyncTimeOut(
    int timeOutTime     // time out for loops
    )
{
    DioSyncTimeOutTime = timeOutTime;
}


void DioSetSyncSleepTime(
    int sleepTime       // sleep time in milliseconds
    )
{
    DioSyncSleepTime = sleepTime;
}


int DioShmInfoAvailable(
    DioShmSync sync
    )
{
    int retVal = (sync != NULL);

    if (sync)
    {
		int timeWaited = 0;
        while (! sync->infoAvailable )
		{
			DioSleep(DioSyncSleepTime);
			timeWaited += DioSyncSleepTime;
			if ( timeWaited > DioSyncTimeOutTime )
			{
				return false;
			}
		}
		retVal = sync->infoAvailable;
    }
    return retVal;
}


int DioShmDataConsumed(
    DioShmSync sync,
    DioShmPart part,
    DsStoreFlag flag
    )
{
    int retVal = (sync != NULL);

    if (sync)
    {
        if ( flag == DsCheck )
        {
			int timeWaited = 0;
            while (sync->dataAvailable[part])
			{
				DioSleep(DioSyncSleepTime);
				timeWaited += DioSyncSleepTime;
				if ( timeWaited > DioSyncTimeOutTime )
				{
					return false;
				}
			}
        }
        else if ( flag == DsConfirm )
        {
            sync->dataAvailable[part] = false;
        }
        else
        {
            retVal = false;
        }
    }
    return retVal;
}


int DioShmDataStored(
    DioShmSync sync,
    DioShmPart part,
    DsStoreFlag flag
    )
{
    int retVal = (sync != NULL);

    if (sync)
    {
        if ( flag == DsCheck )
        {
			int timeWaited = 0;
            while ( (! sync->dataAvailable[part] ) &&
                    (! sync->putterDone          )    )
            {
                DioSleep(DioSyncSleepTime);
				timeWaited += DioSyncSleepTime;
				if ( timeWaited > DioSyncTimeOutTime )
				{
					return false;
				}
            }
            if ( (! sync->dataAvailable[part]) && sync->putterDone)
            {
                retVal = false;
            }
        }
        else if ( flag == DsConfirm )
        {
            sync->dataAvailable[part] = true;
        }
        else
        {
            retVal = false;
        }
    }
    return retVal;
}


//
// Fortran to C interface for set time functions
//


void STDCALL DIOSETSYNCTIMEOUT_C  (int * timeOutTime) { DioSetSyncTimeOut(*timeOutTime); }
void STDCALL DIOSETSYNCSLEEPTIME_C(int * sleepTime  ) { DioSetSyncSleepTime(*sleepTime); }

