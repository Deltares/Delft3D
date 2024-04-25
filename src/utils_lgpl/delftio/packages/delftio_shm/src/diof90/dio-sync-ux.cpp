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

#include "dio-sync-ux.h"

#include <string.h>
#include <errno.h>
#include <unistd.h>

#define MAX_FILE_LEN    256

int makeFifo(
    char * name
    )
{
    int retVal=1;

    if ( mkfifo(name, S_IRWXU) != 0 )
    {
    retVal = ( errno == EEXIST );
    }

    return retVal;
}


void DIOSYNCcMKFIFO(
    int *retVal,
    char * name,
    int nameLen
    )
{
    char locName[MAX_FILE_LEN+1];
    char *p;

    strncpy(locName, name, nameLen); locName[nameLen] = '\0';
    p = locName + nameLen - 1; while(*p==' '&&p>locName){*p--='\0';}

    *retVal = makeFifo(locName);
}


void DIOSYNCcRMFIFO(
    char * name,
    int nameLen
    )
{
    char locName[MAX_FILE_LEN+1];
    char *p;

    strncpy(locName, name, nameLen); locName[nameLen] = '\0';
    p = locName + nameLen - 1; while(*p==' '&&p>locName){*p--='\0';}

    (void) unlink(locName);
}


void DIOSYNCcSLEEP(
    int * numMillisec
    )
{
#if (defined(HAVE_CONFIG_H))
    unsigned long loc_microSec;
#else
    useconds_t loc_microSec;
#endif

    loc_microSec = *numMillisec * 1000;
    usleep(loc_microSec);
}


