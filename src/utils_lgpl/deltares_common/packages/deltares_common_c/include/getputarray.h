//---- LGPL --------------------------------------------------------------------
//
// Copyright (C)  Stichting Deltares, 2011-2025.
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
///--description-----------------------------------------------------------------
// NONE
///--pseudo code and references--------------------------------------------------
// NONE
///-------------------------------------------------------------------------------


/*
 *  Include files and definitions
 */


#include "stream.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#if defined (WIN32)
#   include <sys/types.h>
#   include <sys/stat.h>
#else
#   include <unistd.h>
#endif



/*----------------------------------------------------------------------
 *  API-functions
 *----------------------------------------------------------------------*/

/*
 *  Function names for FORTRAN-C interface.
 */

#if defined(linux)
#   include "config.h"
#   define STDCALL  /* nothing */
#   define CREATESTREAM FC_FUNC(createstream,CREATESTREAM)
#   define GETSTREAM    FC_FUNC(getstream,GETSTREAM)
#   define GETARRAY     FC_FUNC(getarray,GETARRAY)
#   define PUTARRAY     FC_FUNC(putarray,PUTARRAY)
#else
// WIN32
#   define STDCALL  /* nothing */
#   define CREATESTREAM CREATESTREAM
#   define GETSTREAM    GETSTREAM
#   define GETARRAY     GETARRAY
#   define PUTARRAY     PUTARRAY
#endif


/*
 *  Function definitions
 */

#if (defined(__cplusplus)||defined(_cplusplus))
extern "C" {
#endif

int     STDCALL CREATESTREAM(char * filen, int filen_length);
int     STDCALL GETSTREAM   (char * filen, int filen_length);
void    STDCALL GETARRAY    (int  * handleID, double * array, int * size);
void    STDCALL PUTARRAY    (int  * handleID, double * array, int * size);

#if (defined(__cplusplus)||defined(_cplusplus))
}
#endif

/*
 *  Private Functions, Defines, Data
 */

void    fortstr2cstr (char *, int, char *);
void    StreamError  (char *);
void    StreamTrace  (char *);


#define MAXSTREAMS      100
#define BUFSIZE         100

#define INIT_MESSAGE    "GetPutArray"
#define INIT_MLEN       (strlen (INIT_MESSAGE) + 1)


/*
 *  Global data (not thread-safe)
 */

Stream * Danostream [MAXSTREAMS];

int Maxstream = -1;
