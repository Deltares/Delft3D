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
/*
 * dio-sync-ux.h
 *
 * (c) Deltares, aug '96
 *
 * Stef.Hummel@deltares.nl
 */


/*
 *  Include files and definitions
 */

#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>

/*
 *  Function names for FORTRAN-C interface.
 */
#if HAVE_CONFIG_H
#   include "config.h"
#   define STDCALL  /* nothing */
#   define DIOSYNCcMKFIFO FC_FUNC(diosynccmkfifo,DIOSYNCCMKFIFO)
#   define DIOSYNCcRMFIFO FC_FUNC(diosynccrmfifo,DIOSYNCCRMFIFO)
#   define DIOSYNCcSLEEP  FC_FUNC(diosynccsleep,DIOSYNCCSLEEP)
#else
// WIN32
#   define STDCALL  /* nothing */
#   define DIOSYNCcMKFIFO DIOSYNCCMKFIFO
#   define DIOSYNCcRMFIFO DIOSYNCCRMFIFO
#   define DIOSYNCcSLEEP  DIOSYNCCSLEEP
#endif


/*
 *  Function definitions
 */

#if defined(__cplusplus)
extern "C" {
#endif

void  STDCALL   DIOSYNCcMKFIFO(int *, char * name, int nameLen);
void  STDCALL   DIOSYNCcRMFIFO(char * name, int nameLen);
void  STDCALL   DIOSYNCcSLEEP(int *);

#if defined(__cplusplus)
}
#endif

