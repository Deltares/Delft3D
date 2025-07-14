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
/*------------------------------------------------------------------------------
//  Delft3D - C Utilities
//  Global definitions
//
//  Irv.Elshoff@deltares.nl
//  2 mar 05
//
//------------------------------------------------------------------------------*/


#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <time.h>

#if defined (WIN32)
#   include <sys/types.h>
#   include <sys/stat.h>
#   include <windows.h>
#else
#   include <sys/times.h>
#   include <unistd.h>
#   include <libgen.h>
#endif

#define MAX_CMD 1000


/* FTN_CAPITAL is assumed to be the default value */

#if defined(linux)
#   include "config.h"
#   define STDCALL  /* nothing */
#   define CUTIL_GETMP  FC_FUNC(cutil_getmp,CUTIL_GETMP)
#   define CUTIL_GETENV FC_FUNC(cutil_getenv,CUTIL_GETENV)
#   define CUTIL_SYSTEM FC_FUNC(cutil_system,CUTIL_SYSTEM)
#   define CUTIL_SLEEP FC_FUNC(cutil_sleep,CUTIL_SLEEP)
#   define CUTIL_CGETCP FC_FUNC(cutil_cgetcp,CUTIL_CGETCP)
#   define CUTIL_CDATE  FC_FUNC(cutil_cdate,CUTIL_CDATE)
#   define CUTIL_CSTOP  FC_FUNC(cutil_cstop,CUTIL_CSTOP)
#   define CUTIL_MF_SETMAXSTDIO  FC_FUNC(cutil_mf_setmaxstdio,CUTIL_MF_SETMAXSTDIO)
#   define CUTIL_MF_OPEN  FC_FUNC(cutil_mf_open,CUTIL_MF_OPEN)
#   define CUTIL_MF_CLOSE  FC_FUNC(cutil_mf_close,CUTIL_MF_CLOSE)
#   define CUTIL_MF_REWIND  FC_FUNC(cutil_mf_rewind,CUTIL_MF_REWIND)
#   define CUTIL_MF_BACKSPACE  FC_FUNC(cutil_mf_backspace,CUTIL_MF_BACKSPACE)
#   define CUTIL_MF_READ  FC_FUNC(cutil_mf_read,CUTIL_MF_READ)
#   define CUTIL_MF_GETPOS  FC_FUNC(cutil_mf_getpos,CUTIL_MF_GETPOS)
#   define CUTIL_MF_EOF  FC_FUNC(cutil_mf_eof,CUTIL_MF_EOF)
#   define CUTIL_CMP_DOUBLE  FC_FUNC(cutil_cmp_double,CUTIL_CMP_DOUBLE)
#   define CUTIL_CMP_SINGLE  FC_FUNC(cutil_cmp_float,CUTIL_CMP_SINGLE)
#else
// WIN32
#   define STDCALL  /* nothing */
#   define CUTIL_GETMP  CUTIL_GETMP
#   define CUTIL_GETENV CUTIL_GETENV
#   define CUTIL_SYSTEM CUTIL_SYSTEM
#   define CUTIL_SLEEP CUTIL_SLEEP
#   define CUTIL_CGETCP CUTIL_CGETCP
#   define CUTIL_CDATE  CUTIL_CDATE
#   define CUTIL_CSTOP  CUTIL_CSTOP
#   define CUTIL_MF_SETMAXSTDIO  CUTIL_MF_SETMAXSTDIO
#   define CUTIL_MF_OPEN  CUTIL_MF_OPEN
#   define CUTIL_MF_CLOSE  CUTIL_MF_CLOSE
#   define CUTIL_MF_REWIND  CUTIL_MF_REWIND
#   define CUTIL_MF_BACKSPACE  CUTIL_MF_BACKSPACE
#   define CUTIL_MF_READ  CUTIL_MF_READ
#   define CUTIL_MF_GETPOS CUTIL_MF_GETPOS
#   define CUTIL_MF_EOF  CUTIL_MF_EOF
#   define CUTIL_CMP_DOUBLE  CUTIL_CMP_DOUBLE
#   define CUTIL_CMP_SINGLE  CUTIL_CMP_SINGLE
#endif


#if defined (__cplusplus)
    extern "C" {
#endif

#if !defined (WIN32)
void    STDCALL         CUTIL_GETMP (char *, int *, int *);
void    STDCALL         CUTIL_GETENV(char *, int *, char *, int *);
void    STDCALL         CUTIL_SYSTEM(char *, int *);
void    STDCALL         CUTIL_SLEEP (int *);
#else
void    STDCALL         CUTIL_GETMP (char *, int *, int *, int);
void    STDCALL         CUTIL_GETENV(char *, int *, char *, int *, int, int);
void    STDCALL         CUTIL_SYSTEM(char *, int *, int);
void    STDCALL         CUTIL_SLEEP (int *);
#endif

#if defined (__cplusplus)
    }
#endif


int     isdir           (char *);
void    fstr2cstr       (char *, int, char *);
void    cstr2fstr       (char *, int, char *);

