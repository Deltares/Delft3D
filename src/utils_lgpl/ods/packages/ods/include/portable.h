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
/*
 *  portable.h  -  typedef's and constants for utmost portability
 *
 *  Note: extra macro, USE_STDCALL, to control use of __stdcall
 *
 *  Marc Kool
 */
/*
 *  $Author: Markus $
 *  $Date: 15-05-03 13:16 $
 *  $Source: /u/cvsroot/gpp/include/portable.h,v $
*/
/*
 */

#ifndef PORTABLE_H_INCLUDED
#define PORTABLE_H_INCLUDED
#if defined(hpux) || defined(__hpux)
#if !defined(USE_HPUX)
#define USE_HPUX
#endif
#if defined(__hp9000s300) || defined(__hp9000s400)
#define USE_HPUX400
#endif
#if defined(__hp9000s700)
#define USE_HPUX700
#endif
#if defined(__hp9000s800)
#define USE_HPUX800
#endif
#endif
#if defined(_AIX) || defined(__AIX)
#if !defined(USE_AIX)
#define USE_AIX
#endif
#endif
#if defined(sgi) || defined(__sgi)
#define USE_IRIX
#endif
#if defined(sparc)
#define USE_SUNOS
#endif

#if defined(linux)
#define USE_LINUX
#define FORTRAN_UNDERSCORE
#endif

#if !defined(USE_HPUX) && !defined(USE_AIX) && !defined(USE_IRIX) && \
    !defined(USE_SUNOS) && !defined(USE_MSDOS) && !defined(USE_MSWINDOWS) && \
    !defined(USE_WINNT) && !defined(USE_ALPH) && !defined(USE_LINUX)
PORTABLE__H_ERROR
/* error in portable.h : no USE_ macro defined */
#endif
/* 32-bit Unix OS types */
#if defined(USE_HPUX) || defined(USE_AIX) || defined(USE_IRIX) || \
    defined(USE_SUNOS)|| defined(USE_ALPH) || defined(USE_LINUX)
typedef void		TVoid;
typedef unsigned char	TByte;
typedef char		TChar;
typedef int		TInt2;
typedef unsigned int	TUint2;
typedef int		TInt4;
typedef unsigned int	TUint4;
typedef float		TReal4;
typedef double		TReal8;
typedef char *		TString;
typedef void *		TPointer;
#endif
/* 16-bit DOS */
#if defined(USE_MSDOS)
typedef void		TVoid;
typedef unsigned char	TByte;
typedef char		TChar;
typedef short		TInt2;
typedef unsigned short	TUint2;
typedef long		TInt4;
typedef unsigned long	TUint4;
typedef float		TReal4;
typedef double		TReal8;
typedef char *		TString;
typedef void *		TPointer;
#endif

/* 32-bit MS Windows */
#if defined(USE_MSWINDOWS) || defined(USE_WINNT)
typedef void		TVoid;
typedef unsigned char	TByte;
typedef char		TChar;
typedef int		TInt2;
typedef unsigned int	TUint2;
typedef long		TInt4;
typedef unsigned long	TUint4;
typedef float		TReal4;
typedef double		TReal8;
typedef char *		TString;
typedef void *		TPointer;
#endif
/* some POSIX include files : */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#if !defined(USE_WINNT)
#include <unistd.h>
#endif
#include <sys/types.h>
#ifndef TRUE
#define TRUE	1
#define FALSE	0
#endif
#ifndef DIR_SEPARATOR
#if defined(USE_MSDOS) || defined(USE_MSWINDOWS) || defined(USE_WINNT)
#define DIR_SEPARATOR '\\'
#else
#define DIR_SEPARATOR '/'
#endif
#endif

/* Define the calling convention
*/
#if defined(USE_WINNT) || defined(WINNT)
#if defined(USE_STDCALL)
#define IN_BETWEEN
#define FOR_CALL __stdcall
#else
#define FOR_CALL
#endif
#else
#define FOR_CALL
#endif

#endif  /* PORTABLE_H_INCLUDED */




