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
 *  Wrapper for FORTRAN main program: MDVER
 *
 *  Irv.Elshoff@deltares.nl
 *  Adri.Mourits@deltares.nl
 *  07 may 2004
 */

#if HAVE_CONFIG_H
#   include "config.h"
#   define STDCALL  /* nothing */
#   define TEST_01 FC_FUNC(test_01,TEST_01)
#else
/* WIN32 */
#   define STDCALL  /* nothing */
#   define TEST_01 TEST_01
#endif


#if defined (__cplusplus)
    extern "C" {
#endif

extern void STDCALL TEST_01 ( void );

#if defined (__cplusplus)
    }
#endif


int
main (
    int     argc,
    char *  argv[],
    char *  envp[]
    ) {

    TEST_01 ();
    }

