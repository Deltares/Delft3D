//---- LGPL --------------------------------------------------------------------
//
// Copyright (C)  Stichting Deltares, 2011-2026.
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
// $Id: coredump.cpp 962 2011-10-31 21:52:47Z elshoff $
// $HeadURL: $
//------------------------------------------------------------------------------
//  COREDUMP Function
//  Cause a core dump for debugging purposes; callable from Fortran and C/C++
//
//  Irv.Elshoff@Deltares.NL
//  29 oct 11
//------------------------------------------------------------------------------

#include "dimr.h"

// Callable from Fortran via: subroutine dimr_coredump() bind(C, name="dimr_coredump")
extern "C" void dimr_coredump()
{
    // ToDo: Check whether the core dump was actually requested.
    //       Only activate the printf statement when a core dump is actually requested
    // printf ("\n!! INTENDED CORE DUMP OF DIMR FOR DEBUGGING PURPOSES !!\n\n");
    fflush(stdout);
    fflush(stderr);

    // When requested: generate a core dump:
    // int * null = NULL;
    // int never = *null;
}
