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
///--description-----------------------------------------------------------------
//
//  Delft3D-MORFLOW - Definitions of primary MORFLOW routines for DD code
//
///--pseudo code and references--------------------------------------------------
//
//  Irv.Elshoff@deltares.nl
//  8 oct 05
//
//-------------------------------------------------------------------------------

#ifndef MORFLOW_H
#define MORFLOW_H


#define CONTEXT_PAGESIZE    (32*1024)   // for share memory contexts (in KB)
#define RUNID_LEN           256         // length of runid file name string for trisimtest


//------------------------------------------------------------------------------
//  Function names for FORTRAN-C interface.
#if HAVE_CONFIG_H
#   include "config.h"
#   define STDCALL  /* nothing */
#   define TRISIMTEST FC_FUNC(trisimtest,TRISIMTEST)
#else
// WIN32
#   define STDCALL  /* nothing */
#   define TRISIMTEST TRISIMTEST
#endif


//------------------------------------------------------------------------------
//  Function declarations

#if (defined(__cplusplus)||defined(_cplusplus))
extern "C" {
#endif

void    STDCALL TRISIMTEST(int * cId, int * fsm_flags);

#if (defined(__cplusplus)||defined(_cplusplus))
}
#endif

#endif
