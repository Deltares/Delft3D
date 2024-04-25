//---- GPL ---------------------------------------------------------------------
//
// Copyright (C)  Stichting Deltares, 2011-2024.
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation version 3.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
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
 *  Wrapper for FORTRAN main program: LINT
 *
 *  29 Jan 2007
 */
#if defined (HAVE_CONFIG_H)
#   include "config.h"
#endif

#if defined (WIN32)
#   define FTNCALL __stdcall
#else
#   define FTNCALL
#   if defined (FTN_UNDERSCORE)
#      define MAIN_LINT main_lint_
#   else
#      if defined (HAVE_CONFIG_H)
#         define MAIN_LINT FC_FUNC(main_lint, MAIN_LINT)
#      else
#         define MAIN_LINT main_lint
#      endif
#   endif
#endif

#if defined (__cplusplus)
    extern "C" {
#endif

//extern void FTNCALL MAIN_LINT (void);

void MAIN_LINT (void);

#if defined (__cplusplus)
    }
#endif


int
main (
    int     argc,
    char *  argv[],
    char *  envp[]
    ) {

    MAIN_LINT ();

    return 0;
    }

