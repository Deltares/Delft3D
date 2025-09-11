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
/*
 *   <er.c> - Function to print error numbers and message
 *
 *
 *   J. Mooiman
 */
/*
 *   CVS/RCS/PVCS keywords
 *   ....
 */
/*
 *   Comment:
 */

#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#if defined(_WIN32) || defined(GNU_PC)
#  include <io.h>
#elif defined(salford32)
#  include <io.h>
#  include <windows.h>
#endif

#include "btps.h"
#include "nefis.h" /* needed for definition of LENGTH_ERROR_MESSAGE */
#include "nef-def.h"
#include "er.h"

BInt4 nefis_errno;
BInt4 nefis_errcnt;

/*
 *----------------------------------------------------------------------
 * Definition of subroutine call for various hardware platforms using
 * define macros
 *----------------------------------------------------------------------
 *
 * Error numbers:
 * c2c       1000 + error_code
 * f2c       2000 + error_code
 * gt        3000 + error_code
 * pt        4000 + error_code
 * df        5000 + error_code
 * gp        6000 + error_code
 * hs        7000 + error_code
 * oc        8000 + error_code
 * rt        9000 + error_code
 * wl-xdr   10000 + error_code
 *
 *
 * Input : print_flag     0: Print error message into string error_string
 *                        1: Print error message into stdout
 *                        2: Print error message into stderr
 * Output: error_string   String contains error messages
 * Return: 0              No other value
 *
 * HACK: Length error_string have to be larger then error_text !!!!!
 *
 */
BInt4 nefis_error ( BInt4 print_flag  , /* I,print error at stderr (yes==1) */
                    BText error_string)
{
  FILE * fp_out;

  fp_out = stdout;
  if ( print_flag == 2 )
  {
      fp_out = stderr;
  }

  if ( print_flag != 0 )
  {
    if ( nefis_errno != 0 )
    {
      fprintf(fp_out, "\nNEFIS message %d:\n", nefis_errno);
      fprintf(fp_out, " %s\n", error_text );
      error_text[0] = '\0';
      nefis_errno   = 0;
    }
    else if ( nefis_errcnt != 0 )
    {
      fprintf(fp_out,"\nThe total number of NEFIS errors/warnings is %d\n",
                     nefis_errcnt);
    }
    else
    {
      fprintf(fp_out,"\nNo NEFIS errors encountered\n");
    }
  }
  else
  {
    if ( nefis_errno != 0 )
    {
      sprintf(error_string, "\nNEFIS message %d:\n", nefis_errno);
      strcat (error_string, error_text);
      error_text[0] = '\0';
      nefis_errno   = 0;
    }
    else if ( nefis_errcnt != 0 )
    {
      sprintf(error_string,
              "The total number of NEFIS errors/warnings is %d",
              nefis_errcnt);
    }
    else
    {
      sprintf(error_string, "No NEFIS errors encountered");
    }
  }
  return 0;
}
