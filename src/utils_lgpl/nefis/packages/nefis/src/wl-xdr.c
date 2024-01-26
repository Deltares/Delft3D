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
 *   <nef-xdr.c> - Conversion from machine code and IEEE by swapping bytes
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
 *   Byte pattern: MSB first and LSB last
 */
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "btps.h"
#include "nefis.h" /* needed for definition of LENGTH_ERROR_MESSAGE */
#include "nef-def.h"
#include "wl-xdr.h"

BInt4 convert_ieee ( voidp * pvp_getal    ,
                     charp * pxdr_buffer ,
                     BUInt8  num_bytes   ,
                     BInt4   single_bytes,
                     BText   elm_type    ,
                     BInt4   from_xdr    )
/*
 * pvp_getal   : pointer to BVoid pointer of array (integer, real, ...)
 * pxdr_buffer : pointer to character pointer
 * num_bytes   : Total number of bytes occupied by array
 * single_bytes: Number of bytes occupied by a single array element
 * elm_type    : Kind of element (CHARACTE, INTEGER, REAL, LOGICAL or COMPLEX)
 * from_xdr    : 0    local representation to IEEE
 *               1    IEEE  representation to local
 */
/*
 * max_num     : Number of array elements
 */
{
  BInt4   direction;
  BInt4   i        ;
  BChar   tmpchar  ;

  if      ( from_xdr == 0 )
  {
    direction = XDR_ENCODE;
  }
  else if ( from_xdr == 1 )
  {
    direction = XDR_DECODE;
  }
  else
  {
    direction = XDR_FREE;
  }

/*
 *----------------------------------------------------------------------------
 * INTEGER
 *----------------------------------------------------------------------------
 */
  if ( strncmp(elm_type,"INTEGER",7) == 0 )
  {
    if ( single_bytes == sizeof(BInt2) )
    {
      if ( direction == XDR_ENCODE )
      {
        for ( i=0; i<num_bytes; i+=2 )
        {
          tmpchar                          = *( (BText)(*pvp_getal) + i   );
          *( (BText)(*pxdr_buffer) + i   ) = *( (BText)(*pvp_getal) + i+1 );
          *( (BText)(*pxdr_buffer) + i+1 ) = tmpchar;
        }
      }
      if ( direction == XDR_DECODE )
      {
        for ( i=0; i<num_bytes; i+=2 )
        {
          tmpchar                        = *( (BText)(*pxdr_buffer) + i   );
          *( (BText)(*pvp_getal) + i   ) = *( (BText)(*pxdr_buffer) + i+1 );
          *( (BText)(*pvp_getal) + i+1 ) = tmpchar;
        }
      }
    }
    else if ( single_bytes == sizeof(BInt4) )
    {
      if ( direction == XDR_ENCODE )
      {
        for ( i=0; i<num_bytes; i+=4 )
        {
          tmpchar                          = *( (BText)(*pvp_getal) + i   );
          *( (BText)(*pxdr_buffer) + i   ) = *( (BText)(*pvp_getal) + i+3 );
          *( (BText)(*pxdr_buffer) + i+3 ) = tmpchar;
          tmpchar                          = *( (BText)(*pvp_getal) + i+1 );
          *( (BText)(*pxdr_buffer) + i+1 ) = *( (BText)(*pvp_getal) + i+2 );
          *( (BText)(*pxdr_buffer) + i+2 ) = tmpchar;
        }
      }
      if ( direction == XDR_DECODE )
      {
        for ( i=0; i<num_bytes; i+=4 )
        {
          tmpchar                        = *( (BText)(*pxdr_buffer) + i   );
          *( (BText)(*pvp_getal) + i   ) = *( (BText)(*pxdr_buffer) + i+3 );
          *( (BText)(*pvp_getal) + i+3 ) = tmpchar;
          tmpchar                        = *( (BText)(*pxdr_buffer) + i+1 );
          *( (BText)(*pvp_getal) + i+1 ) = *( (BText)(*pxdr_buffer) + i+2 );
          *( (BText)(*pvp_getal) + i+2 ) = tmpchar;
        }
      }
    }
    else if ( single_bytes == sizeof(BInt8) )
    {
      if ( direction == XDR_ENCODE )
      {
        for ( i=0; i<num_bytes; i+=8 )
        {
          tmpchar                          = *( (BText)(*pvp_getal) + i   );
          *( (BText)(*pxdr_buffer) + i   ) = *( (BText)(*pvp_getal) + i+7 );
          *( (BText)(*pxdr_buffer) + i+7 ) = tmpchar;
          tmpchar                          = *( (BText)(*pvp_getal) + i+1 );
          *( (BText)(*pxdr_buffer) + i+1 ) = *( (BText)(*pvp_getal) + i+6 );
          *( (BText)(*pxdr_buffer) + i+6 ) = tmpchar;
          tmpchar                          = *( (BText)(*pvp_getal) + i+2 );
          *( (BText)(*pxdr_buffer) + i+2 ) = *( (BText)(*pvp_getal) + i+5 );
          *( (BText)(*pxdr_buffer) + i+5 ) = tmpchar;
          tmpchar                          = *( (BText)(*pvp_getal) + i+3 );
          *( (BText)(*pxdr_buffer) + i+3 ) = *( (BText)(*pvp_getal) + i+4 );
          *( (BText)(*pxdr_buffer) + i+4 ) = tmpchar;
        }
      }
      if ( direction == XDR_DECODE )
      {
        for ( i=0; i<num_bytes; i+=8 )
        {
          tmpchar                        = *( (BText)(*pxdr_buffer) + i   );
          *( (BText)(*pvp_getal) + i   ) = *( (BText)(*pxdr_buffer) + i+7 );
          *( (BText)(*pvp_getal) + i+7 ) = tmpchar;
          tmpchar                        = *( (BText)(*pxdr_buffer) + i+1 );
          *( (BText)(*pvp_getal) + i+1 ) = *( (BText)(*pxdr_buffer) + i+6 );
          *( (BText)(*pvp_getal) + i+6 ) = tmpchar;
          tmpchar                        = *( (BText)(*pxdr_buffer) + i+2 );
          *( (BText)(*pvp_getal) + i+2 ) = *( (BText)(*pxdr_buffer) + i+5 );
          *( (BText)(*pvp_getal) + i+5 ) = tmpchar;
          tmpchar                        = *( (BText)(*pxdr_buffer) + i+3 );
          *( (BText)(*pvp_getal) + i+3 ) = *( (BText)(*pxdr_buffer) + i+4 );
          *( (BText)(*pvp_getal) + i+4 ) = tmpchar;
        }
      }
    }
    else
    {
      nefis_errno   = 10001;
      nefis_errcnt += 1;
      sprintf(error_text,
        "This size of integer (%d) is not supported\n",
      single_bytes);
      return nefis_errno;
    }
  }
/*
 *----------------------------------------------------------------------------
 * REAL
 *----------------------------------------------------------------------------
 */
  else if ( strncmp(elm_type,"REAL",4) == 0 )
  {
    if ( single_bytes == sizeof(BRea4) )
    {
      if ( direction == XDR_ENCODE )
      {
        for ( i=0; i<num_bytes; i+=4 )
        {
          tmpchar                          = *( (BText)(*pvp_getal) + i   );
          *( (BText)(*pxdr_buffer) + i   ) = *( (BText)(*pvp_getal) + i+3 );
          *( (BText)(*pxdr_buffer) + i+3 ) = tmpchar;
          tmpchar                          = *( (BText)(*pvp_getal) + i+1 );
          *( (BText)(*pxdr_buffer) + i+1 ) = *( (BText)(*pvp_getal) + i+2 );
          *( (BText)(*pxdr_buffer) + i+2 ) = tmpchar;
        }
      }
      if ( direction == XDR_DECODE )
      {
        for ( i=0; i<num_bytes; i+=4 )
        {
          tmpchar                        = *( (BText)(*pxdr_buffer) + i   );
          *( (BText)(*pvp_getal) + i   ) = *( (BText)(*pxdr_buffer) + i+3 );
          *( (BText)(*pvp_getal) + i+3 ) = tmpchar;
          tmpchar                        = *( (BText)(*pxdr_buffer) + i+1 );
          *( (BText)(*pvp_getal) + i+1 ) = *( (BText)(*pxdr_buffer) + i+2 );
          *( (BText)(*pvp_getal) + i+2 ) = tmpchar;
        }
      }
    }
    else if ( single_bytes == sizeof(BRea8) )
    {
      if ( direction == XDR_ENCODE )
      {
        for ( i=0; i<num_bytes; i+=8 )
        {
          tmpchar                          = *( (BText)(*pvp_getal) + i   );
          *( (BText)(*pxdr_buffer) + i   ) = *( (BText)(*pvp_getal) + i+7 );
          *( (BText)(*pxdr_buffer) + i+7 ) = tmpchar;
          tmpchar                          = *( (BText)(*pvp_getal) + i+1 );
          *( (BText)(*pxdr_buffer) + i+1 ) = *( (BText)(*pvp_getal) + i+6 );
          *( (BText)(*pxdr_buffer) + i+6 ) = tmpchar;
          tmpchar                          = *( (BText)(*pvp_getal) + i+2 );
          *( (BText)(*pxdr_buffer) + i+2 ) = *( (BText)(*pvp_getal) + i+5 );
          *( (BText)(*pxdr_buffer) + i+5 ) = tmpchar;
          tmpchar                          = *( (BText)(*pvp_getal) + i+3 );
          *( (BText)(*pxdr_buffer) + i+3 ) = *( (BText)(*pvp_getal) + i+4 );
          *( (BText)(*pxdr_buffer) + i+4 ) = tmpchar;
        }
      }
      if ( direction == XDR_DECODE )
      {
        for ( i=0; i<num_bytes; i+=8 )
        {
          tmpchar                        = *( (BText)(*pxdr_buffer) + i   );
          *( (BText)(*pvp_getal) + i   ) = *( (BText)(*pxdr_buffer) + i+7 );
          *( (BText)(*pvp_getal) + i+7 ) = tmpchar;
          tmpchar                        = *( (BText)(*pxdr_buffer) + i+1 );
          *( (BText)(*pvp_getal) + i+1 ) = *( (BText)(*pxdr_buffer) + i+6 );
          *( (BText)(*pvp_getal) + i+6 ) = tmpchar;
          tmpchar                        = *( (BText)(*pxdr_buffer) + i+2 );
          *( (BText)(*pvp_getal) + i+2 ) = *( (BText)(*pxdr_buffer) + i+5 );
          *( (BText)(*pvp_getal) + i+5 ) = tmpchar;
          tmpchar                        = *( (BText)(*pxdr_buffer) + i+3 );
          *( (BText)(*pvp_getal) + i+3 ) = *( (BText)(*pxdr_buffer) + i+4 );
          *( (BText)(*pvp_getal) + i+4 ) = tmpchar;
        }
      }
    }
    else
    {
      nefis_errno   = 10002;
      nefis_errcnt += 1;
      sprintf(error_text,
       "This size of real (%d) is not supported\n",
        single_bytes);
      return nefis_errno;
    }
  }
/*
 *----------------------------------------------------------------------------
 * Character
 * Just copy the array, single bytes are still in IEEE format
 *----------------------------------------------------------------------------
 */
  else if ( strncmp(elm_type,"CHARACTE",8) == 0 )
  {
    if ( 1 == sizeof(BChar) )
    {
      if ( direction == XDR_ENCODE )
      {
        for ( i=0; i<num_bytes; i++ )
        {
          *( (BText)(*pxdr_buffer) +i ) = *( (BText)(*pvp_getal) +i );
        }
      }
      if ( direction == XDR_DECODE )
      {
        for ( i=0; i<num_bytes; i++ )
        {
          *( (BText)(*pvp_getal)+i )   = *( (BText)(*pxdr_buffer) +i ) ;
        }
      }
    }
    else
    {
      nefis_errno   = 10003;
      nefis_errcnt += 1;
      sprintf(error_text,
       "This size of character (!=1) is not supported\n");
      return nefis_errno;
    }
  }
/*
 *----------------------------------------------------------------------------
 * COMPLEX
 *  8 bytes: real 4 and imaginar 4 bytes
 * 16 bytes: real 8 and imaginar 8 bytes
 *----------------------------------------------------------------------------
 */
  else if ( strncmp(elm_type,"COMPLEX",7) == 0 )
  {
    if ( single_bytes == 2*sizeof(BRea4) )
    {
      if ( direction == XDR_ENCODE )
      {
        for ( i=0; i<num_bytes; i+=4 )
        {
          tmpchar                          = *( (BText)(*pvp_getal) + i   );
          *( (BText)(*pxdr_buffer) + i   ) = *( (BText)(*pvp_getal) + i+3 );
          *( (BText)(*pxdr_buffer) + i+3 ) = tmpchar;
          tmpchar                          = *( (BText)(*pvp_getal) + i+1 );
          *( (BText)(*pxdr_buffer) + i+1 ) = *( (BText)(*pvp_getal) + i+2 );
          *( (BText)(*pxdr_buffer) + i+2 ) = tmpchar;
        }
      }
      if ( direction == XDR_DECODE )
      {
        for ( i=0; i<num_bytes; i+=4 )
        {
          tmpchar                        = *( (BText)(*pxdr_buffer) + i   );
          *( (BText)(*pvp_getal) + i   ) = *( (BText)(*pxdr_buffer) + i+3 );
          *( (BText)(*pvp_getal) + i+3 ) = tmpchar;
          tmpchar                        = *( (BText)(*pxdr_buffer) + i+1 );
          *( (BText)(*pvp_getal) + i+1 ) = *( (BText)(*pxdr_buffer) + i+2 );
          *( (BText)(*pvp_getal) + i+2 ) = tmpchar;
        }
      }
    }
    else if ( single_bytes == 2*sizeof(BRea8) )
    {
      if ( direction == XDR_ENCODE )
      {
        for ( i=0; i<num_bytes; i+=8 )
        {
          tmpchar                          = *( (BText)(*pvp_getal) + i   );
          *( (BText)(*pxdr_buffer) + i   ) = *( (BText)(*pvp_getal) + i+7 );
          *( (BText)(*pxdr_buffer) + i+7 ) = tmpchar;
          tmpchar                          = *( (BText)(*pvp_getal) + i+1 );
          *( (BText)(*pxdr_buffer) + i+1 ) = *( (BText)(*pvp_getal) + i+6 );
          *( (BText)(*pxdr_buffer) + i+6 ) = tmpchar;
          tmpchar                          = *( (BText)(*pvp_getal) + i+2 );
          *( (BText)(*pxdr_buffer) + i+2 ) = *( (BText)(*pvp_getal) + i+5 );
          *( (BText)(*pxdr_buffer) + i+5 ) = tmpchar;
          tmpchar                          = *( (BText)(*pvp_getal) + i+3 );
          *( (BText)(*pxdr_buffer) + i+3 ) = *( (BText)(*pvp_getal) + i+4 );
          *( (BText)(*pxdr_buffer) + i+4 ) = tmpchar;
        }
      }
      if ( direction == XDR_DECODE )
      {
        for ( i=0; i<num_bytes; i+=8 )
        {
          tmpchar                        = *( (BText)(*pxdr_buffer) + i   );
          *( (BText)(*pvp_getal) + i   ) = *( (BText)(*pxdr_buffer) + i+7 );
          *( (BText)(*pvp_getal) + i+7 ) = tmpchar;
          tmpchar                        = *( (BText)(*pxdr_buffer) + i+1 );
          *( (BText)(*pvp_getal) + i+1 ) = *( (BText)(*pxdr_buffer) + i+6 );
          *( (BText)(*pvp_getal) + i+6 ) = tmpchar;
          tmpchar                        = *( (BText)(*pxdr_buffer) + i+2 );
          *( (BText)(*pvp_getal) + i+2 ) = *( (BText)(*pxdr_buffer) + i+5 );
          *( (BText)(*pvp_getal) + i+5 ) = tmpchar;
          tmpchar                        = *( (BText)(*pxdr_buffer) + i+3 );
          *( (BText)(*pvp_getal) + i+3 ) = *( (BText)(*pxdr_buffer) + i+4 );
          *( (BText)(*pvp_getal) + i+4 ) = tmpchar;
        }
      }
    }
    else
    {
      nefis_errno   = 10004;
      nefis_errcnt += 1;
      sprintf(error_text,
        "This size of complex (%d) is not supported\n",
        single_bytes);
      return nefis_errno;
    }
  }
/*
 *----------------------------------------------------------------------------
 * Logical
 *  4 bytes: fortran 66 standard
 *----------------------------------------------------------------------------
 */
  else if ( strncmp(elm_type,"LOGICAL",7) == 0 )
  {
    if ( single_bytes == sizeof(BInt4) )
    {
      if ( direction == XDR_ENCODE )
      {
        for ( i=0; i<num_bytes; i+=4 )
        {
          tmpchar                          = *( (BText)(*pvp_getal) + i   );
          *( (BText)(*pxdr_buffer) + i   ) = *( (BText)(*pvp_getal) + i+3 );
          *( (BText)(*pxdr_buffer) + i+3 ) = tmpchar;
          tmpchar                          = *( (BText)(*pvp_getal) + i+1 );
          *( (BText)(*pxdr_buffer) + i+1 ) = *( (BText)(*pvp_getal) + i+2 );
          *( (BText)(*pxdr_buffer) + i+2 ) = tmpchar;
        }
      }
      if ( direction == XDR_DECODE )
      {
        for ( i=0; i<num_bytes; i+=4 )
        {
          tmpchar                        = *( (BText)(*pxdr_buffer) + i   );
          *( (BText)(*pvp_getal) + i   ) = *( (BText)(*pxdr_buffer) + i+3 );
          *( (BText)(*pvp_getal) + i+3 ) = tmpchar;
          tmpchar                        = *( (BText)(*pxdr_buffer) + i+1 );
          *( (BText)(*pvp_getal) + i+1 ) = *( (BText)(*pxdr_buffer) + i+2 );
          *( (BText)(*pvp_getal) + i+2 ) = tmpchar;
        }
      }
    }
    else if ( single_bytes == sizeof(BInt2) )
    {
      if ( direction == XDR_ENCODE )
      {
        for ( i=0; i<num_bytes; i+=2 )
        {
          tmpchar                          = *( (BText)(*pvp_getal) + i   );
          *( (BText)(*pxdr_buffer) + i   ) = *( (BText)(*pvp_getal) + i+1 );
          *( (BText)(*pxdr_buffer) + i+1 ) = tmpchar;
        }
      }
      if ( direction == XDR_DECODE )
      {
        for ( i=0; i<num_bytes; i+=2 )
        {
          tmpchar                        = *( (BText)(*pxdr_buffer) + i   );
          *( (BText)(*pvp_getal) + i   ) = *( (BText)(*pxdr_buffer) + i+1 );
          *( (BText)(*pvp_getal) + i+1 ) = tmpchar;
        }
      }
    }
    else
    {
      nefis_errno   = 10005;
      nefis_errcnt += 1;
      sprintf(error_text,
        "This size of logical (%d) is not supported\n",
        single_bytes);
      return nefis_errno;
    }
  }
  else
  {
    nefis_errno   = 10006;
    nefis_errcnt += 1;
    sprintf(error_text,
            "This element type is not supported \'%s\'\n",
            elm_type);
    return nefis_errno;
  }

  return nefis_errno;
}
