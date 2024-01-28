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
 *   <wl-xdr.c> - Conversion from machine code and IEEE by calling XDR routines
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
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#if defined(_WIN32)
#  include <rpc\types.h>
#  include <rpc\xdr.h>
#endif
#if defined(salford32)
#  include <rpc/types.h>
#  include <rpc/xdr.h>
#endif
#if defined(USE_HPUX) || defined(USE_SGI) || defined(USE_SUN) || defined(HAVE_CONFIG_H) || defined(GNU_PC)
#include <sys/types.h>
#include <rpc/rpc.h>
#include <rpc/xdr.h>
#endif

#include "btps.h"
#include "nef-def.h"
#include "nef-xdr.h"

typedef BData voidp;
typedef BText charp;

BInt4 convert_ieee ( voidp * pvp_getal   ,  /* local representation */
                     charp * pxdr_buffer ,  /* ieee  representation */
                     BInt4   num_bytes   ,
                     BInt4   single_bytes,
                     BText   elm_type    ,
                     BInt4   from_xdr    )
/*
 * pvp_getal   : pointer to BVoid pointer of array (integer, real, ...)
 * pxdr_buffer : pointer to character pointer
 * num_bytes   : Total number of bytes occupied by array
 * single_bytes: Number of bytes occupied by a single array element
 * elm_type    : Kind of element (CHARACTER, INTEGER, REAL, LOGICAL or COMPLEX)
 * from_xdr    : 0    local representation to IEEE
 *               1    IEEE  representation to local
 */
/*
 * max_num     : Number of array elements
 */
{
  static  XDR xdrs ;
  enum xdr_op direction;
  BInt4   i      ;
  BInt4   max_num;
  BChar   tmpchar;
  BInt4 * ip;
  BInt4 * lp;
  BRea4 * fp;
  BRea8 * dp;
  BInt4   error = 1;

  max_num = num_bytes/single_bytes;

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

  xdrmem_create ( &xdrs, *pxdr_buffer, num_bytes, direction);

/*
 * Use XDR routines
 */
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
#if defined(USE_SUN)
          tmpchar                          = *( (BText)(*pvp_getal) + i   );
          *( (BText)(*pxdr_buffer) + i+1 ) = *( (BText)(*pvp_getal) + i+1 );
          *( (BText)(*pxdr_buffer) + i   ) = tmpchar;
#else
          tmpchar                          = *( (BText)(*pvp_getal) + i   );
          *( (BText)(*pxdr_buffer) + i   ) = *( (BText)(*pvp_getal) + i+1 );
          *( (BText)(*pxdr_buffer) + i+1 ) = tmpchar;
#endif
        }
      }
      if ( direction == XDR_DECODE )
      {
        for ( i=0; i<num_bytes; i+=2 )
        {
#if defined(USE_SUN)
          tmpchar                        = *( (BText)(*pxdr_buffer) + i   );
          *( (BText)(*pvp_getal) + i+1 ) = *( (BText)(*pxdr_buffer) + i+1 );
          *( (BText)(*pvp_getal) + i   ) = tmpchar;
#else
          tmpchar                        = *( (BText)(*pxdr_buffer) + i   );
          *( (BText)(*pvp_getal) + i   ) = *( (BText)(*pxdr_buffer) + i+1 );
          *( (BText)(*pvp_getal) + i+1 ) = tmpchar;
#endif
        }
      }
      error = 0;
    }
    else if ( single_bytes == sizeof(BInt4) )
    {
      lp = (BInt4 *) *pvp_getal;
      xdr_vector( &xdrs, (BText)lp, max_num, single_bytes,(xdrproc_t) xdr_long );
      if ( direction == XDR_ENCODE )
      {
        *pxdr_buffer = (BText) *pxdr_buffer;
      }
      if ( direction == XDR_DECODE )
      {
        *pvp_getal   = (BData) lp;
      }
      error=0;
    }
    else
    {
      nefis_errno = 10001;
      sprintf(error_text,
        "This size of integer (%ld) is not supported\n",
      single_bytes);
      return 1;
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
      fp = (BRea4 *) *pvp_getal;
      xdr_vector( &xdrs, (BText)fp, max_num, single_bytes,(xdrproc_t) xdr_float );
      if ( direction == XDR_ENCODE )
      {
        *pxdr_buffer = (BText) *pxdr_buffer;
      }
      if ( direction == XDR_DECODE )
      {
        *pvp_getal   = (BData) fp;
      }
      error=0;
    }
    else if ( single_bytes == sizeof(BRea8) )
    {
      dp = (BRea8 *) *pvp_getal;
      xdr_vector( &xdrs, (BText)dp, max_num, single_bytes,(xdrproc_t) xdr_double );
      if ( direction == XDR_ENCODE )
      {
        *pxdr_buffer = (BText) *pxdr_buffer;
      }
      if ( direction == XDR_DECODE )
      {
        *pvp_getal   = (BData) dp;
      }
      error=0;
    }
    else
    {
      nefis_errno = 10002;
      sprintf(error_text,
       "This size of real (%ld) is not supported\n",
        single_bytes);
      return 1;
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
    error = 0;
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
      fp = (BRea4 *) *pvp_getal;
      xdr_vector( &xdrs, (BText)fp, 2*max_num, single_bytes/2,(xdrproc_t) xdr_float );
      if ( direction == XDR_ENCODE )
      {
        *pxdr_buffer = (BText) *pxdr_buffer;
      }
      if ( direction == XDR_DECODE )
      {
        *pvp_getal   = (BData) fp;
      }
      error=0;
    }
    else if ( single_bytes == 2*sizeof(BRea8) )
    {
      dp = (BRea8 *) *pvp_getal;
      xdr_vector( &xdrs, (BText)dp, 2*max_num, single_bytes/2,(xdrproc_t) xdr_double );
      if ( direction == XDR_ENCODE )
      {
        *pxdr_buffer = (BText) *pxdr_buffer;
      }
      if ( direction == XDR_DECODE )
      {
        *pvp_getal   = (BData) dp;
      }
      error=0;
    }
    else
    {
      nefis_errno = 10004;
      sprintf(error_text,
        "This size of complex (%ld) is not supported\n",
        single_bytes);
      return 1;
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
      ip = (BInt4 *) *pvp_getal;
      xdr_vector( &xdrs, (BText)ip, max_num, single_bytes,(xdrproc_t) xdr_long );
      if ( direction == XDR_ENCODE )
      {
        *pxdr_buffer = (BText) *pxdr_buffer;
      }
      if ( direction == XDR_DECODE )
      {
        *pvp_getal   = (BData) ip;
      }
      error=0;
    }
    else if ( single_bytes == sizeof(BInt2) )
    {
      if ( direction == XDR_ENCODE )
      {
        for ( i=0; i<num_bytes; i+=2 )
        {
#if defined(USE_SUN)
          tmpchar                          = *( (BText)(*pvp_getal) + i   );
          *( (BText)(*pxdr_buffer) + i+1 ) = *( (BText)(*pvp_getal) + i+1 );
          *( (BText)(*pxdr_buffer) + i   ) = tmpchar;
#else
          tmpchar                          = *( (BText)(*pvp_getal) + i   );
          *( (BText)(*pxdr_buffer) + i   ) = *( (BText)(*pvp_getal) + i+1 );
          *( (BText)(*pxdr_buffer) + i+1 ) = tmpchar;
#endif
        }
      }
      if ( direction == XDR_DECODE )
      {
        for ( i=0; i<num_bytes; i+=2 )
        {
#if defined(USE_SUN)
          tmpchar                        = *( (BText)(*pxdr_buffer) + i   );
          *( (BText)(*pvp_getal) + i+1 ) = *( (BText)(*pxdr_buffer) + i+1 );
          *( (BText)(*pvp_getal) + i   ) = tmpchar;
#else
          tmpchar                        = *( (BText)(*pxdr_buffer) + i   );
          *( (BText)(*pvp_getal) + i   ) = *( (BText)(*pxdr_buffer) + i+1 );
          *( (BText)(*pvp_getal) + i+1 ) = tmpchar;
#endif
        }
      }
      error = 0;
    }
    else
    {
      nefis_errno = 10005;
      sprintf(error_text,
        "This size of logical (%ld) is not supported\n",
        single_bytes);
      return 1;
    }
  }
  else
  {
    nefis_errno = 10005;
    sprintf(error_text,
            "This element type is not supported \'%s\'\n",
            elm_type);
    return 1;
  }
  xdr_destroy   ( &xdrs );

  return error;
}
