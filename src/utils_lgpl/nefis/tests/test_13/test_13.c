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
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <limits.h>

#if !defined(WIN32)
#  include <rpc/types.h>
#  include <rpc/xdr.h>
#else
#  include <sys/types.h>
#  include <rpc/rpc.h>
#  include <rpc/xdr.h>
#endif

#include "btps.h"
#include "nef-def.h"
#include "nefis.h"
#include "df.h"         /* prototype for convert_ieee */

#define BUFSIZE 11

static BUInt4 *localVal1, *localVal2, *localVal3;
static BChar  *ieeeVal1, *ieeeVal2;

/* Deze test controleert of xdr_long hetzelfde doet als xdr_u_long
 */
int main()
{
  XDR    xdrs1, xdrs2, xdrs3;
  BInt4  i;
  BUInt4 val;

  ieeeVal1 = (BChar*)malloc( sizeof(BUInt4)*BUFSIZE );
  ieeeVal2 = (BChar*)malloc( sizeof(BUInt4)*BUFSIZE );
  localVal1 = (BUInt4*)malloc( BUFSIZE * sizeof(BUInt4) );
  localVal2 = (BUInt4*)malloc( BUFSIZE * sizeof(BUInt4) );
  localVal3 = (BUInt4*)malloc( BUFSIZE * sizeof(BUInt4) );

  printf("Initialise\n");
  localVal1[0] = 0;
  localVal2[0] = 0;
  val = 1;
  for (i=1; i<BUFSIZE-1; i++)
  {
    localVal1[i] = val;
    localVal2[i] = val;
    val*=10;
  }
  localVal1[BUFSIZE-1] = ULONG_MAX;
  localVal2[BUFSIZE-1] = ULONG_MAX;

  for (i=0; i<sizeof(BUInt4)*BUFSIZE; i++)
  {
    ieeeVal1[i] = '1';
    ieeeVal2[i] = '2';
  }

  printf("Encode\n");
  xdrmem_create( &xdrs1, ieeeVal1, sizeof(BUInt4)*BUFSIZE, XDR_ENCODE);
  xdrmem_create( &xdrs2, ieeeVal2, sizeof(BUInt4)*BUFSIZE, XDR_ENCODE);
  xdr_vector( &xdrs1, (BText)localVal1, BUFSIZE, sizeof(BUInt4), (xdrproc_t)xdr_long );
  xdr_vector( &xdrs2, (BText)localVal2, BUFSIZE, sizeof(BUInt4), (xdrproc_t)xdr_u_long );
  xdr_destroy( &xdrs1 );
  xdr_destroy( &xdrs2 );

  printf("Check\n");
  if (memcmp( ieeeVal1, ieeeVal2, sizeof(BUInt4)*BUFSIZE ) != 0)
  {
    printf("\nError while encoding values!!\n");
    for (i=0; i<sizeof(BUInt4)*BUFSIZE; i++)
    {
      if (ieeeVal1[i] != ieeeVal2[i])
      {
        printf("i: %u\n", i);
        printf("index: %u\n", i/sizeof(BUInt4));
        printf("localVal1: %u\n", localVal1[i/sizeof(BUInt4)]);
        printf("localVal2: %u\n", localVal2[i/sizeof(BUInt4)]);
        printf("ieeeVal1: %u\n", (BUInt4)ieeeVal1[i]);
        printf("ieeeVal2: %u\n", (BUInt4)ieeeVal2[i]);
        exit( 1 );
      }
    }
  }

  printf("Decode\n");
  xdrmem_create( &xdrs3, ieeeVal2, sizeof(BUInt4)*BUFSIZE, XDR_DECODE);
  xdr_vector( &xdrs3, (BText)localVal3, BUFSIZE, sizeof(BUInt4), (xdrproc_t)xdr_long );
  xdr_destroy( &xdrs3 );

  printf("Check\n");
  if (memcmp( localVal1, localVal3, sizeof(BUInt4)*BUFSIZE ) != 0)
  {
    printf("\nError while decoding values!!\n");
    for (i=0; i<BUFSIZE; i++)
    {
      if (localVal1[i] != localVal3[i])
      {
        printf("i: %u\n", i*sizeof(BUInt4));
        printf("index: %u\n", i);
        printf("localVal1: %u\n", localVal1[i]);
        printf("localVal3: %u\n", localVal3[i]);
        printf("ieeeVal1: %u, %u, %u, %u\n",
                          (BUInt4)ieeeVal1[i*sizeof(BUInt4)],
                          (BUInt4)ieeeVal1[i*sizeof(BUInt4)+1],
                          (BUInt4)ieeeVal1[i*sizeof(BUInt4)+2],
                          (BUInt4)ieeeVal1[i*sizeof(BUInt4)+3]);
        printf("ieeeVal2: %u, %u, %u, %u\n",
                          (BUInt4)ieeeVal2[i*sizeof(BUInt4)],
                          (BUInt4)ieeeVal2[i*sizeof(BUInt4)+1],
                          (BUInt4)ieeeVal2[i*sizeof(BUInt4)+2],
                          (BUInt4)ieeeVal2[i*sizeof(BUInt4)+3]);
        exit( 1 );
      }
    }
  }

  printf( "No errors\n" );

  return 0;
}
