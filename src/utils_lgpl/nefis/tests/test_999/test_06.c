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
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "nefis.h"

#define MAX_DESC   64
#define MAX_DIM     5
#define MAX_ELM_AC  9
#define MAX_NAME   16
#define MAX_TYPE    8

int main()
{
  long  error;
  BChar error_string[134];
  long  fds;
  long  i  ;
  char  teller[10];

  BText   elm_desc    ;
  BInt4 * elm_dimens  ;
  BText   elm_name    ;
  BChar   elm_names[250][MAX_NAME+1];
  BInt4   elm_num_dim = -1 ;
  BText   elm_quantity;
  BInt4   elm_single_byte;
  BText   elm_type    ;
  BText   elm_unity   ;

  BText   cel_name    ;
  BInt4   cel_num_dim ;
  BText   grp_name    ;
  BText   grp_defined ;
  BText   cel_names   ;

  elm_name     = (BText) malloc( sizeof(BChar) * (MAX_NAME + 1) );
  elm_type     = (BText) malloc( sizeof(BChar) * (MAX_TYPE + 1) );
  elm_quantity = (BText) malloc( sizeof(BChar) * (MAX_NAME + 1) );
  elm_unity    = (BText) malloc( sizeof(BChar) * (MAX_NAME + 1) );
  elm_desc     = (BText) malloc( sizeof(BChar) * (MAX_DESC + 1) );
  elm_dimens   = (BInt4 *) malloc( sizeof(BInt4 ) * MAX_DIM  );

  cel_name     = (BText) malloc( sizeof(BChar) * (MAX_NAME + 1) );

  grp_name     = (BText) malloc( sizeof(BChar) * (MAX_NAME + 1) );
  grp_defined  = (BText) malloc( sizeof(BChar) * (MAX_NAME + 1) );

  cel_names    = (BText) malloc( sizeof(BChar) * (MAX_NAME + 1) * MAX_DIM );

/*
 * Start of program
 */

  fds = -1;
/*
 * Create NEFIS file
 */
  error = Crenef( &fds, "datac16.dat","datac16.def", 'N', 'c');
  if (error!=0) {error = Neferr( 1, error_string);}

  cel_num_dim = 100;

/*
 * Define elements
 */
  for (i=0; i<cel_num_dim; i++)
  {
      strcpy(elm_name    ,"elmname_");
      sprintf(teller,"%03d",i+1);
      strcat(elm_name, teller);

      strcpy(elm_names[i],elm_name);

      strcpy(elm_type    ,"REAL");
      strcpy(elm_quantity,"Waterstanden");
      strcpy(elm_unity   ,"meter");
      strcpy(elm_desc    ,"Test tot 100 elmenten per cel");
      elm_single_byte = 4;
      elm_num_dim  = 1;
      elm_dimens[0]= 11;

      if (error == 0 ) {
        error  = Defelm(&fds , elm_names[i], elm_type   , elm_single_byte,
                               elm_quantity, elm_unity  , elm_desc,
                               elm_num_dim , elm_dimens  );
      }
  }
  if (error!=0) {error = Neferr( 1, error_string);}

/*
 * Define cel
 */
  strcpy( cel_name    ,"cel_name_1");
  if (error == 0 ) {
    printf(" Define first cel     \n");
    error  = Defcel2( &fds, cel_name, cel_num_dim , elm_names);
  }
  if (error!=0) {error = Neferr( 1, error_string);}

  error = Clsnef( &fds );
  return(0);
}
