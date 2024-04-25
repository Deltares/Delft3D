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
#if defined(salford32)
#  include <windows.h>
#  include <io.h>
#elif defined(WIN32)
#  include <io.h>
#endif
#include "btps.h"
#include "nefis.h"

#define MAX_CEL_DIM 100
#define MAX_DESC   64
#define MAX_DIM     5
#define MAX_ELM_AC  9
#define MAX_NAME   16
#define MAX_TYPE    8

int main()
{
  BText   cel_name    ;
  BText   cel_names   ;
  BInt4   cel_num_dim =-1 ;
  BChar   coding          ;
  BChar   dat_file    [21];
  BChar   def_file    [21];
  BText   elm_desc    ;
  BInt4 * elm_dimens  ;
  BText   elm_name    ;
  BText * elm_names   ;
  BInt4   elm_num_dim =-1 ;
  BText   elm_quantity;
  BInt4   elm_single_byte;
  BText   elm_type    ;
  BText   elm_unity   ;
  BChar   error_string[LENGTH_ERROR_MESSAGE];
  BInt4   fd_nefis=-1;
  BRea4 * getal;
  BText   grp_defined ;
  BInt4 * grp_dimens  ;
  BText   grp_name    ;
  BInt4   grp_num_dim =-1 ;
  BInt4 * grp_order   ;
  BInt4   i, j, error=0;
  BInt4   max_index       ;
  BChar   rdwr            ;
  BInt4   usr_index [5][3];
  BInt4 * usr_order   ;
  BText   nef_version ;

  error = Getnfv(&nef_version);

  printf(" -----------------------------------------------\n");
  printf(" Version: %s\n", nef_version+4);
  printf(" -----------------------------------------------\n");

  getal        = (BRea4 * ) malloc( sizeof( BRea4   ) *  60 );

  elm_name     = (BText   ) malloc( sizeof(BChar) * (MAX_NAME + 1) );
  elm_type     = (BText   ) malloc( sizeof(BChar) * (MAX_TYPE + 1) );
  elm_quantity = (BText   ) malloc( sizeof(BChar) * (MAX_NAME + 1) );
  elm_unity    = (BText   ) malloc( sizeof(BChar) * (MAX_NAME + 1) );
  elm_desc     = (BText   ) malloc( sizeof(BChar) * (MAX_DESC + 1) );
  cel_name     = (BText   ) malloc( sizeof(BChar) * (MAX_NAME + 1) );
  grp_name     = (BText   ) malloc( sizeof(BChar) * (MAX_NAME + 1) );
  grp_defined  = (BText   ) malloc( sizeof(BChar) * (MAX_NAME + 1) );
  cel_names    = (BText   ) malloc( sizeof(BChar) * (MAX_NAME + 1) * MAX_CEL_DIM );

  elm_dimens   = (BInt4 *) malloc( sizeof(BInt4 ) * MAX_DIM  );
  grp_dimens   = (BInt4 *) malloc( sizeof(BInt4 ) * MAX_DIM  );
  grp_order    = (BInt4 *) malloc( sizeof(BInt4 ) * MAX_DIM  );
  usr_order    = (BInt4 *) malloc( sizeof(BInt4 ) * MAX_DIM  );

  elm_names    = (char **) malloc( sizeof( char *) * MAX_CEL_DIM);
  for (i=0; i<MAX_NAME+1; i++) {
	  elm_names[i] = (char *) malloc( sizeof(char) * (MAX_NAME +1));
  }

  rdwr = 'C';
  coding = 'B';

  for ( i=0; i<MAX_DIM; i++) {
    elm_dimens[i] = 0;
    grp_dimens[i] = 0;
    grp_order [i] = 0;
    usr_order [i] = 0;
    for ( j=0; j<3; j++) {
      usr_index[i][j] = 0;
    }
  }

#if DEBUG_NEFIS
  printf("Input         \n");
  printf(" Error    : %d\n", error);
  printf(" ReadWrite: %c\n", rdwr );
#endif

  strcpy(dat_file,"data_c10.dat");
  strcpy(def_file,"data_c10.def");

#if DEBUG_NEFIS
  printf(" Filenames: %s %s\n", dat_file, def_file);
#endif

/*
 * Open NEFIS files
 */

  error = Crenef( &fd_nefis , dat_file, def_file, coding,  rdwr);

/*
 * elm_type: real, integer, character
 */

/*-------------------------------------------------------------------------*/
/*---------------------1234567890123456------------------------------------*/

  strcpy(elm_name    ,"Real4");
  strcpy(elm_type    ,"Real");
  strcpy(elm_quantity,"Real-qty");
  strcpy(elm_unity   ,"Real-unit");
  strcpy(elm_desc    ,"Real-desc");
  elm_single_byte = 4;
  elm_num_dim  = 2;
  elm_dimens[0]= 3;
  elm_dimens[1]= 5;
  printf(" Define Real(4) element\n");
  error  = Defelm(&fd_nefis , elm_name    , elm_type   , elm_single_byte,
                              elm_quantity, elm_unity  , elm_desc,
                              elm_num_dim , elm_dimens  );
  if (error != 0)
  {
     error = Neferr( 1, error_string);
     return 0;
  }

/*-------------------------------------------------------------------------*/

    cel_num_dim = 1;
    strcpy( cel_name    ,"cel1");
    strcpy( elm_names[0],"Real4");
    printf(" Define Real4 cel     \n");
    error  = Defcel2( &fd_nefis, cel_name, cel_num_dim , elm_names);
    if (error != 0)
    {
      error = Neferr( 1, error_string);
      return 0;
    }

/*-------------------------------------------------------------------------*/

    strcpy( grp_name    ,"grp1");
    strcpy(&cel_names[0],"cel1");
    grp_num_dim   = 1;
    grp_dimens[0] = 0;
    grp_order [0] = 1;
    printf(" Define Real4 grp1\n");
    error  = Defgrp ( &fd_nefis  , grp_name  , cel_names, grp_num_dim,
                                   grp_dimens, grp_order);
    if (error != 0)
    {
       error = Neferr( 1, error_string);
       return 0;
    }

    strcpy( grp_name    ,"grp2");
    strcpy(&cel_names[0],"cel1");
    grp_num_dim   = 1;
    grp_dimens[0] = 0;
    grp_order [0] = 1;
    printf(" Define Real grp2\n");
    error  = Defgrp ( &fd_nefis  , grp_name  , cel_names, grp_num_dim,
                                   grp_dimens, grp_order);
    if (error != 0)
    {
       error = Neferr( 1, error_string);
       return 0;
    }

    strcpy( grp_name    ,"grp3");
    strcpy(&cel_names[0],"cel1");
    grp_num_dim   = 1;
    grp_dimens[0] = 1601;
    grp_order [0] = 1;
    printf(" Define Real grp3\n");
    error  = Defgrp ( &fd_nefis  , grp_name  , cel_names, grp_num_dim,
                                   grp_dimens, grp_order);
    if (error != 0)
    {
       error = Neferr( 1, error_string);
       return 0;
    }

/*-------------------------------------------------------------------------*/

    strcpy(grp_name     ,"var_250");
    strcpy(grp_defined  ,"grp1");
    printf(" Create data group \'%s\'\n", grp_name);
    error  = Credat( &fd_nefis, grp_name, grp_defined);
    if (error != 0)
    {
       error = Neferr( 1, error_string);
       return 0;
    }

    strcpy(grp_name     ,"var_1601");
    strcpy(grp_defined  ,"grp2");
    printf(" Create data group \'%s\'\n", grp_name);
    error  = Credat( &fd_nefis, grp_name, grp_defined);
    if (error != 0)
    {
       error = Neferr( 1, error_string);
       return 0;
    }

    strcpy(grp_name     ,"fixed_1601");
    strcpy(grp_defined  ,"grp3");
    printf(" Create data group \'%s\'\n", grp_name);
    error  = Credat( &fd_nefis, grp_name, grp_defined);
    if (error != 0)
    {
       error = Neferr( 1, error_string);
       return 0;
    }

/*-------------------------------------------------------------------------*/
/* Put data into Nefis file                                                 */
/*-------------------------------------------------------------------------*/

    strcpy(elm_name,"Real4");
    strcpy(grp_name, "var_250");
    printf(" Put data in group \'%s\'\n", grp_name);
     for ( i=0; i<250; i++)
     {
       usr_index[0][0] = i+1;
       usr_index[0][1] = i+1;
       usr_index[0][2] = 1;
       usr_order[0]    = 1;
       for ( j=0; j<15; j++)
       {
         getal[j]= (float) ((i+1)*1000 + j+1);
       }
       error = Putelt(&fd_nefis, grp_name , elm_name,
                       (BInt4 *) usr_index, usr_order, (BData) getal   );
     }
     if (error != 0)
     {
        error = Neferr( 1, error_string);
        return 0;
     }

     strcpy(elm_name,"Real4");
     strcpy(grp_name, "var_1601");
    printf(" Put data in group \'%s\'\n", grp_name);
     for ( i=0; i<1601; i++)
     {
       usr_index[0][0] = i+1;
       usr_index[0][1] = i+1;
       usr_index[0][2] = 1;
       usr_order[0]    = 1;
       for ( j=0; j<15; j++)
       {
         getal[j]= (float) ((i+1)*1000 + j+1);
       }
       error = Putelt(&fd_nefis, grp_name , elm_name,
                       (BInt4 *) usr_index, usr_order, getal   );
     }
     if (error != 0)
     {
        error = Neferr( 1, error_string);
        return 0;
     }

     strcpy(elm_name,"Real4");
     strcpy(grp_name, "fixed_1601");
    printf(" Put data in group \'%s\'\n", grp_name);
     for ( i=0; i<1601; i++)
     {
       usr_index[0][0] = i+1;
       usr_index[0][1] = i+1;
       usr_index[0][2] = 1;
       usr_order[0]    = 1;
       for ( j=0; j<15; j++)
       {
         getal[j]= (float) ((i+1)*1000 + j+1);
       }
       error = Putelt(&fd_nefis, grp_name , elm_name,
                       (BInt4 *) usr_index, usr_order, getal  );
     }
     if (error != 0)
     {
        error = Neferr( 1, error_string);
        return 0;
     }

/*-------------------------------------------------------------------------*/
/*  Determine sizes of groups                                              */
/*-------------------------------------------------------------------------*/

    strcpy(grp_name,"var_250");
    error = Inqmxi (&fd_nefis, grp_name, &max_index);
    printf(" Largest dimension for group <%s>: %d\n", grp_name, max_index);
    if (error != 0)
    {
       error = Neferr( 1, error_string);
       return 0;
    }

    strcpy(grp_name,"var_1601");
    error = Inqmxi (&fd_nefis, grp_name, &max_index);
    printf(" Largest dimension for group <%s>: %d\n", grp_name, max_index);
    if (error != 0)
    {
       error = Neferr( 1, error_string);
       return 0;
    }

    strcpy(grp_name,"fixed_1601");
    error = Inqmxi (&fd_nefis, grp_name, &max_index);
    printf(" Largest dimension for group <%s>: %d\n", grp_name, max_index);
    if (error != 0)
    {
       error = Neferr( 1, error_string);
       return 0;
    }


/*-------------------------------------------------------------------------*/
/*   Get elements                                                          */
/*-------------------------------------------------------------------------*/

    strcpy(elm_name, "Real4");
    strcpy(grp_name, "var_250");
    printf("\n Get data from group \'%s\'\n", grp_name);
    for ( i=0; i<250; i++)
    {
      usr_index[0][0] = i+1;
      usr_index[0][1] = i+1;
      usr_index[0][2] = 1;
      usr_order[0]    = 1;
      j = 60;
      error = Getelt(&fd_nefis, grp_name , elm_name,
                      (BInt4 *) usr_index, usr_order, &j, (BData) getal );
      for ( j=0; j<15; j++)
      {
        printf( " %14.6e", getal[j] );
      }
        printf( "\n" );
    }
    if (error != 0)
    {
       error = Neferr( 1, error_string);
       return 0;
    }

    strcpy(elm_name, "Real4");
    strcpy(grp_name, "var_1601");
    printf("\n Get data from group \'%s\'\n", grp_name);
    for ( i=0; i<1601; i++)
    {
      usr_index[0][0] = i+1;
      usr_index[0][1] = i+1;
      usr_index[0][2] = 1;
      usr_order[0]    = 1;
      j = 60;
      error = Getelt(&fd_nefis, grp_name , elm_name,
                      (BInt4 *) usr_index, usr_order, &j, (BData) getal );
      for ( j=0; j<15; j++)
      {
        printf( " %14.6e", getal[j] );
      }
        printf( "\n" );
    }
    if (error != 0)
    {
       error = Neferr( 1, error_string);
       return 0;
    }

    strcpy(elm_name, "Real4");
    strcpy(grp_name, "fixed_1601");
    printf("\n Get data from group \'%s\'\n", grp_name);
    for ( i=0; i<1601; i++)
    {
      usr_index[0][0] = i+1;
      usr_index[0][1] = i+1;
      usr_index[0][2] = 1;
      usr_order[0]    = 1;
      j = 60;
      error = Getelt(&fd_nefis, grp_name , elm_name,
                      (BInt4 *) usr_index, usr_order, &j, (BData) getal );
      for ( j=0; j<15; j++)
      {
        printf( " %14.6e", getal[j] );
      }
      printf( "\n");
    }
    if (error != 0)
    {
       error = Neferr( 1, error_string);
       return 0;
    }
/*-------------------------------------------------------------------------*/
  if ( error != 0 ) error = Neferr( 1, error_string);
  error = Clsnef( &fd_nefis  );
  if ( error != 0 ) error = Neferr( 1, error_string);


  free( (BData) getal       );
  free( (BData) elm_name    );
  free( (BData) elm_type    );
  free( (BData) elm_quantity);
  free( (BData) elm_unity   );
  free( (BData) elm_desc    );
  free( (BData) cel_name    );
  free( (BData) grp_name    );
  free( (BData) grp_defined );
  free( (BData) cel_names   );
  free( (BData) elm_dimens  );
  free( (BData) grp_dimens  );
  free( (BData) grp_order   );
  free( (BData) usr_order   );
  free( (BData) nef_version );

  printf("\nEnd program\n\n");

  return 0;
}
