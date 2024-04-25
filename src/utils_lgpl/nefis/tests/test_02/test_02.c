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
  BInt4   fd_nefisa=-1;
  BInt2 * getal2;
  BInt4 * getal4;
  BText   grp_defined ;
  BInt4 * grp_dimens  ;
  BText   grp_name    ;
  BInt4   grp_num_dim =-1 ;
  BInt4 * grp_order   ;
#if DEBUG_NEFIS
  BInt4   ii;
#endif
  BInt4   i, j, jj, error=0;
  BInt4   max_index       ;
/*  BChar **  namst  ; */
/*  BChar *   c  ;     */
/*  BChar **  namstat  ;*/
/*  BChar *   d  ; */
  BChar   namst  [11][21]  ;
  BChar   namstat[11][21]  ;
  BChar   rdwr             ;
  BInt4   usr_index [5][3];
  BInt4 * usr_order   ;
  BRea4 * zeta4;
  BRea8 * zeta8;
  BRea4 * cmplx8 ;
  BRea8 * cmplx16;
  BInt2 * logical2;
  BInt4 * logical4;
  BText   nef_version;

  error = Getnfv(&nef_version);

  printf(" -----------------------------------------------\n");
  printf(" Version: %s\n", nef_version+4);
  printf(" -----------------------------------------------\n");

  getal2       = (BInt2 * ) malloc( sizeof( BInt2   ) *  22 );
  getal4       = (BInt4 * ) malloc( sizeof( BInt4   ) *  44 );
  zeta4        = (BRea4 * ) malloc( sizeof( BRea4   ) *  44 );
  zeta8        = (BRea8 * ) malloc( sizeof( BRea8   ) *  88 );
  logical2     = (BInt2 * ) malloc( sizeof( BInt2   ) *  22 );
  logical4     = (BInt4 * ) malloc( sizeof( BInt4   ) *  44 );
  cmplx8       = (BRea4 * ) malloc( sizeof( BRea4   ) *  88 );
  cmplx16      = (BRea8 * ) malloc( sizeof( BRea8   ) * 176 );

/*
  namstat      = (BChar ** ) malloc( sizeof( BChar ) * 11);
  d            = (BChar  * ) malloc( sizeof( BChar ) * 11 * 21 );
  for ( jj=0; jj<11; jj++ )
  {
    namstat[jj]   = (d +  jj * 21 );
  }
*/

  elm_name     = (BText) malloc( sizeof(BChar) * (MAX_NAME + 1) );
  elm_type     = (BText) malloc( sizeof(BChar) * (MAX_TYPE + 1) );
  elm_quantity = (BText) malloc( sizeof(BChar) * (MAX_NAME + 1) );
  elm_unity    = (BText) malloc( sizeof(BChar) * (MAX_NAME + 1) );
  elm_desc     = (BText) malloc( sizeof(BChar) * (MAX_DESC + 1) );
  cel_name     = (BText) malloc( sizeof(BChar) * (MAX_NAME + 1) );
  grp_name     = (BText) malloc( sizeof(BChar) * (MAX_NAME + 1) );
  grp_defined  = (BText) malloc( sizeof(BChar) * (MAX_NAME + 1) );
  cel_names    = (BText) malloc( sizeof(BChar) * (MAX_NAME + 1) * MAX_DIM );

  elm_dimens   = (BInt4 *) malloc( sizeof(BInt4 ) * MAX_DIM  );
  grp_dimens   = (BInt4 *) malloc( sizeof(BInt4 ) * MAX_DIM  );
  grp_order    = (BInt4 *) malloc( sizeof(BInt4 ) * MAX_DIM  );
  usr_order    = (BInt4 *) malloc( sizeof(BInt4 ) * MAX_DIM  );

  elm_names    = (char **) malloc( sizeof( char *) * MAX_CEL_DIM);
  for (i=0; i<MAX_NAME+1; i++) {
	  elm_names[i] = (char *) malloc( sizeof(char) * (MAX_NAME +1));
  }

  rdwr = 'C';
  coding = 'B';  /* Big    endian */
  coding = 'L';  /* Little endian */

  for ( i=0; i<MAX_DIM; i++) {
    elm_dimens[i] = 0;
    grp_dimens[i] = 0;
    grp_order [i] = 0;
    usr_order [i] = 0;
    for ( j=0; j<3; j++) {
      usr_index[i][j] = 0;
    }
  }
  for ( i=0; i<11; i++) {
    for ( j=0; j<21; j++) {
      namst  [i][j] = '\0';
      namstat[i][j] = '\0';
    }
  }

#if DEBUG_NEFIS
  printf("Input         \n");
  printf(" Error    : %d\n", error);
  printf(" ReadWrite: %c\n", rdwr );
#endif

/*
 * Open NEFIS files
 */

  strcpy(dat_file,"data_c02.dat");
  strcpy(def_file,"data_c02.def");

#if DEBUG_NEFIS
  printf(" Filenames: %s %s\n", dat_file, def_file);
#endif

  error = Crenef( &fd_nefis , dat_file, def_file, coding,  rdwr);
  if (error != 0)
  {
     error = Neferr( 1, error_string);
     return 0;
  }

  strcpy(dat_file,"data_c02.dta");
  strcpy(def_file,"data_c02.dfe");
  error = Crenef( &fd_nefisa, dat_file, def_file, coding,  rdwr);
  if (error != 0)
  {
     error = Neferr( 1, error_string);
     return 0;
  }
/*
 * elm_type: real, integer, character
 */

/*-------------------------------------------------------------------------*/
/*---------------------1234567890123456------------------------------------*/

  strcpy(elm_name    ,"Real (*4)");
  strcpy(elm_type    ,"REAL");
  strcpy(elm_quantity,"Real-4-qty");
  strcpy(elm_unity   ,"Real-4-unit");
  strcpy(elm_desc    ,"Real-4-desc");
  elm_single_byte = 4;
  elm_num_dim  = 1;
  elm_dimens[0]= 11;

  printf(" Define real(4)       element \n");
  error  = Defelm(&fd_nefis , elm_name    , elm_type   , elm_single_byte,
                              elm_quantity, elm_unity  , elm_desc,
                              elm_num_dim , elm_dimens  );
  if (error != 0)
  {
     error = Neferr( 1, error_string);
     return 0;
  }

  strcpy(elm_name    ,"Real (*8)");
  strcpy(elm_type    ,"REAL");
  strcpy(elm_quantity,"Real-8-qty");
  strcpy(elm_unity   ,"Real-8-unit");
  strcpy(elm_desc    ,"Real-8-desc");
  elm_single_byte = 8;
  elm_num_dim  = 1;
  elm_dimens[0]= 11;

  printf(" Define real(8)       element \n");
  error  = Defelm(&fd_nefis , elm_name    , elm_type   , elm_single_byte,
                              elm_quantity, elm_unity  , elm_desc,
                              elm_num_dim , elm_dimens  );
  if (error != 0)
  {
     error = Neferr( 1, error_string);
     return 0;
  }

  strcpy(elm_name    ,"Int (*2)");
  strcpy(elm_type    ,"INTEGER");
  strcpy(elm_quantity,"Int-qty");
  strcpy(elm_unity   ,"Int-unit");
  strcpy(elm_desc    ,"Int-desc");
  elm_single_byte = 2;
  elm_num_dim  = 1;
  elm_dimens[0]= 11;

  printf(" Define integer(2)    element \n");
  error  = Defelm(&fd_nefis , elm_name    , elm_type   , elm_single_byte,
                              elm_quantity, elm_unity  , elm_desc,
                              elm_num_dim , elm_dimens  );
  if (error != 0)
  {
     error = Neferr( 1, error_string);
     return 0;
  }

  strcpy(elm_name    ,"Int (*4)");
  strcpy(elm_type    ,"INTEGER");
  strcpy(elm_quantity,"Int-qty");
  strcpy(elm_unity   ,"Int-unit");
  strcpy(elm_desc    ,"Int-desc");
  elm_single_byte = 4;
  elm_num_dim  = 1;
  elm_dimens[0]= 11;

  printf(" Define integer(4)    element \n");
  error  = Defelm(&fd_nefis , elm_name    , elm_type   , elm_single_byte,
                              elm_quantity, elm_unity  , elm_desc,
                              elm_num_dim , elm_dimens  );
  if (error != 0)
  {
     error = Neferr( 1, error_string);
     return 0;
  }

  strcpy(elm_name    ,"Logical (*2)");
  strcpy(elm_type    ,"LOGICAL");
  strcpy(elm_quantity,"Log-2-qty");
  strcpy(elm_unity   ,"Log-2-unit");
  strcpy(elm_desc    ,"Log-2-desc");
  elm_single_byte = 2;
  elm_num_dim  = 1;
  elm_dimens[0]= 11;

  printf(" Define logical(2)    element \n");
  error  = Defelm(&fd_nefis , elm_name    , elm_type   , elm_single_byte,
                              elm_quantity, elm_unity  , elm_desc,
                              elm_num_dim , elm_dimens  );
  if (error != 0)
  {
     error = Neferr( 1, error_string);
     return 0;
  }

  strcpy(elm_name    ,"Logical (*4)");
  strcpy(elm_type    ,"LOGICAL");
  strcpy(elm_quantity,"Log-4-qty");
  strcpy(elm_unity   ,"Log-4-unit");
  strcpy(elm_desc    ,"Log-4-desc");
  elm_single_byte = 4;
  elm_num_dim  = 1;
  elm_dimens[0]= 11;

  printf(" Define logical(4)    element \n");
  error  = Defelm(&fd_nefis , elm_name    , elm_type   , elm_single_byte,
                              elm_quantity, elm_unity  , elm_desc,
                              elm_num_dim , elm_dimens  );
  if (error != 0)
  {
     error = Neferr( 1, error_string);
     return 0;
  }

  strcpy(elm_name    ,"Character (21)");
  strcpy(elm_type    ,"CHARACTE");
  strcpy(elm_quantity,"Char-21-qty");
  strcpy(elm_unity   ,"Char-21-unit");
  strcpy(elm_desc    ,"Char-21-desc");
  elm_single_byte = 21;
  elm_num_dim  = 1;
  elm_dimens[0]= 11;

  printf(" Define character(21) element \n");
  error  = Defelm(&fd_nefis , elm_name    , elm_type   , elm_single_byte,
                              elm_quantity, elm_unity  , elm_desc,
                              elm_num_dim , elm_dimens  );
  if (error != 0)
  {
     error = Neferr( 1, error_string);
     return 0;
  }

  strcpy(elm_name    ,"Complex (*8)");
  strcpy(elm_type    ,"COMPLEX");
  strcpy(elm_quantity,"Cmp-8-qty");
  strcpy(elm_unity   ,"Cmp-8-unit");
  strcpy(elm_desc    ,"Cmp-8-desc");
  elm_single_byte = 8;
  elm_num_dim  = 1;
  elm_dimens[0]= 11;

  printf(" Define complex(8)    element \n");
  error  = Defelm(&fd_nefis , elm_name    , elm_type   , elm_single_byte,
                              elm_quantity, elm_unity  , elm_desc,
                              elm_num_dim , elm_dimens  );
  if (error != 0)
  {
     error = Neferr( 1, error_string);
     return 0;
  }

  strcpy(elm_name    ,"Complex (*16)");
  strcpy(elm_type    ,"COMPLEX");
  strcpy(elm_quantity,"Cmp-16-qty");
  strcpy(elm_unity   ,"Cmp-16-unit");
  strcpy(elm_desc    ,"Cmp-16-desc");
  elm_single_byte = 16;
  elm_num_dim  = 1;
  elm_dimens[0]= 11;

  printf(" Define complex(16)   element \n");
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
    strcpy( elm_names[0],"Real (*4)");
    printf(" Define real(4)       cel     \n");
    error  = Defcel2( &fd_nefis, cel_name, cel_num_dim , elm_names);
    if (error != 0)
    {
      error = Neferr( 1, error_string);
      return 0;
    }

    cel_num_dim = 1;
    strcpy( cel_name    ,"cel2");;
    strcpy( elm_names[0],"Real (*8)");
    printf(" Define real(8)       cel     \n");
    error  = Defcel2( &fd_nefis, cel_name, cel_num_dim , elm_names);
    if (error != 0)
    {
      error = Neferr( 1, error_string);
      return 0;
    }

    cel_num_dim = 1;
    strcpy( cel_name    ,"cel3");
    strcpy( elm_names[0],"Int (*2)");
    printf(" Define integer(2)    cel     \n");
    error  = Defcel2( &fd_nefis, cel_name, cel_num_dim , elm_names);
    if (error != 0)
    {
      error = Neferr( 1, error_string);
      return 0;
    }

    cel_num_dim = 1;
    strcpy( cel_name    ,"cel4");
    strcpy( elm_names[0],"Int (*4)");
    printf(" Define integer(4)    cel     \n");
    error  = Defcel2( &fd_nefis, cel_name, cel_num_dim , elm_names);
    if (error != 0)
    {
      error = Neferr( 1, error_string);
      return 0;
    }

    cel_num_dim = 1;
    strcpy( cel_name    ,"cel5");
    strcpy( elm_names[0],"Logical (*2)");
    printf(" Define logical(2)    cel     \n");
    error  = Defcel2( &fd_nefis, cel_name, cel_num_dim , elm_names);
    if (error != 0)
    {
      error = Neferr( 1, error_string);
      return 0;
    }

    cel_num_dim = 1;
    strcpy( cel_name    ,"cel6");
    strcpy( elm_names[0],"Logical (*4)");
    printf(" Define logical(4)    cel     \n");
    error  = Defcel2( &fd_nefis, cel_name, cel_num_dim , elm_names);
    if (error != 0)
    {
      error = Neferr( 1, error_string);
      return 0;
    }

    cel_num_dim = 1;
    strcpy( cel_name    ,"cel7");
    strcpy( elm_names[0],"Character (21)");
    printf(" Define character(21) cel     \n");
    error  = Defcel2( &fd_nefis, cel_name, cel_num_dim , elm_names);
    if (error != 0)
    {
      error = Neferr( 1, error_string);
      return 0;
    }

    cel_num_dim = 1;
    strcpy( cel_name    ,"cel8");
    strcpy( elm_names[0],"Complex (*8)");
    printf(" Define complex(8)    cel     \n");
    error  = Defcel2( &fd_nefis, cel_name, cel_num_dim , elm_names);
    if (error != 0)
    {
      error = Neferr( 1, error_string);
      return 0;
    }

    cel_num_dim = 1;
    strcpy( cel_name    ,"cel9");
    strcpy( elm_names[0],"Complex (*16)");
    printf(" Define complex(16)   cel     \n");
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
    grp_dimens[0] = 1;
    grp_order [0] = 1;
    printf(" Define real(4)       group   \n");
    error  = Defgrp ( &fd_nefis  , grp_name  , cel_names, grp_num_dim,
                                   grp_dimens, grp_order);
    if (error != 0)
    {
       error = Neferr( 1, error_string);
       return 0;
    }

    strcpy( grp_name    ,"grp2");
    strcpy(&cel_names[0],"cel2");
    grp_num_dim   = 1;
    grp_dimens[0] = 1;
    grp_order [0] = 1;
    printf(" Define real(8)       group   \n");
    error  = Defgrp ( &fd_nefis  , grp_name  , cel_names, grp_num_dim,
                                   grp_dimens, grp_order);
    if (error != 0)
    {
       error = Neferr( 1, error_string);
       return 0;
    }

    strcpy( grp_name    ,"grp3");
    strcpy(&cel_names[0],"cel3");
    grp_num_dim   = 1;
    grp_dimens[0] = 1;
    grp_order [0] = 1;
    printf(" Define integer(2)    group   \n");
    error  = Defgrp ( &fd_nefis  , grp_name  , cel_names, grp_num_dim,
                                   grp_dimens, grp_order);
    if (error != 0)
    {
       error = Neferr( 1, error_string);
       return 0;
    }

    strcpy( grp_name    ,"grp4");
    strcpy(&cel_names[0],"cel4");
    grp_num_dim   = 1;
    grp_dimens[0] = 1;
    grp_order [0] = 1;
    printf(" Define integer(4)    group   \n");
    error  = Defgrp ( &fd_nefis  , grp_name  , cel_names, grp_num_dim,
                                   grp_dimens, grp_order);
    if (error != 0)
    {
       error = Neferr( 1, error_string);
       return 0;
    }

    strcpy( grp_name    ,"grp5");
    strcpy(&cel_names[0],"cel5");
    grp_num_dim   = 1;
    grp_dimens[0] = 1;
    grp_order [0] = 1;
    printf(" Define logical(2)    group   \n");
    error  = Defgrp ( &fd_nefis  , grp_name  , cel_names, grp_num_dim,
                                   grp_dimens, grp_order);
    if (error != 0)
    {
       error = Neferr( 1, error_string);
       return 0;
    }

    strcpy( grp_name    ,"grp6");
    strcpy(&cel_names[0],"cel6");
    grp_num_dim   = 1;
    grp_dimens[0] = 1;
    grp_order [0] = 1;
    printf(" Define logical(4)    group   \n");
    error  = Defgrp ( &fd_nefis  , grp_name  , cel_names, grp_num_dim,
                                   grp_dimens, grp_order);
    if (error != 0)
    {
       error = Neferr( 1, error_string);
       return 0;
    }

    strcpy( grp_name    ,"grp7");
    strcpy(&cel_names[0],"cel7");
    grp_num_dim   = 1;
    grp_dimens[0] = 1;
    grp_order [0] = 1;
    printf(" Define character(21) group   \n");
    error  = Defgrp ( &fd_nefis  , grp_name  , cel_names, grp_num_dim,
                                   grp_dimens, grp_order);
    if (error != 0)
    {
       error = Neferr( 1, error_string);
       return 0;
    }

    strcpy( grp_name    ,"grp8");
    strcpy(&cel_names[0],"cel8");
    grp_num_dim   = 1;
    grp_dimens[0] = 1;
    grp_order [0] = 1;
    printf(" Define complex(8)    group   \n");
    error  = Defgrp ( &fd_nefis  , grp_name  , cel_names, grp_num_dim,
                                   grp_dimens, grp_order);
    if (error != 0)
    {
       error = Neferr( 1, error_string);
       return 0;
    }

    strcpy( grp_name    ,"grp9");
    strcpy(&cel_names[0],"cel9");
    grp_num_dim   = 1;
    grp_dimens[0] = 1;
    grp_order [0] = 1;
    printf(" Define complex(16)   group   \n");
    error  = Defgrp ( &fd_nefis  , grp_name  , cel_names, grp_num_dim,
                                   grp_dimens, grp_order);
    if (error != 0)
    {
       error = Neferr( 1, error_string);
       return 0;
    }

/*-------------------------------------------------------------------------*/

    printf(" Create real(4)       group   \n");
    strcpy(grp_name     ,"group1");
    strcpy(grp_defined  ,"grp1");
    error  = Credat( &fd_nefis, grp_name, grp_defined);
    if (error != 0)
    {
       error = Neferr( 1, error_string);
       return 0;
    }

    printf(" Create real(8)       group   \n");
    strcpy(grp_name     ,"group2");
    strcpy(grp_defined  ,"grp2");
    error  = Credat( &fd_nefis, grp_name, grp_defined);
    if (error != 0)
    {
       error = Neferr( 1, error_string);
       return 0;
    }

    printf(" Create integer(2)    group   \n");
    strcpy(grp_name     ,"group3");
    strcpy(grp_defined  ,"grp3");
    error  = Credat( &fd_nefis, grp_name, grp_defined);
    if (error != 0)
    {
       error = Neferr( 1, error_string);
       return 0;
    }

    printf(" Create integer(4)    group   \n");
    strcpy(grp_name     ,"group4");
    strcpy(grp_defined  ,"grp4");
    error  = Credat( &fd_nefis, grp_name, grp_defined);
    if (error != 0)
    {
       error = Neferr( 1, error_string);
       return 0;
    }

    printf(" Create logical(2)    group   \n");
    strcpy(grp_name     ,"group5");
    strcpy(grp_defined  ,"grp5");
    error  = Credat( &fd_nefis, grp_name, grp_defined);
    if (error != 0)
    {
       error = Neferr( 1, error_string);
       return 0;
    }

    printf(" Create logical(4)    group   \n");
    strcpy(grp_name     ,"group6");
    strcpy(grp_defined  ,"grp6");
    error  = Credat( &fd_nefis, grp_name, grp_defined);
    if (error != 0)
    {
       error = Neferr( 1, error_string);
       return 0;
    }

    printf(" Create character(21) group   \n");
    strcpy(grp_name     ,"group7");
    strcpy(grp_defined  ,"grp7");
    error  = Credat( &fd_nefis, grp_name, grp_defined);
    if (error != 0)
    {
       error = Neferr( 1, error_string);
       return 0;
    }

    printf(" Create complex(8)    group   \n");
    strcpy(grp_name     ,"group8");
    strcpy(grp_defined  ,"grp8");
    error  = Credat( &fd_nefis, grp_name, grp_defined);
    if (error != 0)
    {
       error = Neferr( 1, error_string);
       return 0;
    }

    printf(" Create complex(16)   group   \n");
    strcpy(grp_name     ,"group9");
    strcpy(grp_defined  ,"grp9");
    error  = Credat( &fd_nefis, grp_name, grp_defined);
    if (error != 0)
    {
       error = Neferr( 1, error_string);
       return 0;
    }

/*-------------------------------------------------------------------------*/

    printf(" Check user group on data file       \n");
    strcpy(grp_name     ,"group1");
    error  = Inqdat(&fd_nefis, grp_name, grp_defined);
    printf(" User    group name: <%s>\n", grp_name   );
    printf(" Defined group name: <%s>\n", grp_defined);
    if (error != 0)
    {
       error = Neferr( 1, error_string);
       return 0;
    }

    strcpy(grp_name     ,"group2");
    error  = Inqdat(&fd_nefis, grp_name, grp_defined);
    printf(" User    group name: <%s>\n", grp_name   );
    printf(" Defined group name: <%s>\n", grp_defined);
    if (error != 0)
    {
       error = Neferr( 1, error_string);
       return 0;
    }

    strcpy(grp_name     ,"group3");
    error  = Inqdat(&fd_nefis, grp_name, grp_defined);
    printf(" User    group name: <%s>\n", grp_name   );
    printf(" Defined group name: <%s>\n", grp_defined);
    if (error != 0)
    {
       error = Neferr( 1, error_string);
       return 0;
    }

    strcpy(grp_name     ,"group4");
    error  = Inqdat(&fd_nefis, grp_name, grp_defined);
    printf(" User    group name: <%s>\n", grp_name   );
    printf(" Defined group name: <%s>\n", grp_defined);
    if (error != 0)
    {
       error = Neferr( 1, error_string);
       return 0;
    }

    strcpy(grp_name     ,"group5");
    error  = Inqdat(&fd_nefis, grp_name, grp_defined);
    printf(" User    group name: <%s>\n", grp_name   );
    printf(" Defined group name: <%s>\n", grp_defined);
    if (error != 0)
    {
       error = Neferr( 1, error_string);
       return 0;
    }

    strcpy(grp_name     ,"group6");
    error  = Inqdat(&fd_nefis, grp_name, grp_defined);
    printf(" User    group name: <%s>\n", grp_name   );
    printf(" Defined group name: <%s>\n", grp_defined);
    if (error != 0)
    {
       error = Neferr( 1, error_string);
       return 0;
    }

    strcpy(grp_name     ,"group7");
    error  = Inqdat(&fd_nefis, grp_name, grp_defined);
    printf(" User    group name: <%s>\n", grp_name   );
    printf(" Defined group name: <%s>\n", grp_defined);
    if (error != 0)
    {
       error = Neferr( 1, error_string);
       return 0;
    }

    strcpy(grp_name     ,"group8");
    error  = Inqdat(&fd_nefis, grp_name, grp_defined);
    printf(" User    group name: <%s>\n", grp_name   );
    printf(" Defined group name: <%s>\n", grp_defined);
    if (error != 0)
    {
       error = Neferr( 1, error_string);
       return 0;
    }

    strcpy(grp_name     ,"group9");
    error  = Inqdat(&fd_nefis, grp_name, grp_defined);
    printf(" User    group name: <%s>\n", grp_name   );
    printf(" Defined group name: <%s>\n", grp_defined);
    if (error != 0)
    {
       error = Neferr( 1, error_string);
       return 0;
    }


/*-------------------------------------------------------------------------*/
/*  Determine sizes of groups                                              */
/*-------------------------------------------------------------------------*/

    strcpy(grp_name,"group1");
    error = Inqmxi (&fd_nefis, grp_name, &max_index);
    printf(" Largest dimension for group <%s>: %d\n", grp_name, max_index);
    if (error != 0)
    {
       error = Neferr( 1, error_string);
       return 0;
    }

    strcpy(grp_name,"group2");
    error = Inqmxi (&fd_nefis, grp_name, &max_index);
    printf(" Largest dimension for group <%s>: %d\n", grp_name, max_index);
    if (error != 0)
    {
       error = Neferr( 1, error_string);
       return 0;
    }

    strcpy(grp_name,"group3");
    error = Inqmxi (&fd_nefis, grp_name, &max_index);
    printf(" Largest dimension for group <%s>: %d\n", grp_name, max_index);
    if (error != 0)
    {
       error = Neferr( 1, error_string);
       return 0;
    }

    strcpy(grp_name,"group4");
    error = Inqmxi (&fd_nefis, grp_name, &max_index);
    printf(" Largest dimension for group <%s>: %d\n", grp_name, max_index);
    if (error != 0)
    {
       error = Neferr( 1, error_string);
       return 0;
    }

    strcpy(grp_name,"group5");
    error = Inqmxi (&fd_nefis, grp_name, &max_index);
    printf(" Largest dimension for group <%s>: %d\n", grp_name, max_index);
    if (error != 0)
    {
       error = Neferr( 1, error_string);
       return 0;
    }

    strcpy(grp_name,"group6");
    error = Inqmxi (&fd_nefis, grp_name, &max_index);
    printf(" Largest dimension for group <%s>: %d\n", grp_name, max_index);
    if (error != 0)
    {
       error = Neferr( 1, error_string);
       return 0;
    }

    strcpy(grp_name,"group7");
    error = Inqmxi (&fd_nefis, grp_name, &max_index);
    printf(" Largest dimension for group <%s>: %d\n", grp_name, max_index);
    if (error != 0)
    {
       error = Neferr( 1, error_string);
       return 0;
    }

    strcpy(grp_name,"group8");
    error = Inqmxi (&fd_nefis, grp_name, &max_index);
    printf(" Largest dimension for group <%s>: %d\n", grp_name, max_index);
    if (error != 0)
    {
       error = Neferr( 1, error_string);
       return 0;
    }

    strcpy(grp_name,"group9");
    error = Inqmxi (&fd_nefis, grp_name, &max_index);
    printf(" Largest dimension for group <%s>: %d\n", grp_name, max_index);
    if (error != 0)
    {
       error = Neferr( 1, error_string);
       return 0;
    }

/*-------------------------------------------------------------------------*/
/* list all groups                                                         */
/*-------------------------------------------------------------------------*/

    error = Inqfst (&fd_nefis, grp_name, grp_defined);
    if (error != 0)
    {
       error = Neferr( 1, error_string);
       return 0;
    }
    i = 0;
    i++;
    printf(" group[%d] = <%s> which is defined as <%s>\n",
      i, grp_name , grp_defined);


    error = Inqnxt (&fd_nefis, grp_name, grp_defined);
    if (error != 0)
    {
       error = Neferr( 1, error_string);
       return 0;
    }
    i++;
    printf(" group[%d] = <%s> which is defined as <%s>\n",
      i, grp_name , grp_defined);


    error = Inqnxt (&fd_nefis, grp_name, grp_defined);
    if (error != 0)
    {
       error = Neferr( 1, error_string);
       return 0;
    }
    i++;
    printf(" group[%d] = <%s> which is defined as <%s>\n",
      i, grp_name , grp_defined);


    error = Inqnxt (&fd_nefis, grp_name, grp_defined);
    if (error != 0)
    {
       error = Neferr( 1, error_string);
       return 0;
    }
    i++;
    printf(" group[%d] = <%s> which is defined as <%s>\n",
      i, grp_name , grp_defined);

    error = Inqnxt (&fd_nefis, grp_name, grp_defined);
    if (error != 0)
    {
       error = Neferr( 1, error_string);
       return 0;
    }
    i++;
    printf(" group[%d] = <%s> which is defined as <%s>\n",
      i, grp_name , grp_defined);

    error = Inqnxt (&fd_nefis, grp_name, grp_defined);
    if (error != 0)
    {
       error = Neferr( 1, error_string);
       return 0;
    }
    i++;
    printf(" group[%d] = <%s> which is defined as <%s>\n",
      i, grp_name , grp_defined);

    error = Inqnxt (&fd_nefis, grp_name, grp_defined);
    if (error != 0)
    {
       error = Neferr( 1, error_string);
       return 0;
    }
    i++;
    printf(" group[%d] = <%s> which is defined as <%s>\n",
      i, grp_name , grp_defined);

    error = Inqnxt (&fd_nefis, grp_name, grp_defined);
    if (error != 0)
    {
       error = Neferr( 1, error_string);
       return 0;
    }
    i++;
    printf(" group[%d] = <%s> which is defined as <%s>\n",
      i, grp_name , grp_defined);

    error = Inqnxt (&fd_nefis, grp_name, grp_defined);
    if (error > 0)
    {
       error = Neferr( 1, error_string);
       return 0;
    }
    else if ( error < 0 )
    {
       error = Neferr( 1, error_string);
    }
    i++;
    printf(" group[%d] = <%s> which is defined as <%s>\n",
      i, grp_name , grp_defined);

    error = Inqnxt (&fd_nefis, grp_name, grp_defined);
    if ( error != 0 && error >= 0 )
    {
       error = Neferr( 1, error_string);
       return 0;
    }
    else if ( error <= 0 )
    {
       error = Neferr( 1, error_string);
    }
    else
    {
      i++;
      printf(" group[%d] = <%s> which is defined as <%s>\n",
        i, grp_name , grp_defined);
   }

/*-------------------------------------------------------------------------*/
/* Put data into Nefis file                                                 */
/*-------------------------------------------------------------------------*/

     strcpy(elm_name,"Real (*4)");
     strcpy(grp_name, "group1");
     usr_index[0][0] = 1;
     usr_index[0][1] = 1;
     usr_index[0][2] = 1;
     usr_order[0]    = 1;
     for ( jj=0; jj<11; jj++)
     {
       zeta4[jj]= (BRea4) (jj+1);
     }
     error = Putelt(&fd_nefis, grp_name , elm_name,
                     (BInt4 *) usr_index, usr_order, (BData) zeta4   );
     if (error != 0)
     {
        error = Neferr( 1, error_string);
        return 0;
     }
     for ( jj=0; jj<11; jj++)
     {
       zeta4[jj]= 0.0;
     }

     strcpy(elm_name,"Real (*8)");
     strcpy(grp_name, "group2");
     usr_index[0][0] = 1;
     usr_index[0][1] = 1;
     usr_index[0][2] = 1;
     usr_order[0]    = 1;
     for ( jj=0; jj<11; jj++)
     {
       zeta8[jj]= (BRea8) (jj+1)*100000000;
     }
     error = Putelt(&fd_nefis, grp_name , elm_name,
                     (BInt4 *) usr_index, usr_order, (BData) zeta8   );
     if (error != 0)
     {
        error = Neferr( 1, error_string);
        return 0;
     }
     for ( jj=0; jj<11; jj++)
     {
       zeta8[jj]= 0.0;
     }

     strcpy(elm_name,"Int (*2)");
     strcpy(grp_name, "group3");
     usr_index[0][0] = 1;
     usr_index[0][1] = 1;
     usr_index[0][2] = 1;
     usr_order[0]    = 1;
     for ( jj=0; jj<11; jj++)
     {
       getal2[jj]= (BInt2) (jj+1);
     }
     error = Putelt(&fd_nefis, grp_name , elm_name,
                     (BInt4 *) usr_index, usr_order, (BData) getal2  );
     if (error != 0)
     {
        error = Neferr( 1, error_string);
        return 0;
     }
     for ( jj=0; jj<11; jj++)
     {
       getal2[jj]= 0;
     }

     strcpy(elm_name,"Int (*4)");
     strcpy(grp_name, "group4");
     usr_index[0][0] = 1;
     usr_index[0][1] = 1;
     usr_index[0][2] = 1;
     usr_order[0]    = 1;
     for ( jj=0; jj<11; jj++)
     {
       getal4[jj]= 40000+jj+1;
     }
     error = Putelt(&fd_nefis, grp_name , elm_name,
                     (BInt4 *) usr_index, usr_order, (BData) getal4  );
     if (error != 0)
     {
        error = Neferr( 1, error_string);
        return 0;
     }
     for ( jj=0; jj<11; jj++)
     {
       getal4[jj]= 0;
     }

     strcpy(elm_name,"Logical (*2)");
     strcpy(grp_name, "group5");
     usr_index[0][0] = 1;
     usr_index[0][1] = 1;
     usr_index[0][2] = 1;
     usr_order[0]    = 1;
     for ( jj=0; jj<11; jj++)
     {
       logical2[jj]= jj%2;
     }
     error = Putelt(&fd_nefis, grp_name , elm_name,
                     (BInt4 *) usr_index, usr_order, (BData) logical2);
     if (error != 0)
     {
        error = Neferr( 1, error_string);
        return 0;
     }
     for ( jj=0; jj<11; jj++)
     {
       logical2[jj]= 0;
     }

     strcpy(elm_name,"Logical (*4)");
     strcpy(grp_name, "group6");
     usr_index[0][0] = 1;
     usr_index[0][1] = 1;
     usr_index[0][2] = 1;
     usr_order[0]    = 1;
     for ( jj=0; jj<11; jj++)
     {
       logical4[jj]= jj%2;
     }
     error = Putelt(&fd_nefis, grp_name , elm_name,
                     (BInt4 *) usr_index, usr_order, (BData) logical4);
     if (error != 0)
     {
        error = Neferr( 1, error_string);
        return 0;
     }
     for ( jj=0; jj<11; jj++)
     {
       logical4[jj]= 0;
     }

     strcpy(elm_name,"Character (21)");
     strcpy(grp_name, "group7");
     usr_index[0][0] = 1;
     usr_index[0][1] = 1;
     usr_index[0][2] = 1;
     usr_order[0]    = 1;
     for ( jj=0; jj<11; jj++)
     {
       sprintf( namst[jj], "name_%03d", jj );
     }
     error = Putelt(&fd_nefis, grp_name , elm_name,
                     (BInt4 *) usr_index, usr_order, (BData) namst[0]    );
     for ( jj=0; jj<11; jj++)
     {
       sprintf( namst[jj], "name_...");
     }
     if (error != 0)
     {
        error = Neferr( 1, error_string);
        return 0;
     }

     strcpy(elm_name,"Complex (*8)");
     strcpy(grp_name, "group8");
     usr_index[0][0] = 1;
     usr_index[0][1] = 1;
     usr_index[0][2] = 1;
     usr_order[0]    = 1;
     for ( jj=0; jj<11; jj++)
     {
       cmplx8[2*jj  ]= (BRea4) (jj + 1);
       cmplx8[2*jj+1]= (BRea4) (cmplx8[2*jj]+0.25);
     }
     error = Putelt(&fd_nefis, grp_name , elm_name,
                     (BInt4 *) usr_index, usr_order, (BData) cmplx8  );
     if (error != 0)
     {
        error = Neferr( 1, error_string);
        return 0;
     }
     for ( jj=0; jj<11; jj++)
     {
       cmplx8[2*jj  ]= 0.0;
       cmplx8[2*jj+1]= 0.0;
     }

     strcpy(elm_name,"Complex (*16)");
     strcpy(grp_name, "group9");
     usr_index[0][0] = 1;
     usr_index[0][1] = 1;
     usr_index[0][2] = 1;
     usr_order[0]    = 1;
     for ( jj=0; jj<11; jj++)
     {
       cmplx16[2*jj  ]= (BRea8) (100000000 + jj + 1);
       cmplx16[2*jj+1]= (BRea8) (cmplx16[2*jj] + 0.33);
     }
     error = Putelt(&fd_nefis, grp_name , elm_name,
                     (BInt4 *) usr_index, usr_order, (BData) cmplx16 );
     if (error != 0)
     {
        error = Neferr( 1, error_string);
        return 0;
     }
     for ( jj=0; jj<11; jj++)
     {
       cmplx16[2*jj  ]= -0.0;
       cmplx16[2*jj+1]= -0.0;
     }

/*-------------------------------------------------------------------------*/
/*   Get elements                                                          */
/*-------------------------------------------------------------------------*/

     strcpy(elm_name,"Real (*4)");
     strcpy(grp_name, "group1");
     usr_index[0][0] = 1;
     usr_index[0][1] = 1;
     usr_index[0][2] = 1;
     usr_order[0]    = 1;
     jj = 44;
     error = Getelt(&fd_nefis, grp_name , elm_name,
                     (BInt4 *) usr_index, usr_order, &jj, (BData) zeta4  );
     printf(" Results Real (*4):\n");
     for ( jj=0; jj<11; jj++)
     {
       printf( " %f\n", zeta4[jj] );
     }
     if (error != 0)
     {
        error = Neferr( 1, error_string);
        return 0;
     }
     strcpy(elm_name,"Character (21)");

     strcpy(elm_name,"Real (*8)");
     strcpy(grp_name, "group2");
     usr_index[0][0] = 1;
     usr_index[0][1] = 1;
     usr_index[0][2] = 1;
     usr_order[0]    = 1;
     jj = 88;
     error = Getelt(&fd_nefis, grp_name , elm_name,
                     (BInt4 *) usr_index, usr_order, &jj, (BData) zeta8  );
     printf(" Results Real (*8):\n");
     for ( jj=0; jj<11; jj++)
     {
       printf( " %f\n", zeta8[jj] );
     }
     if (error != 0)
     {
        error = Neferr( 1, error_string);
        return 0;
     }

     strcpy(elm_name,"Int (*2)");
     strcpy(grp_name, "group3");
     usr_index[0][0] = 1;
     usr_index[0][1] = 1;
     usr_index[0][2] = 1;
     usr_order[0]    = 1;
     jj = 22;
     error = Getelt(&fd_nefis, grp_name , elm_name,
                     (BInt4 *) usr_index, usr_order, &jj, (BData) getal2 );
     printf(" Results Integer (*2):\n");
     for ( jj=0; jj<11; jj++)
     {
       printf( " %d\n", getal2[jj] );
     }
     if (error != 0)
     {
        error = Neferr( 1, error_string);
        return 0;
     }

     strcpy(elm_name,"Int (*4)");
     strcpy(grp_name, "group4");
     usr_index[0][0] = 1;
     usr_index[0][1] = 1;
     usr_index[0][2] = 1;
     usr_order[0]    = 1;
     jj = 44;
     error = Getelt(&fd_nefis, grp_name , elm_name,
                     (BInt4 *) usr_index, usr_order, &jj, (BData) getal4 );
     printf(" Results Integer (*4):\n");
     for ( jj=0; jj<11; jj++)
     {
       printf( " %d\n", getal4[jj] );
     }
     if (error != 0)
     {
        error = Neferr( 1, error_string);
        return 0;
     }

     strcpy(elm_name,"Logical (*2)");
     strcpy(grp_name, "group5");
     usr_index[0][0] = 1;
     usr_index[0][1] = 1;
     usr_index[0][2] = 1;
     usr_order[0]    = 1;
     jj = 22;
     error = Getelt(&fd_nefis, grp_name , elm_name,
                     (BInt4 *) usr_index, usr_order, &jj, (BData) logical2);
     printf(" Results Logical (*2):\n");
     for ( jj=0; jj<11; jj++)
     {
       printf( " %d", logical2[jj] );
     }
     printf("\n");
     if (error != 0)
     {
        error = Neferr( 1, error_string);
        return 0;
     }

     strcpy(elm_name,"Logical (*4)");
     strcpy(grp_name, "group6");
     usr_index[0][0] = 1;
     usr_index[0][1] = 1;
     usr_index[0][2] = 1;
     usr_order[0]    = 1;
     jj = 44;
     error = Getelt(&fd_nefis, grp_name , elm_name,
                     (BInt4 *) usr_index, usr_order, &jj, (BData) logical4);
     printf(" Results Logical (*4):\n");
     for ( jj=0; jj<11; jj++)
     {
       printf( " %d", logical4[jj] );
     }
     printf("\n");
     if (error != 0)
     {
        error = Neferr( 1, error_string);
        return 0;
     }

     strcpy(elm_name,"Character (21)");
     strcpy(grp_name, "group7");
     usr_index[0][0] = 1;
     usr_index[0][1] = 1;
     usr_index[0][2] = 1;
     usr_order[0]    = 1;
     jj = 231;
     error = Getelt(&fd_nefis, grp_name , elm_name,
                     (BInt4 *) usr_index, usr_order, &jj, (BData) namstat  );
     printf(" Results Character (*21):\n");
     for ( jj=0; jj<11; jj++)
     {
       printf( " \'%s\'\n", namstat[jj] );
     }
     if (error != 0)
     {
        error = Neferr( 1, error_string);
        return 0;
     }

     strcpy(elm_name,"Complex (*8)");
     strcpy(grp_name, "group8");
     usr_index[0][0] = 1;
     usr_index[0][1] = 1;
     usr_index[0][2] = 1;
     usr_order[0]    = 1;
     jj = 88;
     error = Getelt(&fd_nefis, grp_name , elm_name,
                     (BInt4 *) usr_index, usr_order, &jj, (BData) cmplx8 );
     printf(" Results Complex (*8):\n");
     for ( jj=0; jj<11; jj++)
     {
       printf( " z=(%f,%f)\n", cmplx8[2*jj], cmplx8[2*jj+1]);
     }
     if (error != 0)
     {
        error = Neferr( 1, error_string);
        return 0;
     }

     strcpy(elm_name,"Complex (*16)");
     strcpy(grp_name, "group9");
     usr_index[0][0] = 1;
     usr_index[0][1] = 1;
     usr_index[0][2] = 1;
     usr_order[0]    = 1;
     jj = 176;
     error = Getelt(&fd_nefis, grp_name , elm_name,
                     (BInt4 *) usr_index, usr_order, &jj, (BData) cmplx16 );
     printf(" Results Complex (*16):\n");
     for ( jj=0; jj<11; jj++)
     {
       printf( " z=(%f,%f)\n", cmplx16[2*jj], cmplx16[2*jj+1]);
     }
     if (error != 0)
     {
        error = Neferr( 1, error_string);
        return 0;
     }

/*-------------------------------------------------------------------------*/
  if ( error != 0 ) error = Neferr( 1, error_string);
  error = 0;
  if ( error == 0 ) error = Clsnef( &fd_nefisa );
  if ( error == 0 ) error = Clsnef( &fd_nefis  );

  error = Neferr( 1, error_string);

  free( (BData) getal2      );
  free( (BData) getal4      );
  free( (BData) zeta4       );
  free( (BData) zeta8       );
  free( (BData) logical2    );
  free( (BData) logical4    );
  free( (BData) cmplx8      );
  free( (BData) cmplx16     );
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
  free( nef_version );

  printf("\nEnd program\n\n");

  return 0;
}
