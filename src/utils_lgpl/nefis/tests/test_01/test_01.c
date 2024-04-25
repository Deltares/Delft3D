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

#define A2D(m,n)  ((n)+3*(m))

#define MAX_CEL_DIM 100
#define MAX_DESC     64
#define MAX_DIM       5
#define MAX_NAME     16
#define MAX_TYPE      8

int main(){
  BText   cel_name    ;
  BText   cel_names   ;
  BInt4   cel_num_dim   = -1;
  BInt4   cel_num_bytes = -1;
  BChar   coding          ;
  BChar   dat_file    [21];
  BChar   def_file    [21];
  BInt4   elm_bytes   ;
  BText   elm_desc    ;
  BInt4 * elm_dimens  ;
  BText   elm_name    ;
  BText * elm_names;
  BInt4   elm_num_dim = -1;
  BText   elm_quantity;
  BInt4   elm_single_bytes;
  BText   elm_type    ;
  BText   elm_unity   ;
  BInt4   error = 0   ;
  BChar   error_string[LENGTH_ERROR_MESSAGE];
  BInt4   fd_nefis = -1;
  BInt4   four        ;
  BText   grp_defined ;
  BInt4 * grp_dimens  ;
  BText   grp_name        ;
  BInt4   grp_num_dim =-1 ;
  BInt4 * grp_order       ;
  BInt4   i               ;
  BInt4   j               ;
  BChar   rdwr            ;
  BText   nef_version     ;

  error = Getnfv(&nef_version);

  elm_name     = (BText   ) malloc( sizeof(BChar) * (MAX_NAME + 1) );
  elm_type     = (BText   ) malloc( sizeof(BChar) * (MAX_TYPE + 1) );
  elm_quantity = (BText   ) malloc( sizeof(BChar) * (MAX_NAME + 1) );
  elm_unity    = (BText   ) malloc( sizeof(BChar) * (MAX_NAME + 1) );
  elm_desc     = (BText   ) malloc( sizeof(BChar) * (MAX_DESC + 1) );
  cel_name     = (BText   ) malloc( sizeof(BChar) * (MAX_NAME + 1) );
  grp_name     = (BText   ) malloc( sizeof(BChar) * (MAX_NAME + 1) );
  grp_defined  = (BText   ) malloc( sizeof(BChar) * (MAX_NAME + 1) );
  cel_names    = (BText   ) malloc( sizeof(BChar) * (MAX_NAME + 1) * MAX_DIM );

  elm_dimens   = (BInt4  *) malloc( sizeof(BInt4) *  MAX_DIM  );
  grp_dimens   = (BInt4  *) malloc( sizeof(BInt4) *  MAX_DIM  );
  grp_order    = (BInt4  *) malloc( sizeof(BInt4) *  MAX_DIM  );

  elm_names    = (char **) malloc( sizeof( char *) * MAX_CEL_DIM);
  for (i=0; i<MAX_NAME+1; i++) {
	  elm_names[i] = (char *) malloc( sizeof(char) * (MAX_NAME +1));
  }
  
  printf(" -----------------------------------------------\n");
  printf(" Version: %s\n", nef_version+4);

  printf(" -----------------------------------------------\n");
  printf(" Test: initialisation of nefis files            \n");
  printf("       defines: element, cel, grp and data file \n");
  printf(" -----------------------------------------------\n");

  rdwr = 'C';
  coding = 'N'; /* Nefis5: Little endian; Nefis4: Big endian*/
  coding = 'B'; /* Big endian */
  coding = 'L'; /* Little endian */

  strcpy(dat_file,"data_c01.dat");
  strcpy(def_file,"data_c01.def");

  elm_dimens[0]= -1;
  elm_dimens[1]= -1;
  elm_dimens[2]= -1;
  elm_dimens[3]= -1;
  elm_dimens[4]= -1;

  printf(" Filenames: \"%s\" \"%s\"\n", dat_file, def_file);

/*
 * Open NEFIS files
 */

  error = Crenef( &fd_nefis, dat_file, def_file, coding,  rdwr);

/*
 * elm_type: real, integer, character
 */

/*-------------------------------------------------------------------------*/
/*---------------------1234567890123456------------------------------------*/
  strcpy(elm_name    ,"aa");
  strcpy(elm_type    ,"INTEGER");
  strcpy(elm_quantity,"Waterstanden");
  strcpy(elm_unity   ,"meter");
  strcpy(elm_desc    ,"Mooiman, Vlaardingen, Nederland");
  elm_num_dim  = 1;
  elm_dimens[0]= 11;

  if (error == 0)
  {
    printf("\n Define first element                \n");
    four   = 4;
    error  = Defelm(&fd_nefis , elm_name, elm_type   , four        , elm_quantity,
                     elm_unity, elm_desc, elm_num_dim, elm_dimens  );
  }

  elm_num_dim  = 2;
  elm_dimens[0]= 4;
  elm_dimens[1]= 6;
  if (error == 0)
  {
    printf("\n Define next element (2)             \n");
    strcpy(elm_type    ,"REAL");
    four   = 4;
    strcpy(elm_name    ,"ab");
    error  = Defelm(&fd_nefis , elm_name, elm_type   , four        , elm_quantity,
                     elm_unity, elm_desc, elm_num_dim, elm_dimens  );
  }

  elm_num_dim  = 1;
  elm_dimens[0]= 5;
  if (error == 0)
  {
    printf("\n Define next element (3)             \n");
    strcpy(elm_type    ,"INTEGER");
    strcpy(elm_name    ,"ac");
    four   = 4;
    error  = Defelm(&fd_nefis , elm_name, elm_type   , four        , elm_quantity,
                     elm_unity, elm_desc, elm_num_dim, elm_dimens  );
  }


  elm_num_dim  = 1;
  elm_dimens[0]= 5;
  if (error == 0)
  {
    printf("\n Define next element (4)             \n");
    strcpy(elm_type    ,"CHARACTE");
    strcpy(elm_name    ,"ad");
    strcpy(elm_quantity,"Stations");
    strcpy(elm_unity   ,"[-]");
    strcpy(elm_desc    ,"Meetstationsnamen in Nederland");
    four   = 20;
    error  = Defelm(&fd_nefis , elm_name, elm_type   , four        , elm_quantity,
                     elm_unity, elm_desc, elm_num_dim, elm_dimens  );
  }

  elm_num_dim  = 1;
  elm_dimens[0]= 5;
  if (error == 0)
  {
    printf("\n Define next element (5)             \n");
    strcpy(elm_type    ,"CHARACTE");
    strcpy(elm_name    ,"aaaabbbb");
    strcpy(elm_quantity,"Stations");
    strcpy(elm_unity   ,"[-]");
    strcpy(elm_desc    ,"This element aaaabbbb is only used in cell definition");
    four   = 20;
    error  = Defelm(&fd_nefis , elm_name, elm_type   , four        , elm_quantity,
                     elm_unity, elm_desc, elm_num_dim, elm_dimens  );
  }
  if (error == 0)
  {
    printf("\n Define next element (6)             \n");
    strcpy(elm_type    ,"CHARACTE");
    strcpy(elm_name    ,"bbbbaaaa");
    strcpy(elm_quantity,"Stations");
    strcpy(elm_unity   ,"[-]");
    strcpy(elm_desc    ,"This element bbbbaaaa is only used in cell definition");
    four   = 20;
    error  = Defelm(&fd_nefis , elm_name, elm_type   , four        , elm_quantity,
                     elm_unity, elm_desc, elm_num_dim, elm_dimens  );
  }
  if (error == 0)
  {
    printf("\n Define next element (7)             \n");
    strcpy(elm_type    ,"CHARACTE");
    strcpy(elm_name    ,"ccccaaaa");
    strcpy(elm_quantity,"Stations");
    strcpy(elm_unity   ,"[-]");
    strcpy(elm_desc    ,"This element ccccaaaa is never used in cell definition");
    four   = 20;
    error  = Defelm(&fd_nefis , elm_name, elm_type   , four        , elm_quantity,
                     elm_unity, elm_desc, elm_num_dim, elm_dimens  );
  }
  if (error == 0)
  {
    printf("\n Define next element (8)             \n");
    strcpy(elm_type    ,"CHARACTE");
    strcpy(elm_name    ,"aaaacccc");
    strcpy(elm_quantity,"Stations");
    strcpy(elm_unity   ,"[-]");
    strcpy(elm_desc    ,"This element aaaaccccc is never used in cell definition");
    four   = 20;
    error  = Defelm(&fd_nefis , elm_name, elm_type   , four        , elm_quantity,
                     elm_unity, elm_desc, elm_num_dim, elm_dimens  );
  }

/*-------------------------------------------------------------------------*/

  strcpy( cel_name    ,"cel1");
  cel_num_dim = 1;
  strcpy( elm_names[0],"aa");
  if (error == 0) {
    printf("\n Define first cel                    \n");
    error  = Defcel2( &fd_nefis, cel_name, cel_num_dim , elm_names);
  }

  strcpy( cel_name    ,"cel2");;
  cel_num_dim = 2;
  strcpy( elm_names[0], "aa");
  strcpy( elm_names[1], "ab");

  if (error == 0) {
    printf("\n Define next cel (2)                 \n");
    error  = Defcel2( &fd_nefis, cel_name, cel_num_dim , elm_names);
  }

  strcpy( cel_name    ,"cel3");
  cel_num_dim = 1;
  strcpy( elm_names[0],"ac");

  if (error == 0) {
    printf("\n Define next cel (3)                 \n");
    error  = Defcel2( &fd_nefis, cel_name, cel_num_dim , elm_names);
  }

  strcpy( cel_name    ,"cel4");
  cel_num_dim = 1;
  strcpy( elm_names[0],"ad");

  if (error == 0) {
    printf("\n Define next cel (4)                 \n");
    error  = Defcel2( &fd_nefis, cel_name, cel_num_dim , elm_names);
  }

  strcpy( cel_name    ,"cel5");;
  cel_num_dim = 4;
  strcpy( elm_names[0], "ad");
  strcpy( elm_names[1], "ac");
  strcpy( elm_names[2], "aa");
  strcpy( elm_names[3], "ab");

  if (error == 0) {
    printf("\n Define next cel (5)                 \n");
    error  = Defcel2( &fd_nefis, cel_name, cel_num_dim , elm_names);
  }

  strcpy( cel_name    ,"aaaabbbb");
  cel_num_dim = 2;
  strcpy( elm_names[0],"aaaabbbb");
  strcpy( elm_names[1],"bbbbaaaa");

  if (error == 0) {
    printf("\n Define next cel (6)                 \n");
    error  = Defcel2( &fd_nefis, cel_name, cel_num_dim , elm_names);
  }

  strcpy( cel_name    ,"bbbbaaaa");
  cel_num_dim = 2;
  strcpy( elm_names[0],"bbbbaaaa");
  strcpy( elm_names[1],"aaaabbbb");

  if (error == 0) {
    printf("\n Define next cel (7)                 \n");
    error  = Defcel2( &fd_nefis, cel_name, cel_num_dim , elm_names);
  }

/*-------------------------------------------------------------------------*/

  strcpy( grp_name    ,"grp1");
  strcpy(&cel_names[0],"cel1");
  grp_num_dim   = 1;
  grp_dimens[0] = 7;
  grp_order [0] = 1;

  if (error == 0) {
    printf("\n Define first group                  \n");
    error  = Defgrp(&fd_nefis  , grp_name , cel_names, grp_num_dim,
                     grp_dimens, grp_order);
  }

  strcpy(grp_name     ,"grp2");
  strcpy(&cel_names[0],"cel2");
  grp_num_dim   = 3;

  grp_dimens[0] = 3;
  grp_dimens[1] = 5;
  grp_dimens[2] = 7;

  grp_order [0] = 1;
  grp_order [1] = 3;
  grp_order [2] = 2;

  if (error == 0) {
    printf("\n Define next group (2)               \n");
    error  = Defgrp( &fd_nefis  , grp_name , cel_names, grp_num_dim,
                      grp_dimens, grp_order);
  }

  strcpy(grp_name     ,"grp3");
  strcpy(&cel_names[0],"cel3");
  grp_num_dim   = 1;
  grp_dimens[0] = 13;
  grp_order [0] = 1;

  if (error == 0) {
    printf("\n Define next group (3)               \n");
    error  = Defgrp( &fd_nefis  , grp_name , cel_names, grp_num_dim,
                      grp_dimens, grp_order);
  }

  strcpy(grp_name     ,"grp4");
  strcpy(&cel_names[0],"cel4");
  grp_num_dim   = 1;
  grp_dimens[0] = 5;
  grp_order [0] = 1;

  if (error == 0) {
    printf("\n Define next group (4)               \n");
    error  = Defgrp( &fd_nefis  , grp_name , cel_names, grp_num_dim,
                      grp_dimens, grp_order);
  }

  strcpy(grp_name     ,"grp5");
  strcpy(&cel_names[0],"cel5");
  grp_num_dim   = 1;
  grp_dimens[0] = 7;
  grp_order [0] = 1;

  if (error == 0) {
    printf("\n Define next group (5)               \n");
    error  = Defgrp( &fd_nefis  , grp_name , cel_names, grp_num_dim,
                      grp_dimens, grp_order);
  }

  strcpy(grp_name     ,"aaaabbbb");
  strcpy(&cel_names[0],"aaaabbbb");
  grp_num_dim   = 1;
  grp_dimens[0] =17;
  grp_order [0] = 1;

  if (error == 0) {
    printf("\n Define next group (6)               \n");
    error  = Defgrp( &fd_nefis  , grp_name , cel_names, grp_num_dim,
                      grp_dimens, grp_order);
  }

  strcpy(grp_name     ,"bbbbaaaa");
  strcpy(&cel_names[0],"aaaabbbb");
  grp_num_dim   = 1;
  grp_dimens[0] =17;
  grp_order [0] = 1;

  if (error == 0) {
    printf("\n Define next group (7)               \n");
    error  = Defgrp( &fd_nefis  , grp_name , cel_names, grp_num_dim,
                      grp_dimens, grp_order);
  }

/*-------------------------------------------------------------------------*/

  if (error == 0) {
    printf("\n Define first user group on data file\n");
    strcpy(grp_name     ,"group1");
    strcpy(grp_defined  ,"grp1");
    error  = Credat( &fd_nefis, grp_name, grp_defined);
  }

  if (error == 0) {
    printf("\n Define user group on data file (2)  \n");
    strcpy(grp_name     ,"group2");
    strcpy(grp_defined  ,"grp2");
    error  = Credat( &fd_nefis, grp_name, grp_defined);
  }

  if (error == 0) {
    printf("\n Define user group on data file (3)  \n");
    strcpy(grp_name     ,"group3");
    strcpy(grp_defined  ,"grp3");
    error  = Credat( &fd_nefis, grp_name, grp_defined);
  }

  if (error == 0) {
    printf("\n Define user group on data file (4)  \n");
    strcpy(grp_name     ,"group4");
    strcpy(grp_defined  ,"grp4");
    error  = Credat( &fd_nefis, grp_name, grp_defined);
  }

  if (error == 0) {
    printf("\n Define user group on data file (5)  \n");
    strcpy(grp_name     ,"group5");
    strcpy(grp_defined  ,"grp5");
    error  = Credat( &fd_nefis, grp_name, grp_defined);
  }

  if (error == 0) {
    printf("\n Define user group on data file (6)  \n");
    strcpy(grp_name     ,"group6");
    strcpy(grp_defined  ,"grp2");
    error  = Credat( &fd_nefis, grp_name, grp_defined);
  }

/*-------------------------------------------------------------------------*/
  printf("\n");
  if (error == 0) {
    printf(" Check user group on data file       \n");
    strcpy(grp_name     ,"group1");
    error  = Inqdat( &fd_nefis, grp_name, grp_defined);
    printf(" User    group name: <%s>\n", grp_name   );
    printf(" Defined group name: <%s>\n", grp_defined);
  }
  if (error == 0) {
    strcpy(grp_name     ,"group2");
    error  = Inqdat( &fd_nefis, grp_name, grp_defined);
    printf(" User    group name: <%s>\n", grp_name   );
    printf(" Defined group name: <%s>\n", grp_defined);
  }
  if (error == 0) {
    strcpy(grp_name     ,"group3");
    error  = Inqdat( &fd_nefis, grp_name, grp_defined);
    printf(" User    group name: <%s>\n", grp_name   );
    printf(" Defined group name: <%s>\n", grp_defined);
  }
  if (error == 0) {
    strcpy(grp_name     ,"group4");
    error  = Inqdat( &fd_nefis, grp_name, grp_defined);
    printf(" User    group name: <%s>\n", grp_name   );
    printf(" Defined group name: <%s>\n", grp_defined);
  }
  if (error == 0) {
    strcpy(grp_name     ,"group5");
    error  = Inqdat( &fd_nefis, grp_name, grp_defined);
    printf(" User    group name: <%s>\n", grp_name   );
    printf(" Defined group name: <%s>\n", grp_defined);
  }
  if (error == 0) {
    strcpy(grp_name     ,"group6");
    error  = Inqdat( &fd_nefis, grp_name, grp_defined);
    printf(" User    group name: <%s>\n", grp_name   );
    printf(" Defined group name: <%s>\n", grp_defined);
  }

/*-------------------------------------------------------------------------*/

  printf("\n");
  if (error == 0) {
    error  = Inqfel( &fd_nefis        ,  elm_name     ,
                      elm_type        ,  elm_quantity ,
                      elm_unity       ,  elm_desc     ,
                     &elm_single_bytes, &elm_bytes    ,
                     &elm_num_dim     ,  elm_dimens  );
    printf(" 1 Element name: <%s>\n", elm_name );
  }
  i = 1;
  while (error == 0)
  {
    i++;
    error  = Inqnel( &fd_nefis        ,  elm_name    ,
                      elm_type        ,  elm_quantity,
                      elm_unity       ,  elm_desc    ,
                     &elm_single_bytes, &elm_bytes   ,
                     &elm_num_dim     ,  elm_dimens  );
    if (error == 0)
    {
      printf(" %d Element name: <%s>\n", i, elm_name );
    }
    else
    {
      (BVoid)Neferr(1, error_string);
    }
  }
  error = 0;

/*-------------------------------------------------------------------------*/

  printf("\n");
  cel_num_dim = 100;
  if (error == 0) {
    error  = Inqfcl2( &fd_nefis       ,  cel_name    ,
                     &cel_num_dim    , &cel_num_bytes,
                     &elm_names      );
      printf(" 1 Cell    name   : <%s> %d %d \n",
             cel_name, cel_num_dim, cel_num_bytes);
    for ( i=0; i<cel_num_dim; i++ )
    {
      printf("   Element name[%d]: <%s>\n", i, &elm_names[i][0] );
    }
  }
  i = 1;
  while (error == 0)
  {
    i++;
    cel_num_dim = 100;
    error  = Inqncl2( &fd_nefis       ,  cel_name    ,
                     &cel_num_dim    , &cel_num_bytes,
                     &elm_names      );
    if (error == 0)
    {
      printf(" %d Cell    name   : <%s> %d %d \n",
               i,cel_name, cel_num_dim, cel_num_bytes);
      for ( j=0; j<cel_num_dim; j++ )
      {
        printf("   Element name[%d]: <%s>\n", j, &elm_names[j][0] );
      }
    }
    else
    {
      (BVoid)Neferr(1, error_string);
    }
  }
  error = 0;
/*-------------------------------------------------------------------------*/

  printf("\n");
  if (error == 0)
  {
    grp_num_dim = 5;
    error  = Inqfgr( &fd_nefis       , grp_name    ,
                      cel_name       ,&grp_num_dim ,
                      grp_dimens     , grp_order   );
    if (error == 0)
    {
      printf(" 1 Group   name   : <%s> %d \n",
             grp_name, grp_num_dim);
    }
    else
    {
      (BVoid)Neferr(1, error_string);
    }
  }
  i = 1;
  while (error == 0)
  {
    i++;
    grp_num_dim = 5;
    error  = Inqngr( &fd_nefis       , grp_name    ,
                      cel_name       ,&grp_num_dim ,
                      grp_dimens     , grp_order   );
    if (error == 0)
    {
      printf(" %d Group   name   : <%s> %d \n",
             i, grp_name, grp_num_dim);
    }
    else
    {
      (BVoid)Neferr(1, error_string);
    }
    if ( i > 10 ) error = 1;
  }
  error = 0;
/*-------------------------------------------------------------------------*/

  if (error == 0) error = Clsnef( &fd_nefis );

  error = Neferr( 1, error_string);

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
  free( (BData) nef_version );

  printf("\nEnd program\n\n");

  return 0;
}
