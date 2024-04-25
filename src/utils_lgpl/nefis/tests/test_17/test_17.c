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

#if defined(WIN32)
# define strdup _strdup
#endif

#define MAX_DESC     64
#define MAX_DIM       5
#define MAX_NAME     16
#define MAX_TYPE      8
#define MAX_CEL_DIM 100

int main(){
    BInt4   buf_len     ;
    BText   cel_name    ;
    BText   cel_names   ;
    BInt4   cel_num_dim =-1 ;
    BChar   coding          ;
    BText   dat_file;
    BText   def_file;
    BText   elm_desc    ;
    BInt4 * elm_dimens  ;
    BText   elm_name    ;
    BText * elm_names;
    BInt4   elm_num_dim =-1 ;
    BText   elm_quantity;
    BInt4   elm_single_byte;
    BText   elm_type    ;
    BText   elm_unity   ;
    BChar   error_string[LENGTH_ERROR_MESSAGE];
    BInt4   fd_nefis=-1;
    BInt4   fd_nefisa=-1;
    BInt4 * getal;
    BInt4 * getallen;
    BText   grp_defined ;
    BInt4 * grp_dimens  ;
    BText   grp_name    ;
    BInt4   grp_num_dim =-1 ;
    BInt4 * grp_order   ;
#if DEBUG_NEFIS
      BInt4   ii;
#endif
    BInt4   i, jj, kk, ll, mm, nn, error=0;
    BInt4   max_index       ;
    BChar   namst    [3][3][3][21];
    BChar   namstat  [3][3][3][21];
    BChar   rdwr            ;
    BInt4   usr_index [5][3];
    BInt4 * usr_order   ;
    BRea4 * zeta ;
    BRea4 * zetaa;
    BText nef_version;

    error = Getnfv(&nef_version);

	printf(" -----------------------------------------------\n");
	printf(" Version: %s\n", nef_version+4);
	printf(" -----------------------------------------------\n");

    getal        = (BInt4 * ) malloc( sizeof( BInt4   ) * 24 );
    getallen     = (BInt4 * ) malloc( sizeof( BInt4   ) * 24 );
    zeta         = (BRea4 * ) malloc( sizeof( BRea4 ) * 7*5*3*4*6 );
    zetaa        = (BRea4 * ) malloc( sizeof( BRea4 ) * 7*5*3*4*6 );

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

    for ( i=0; i<MAX_DIM; i++)
    {
        elm_dimens[i] = 0;
        grp_dimens[i] = 0;
        grp_order [i] = 0;
        usr_order [i] = 0;
    }

    rdwr = 'U';
    coding = ' ';

    dat_file = strdup("trim-f34.dat");
    def_file = strdup("trim-f34.def");
    printf(" Filenames: %s %s\n", dat_file, def_file);
    error = Crenef( &fd_nefis, dat_file, def_file, coding,  rdwr);
    if (error != 0) {
        error = Neferr( 1, error_string);
        exit(1);
    }

/*-------------------------------------------------------------------------*/
    if ( error == 0 )
    {
        buf_len = sizeof(BChar) * (22*15*6+1 );
        for ( i=0; i<MAX_DIM; i++) {
           usr_index[i][0] = 1;
           usr_index[i][1] = 1;
           usr_index[i][2] = 1;
           usr_order[i]    = i+1;
        }
        grp_name = strdup("map-series");
        elm_name = strdup("S1");
        error = Getelt( &fd_nefis      , grp_name , elm_name,
                       (BUInt4 *) usr_index, usr_order ,&buf_len , (BData) zeta   );
        if (error != 0) error = Neferr( 1, error_string);
        printf(" Result: \'%16s\', %g\n", elm_name,   zeta[150]);
    }

/*---------------------1234567890123456------------------------------------*/
  strcpy(elm_name    ,"aa");
  strcpy(elm_type    ,"INTEGER");
  strcpy(elm_quantity,"Waterstanden");
  strcpy(elm_unity   ,"meter");
  strcpy(elm_desc    ,"Mooiman, Vlaardingen, Nederland");
  elm_single_byte = 4;
  elm_num_dim  = 1;
  elm_dimens[0]= 11;

  if (error == 0 ) {
    printf(" Define first element \n");
    error  = Defelm(&fd_nefis , elm_name    , elm_type   , elm_single_byte,
                                elm_quantity, elm_unity  , elm_desc,
                                elm_num_dim , elm_dimens  );
  }

  elm_num_dim     = 2;
  elm_dimens[0]   = 6;
  elm_dimens[1]   = 4;
  if (error == 0 ) {
    strcpy(elm_type    ,"Real");
    strcpy(elm_name    ,"ab");
    printf(" Define next element (2)\n");
    error  = Defelm(&fd_nefis , elm_name    , elm_type   , elm_single_byte,
                                elm_quantity, elm_unity  , elm_desc,
                                elm_num_dim , elm_dimens  );
  }

  elm_num_dim  = 1;
  elm_dimens[0]= 5;
  if (error == 0 ) {
    strcpy(elm_type    ,"INTEGER");
    strcpy(elm_name    ,"ac");
    printf(" Define next element (3)\n");
    error  = Defelm(&fd_nefis , elm_name    , elm_type   , elm_single_byte,
                                elm_quantity, elm_unity  , elm_desc,
                                elm_num_dim , elm_dimens  );
  }

  strcpy(elm_type    ,"CHARACTE");
  strcpy(elm_name    ,"ad");
  strcpy(elm_quantity,"Stationsnamen");
  strcpy(elm_unity   ,"[-]");
  strcpy(elm_desc    ,"Stations met metingen");
  elm_single_byte = 21;
  elm_num_dim   = 3;
  elm_dimens[0] = 3;
  elm_dimens[1] = 3;
  elm_dimens[2] = 3;

  if (error == 0 ) {
    printf(" Define next element (4)\n");
    error  = Defelm(&fd_nefis , elm_name    , elm_type   , elm_single_byte,
                                elm_quantity, elm_unity  , elm_desc,
                                elm_num_dim , elm_dimens  );
  }
/*-------------------------------------------------------------------------*/

  cel_num_dim = 1;
  strcpy( cel_name    ,"cel1");
  strcpy( elm_names[0],"aa"  );
  if (error == 0 ) {
    printf(" Define first cel     \n");
    error  = Defcel2( &fd_nefis, cel_name, cel_num_dim , elm_names);
  }

  cel_num_dim = 2;
  strcpy( cel_name    ,"cel2");;
  strcpy( elm_names[0], "aa");
  strcpy( elm_names[1], "ab");

  if (error == 0 ) {
    printf(" Define next cel (2)\n");
    error  = Defcel2( &fd_nefis, cel_name, cel_num_dim , elm_names);
  }

  cel_num_dim = 1;
  strcpy( cel_name    ,"cel3");
  strcpy( elm_names[0],"ac"  );

  if (error == 0 ) {
    printf(" Define next cel (3)\n");
    error  = Defcel2( &fd_nefis, cel_name, cel_num_dim , elm_names);
  }

  cel_num_dim = 1;
  strcpy( cel_name    ,"cel4");
  strcpy( elm_names[0],"ad"  );

  if (error == 0 ) {
    printf(" Define next cel (4)\n");
    error  = Defcel2( &fd_nefis, cel_name, cel_num_dim , elm_names);
  }

/*-------------------------------------------------------------------------*/

  strcpy( grp_name    ,"grp1");
  strcpy(&cel_names[0],"cel1");
  grp_num_dim   = 1;
  grp_dimens[0] = 6;
  grp_order [0] = 1;

  if (error == 0 ) {
    printf(" Define first group\n");
    error  = Defgrp ( &fd_nefis  , grp_name  , cel_names, grp_num_dim,
                                   grp_dimens, grp_order);
  }

  strcpy(grp_name     ,"grp2");
  strcpy(&cel_names[0],"cel2");
  grp_num_dim   = 3;

  grp_dimens[0] = 3;
  grp_dimens[1] = 5;
  grp_dimens[2] = 0;

  grp_order [0] = 1;
  grp_order [1] = 3;
  grp_order [2] = 2;

  if (error == 0 ) {
    printf(" Define next group (2)\n");
    error  = Defgrp ( &fd_nefis  , grp_name  , cel_names, grp_num_dim,
                                   grp_dimens, grp_order);
  }

  strcpy(grp_name     ,"grp3");
  strcpy(&cel_names[0],"cel3");
  grp_num_dim   = 1;
  grp_dimens[0] = 13;
  grp_order [0] = 1;

  if (error == 0 ) {
    printf(" Define next group (3)\n");
    error  = Defgrp ( &fd_nefis  , grp_name  , cel_names, grp_num_dim,
                                   grp_dimens, grp_order);
  }

  strcpy(grp_name     ,"grp4");
  strcpy(&cel_names[0],"cel4");
  grp_num_dim   = 1;
  grp_dimens[0] = 13;
  grp_order [0] = 1;

  if (error == 0 ) {
    printf(" Define next group (4)\n");
    error  = Defgrp ( &fd_nefis  , grp_name  , cel_names, grp_num_dim,
                                   grp_dimens, grp_order);
  }
/*-------------------------------------------------------------------------*/

  if (error == 0 ) {
    printf(" Define first user group on data file\n");
    strcpy(grp_name     ,"group1");
    strcpy(grp_defined  ,"grp1");
    error  = Credat( &fd_nefis, grp_name, grp_defined);
  }

  if (error == 0 ) {
    printf(" Define user group on data file (2)\n");
    strcpy(grp_name     ,"group2");
    strcpy(grp_defined  ,"grp2");
    error  = Credat( &fd_nefis, grp_name, grp_defined);
  }

  if (error == 0 ) {
    printf(" Define user group on data file (3)\n");
    strcpy(grp_name     ,"group3");
    strcpy(grp_defined  ,"grp3");
    error  = Credat( &fd_nefis, grp_name, grp_defined);
  }

  if (error == 0 ) {
    printf(" Define user group on data file (4)\n");
    strcpy(grp_name     ,"group4");
    strcpy(grp_defined  ,"grp2");
    error  = Credat( &fd_nefis, grp_name, grp_defined);
  }

  if (error == 0 ) {
    printf(" Define user group on data file (5)\n");
    strcpy(grp_name     ,"group5");
    strcpy(grp_defined  ,"grp4");
    error  = Credat( &fd_nefis, grp_name, grp_defined);
  }

/*-------------------------------------------------------------------------*/

  if (error == 0 ) {
    printf(" Check user group on data file       \n");
    strcpy(grp_name     ,"group1");
    error  = Inqdat(&fd_nefis, grp_name, grp_defined);
    printf(" User    group name: <%s>\n", grp_name   );
    printf(" Defined group name: <%s>\n", grp_defined);
  }
  if (error == 0 ) {
    strcpy(grp_name     ,"group2");
    error  = Inqdat(&fd_nefis, grp_name, grp_defined);
    printf(" User    group name: <%s>\n", grp_name   );
    printf(" Defined group name: <%s>\n", grp_defined);
  }
  if (error == 0 ) {
    strcpy(grp_name     ,"group3");
    error  = Inqdat(&fd_nefis, grp_name, grp_defined);
    printf(" User    group name: <%s>\n", grp_name   );
    printf(" Defined group name: <%s>\n", grp_defined);
  }
  if (error == 0 ) {
    strcpy(grp_name     ,"group4");
    error  = Inqdat(&fd_nefis, grp_name, grp_defined);
    printf(" User    group name: <%s>\n", grp_name   );
    printf(" Defined group name: <%s>\n", grp_defined);
  }
  if (error == 0 ) {
    strcpy(grp_name     ,"group5");
    error  = Inqdat(&fd_nefis, grp_name, grp_defined);
    printf(" User    group name: <%s>\n", grp_name   );
    printf(" Defined group name: <%s>\n", grp_defined);
  }

/*-------------------------------------------------------------------------*/
/*  Put data into NEFIS files                                              */
/*-------------------------------------------------------------------------*/
  for ( i=0; i<6; i++) {
    if ( error == 0 ) {
      strcpy(elm_name, "aa");
      strcpy(grp_name, "group1");
      printf(" Put integer data on data file: <%s><%s>\n", grp_name, elm_name);

      usr_index[0][0] = i+1;
      usr_index[0][1] = i+1;
      usr_index[0][2] = 1;

      usr_order[0]    = 1;

      for ( jj=0; jj<11; jj++) {
        getal[jj]=1000*(i+1) + (jj+1);
        printf(" %5d",getal[jj]);
      }
      printf("\n");

      error = Putelt(&fd_nefis, grp_name , elm_name,
                     (BInt4 *) usr_index, usr_order, (BData) getal   );
    }
  }

  for ( i=0; i<2; i++) {
    if ( error == 0 ) {
      strcpy(elm_name, "ab");
      strcpy(grp_name, "group2");
      printf(" Put real    data on data file: <%s><%s>\n", grp_name, elm_name);

      usr_index[0][0] = 1;
      usr_index[0][1] = 7;
      usr_index[0][2] = 1;

      usr_index[1][0] = 1;
      usr_index[1][1] = 3;
      usr_index[1][2] = 1;

      usr_index[2][0] = 1;
      usr_index[2][1] = 5;
      usr_index[2][2] = 1;

      usr_order[0]    = 3;
      usr_order[1]    = 1;
      usr_order[2]    = 2;

      for ( nn=0; nn<7 ; nn++) {
      for ( mm=0; mm<5 ; mm++) {
      for ( ll=0; ll<3 ; ll++) {
        for ( jj=0; jj<4 ; jj++) {
        for ( kk=0; kk<6 ; kk++) {
          zeta [kk+6*jj+6*4*ll+6*4*3*mm+6*4*3*5*nn]
                      = (BRea4)(  100*(nn+1)+
                                 1000*(mm+1)+
                                10000*(ll+1)+
                                   10*(jj+1)+
                                       kk+1   );
        }}
      }}}

/*      {
          BInt4 ii;
          for ( nn=0; nn<7 ; nn++) {
          for ( mm=0; mm<5 ; mm++) {
          for ( ll=0; ll<3 ; ll++) {
            printf("\n %d(3)  %d(5)  %d(7)", ll+1, mm+1, nn+1);
            for ( jj=0; jj<4 ; jj++) {
            printf("\n");
            for ( kk=0; kk<6 ; kk++) {
              ii= kk+6*jj+6*4*ll+6*4*3*mm+6*4*3*5*nn;
              printf(" %5.1f",zeta[ii]);
            }}
          }}}
          printf("\n");
      }
*/

      error = Putelt(&fd_nefis, grp_name , elm_name,
                    (BInt4 *) usr_index, usr_order, (BData) zeta    );
    }
  }

  strcpy(grp_name, "group3");
  printf(" Group <%s> not filled with data\n", grp_name);
  strcpy(grp_name, "group4");
  printf(" Group <%s> not filled with data\n", grp_name);

  for ( i=0; i<13; i++) {
    if ( error == 0 ) {
      strcpy(elm_name, "ad");
      strcpy(grp_name, "group5");
      printf(" Put character data on data file: <%s><%s>\n", grp_name, elm_name);

      usr_index[0][0] = i+1;
      usr_index[0][1] = i+1;
      usr_index[0][2] = 1;

      usr_order[0]    = 1;

      for ( ll=0; ll<3 ; ll++) {
        for ( jj=0; jj<3 ; jj++) {
        for ( kk=0; kk<3 ; kk++) {
          sprintf( namst[ll][jj][kk], "Meet%02d%2d%2d%2d",
          i, ll, jj, kk);
          sprintf( namstat[ll][jj][kk], "----");
        }}
      }


      for ( ll=0; ll<3 ; ll++) {
        for ( jj=0; jj<3 ; jj++) {
        for ( kk=0; kk<3 ; kk++) {
/*          printf( " In : Meetstation: <%s>\n", namst[ll][jj][kk]); */
        }}
      }

      error = Putelt(&fd_nefis, grp_name , elm_name,
                     (BInt4 *) usr_index, usr_order, (BData) namst   );
    }
  }

/*-------------------------------------------------------------------------*/

  for ( i=0; i<2; i++) {
    if ( error == 0 ) {
      strcpy(elm_name, "aa");
      strcpy(grp_name, "group1");

      printf(" Get integer data of data file: <%s><%s>\n",
            grp_name, elm_name);

      usr_index[0][0] = i+3;
      usr_index[0][1] = i+3;
      usr_index[0][2] = 1;

      usr_order[0]    = 1;
      buf_len         = 44;

      error = Getelt( &fd_nefis , grp_name , elm_name,
                       (BInt4 *) usr_index,  usr_order,&buf_len , (BData) getallen );

      if ( error != 0 ) printf("***ERROR: Error from getelt: %2d\n", i);

        {
            int ii;
            for ( ii=0; ii<11; ii++) {
                printf(" %5d",getallen[ii]);
            }
            printf("\n");
        }
    }
  }

  for ( i=0; i<2; i++) {
    if ( error == 0 ) {
        strcpy(elm_name, "ab");
        strcpy(grp_name, "group2");

        printf(" Get real data of data file: <%s><%s>\n",
            grp_name, elm_name);

        usr_index[0][0] = 1+i;
        usr_index[0][1] = 1+i;
        usr_index[0][2] = 1;

        usr_index[1][0] = 5;
        usr_index[1][1] = 5;
        usr_index[1][2] = 1;

        usr_index[2][0] = 1;
        usr_index[2][1] = 1;
        usr_index[2][2] = 1;

        usr_order[0]    = 3;
        usr_order[1]    = 2;
        usr_order[2]    = 1;

        buf_len         = 4*4*6; /* One elemnt from group */

        error = Getelt( &fd_nefis , grp_name , elm_name,
                       (BInt4 *) usr_index, usr_order,&buf_len , (BData) zetaa    );

        {
            int ii;
            for ( ii=0; ii<24; ii++) {
                printf(" %5.1f",zetaa[ii]);
                /*printf(" %5d",zetaa[ii]);*/
            }
            printf("\n");
        }
    }
  }

  for ( i=5; i<7; i++) {
    if ( error == 0 ) {
      strcpy(elm_name, "ad");
      strcpy(grp_name, "group5");

      printf(" Get character data of data file: <%s><%s> step %d\n",
            grp_name, elm_name, i);

      usr_index[0][0] = i+1;
      usr_index[0][1] = i+1;
      usr_index[0][2] = 1;

      usr_order[0]    = 1;

      buf_len         = 3*3*3*21;

      error = Getelt( &fd_nefis , grp_name , elm_name,
                       (BInt4 *) usr_index,  usr_order,&buf_len , (BData) namstat  );

      if ( error != 0 ) error = Neferr(1, error_string);
      for ( ll=0; ll<3 ; ll++) {
        for ( jj=0; jj<3 ; jj++) {
        for ( kk=0; kk<3 ; kk++) {
          printf( " Uit: Meetstation: <%s>\n", namstat[ll][jj][kk]);
        }}
      }
    }
  }
/*-------------------------------------------------------------------------*/
  if ( error == 0 ) {
    strcpy(grp_name,"group1");
    error = Inqmxi (&fd_nefis, grp_name, &max_index);
    printf(" Largest dimension for group <%s>: %d\n", grp_name, max_index);
  }
  if ( error == 0 ) {
    strcpy(grp_name,"group2");
    error = Inqmxi (&fd_nefis, grp_name, &max_index);
    printf(" Largest dimension for group <%s>: %d\n", grp_name, max_index);
  }
  if ( error == 0 ) {
    strcpy(grp_name,"group3");
    error = Inqmxi (&fd_nefis, grp_name, &max_index);
    printf(" Largest dimension for group <%s>: %d\n", grp_name, max_index);
  }
  if ( error == 0 ) {
    strcpy(grp_name,"group4");
    error = Inqmxi (&fd_nefis, grp_name, &max_index);
    printf(" Largest dimension for group <%s>: %d\n", grp_name, max_index);
  }
  if ( error == 0 ) {
    strcpy(grp_name,"group5");
    error = Inqmxi (&fd_nefis, grp_name, &max_index);
    printf(" Largest dimension for group <%s>: %d\n", grp_name, max_index);
  }

/*-------------------------------------------------------------------------*/
  if ( error == 0 ) {
    error = Inqfst (&fd_nefis, grp_name, grp_defined);
  }

  i = 0;
  while (error == 0 )
  {
    i++;
    printf(" group[%2d] = <%s> which", i, grp_name);
    printf(" is defined as <%s>\n", grp_defined);
    error = Inqnxt (&fd_nefis, grp_name, grp_defined);
  }
/*-------------------------------------------------------------------------*/
    if (error == 0) error = Clsnef( &fd_nefis );

    error = Neferr( 1, error_string);

    free( (BData) nef_version );

    printf("\nEnd program\n\n");

    return 0;
}
