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

#define MAX_DIM       5
#define MAX_NAME     16

int main(){
  BInt4   error=0;
  BInt4   i;
  BInt4   fd1 = -1;
  BInt4   fd2 = -1;
  BInt4   buf_len;
  BText   elm_name    ;
  BChar   error_string[1024];
  BText   grp_name    ;
  BChar   coding          ;
  BChar   rdwr            ;
  BChar   dat_file    [21];
  BChar   def_file    [21];
  BInt4   usr_index [5][3];
  BInt4   usr_order [5];
  BText   result;
  BText   nef_version;

  error = Getnfv(&nef_version);

  printf(" -----------------------------------------------\n");
  printf(" Version: %s\n", nef_version+4);
  printf(" -----------------------------------------------\n");
  
  elm_name     = (BText) malloc( sizeof(BChar) * (MAX_NAME + 1) );
  grp_name     = (BText) malloc( sizeof(BChar) * (MAX_NAME + 1) );

  buf_len = 80;
  result       = (BText) malloc( sizeof(BChar) * (buf_len+1 ));
  for ( i=0; i<buf_len; i++) { result[i]='\0'; }

  rdwr = 'R';
  coding = 'N';

  strcpy(dat_file,"untitled.wdi");
  strcpy(def_file,"untitled.wdf");
  printf(" Filenames: %s %s\n", dat_file, def_file);
  error = Crenef( &fd1, dat_file, def_file, coding,  rdwr);
  if (error != 0) {
    error = Neferr( 1, error_string);
    exit(1);
  }

  rdwr = 'R';
  coding = 'N';
  strcpy(dat_file,"sewage_1.wdi");
  strcpy(def_file,"sewage_1.wdf");
  printf(" Filenames: %s %s\n", dat_file, def_file);
  error = Crenef( &fd2, dat_file, def_file, coding,  rdwr);
  if (error != 0) {
    error = Neferr( 1, error_string);
    exit(1);
  }


/*-------------------------------------------------------------------------*/
  if ( error == 0 )
  {
    for ( i=0; i<MAX_DIM; i++) {
       usr_index[i][0] = 1;
       usr_index[i][1] = 1;
       usr_index[i][2] = 1;
       usr_order[i]    = i+1;
    }
    strcpy( grp_name, "WANDA");
    strcpy( elm_name, "Wanda_version");
    for ( i=0; i<buf_len; i++) { result[i]='\0'; }
    error = Getelt( &fd1      , grp_name , elm_name,
                     (BUInt4 *)usr_index, usr_order ,&buf_len , result   );
    printf(" Result: (%d),<%s>\n", strlen(result),  result);
    if (error != 0) error = Neferr( 1, error_string);

    for ( i=0; i<MAX_DIM; i++) {
       usr_index[i][0] = 1;
       usr_index[i][1] = 1;
       usr_index[i][2] = 1;
       usr_order[i]    = i+1;
    }
    strcpy( grp_name, "WANDA");
    strcpy( elm_name, "Wanda_release");
    for ( i=0; i<buf_len; i++) { result[i]='\0'; }
    error = Getelt( &fd1      , grp_name , elm_name,
                     (BUInt4 *)usr_index, usr_order ,&buf_len , result   );
    printf(" Result: (%d),<%s>\n", strlen(result),  result);
    if (error != 0) error = Neferr( 1, error_string);

    for ( i=0; i<MAX_DIM; i++) {
       usr_index[i][0] = 1;
       usr_index[i][1] = 1;
       usr_index[i][2] = 1;
       usr_order[i]    = i+1;
    }
    strcpy( grp_name, "WANDA");
    strcpy( elm_name, "Licensee");
    for ( i=0; i<buf_len; i++) { result[i]='\0'; }
    error = Getelt( &fd1      , grp_name , elm_name,
                     (BUInt4 *)usr_index, usr_order ,&buf_len , result   );
    printf(" Result: (%d),<%s>\n", strlen(result),  result);
    if (error != 0) error = Neferr( 1, error_string);

    for ( i=0; i<MAX_DIM; i++) {
       usr_index[i][0] = 1;
       usr_index[i][1] = 1;
       usr_index[i][2] = 1;
       usr_order[i]    = i+1;
    }
    strcpy( grp_name, "WANDA");
    strcpy( elm_name, "Wanda_version");
    for ( i=0; i<buf_len; i++) { result[i]='\0'; }
    error = Getelt( &fd2      , grp_name , elm_name,
                     (BUInt4 *)usr_index, usr_order ,&buf_len , result   );
    printf(" Result: (%d),<%s>\n", strlen(result),  result);
    if (error != 0) error = Neferr( 1, error_string);

    for ( i=0; i<MAX_DIM; i++) {
       usr_index[i][0] = 1;
       usr_index[i][1] = 1;
       usr_index[i][2] = 1;
       usr_order[i]    = i+1;
    }
    strcpy( grp_name, "WANDA");
    strcpy( elm_name, "Wanda_release");
    for ( i=0; i<buf_len; i++) { result[i]='\0'; }
    error = Getelt( &fd2      , grp_name , elm_name,
                     (BUInt4 *)usr_index, usr_order ,&buf_len , result   );
    printf(" Result: (%d),<%s>\n", strlen(result),  result);
    if (error != 0) error = Neferr( 1, error_string);

    for ( i=0; i<MAX_DIM; i++) {
       usr_index[i][0] = 1;
       usr_index[i][1] = 1;
       usr_index[i][2] = 1;
       usr_order[i]    = i+1;
    }
    strcpy( grp_name, "WANDA");
    strcpy( elm_name, "Licensee");
    for ( i=0; i<buf_len; i++) { result[i]='\0'; }
    error = Getelt( &fd2      , grp_name , elm_name,
                     (BUInt4 *)usr_index, usr_order ,&buf_len , result   );
    printf(" Result: (%d),<%s>\n", strlen(result),  result);
    if (error != 0) error = Neferr( 1, error_string);

    for ( i=0; i<MAX_DIM; i++) {
       usr_index[i][0] = 1;
       usr_index[i][1] = 1;
       usr_index[i][2] = 1;
       usr_order[i]    = i+1;
    }
    strcpy( grp_name, "WANDA");
    strcpy( elm_name, "Wanda_version");
    for ( i=0; i<buf_len; i++) { result[i]='\0'; }
    error = Getelt( &fd1      , grp_name , elm_name,
                     (BUInt4 *)usr_index, usr_order ,&buf_len , result   );
    printf(" Result: (%d),<%s>\n", strlen(result),  result);
    if (error != 0) error = Neferr( 1, error_string);

    for ( i=0; i<MAX_DIM; i++) {
       usr_index[i][0] = 1;
       usr_index[i][1] = 1;
       usr_index[i][2] = 1;
       usr_order[i]    = i+1;
    }
    strcpy( grp_name, "WANDA");
    strcpy( elm_name, "Wanda_release");
    for ( i=0; i<buf_len; i++) { result[i]='\0'; }
    error = Getelt( &fd1      , grp_name , elm_name,
                     (BUInt4 *)usr_index, usr_order ,&buf_len , result   );
    printf(" Result: (%d),<%s>\n", strlen(result),  result);
    if (error != 0) error = Neferr( 1, error_string);

    for ( i=0; i<MAX_DIM; i++) {
       usr_index[i][0] = 1;
       usr_index[i][1] = 1;
       usr_index[i][2] = 1;
       usr_order[i]    = i+1;
    }
    strcpy( grp_name, "WANDA");
    strcpy( elm_name, "Licensee");
    for ( i=0; i<buf_len; i++) { result[i]='\0'; }
    error = Getelt( &fd1      , grp_name , elm_name,
                     (BUInt4 *)usr_index, usr_order ,&buf_len , result   );
    printf(" Result: (%d),<%s>\n", strlen(result),  result);
    if (error != 0) error = Neferr( 1, error_string);
  }


/*-------------------------------------------------------------------------*/

  if (error == 0) error = Clsnef( &fd1 );
  if (error == 0) error = Clsnef( &fd2 );

  error = Neferr( 1, error_string);

  free(elm_name); elm_name=NULL;
  free(grp_name); grp_name=NULL;
  free(result  ); result=NULL;
  free((BData) nef_version);nef_version=NULL;
  
  printf("\nEnd program\n\n");

  return 0;
}
