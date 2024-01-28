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

int main()
{
  BChar coding  ;
  BChar error_string[LENGTH_ERROR_MESSAGE];
  BChar rdwr    ;
  BText dat_file;
  BText def_file;
  BInt4 error = 0;
  BInt4 fd_nefis;
  BText nef_version;

  error = Getnfv(&nef_version);

  dat_file = (BText) malloc ( sizeof(BChar) * (20+1) );
  def_file = (BText) malloc ( sizeof(BChar) * (20+1) );

  printf(" -----------------------------------------------\n");
  printf(" Version: %s\n", nef_version+4);

  printf(" -----------------------------------------------\n");
  printf(" Test open/close some files w.r.t read and write\n");

  coding = 'L';

/*===========================================================================*/
  rdwr = 'C';
  strcpy(dat_file,"data_c.dat");
  strcpy(def_file,"data_c.def");

  printf(" -----------------------------------------------\n");
  printf(" Filenames: %c %s %s\n", rdwr, dat_file, def_file);

  error = Crenef( &fd_nefis, dat_file, def_file, coding, rdwr);
  if ( error!=0 ) error = Neferr( 1, error_string);
  error = Clsnef( &fd_nefis );
  if ( error!=0 ) error = Neferr( 1, error_string);

/*===========================================================================*/
  rdwr = 'U';
  strcpy(dat_file,"data_u.dat");
  strcpy(def_file,"data_u.def");

  printf(" -----------------------------------------------\n");
  printf(" Filenames: %c %s %s\n", rdwr, dat_file, def_file);

  if (error == 0)
  {
    error = Crenef( &fd_nefis, dat_file, def_file, coding, rdwr);
    if ( error!=0 ) error = Neferr( 1, error_string);
    error = Clsnef( &fd_nefis );
    if ( error!=0 ) error = Neferr( 1, error_string);
  }

/*===========================================================================*/
  rdwr = 'R';
  strcpy(dat_file,"data_r.dat");
  strcpy(def_file,"data_r.def");

  printf(" -----------------------------------------------\n");
  printf(" Filenames: %c %s %s\n", rdwr, dat_file, def_file);

  if (error == 0)
  {
    error = Crenef( &fd_nefis, dat_file, def_file, coding, rdwr);
    if ( error!=0 ) error = Neferr( 1, error_string);
    error = Clsnef( &fd_nefis );
    if ( error!=0 ) error = Neferr( 1, error_string);
  }

/*===========================================================================*/
  rdwr = 'C';
  strcpy(dat_file,"data_c.dat");
  strcpy(def_file,"data_c.def");

  printf(" -----------------------------------------------\n");
  printf(" Filenames: %c %s %s\n", rdwr, dat_file, def_file);

  if (error == 0)
  {
    error = Crenef( &fd_nefis, dat_file, def_file, coding, rdwr);
    if ( error!=0 ) error = Neferr( 1, error_string);
    error = Clsnef( &fd_nefis );
    if ( error!=0 ) error = Neferr( 1, error_string);
  }

/*===========================================================================*/
  rdwr = 'U';
  strcpy(dat_file,"data_u.dat");
  strcpy(def_file,"data_u.def");

  printf(" -----------------------------------------------\n");
  printf(" Filenames: %c %s %s\n", rdwr, dat_file, def_file);

  if (error == 0)
  {
    error = Crenef( &fd_nefis, dat_file, def_file, coding, rdwr);
    if ( error!=0 ) error = Neferr( 1, error_string);
    error = Clsnef( &fd_nefis );
    if ( error!=0 ) error = Neferr( 1, error_string);
  }

/*===========================================================================*/
  rdwr = 'R';
  strcpy(dat_file,"data_c.dat");
  strcpy(def_file,"data_c.def");

  printf(" -----------------------------------------------\n");
  printf(" Filenames: %c %s %s\n", rdwr, dat_file, def_file);

  if (error == 0)
  {
    error = Crenef( &fd_nefis, dat_file, def_file, coding, rdwr);
    if ( error!=0 ) error = Neferr( 1, error_string);
    error = Clsnef( &fd_nefis );
    if ( error!=0 ) error = Neferr( 1, error_string);
  }


/*===========================================================================*/
  rdwr = 'C';
  strcpy(dat_file,"data_c.daf");
  strcpy(def_file,"data_c.daf");

  printf(" -----------------------------------------------\n");
  printf(" Filenames: %c %s %s\n", rdwr, dat_file, def_file);

  error = Crenef( &fd_nefis, dat_file, def_file, coding, rdwr);
  if ( error!=0 ) error = Neferr( 1, error_string);
  error = Clsnef( &fd_nefis );
  if ( error!=0 ) error = Neferr( 1, error_string);

/*===========================================================================*/
  rdwr = 'U';
  strcpy(dat_file,"data_u.daf");
  strcpy(def_file,"data_u.daf");

  printf(" -----------------------------------------------\n");
  printf(" Filenames: %c %s %s\n", rdwr, dat_file, def_file);

  error = Crenef( &fd_nefis, dat_file, def_file, coding, rdwr);
  if ( error!=0 ) error = Neferr( 1, error_string);
  error = Clsnef( &fd_nefis );
  if ( error!=0 ) error = Neferr( 1, error_string);

/*===========================================================================*/
  rdwr = 'R';
  strcpy(dat_file,"data_r.daf");
  strcpy(def_file,"data_r.daf");

  printf(" -----------------------------------------------\n");
  printf(" Filenames: %c %s %s\n", rdwr, dat_file, def_file);

  error = Crenef( &fd_nefis, dat_file, def_file, coding, rdwr);
  if ( error!=0 ) error = Neferr( 1, error_string);
  error = Clsnef( &fd_nefis );
  if ( error!=0 ) error = Neferr( 1, error_string);

/*===========================================================================*/
  rdwr = 'C';
  strcpy(dat_file,"data_c.daf");
  strcpy(def_file,"data_c.daf");

  printf(" -----------------------------------------------\n");
  printf(" Filenames: %c %s %s\n", rdwr, dat_file, def_file);

  error = Crenef( &fd_nefis, dat_file, def_file, coding, rdwr);
  if ( error!=0 ) error = Neferr( 1, error_string);
  error = Clsnef( &fd_nefis );
  if ( error!=0 ) error = Neferr( 1, error_string);

/*===========================================================================*/
  rdwr = 'U';
  strcpy(dat_file,"data_u.daf");
  strcpy(def_file,"data_u.daf");

  printf(" -----------------------------------------------\n");
  printf(" Filenames: %c %s %s\n", rdwr, dat_file, def_file);

  error = Crenef( &fd_nefis, dat_file, def_file, coding, rdwr);
  if ( error!=0 ) error = Neferr( 1, error_string);
  error = Clsnef( &fd_nefis );
  if ( error!=0 ) error = Neferr( 1, error_string);

/*===========================================================================*/
  rdwr = 'R';
  strcpy(dat_file,"data_u.daf");
  strcpy(def_file,"data_u.daf");

  printf(" -----------------------------------------------\n");
  printf(" Filenames: %c %s %s\n", rdwr, dat_file, def_file);

  error = Crenef( &fd_nefis, dat_file, def_file, coding, rdwr);
  if ( error!=0 ) error = Neferr( 1, error_string);
  error = Clsnef( &fd_nefis );
  if ( error!=0 ) error = Neferr( 1, error_string);

/*===========================================================================*/
  error = Neferr( 1, error_string);

  free ( (BData) dat_file );
  free ( (BData) def_file );
  free ( (BData) nef_version );

  printf("\nEnd program\n\n");

  return 0;
}
