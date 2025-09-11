//---- LGPL --------------------------------------------------------------------
//
// Copyright (C)  Stichting Deltares, 2011-2025.
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
 *   <c2c.c> - Functions related to the conversion C to C
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
 *   This interface between C and C is needed to translate the user
 *   defined arrays to array sizes needed by the core of the NEFIS library
 *
 */

#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>
#include <time.h>

#if defined(NEFIS_DLL)
#  define DLLEXPORT  __declspec( dllexport )
#else
#  define DLLEXPORT
#endif

#if defined(_WIN32)
#  include <io.h>
#  include <sys\stat.h>
#  define strdup _strdup
#elif defined(salford32)
#  include <io.h>
#  include <windows.h>
#endif


#include "btps.h"
#include "nef-def.h"
#include "c2c.h"

#define A2D(m,n)  ((n)+ DEFINE_STEPS*(m))

#define CLOSE_DAT_FILE          Cldtnf
#define CLOSE_DEF_FILE          Cldfnf
#define CLOSE_FLUSH_DAT_FILE    Clsdat
#define CLOSE_FLUSH_DEF_FILE    Clsdef
#define CLOSE_NEFIS             Clsnef
#define CLOSE_ALL_NEFIS         Clsanf
#define CREATE_NEFIS            Crenef
#define DEFINE_CELL             Defcel
#define DEFINE_CELL2            Defcel2
#define DEFINE_CELL3            Defcel3
#define DEFINE_DATA             Credat
#define DEFINE_ELEMENT          Defelm
#define DEFINE_GROUP            Defgrp
#define FLUSH_DAT_FILE          Flsdat
#define FLUSH_DEF_FILE          Flsdef
#define GET_DAT_HEADER          Gethdt
#define GET_DEF_HEADER          Gethdf
#define GET_ELEMENT             Getelt
#define GET_ELEMENT_STRING      Getels
#define GET_INT_ATTRIBUTE       Getiat
#define GET_REAL_ATTRIBUTE      Getrat
#define GET_STRING_ATTRIBUTE    Getsat
#define GET_NEFIS_VERSION       Getnfv
#define INQUIRE_CELL            Inqcel
#define INQUIRE_CELL2           Inqcel2
#define INQUIRE_CELL3           Inqcel3
#define INQUIRE_ELEMENT         Inqelm
#define INQUIRE_FIRST_ELEMENT   Inqfel
#define INQUIRE_FIRST_CELL      Inqfcl
#define INQUIRE_FIRST_CELL2     Inqfcl2
#define INQUIRE_FIRST_CELL3     Inqfcl3
#define INQUIRE_FIRST_DEF_GROUP Inqfgr
#define INQUIRE_FIRST_DAT_GROUP Inqfst
#define INQUIRE_FIRST_INTEGER   Inqfia
#define INQUIRE_FIRST_REAL      Inqfra
#define INQUIRE_FIRST_STRING    Inqfsa
#define INQUIRE_GROUP           Inqgrp
#define INQUIRE_DATA_GROUP      Inqdat
#define INQUIRE_MAX_INDEX       Inqmxi
#define INQUIRE_NEXT_ELEMENT    Inqnel
#define INQUIRE_NEXT_CELL       Inqncl
#define INQUIRE_NEXT_CELL2      Inqncl2
#define INQUIRE_NEXT_CELL3      Inqncl3
#define INQUIRE_NEXT_DEF_GROUP  Inqngr
#define INQUIRE_NEXT_DAT_GROUP  Inqnxt
#define INQUIRE_NEXT_INTEGER    Inqnia
#define INQUIRE_NEXT_REAL       Inqnra
#define INQUIRE_NEXT_STRING     Inqnsa
#define NEFIS_ERROR             Neferr
#define OPEN_DAT_FILE           Opndat
#define OPEN_DEF_FILE           Opndef
#define PUT_ELEMENT             Putelt
#define PUT_ELEMENT_STRING      Putels
#define PUT_INT_ATTRIBUTE       Putiat
#define PUT_REAL_ATTRIBUTE      Putrat
#define PUT_STRING_ATTRIBUTE    Putsat
#define RESET_FILE_VERSION      Resnfv

/*==========================================================================*/
/* Start of C-functions                                                     */
/*==========================================================================*/
static BVoid C_Copy_text( BText name1 ,
                          BText name2 ,
                          BInt4 max_length)
{
  BInt4 i       ;
  BInt4 max_copy;
  max_copy = min ( (BInt4) strlen(name2), max_length);
  strncpy(name1, name2, max_copy);
  for ( i=max_copy; i<max_length; i++ )
  {
    name1[i]= ' ';
  }
  name1[max_length]='\0';
  return;
}

/*==========================================================================*/
/* Start of NEFIS function calls                                            */
/*==========================================================================*/
/*
 * Close data file, and fluh hash buffer
 * Input : * fd                 Nefis file set descriptor
 * Output: * fd                 Nefis file set descriptor set to -1
 * Return:    0                 No error occured
 *            !=0               Error occured
 */

DLLEXPORT BInt4 CLOSE_FLUSH_DAT_FILE ( BInt4  * fd )
{
  nefis_errno = 0;

  nefis_errno = close_nefis_files( fd );

  return nefis_errno;
}
/*==========================================================================*/
/*
 * Close definition file, and fluh hash buffer
 * Input : * fd                 Nefis file set descriptor
 * Output: * fd                 Nefis file set descriptor set to -1
 * Return:    0                 No error occured
 *            !=0               Error occured
 */

DLLEXPORT BInt4 CLOSE_FLUSH_DEF_FILE ( BInt4  * fd )
{
  nefis_errno = 0;

  nefis_errno = close_nefis_files( fd );

  return nefis_errno;
}
/*==========================================================================*/
/*
 * Close data file, and do not flush hash buffer
 * Input : * fd                 Nefis file set descriptor
 * Output: * fd                 Nefis file set descriptor set to -1
 * Return:    0                 No error occured
 *            !=0               Error occured
 */

DLLEXPORT BInt4 CLOSE_DAT_FILE ( BInt4  * fd )
{
  BInt4 tmp_flush;

  nefis_errno = 0;
  nefis_flush = FALSE;
  tmp_flush   = nefis_flush;

  nefis_errno = close_nefis_files( fd );

  nefis_flush = tmp_flush;

  return nefis_errno;
}
/*==========================================================================*/
/*
 * Close definition file, and do not flush hash buffer
 * Input : * fd                 Nefis file set descriptor
 * Output: * fd                 Nefis file set descriptor set to -1
 * Return:    0                 No error occured
 *            !=0               Error occured
 */

DLLEXPORT BInt4 CLOSE_DEF_FILE ( BInt4  * fd )
{
  BInt4 tmp_flush;

  nefis_errno = 0;
  nefis_flush = FALSE;
  tmp_flush   = nefis_flush;

  nefis_errno = close_nefis_files( fd );

  nefis_flush = tmp_flush;

  return nefis_errno;
}
/*==========================================================================*/
/*
 * Close definition and data file, and fluh hash buffer
 * Input : * fd                 Nefis file set descriptor
 * Output: * fd                 Nefis file set descriptor set to -1
 * Return:    0                 No error occured
 *            !=0               Error occured
 */

DLLEXPORT BInt4 CLOSE_NEFIS ( BInt4  * fd )
{
  nefis_errno = 0;

  nefis_errno = close_nefis_files( fd );

  return nefis_errno;
}
/*==========================================================================*/
/*
 * Close definition and data file, and fluh hash buffer
 * Input : * fd                 Nefis file set descriptor
 * Output: * fd                 Nefis file set descriptor set to -1
 * Return:    0                 No error occured
 *            !=0               Error occured
 */

DLLEXPORT BInt4 CLOSE_ALL_NEFIS ( void )
{
  nefis_errno = 0;

  nefis_errno = OC_close_all_nefis_files();

  return nefis_errno;
}
/*==========================================================================*/
/*
 * Define cel on definition file
 * Input : * fd                 Nefis file set descriptor
 *           cl_name            Cel name
 *           cl_num_dim         Number of dimensions (1 <= N <= 5)
 *           el_names           Array containing element names witin cel
 * Output:   -
 * Return:   0                  No error occured
 *           !=0                Error occured
 */
DLLEXPORT BInt4 DEFINE_CELL ( BInt4 * fd             ,
                    BText   cl_name        ,
                    BInt4   cl_num_dim     ,
                    BChar   el_names [][MAX_NAME+1] )
{
    BChar   cel_name [ MAX_NAME + 1           ];
    BInt4   cel_num_dim;
    BInt4   i        ;
    BInt4   max_copy ;

    char * elm_names = (char *) malloc(cl_num_dim * (MAX_NAME+1));

    nefis_errno = 0;

    C_Copy_text( cel_name    , cl_name    , MAX_NAME);

    for ( i=0; i<((MAX_NAME+1)*cl_num_dim); i++ )
    {
        elm_names[i]=' ';
    }
    for ( i=0; i<cl_num_dim; i++ )
    {
        max_copy = min( MAX_NAME, (BInt4) strlen(el_names[i]) );
        strncpy( &elm_names[i*(MAX_NAME+1)],  el_names [i], max_copy );
    }
    for ( i=0; i<cl_num_dim; i++ )
    {
        elm_names[i*(MAX_NAME+1)+MAX_NAME] = '\0';
    }

    cel_num_dim = cl_num_dim;

    nefis_errno = Define_cel ( *fd, cel_name, cel_num_dim, elm_names);

    return nefis_errno;
}
/*==========================================================================*/
/*
 * Define cel on definition file
 * Input : * fd                 Nefis file set descriptor
 *           cl_name            Cel name
 *           cl_num_dim         Number of dimensions (1 <= N <= 5)
 *           el_names           Array containing element names witin cel
 * Output:   -
 * Return:   0                  No error occured
 *           !=0                Error occured
 */
DLLEXPORT BInt4 DEFINE_CELL2 ( BInt4 * fd             ,
                    BText   cl_name        ,
                    BInt4   cl_num_dim     ,
                    BText * el_names       )
{
    BChar   cel_name [ MAX_NAME + 1           ];
    BInt4   cel_num_dim;
    BInt4   i        ;
    BInt4   max_copy ;

    char * elm_names = (char *) malloc(cl_num_dim * (MAX_NAME+1));

    nefis_errno = 0;

    C_Copy_text( cel_name    , cl_name    , MAX_NAME);

    for ( i=0; i<((MAX_NAME+1)*cl_num_dim); i++ )
    {
        elm_names[i]=' ';
    }
    for ( i=0; i<cl_num_dim; i++ )
    {
        max_copy = min( MAX_NAME, (BInt4) strlen(el_names[i]) );
        strncpy( &elm_names[i*(MAX_NAME+1)],  el_names [i], max_copy );
    }
    for ( i=0; i<cl_num_dim; i++ )
    {
        elm_names[i*(MAX_NAME+1)+MAX_NAME] = '\0';
    }

    cel_num_dim = cl_num_dim;

    nefis_errno = Define_cel ( *fd, cel_name, cel_num_dim, elm_names);

    return nefis_errno;
}
/*==========================================================================*/
/*
 * Define cel on definition file
 * Input : * fd                 Nefis file set descriptor
 *           cl_name            Cel name
 *           cl_num_dim         Number of dimensions (1 <= N <= 5)
 *           el_names           Array containing element names witin cel
 * Output:   -
 * Return:   0                  No error occured
 *           !=0                Error occured
 */
DLLEXPORT BInt4 DEFINE_CELL3 ( BInt4 * fd             ,
                    BText   cl_name        ,
                    BInt4   cl_num_dim     ,
                    BText   el_names       )
{
    BChar   cel_name [ MAX_NAME + 1           ];
    BInt4   cel_num_dim;
    BInt4   i        ;
    BInt4   max_copy ;

    char * elm_names = (char *) malloc(cl_num_dim * (MAX_NAME+1));

    nefis_errno = 0;

    C_Copy_text( cel_name    , cl_name    , MAX_NAME);

    for ( i=0; i<((MAX_NAME+1)*cl_num_dim); i++ )
    {
        elm_names[i]=' ';
    }
    for ( i=0; i<cl_num_dim; i++ )
    {
        max_copy = min( MAX_NAME, (BInt4) strlen(el_names+(MAX_NAME+1)*i) );
        strncpy( &elm_names[i*(MAX_NAME+1)],  el_names+(MAX_NAME+1)*i, max_copy );
    }
    for ( i=0; i<cl_num_dim; i++ )
    {
        elm_names[i*(MAX_NAME+1)+MAX_NAME] = '\0';
    }

    cel_num_dim = cl_num_dim;

    nefis_errno = Define_cel ( *fd, cel_name, cel_num_dim, elm_names);

    return nefis_errno;
}
/*==========================================================================*/
/*
 * Create space for data on data file
 * Input:  fd                  Nefis file set descriptor
 *         gr_name             Group name on data file
 *         gr_defined          Group name as defined on definition file
 * Output: -
 * func  : 0                   No error occured
 *         !=0                 Error occured
 */

DLLEXPORT BInt4 DEFINE_DATA ( BInt4 * fd              ,
                    BText   gr_name         ,
                    BText   gr_defined      )
{
  BChar   grp_name   [MAX_NAME + 1];
  BChar   grp_defined[MAX_NAME + 1];

  nefis_errno = 0;

  C_Copy_text( grp_name    , gr_name    , MAX_NAME);
  C_Copy_text( grp_defined , gr_defined , MAX_NAME);

  nefis_errno = Define_data ( *fd, grp_name, grp_defined);

  return nefis_errno;
}
/*==========================================================================*/
/*
 * Define element on definition file
 * Input:  fd                  Nefis file set descriptor
 *         el_name             Element name
 *         el_type             Element type
 *         el_single_byte      Number of bytes occupied by single element
 *         el_quantity         Element quantity
 *         el_unity            Element unity
 *         el_desc             Element description
 *         el_num_dim          Number of dimensions (1 <= N <= 5)
 *         el_dimens           Array containing array sizes
 * Output: -
 * func  : 0                   No error occured
 *         != 0                Error occured
 */

DLLEXPORT BInt4 DEFINE_ELEMENT ( BInt4 * fd             ,
                       BText   el_name        ,
                       BText   el_type        ,
                       BInt4   el_single_byte ,
                       BText   el_quantity    ,
                       BText   el_unity       ,
                       BText   el_desc        ,
                       BInt4   el_num_dim     ,
                       BInt4 * el_dimens      )
{
  BInt4   elm_num_dim  ;
  BChar   elm_name    [MAX_NAME + 1];
  BChar   elm_type    [MAX_TYPE + 1];
  BChar   elm_quantity[MAX_NAME + 1];
  BChar   elm_unity   [MAX_NAME + 1];
  BChar   elm_desc    [MAX_DESC + 1];
  BInt4   elm_dimens  [MAX_DIM     ];
  BInt4   elm_single_byte           ;
  BInt4   i            ;

  nefis_errno = 0;

  C_Copy_text( elm_name    , el_name    , MAX_NAME);
  C_Copy_text( elm_type    , el_type    , MAX_TYPE);
  C_Copy_text( elm_quantity, el_quantity, MAX_NAME);
  C_Copy_text( elm_unity   , el_unity   , MAX_NAME);
  C_Copy_text( elm_desc    , el_desc    , MAX_DESC);

  elm_num_dim = el_num_dim;
  for ( i=0; i<elm_num_dim; i++)
  {
    elm_dimens[i] = el_dimens[i];
  }
  for ( i=elm_num_dim; i<MAX_DIM; i++)
  {
    elm_dimens[i] = 1;
  }

  if ( elm_num_dim == 0 )
  {
    elm_num_dim   = 1;
  }
  elm_single_byte = el_single_byte;

  nefis_errno =
    Define_element ( *fd, elm_name   , elm_type  , elm_quantity   ,
                          elm_unity  , elm_desc  , elm_single_byte,
                          elm_num_dim, elm_dimens);

  return nefis_errno;
}
/*==========================================================================*/
/*
 * Define group on definition file
 * Input : * fd                 Nefis file set descriptor
 *           gr_name            Group name on data file
 *           cl_name            Cel name
 *           gr_num_dim         Number of dimensions of group
 *           gr_dimens          Array with the dimensions
 *           gr_order           Array with the group order on the file
 * Output:    -
 * Return:    0                 No error occured
 *            !=0               Error occured
 */

DLLEXPORT BInt4 DEFINE_GROUP ( BInt4 * fd             ,
                     BText   gr_name        ,
                     BText   cl_name        ,
                     BInt4   gr_num_dim     ,
                     BInt4 * gr_dimens      ,
                     BInt4 * gr_order       )
{
  BChar   cel_name  [MAX_NAME+1];
  BChar   grp_name  [MAX_NAME+1];
  BInt4   grp_dimens[MAX_DIM];
  BInt4   grp_num_dim;
  BInt4   grp_order [MAX_DIM];
  BInt4   i          ;

  nefis_errno = 0;

  C_Copy_text( cel_name    , cl_name    , MAX_NAME);
  C_Copy_text( grp_name    , gr_name    , MAX_NAME);

  grp_num_dim = gr_num_dim;
  for ( i=0; i<grp_num_dim; i++ )
  {
    grp_dimens[i] = gr_dimens[i];
    grp_order [i] = gr_order [i];
  }
  for ( i=grp_num_dim; i<MAX_DIM; i++ )
  {
    grp_dimens[i] = 1;
    grp_order [i] = 1;
  }

  if ( grp_num_dim == 0 )
  {
    grp_num_dim   = 1;
  }

  nefis_errno = Define_group (*fd         , grp_name  , cel_name  ,
                               grp_num_dim, grp_dimens, grp_order );

  return nefis_errno;
}
/*==========================================================================*/
/*
 * Open and create data and defintion file
 * Input : * fd                 Nefis file set descriptor
 *           dat_file           Name of the data file
 *           def_file           Name of the definiton file
 *           coding             Coding: 'N' Neutral, others binair
 *           ac_type            Access type of the NEFIS file set
 * Output:    -
 * Return:    0                 No error occured
 *            !=0               Error occured
 */

DLLEXPORT BInt4 CREATE_NEFIS(BInt4 * fd         ,
                   BText   dat_file   ,
                   BText   def_file   ,
                   BChar   coding     ,
                   BChar   ac_type    )
{
  nefis_errno = 0;

  if ( (BInt4) strlen(dat_file) > (BInt4)(FILENAME_MAX-1) )
  {
    nefis_errcnt += 1;
    nefis_errno   = 1002;
    sprintf( error_text,
      "Crenef: Data filename too long ( length < %d )",FILENAME_MAX-1);
  }
  if ( (BInt4) strlen(def_file) > (BInt4)(FILENAME_MAX-1) )
  {
    nefis_errcnt += 1;
    nefis_errno   = 1003;
    sprintf( error_text,
      "Crenef: Definition filename too long ( length < %d )",FILENAME_MAX-1);
  }

  if ( nefis_errno == 0 )
  {
    nefis_errno =
      create_nefis_files( fd, dat_file, def_file, coding, ac_type);
  }

  return nefis_errno;
}
/*==========================================================================*/
/*
 * Flush hash buffer to data file
 * Input : * fd                 Nefis file set descriptor
 * Output:    -
 * Return:    0                 No error occured
 *            !=0               Error occured
 */

DLLEXPORT BInt4 FLUSH_DAT_FILE ( BInt4 * fd )
{
  BInt4 dat_fds;

  nefis_errno = 0;

  if ( nefis[*fd].one_file == TRUE )
  {
    dat_fds = nefis[*fd].daf_fds;
  }
  else
  {
    dat_fds = nefis[*fd].dat_fds;
  }

  nefis_errno = GP_flush_hash( dat_fds, *fd);

  return nefis_errno;
}
/*==========================================================================*/
/*
 * Flush hash buffer to definition file
 * Input : * fd                 Nefis file set descriptor
 * Output:    -
 * Return:    0                 No error occured
 *            !=0               Error occured
 */

DLLEXPORT BInt4 FLUSH_DEF_FILE ( BInt4  * fd )
{
  BInt4 def_fds;

  nefis_errno = 0;

  if ( nefis[*fd].one_file == TRUE )
  {
    def_fds = nefis[*fd].daf_fds;
  }
  else
  {
    def_fds = nefis[*fd].def_fds;
  }

  nefis_errno = GP_flush_hash( def_fds, *fd);

  return nefis_errno;
}
/*==========================================================================*/
/*
 * Get header from data file
 * Input : * fd                 Nefis file set descriptor
 * Output:   header             Contains the header of the data file
 * Return:    0                 No error occured
 *            !=0               Error occured
 */

DLLEXPORT BInt4 GET_DAT_HEADER( BInt4 * fd           ,
                      BText   header       )
{
  BUInt8 n_read = 0;

  nefis_errno = 0;

  if ( ((BInt4) strlen(header)) < LHDRDT )
  {
    nefis_errcnt += 1;
    nefis_errno   = 1004;
    sprintf( error_text,
      "Gethdt: Supplied character string too small for header: %ld<%ld\n",
      strlen(header), LHDRDT);
    return nefis_errno;
  }

  if ( nefis_errno == 0 )
  {
    n_read = GP_read_file(nefis[*fd].dat_fds, header, 0, LHDRDT);

    if ( n_read == -1 )
    {
      nefis_errcnt += 1;
      nefis_errno   = 1005;
      sprintf( error_text,
        "Gethdt: Unable to read data file header (file write only?)");
    }
    if ( n_read > LHDRDT )
    {
      nefis_errcnt += 1;
      nefis_errno   = 1006;
      sprintf( error_text,
        "Gethdt: During reading of data file header");
    }
  }

  return nefis_errno;
}
/*==========================================================================*/
/*
 * Get header from definition file
 * Input : * fd                 Nefis file set descriptor
 * Output:   header             Contains the header of the definition file
 * Return:    0                 No error occured
 *            !=0               Error occured
 */

DLLEXPORT BInt4 GET_DEF_HEADER( BInt4 * fd           ,
                      BText   header       )
{
  BUInt8 n_read = 0;

  nefis_errno = 0;

  if ( ((BInt4) strlen(header)) < LHDRDT )
  {
    nefis_errcnt += 1;
    nefis_errno   = 1007;
    sprintf( error_text,
        "Gethdf: Supplied character string too small for header %ld<%ld\n",
        strlen(header), LHDRDF);
      return nefis_errno;}

  if ( nefis_errno == 0 )
  {
    n_read = GP_read_file(nefis[*fd].def_fds, header, 0, LHDRDF);
    if ( n_read == -1 )
    {
      nefis_errcnt += 1;
      nefis_errno   = 1008;
      sprintf( error_text,
        "Gethdf: Unable to read definition file header (file write only?)");
    }
    if ( n_read > LHDRDF )
    {
      nefis_errcnt += 1;
      nefis_errno   = 1009;
      sprintf( error_text,
        "Gethdf: During reading of definition file header");
    }
  }

  return nefis_errno;
}
/*==========================================================================*/
/*
 * Retrieve character and alpha numeric element from data file
 * Input:  fd                  Nefis file set descriptor
 * Output: -
 * Return: 0                   No error occured
 *         !=0                 Error occured
 */

DLLEXPORT BInt4 GET_ELEMENT    ( BInt4 * fd               ,
                       BText   gr_name          ,
                       BText   el_name          ,
                       BInt4 * user_index       ,
                       BInt4 * user_order       ,
                       BInt4 * buffer_length    ,
                       BData   buffer           )
{
  BChar   elm_name[MAX_NAME + 1];
  BChar   grp_name[MAX_NAME + 1];
  BInt4   i           ;
  BInt4   j           ;
  BInt4   usr_index[MAX_DIM * DEFINE_STEPS];
  BInt4   usr_order[MAX_DIM    ];

  nefis_errno = 0;

  C_Copy_text( elm_name    , el_name    , MAX_NAME);
  C_Copy_text( grp_name    , gr_name    , MAX_NAME);

  for ( i=0; i<MAX_DIM; i++ )
  {
    usr_order[i] =user_order[i];
  }

  for ( i=0; i<MAX_DIM     ; i++ )
  {
    for ( j=0; j<DEFINE_STEPS; j++ )
    {
      usr_index[A2D(i,j)] = user_index[A2D(i,j)];
    }
  }

  nefis_errno =
    Get_element ( *fd, grp_name   , elm_name     , usr_index,
                       usr_order  ,*buffer_length, buffer   );

  return nefis_errno;
}
/*==========================================================================*/
/*
 * Retrieve character element from data file
 * Input:  fd                  Nefis file set descriptor
 * Output: -
 * Return: 0                   No error occured
 *         !=0                 Error occured
 */

DLLEXPORT BInt4 GET_ELEMENT_STRING ( BInt4 * fd               ,
                           BText   gr_name          ,
                           BText   el_name          ,
                           BInt4 * user_index       ,
                           BInt4 * user_order       ,
                           BInt4 * buffer_length    ,
                           BData   buffer           )
{
  nefis_errno = 0;

  nefis_errno = GET_ELEMENT  ( fd               ,
                               gr_name          ,
                               el_name          ,
                               user_index       ,
                               user_order       ,
                               buffer_length    ,
                               buffer           );

  return nefis_errno;
}
/*==========================================================================*/
/*
 * Get integer attribute on data file
 * Input : * fd                 Nefis file set descriptor
 *           gr_name            Group name on data file
 *           at_name            Attribute name
 * Output:   at_value           Attribute value
 * Return:    0                 No error occured
 *            !=0               Error occured
 */
DLLEXPORT BInt4 GET_INT_ATTRIBUTE    ( BInt4 * fd             ,
                             BText   gr_name        ,
                             BText   at_name        ,
                             BInt4 * at_value       )
{
  BChar   att_name[MAX_NAME+1];
  BChar   att_type[MAX_TYPE+1];
  BChar   grp_name[MAX_NAME+1];
  BInt4   next  = 0  ;

  nefis_errno = 0;

  C_Copy_text( grp_name    , gr_name    , MAX_NAME);
  C_Copy_text( att_name    , at_name    , MAX_NAME);

  strcpy(att_type, "INTEGER");

  nefis_errno = GP_get_attribute(*fd      , next    , grp_name,
                                  att_name, at_value, att_type);

  return nefis_errno;
}
/*==========================================================================*/
/*
 * Get integer attribute on data file
 * Input : * fd                 Nefis file set descriptor
 *           gr_name            Group name on data file
 *           at_name            Attribute name
 * Output:   at_value           Attribute value
 * Return:    0                 No error occured
 *            !=0               Error occured
 */
DLLEXPORT BInt4 GET_REAL_ATTRIBUTE   ( BInt4 * fd            ,
                             BText   gr_name       ,
                             BText   at_name       ,
                             BRea4 * at_value      )
{
  BChar   att_name[MAX_NAME+1];
  BChar   att_type[MAX_TYPE+1];
  BChar   grp_name[MAX_NAME+1];
  BInt4   next  = 0  ;

  nefis_errno = 0;

  C_Copy_text( grp_name    , gr_name    , MAX_NAME);
  C_Copy_text( att_name    , at_name    , MAX_NAME);

  strcpy(att_type, "REAL");

  nefis_errno = GP_get_attribute(*fd      , next    , grp_name,
                                  att_name, at_value, att_type);

  return nefis_errno;
}
/*==========================================================================*/
/*
 * Get integer attribute on data file
 * Input : * fd                 Nefis file set descriptor
 *           gr_name            Group name on data file
 *           at_name            Attribute name
 * Output:   at_value           Attribute value
 * Return:    0                 No error occured
 *            !=0               Error occured
 */
DLLEXPORT BInt4 GET_STRING_ATTRIBUTE ( BInt4 * fd            ,
                             BText   gr_name       ,
                             BText   at_name       ,
                             BText   at_value      )
{
  BChar   att_name[MAX_NAME+1];
  BChar   att_type[MAX_TYPE+1];
  BChar   grp_name[MAX_NAME+1];
  BInt4   next  = 0  ;

  nefis_errno = 0;

  C_Copy_text( grp_name    , gr_name    , MAX_NAME);
  C_Copy_text( att_name    , at_name    , MAX_NAME);

  strcpy(att_type, "CHARACTE");

  nefis_errno = GP_get_attribute(*fd      , next    , grp_name,
                                  att_name, at_value, att_type);

  return nefis_errno;
}
/*==========================================================================*/
/*
 * Read cel definition from defintion file
 * Input : * fd                 Nefis file set descriptor
 *           cl_name            Group name on data file
 *           cl_num_dim         Number of dimensions
 * Output:   el_names
 * Return:    0                 No error occured
 *            !=0               Error occured
 */

DLLEXPORT BInt4 INQUIRE_CELL        ( BInt4 * fd             ,
                            BText   cl_name        ,
                            BInt4 * cl_num_dim     ,
                            BChar   el_names [][MAX_NAME+1] )
{
	BUInt8  cel_num_bytes = 0;
	BUInt4  cel_num_dim   = 0;
	BChar   cel_name [  MAX_NAME+1  ];
    BInt4   i        ;
    BText   elm_names;

	nefis_errno = 0;
    elm_names = (BText) malloc((MAX_NAME+1) * *cl_num_dim);

    C_Copy_text( cel_name    , cl_name    , MAX_NAME);

    for ( i=0; i<(MAX_NAME+1)* *cl_num_dim; i++ )
    {
        elm_names   [i]= 'j';
    }
    for ( i=0; i<*cl_num_dim; i++ )
    {
        elm_names[i*(MAX_NAME+1)+MAX_NAME] = '\0';
    }

	nefis_errno = GP_inquire_cel(*fd       , cel_name     ,&cel_num_dim,
								&elm_names,&cel_num_bytes);

    if ( nefis_errno == 0 )
    {
        if ( cel_num_dim > (BUInt4) *cl_num_dim )
        {
            nefis_errcnt += 1;
            nefis_errno   = 1011;
            sprintf(error_text,
            "Inqcel: User supplied array too small to contain Cell properties: \'%s\' %ld>%ld \n",
                        cel_name, cel_num_dim, *cl_num_dim);
            return nefis_errno;
        }
        for ( i=0; i<cel_num_dim; i++ )
        {
            strncpy( el_names[i], &elm_names[i*(MAX_NAME+1)], MAX_NAME );
            el_names[i][MAX_NAME] = '\0';
        }
        *cl_num_dim = cel_num_dim;
    }

  return nefis_errno;
}
/*==========================================================================*/
/*
 * Read cel definition from defintion file
 * Input : * fd                 Nefis file set descriptor
 *           cl_name            Group name on data file
 *           cl_num_dim         Number of dimensions
 * Output:   el_names
 * Return:    0                 No error occured
 *            !=0               Error occured
 */

DLLEXPORT BInt4 INQUIRE_CELL2( BInt4 * fd             ,
                            BText    cl_name        ,
                            BInt4 *  cl_num_dim     ,
                            BText ** el_names       )
{
    BUInt8  cel_num_bytes = 0;
    BUInt4  cel_num_dim   = 0;
    BChar   cel_name [  MAX_NAME+1  ];
    BInt4   i        ;
    BText   elm_names;

    nefis_errno = 0;
    elm_names = (BText) malloc((MAX_NAME+1) * *cl_num_dim);

    C_Copy_text( cel_name    , cl_name    , MAX_NAME);

    for ( i=0; i<(MAX_NAME+1)* *cl_num_dim; i++ )
    {
        elm_names   [i]= 'j';
    }
    for ( i=0; i<*cl_num_dim; i++ )
    {
        elm_names[i*(MAX_NAME+1)+MAX_NAME] = '\0';
    }

    nefis_errno = GP_inquire_cel(*fd       , cel_name     ,&cel_num_dim,
                               &elm_names,&cel_num_bytes);

    if ( nefis_errno == 0 )
    {
        if ( cel_num_dim > (BUInt4) *cl_num_dim )
        {
            nefis_errcnt += 1;
            nefis_errno   = 1011;
            sprintf(error_text,
            "Inqcel: User supplied array too small to contain Cell properties: \'%s\' %ld>%ld \n",
                        cel_name, cel_num_dim, *cl_num_dim);
            return nefis_errno;
        }

        for ( i=0; i<cel_num_dim; i++ )
        {
            strncpy( *(el_names[i]), &elm_names[i*(MAX_NAME+1)], MAX_NAME );
            el_names[i][MAX_NAME] = '\0';
        }
        *cl_num_dim = cel_num_dim;
    }

  return nefis_errno;
}
/*==========================================================================*/
/*
 * Read cel definition from defintion file
 * Input : * fd                 Nefis file set descriptor
 *           cl_name            Group name on data file
 *           cl_num_dim         Number of dimensions
 * Output:   el_names
 * Return:    0                 No error occured
 *            !=0               Error occured
 */

DLLEXPORT BInt4 INQUIRE_CELL3( BInt4 * fd             ,
                            BText    cl_name        ,
                            BInt4 *  cl_num_dim     ,
                            BText    el_names       )
{
    BUInt8  cel_num_bytes = 0;
    BUInt4  cel_num_dim   = 0;
    BChar   cel_name [  MAX_NAME+1  ];
    BInt4   i        ;
    BText   elm_names;

    nefis_errno = 0;
    elm_names = (BText) malloc((MAX_NAME+1) * *cl_num_dim);

    C_Copy_text( cel_name    , cl_name    , MAX_NAME);

    for ( i=0; i<(MAX_NAME+1)* *cl_num_dim; i++ )
    {
        elm_names   [i]= 'j';
    }
    for ( i=0; i<*cl_num_dim; i++ )
    {
        elm_names[i*(MAX_NAME+1)+MAX_NAME] = '\0';
    }

    nefis_errno = GP_inquire_cel(*fd       , cel_name     ,&cel_num_dim,
                               &elm_names,&cel_num_bytes);

    if ( nefis_errno == 0 )
    {
        if ( cel_num_dim > (BUInt4) *cl_num_dim )
        {
            nefis_errcnt += 1;
            nefis_errno   = 1011;
            sprintf(error_text,
            "Inqcel: User supplied array too small to contain Cell properties: \'%s\' %ld>%ld \n",
                        cel_name, cel_num_dim, *cl_num_dim);
            return nefis_errno;
        }

        for ( i=0; i<cel_num_dim; i++ )
        {
            strncpy( el_names+i*(MAX_NAME+1), &elm_names[i*(MAX_NAME+1)], MAX_NAME );
            (el_names + i*(MAX_NAME + 1))[MAX_NAME] = '\0';
        }
        *cl_num_dim = cel_num_dim;
    }

  return nefis_errno;
}
/*==========================================================================*/
/*
 * Read group name from data file
 * Input : * fd                 Nefis file set descriptor
 *           gr_name            Group name on data file
 * Output:   gr_defined         Group name on definition file
 * Return:    0                 No error occured
 *            !=0               Error occured
 */

DLLEXPORT BInt4 INQUIRE_DATA_GROUP  ( BInt4 * fd               ,
                            BText   gr_name          ,
                            BText   gr_defined       )
{
  BUInt8  grp_pointer = NIL;
  BChar   grp_name   [MAX_NAME+1];
  BChar   grp_defined[MAX_NAME+1];

/* TODO: Check on memory to store gr_defined    */

  nefis_errno = 0;

  C_Copy_text( grp_name    , gr_name    , MAX_NAME);

  grp_defined [MAX_NAME]='\0';

  nefis_errno = GP_inquire_dat(*fd      , &grp_pointer,
                                grp_name,  grp_defined);

  if ( nefis_errno == 0 )
  {
    strncpy(gr_defined , grp_defined, MAX_NAME);
    gr_defined [MAX_NAME]='\0';
  }

  return nefis_errno;
}
/*==========================================================================*/
/*
 * Read element definition from defintion file
 * Input:  fd                  NEFIS file set descriptor
 *         el_name             Element name
 * Output: el_type             Element type
 *         el_single_bytes     Number of bytes occupied by a single element
 *         el_quantity         Element quantity
 *         el_unity            Element unity
 *         el_descy            Element description
 *         el_num_dim          Number of relevant dimensions (<=5)
 *         el_dimens           Array containig five dimensions
 * Return: 0                   No error occured
 *         !=0                 Error occured
 */

DLLEXPORT BInt4 INQUIRE_ELEMENT     ( BInt4 * fd             ,
                            BText   el_name        ,
                            BText   el_type        ,
                            BInt4 * el_single_bytes,
                            BText   el_quantity    ,
                            BText   el_unity       ,
                            BText   el_desc        ,
                            BInt4 * el_num_dim     ,
                            BInt4 * el_dimens      )
{
  BUInt8   elm_num_bytes = 0;
  BChar    elm_name        [ MAX_NAME+1 ];
  BChar    elm_type        [ MAX_TYPE+1 ];
  BUInt4   elm_single_bytes = 0          ;
  BChar    elm_quantity    [ MAX_NAME+1 ];
  BChar    elm_unity       [ MAX_NAME+1 ];
  BChar    elm_desc        [ MAX_DESC+1 ];
  BUInt4   elm_num_dim                   ;
  BUInt4   elm_dimens      [ MAX_DIM    ];
  BUInt4   i                             ;

  nefis_errno = 0;

  C_Copy_text( elm_name    , el_name    , MAX_NAME);

  nefis_errno = GP_inquire_elm(*fd           , elm_name   , elm_type        ,
                                elm_quantity , elm_unity  , elm_desc        ,
                               &elm_num_dim  , elm_dimens ,&elm_single_bytes,
                               &elm_num_bytes);

  if ( nefis_errno == 0 )
  {
    strncpy( el_type    , elm_type    , MAX_TYPE);
    el_type    [MAX_TYPE]='\0';

    strncpy( el_quantity, elm_quantity, MAX_NAME);
    el_quantity[MAX_NAME]='\0';

    strncpy( el_unity   , elm_unity   , MAX_NAME);
    el_unity   [MAX_NAME]='\0';

    strncpy( el_desc    , elm_desc    , MAX_DESC);
    el_desc    [MAX_DESC]='\0';

    if ( (BUInt4) (*el_num_dim) < elm_num_dim)
    {
      nefis_errcnt += 1;
      nefis_errno   = 1012;
      sprintf(error_text,
        "Inqelm: User supplied array too small to contain Element properties: \'%s\' %ld>%ld \n",
         elm_name, elm_num_dim, *el_num_dim);
      return nefis_errno;
    }

    *el_single_bytes = elm_single_bytes;
    *el_num_dim      = elm_num_dim     ;
    for ( i=0; i<elm_num_dim; i++ )
    {
      el_dimens[i] = elm_dimens[i];
    }
  }

  return nefis_errno;
}
/*==========================================================================*/
/*
 * Get first group on data file
 * input:      fd        : NEFIS file set descriptor
 *
 * output:   * gr_name   : first/next groupname on data file
 *             gr_defined: corresponding groupname (defined)
 *                         on definition file
 *
 * return:   0    No error occured
 *           !=0  Error occured
 */
DLLEXPORT BInt4 INQUIRE_FIRST_DAT_GROUP  ( BInt4 * fd            ,
                                 BText   gr_name       ,
                                 BText   gr_defined    )
{
  BChar   grp_name   [MAX_NAME+1];
  BChar   grp_defined[MAX_NAME+1];
  BInt4   i      ;

  nefis_errno = 0;

  for ( i=0; i<MAX_NAME; i++ )
  {
    grp_name    [i]= ' ';
  }
  grp_name    [MAX_NAME]='\0';

  for ( i=0; i<MAX_NAME; i++ )
  {
    grp_defined [i]= ' ';
  }
  grp_defined [MAX_NAME]='\0';

  nefis_errno = GP_get_next_grp (*fd      , 0, grp_name, grp_defined);

/* TODO: check on length gr_name and grp_defined */

  if ( nefis_errno == 0 )
  {
    strncpy(gr_name   , grp_name   , MAX_NAME);
    strncpy(gr_defined, grp_defined, MAX_NAME);
    gr_name   [MAX_NAME] = '\0';
    gr_defined[MAX_NAME] = '\0';
  }

  return nefis_errno;
}
/*==========================================================================*/
/*
 * Get first element on definition file
 * input:      fd        : NEFIS file set descriptor
 *
 * output:
 *
 *
 *
 * return:   0    No error occured
 *           !=0  Error occured
 */
/*==========================================================================*/
DLLEXPORT BInt4 INQUIRE_FIRST_ELEMENT( BInt4 * fd              ,/* I */
                             BText   el_name        ,/* O */
                             BText   el_type        ,/* O */
                             BText   el_quantity    ,/* O */
                             BText   el_unity       ,/* O */
                             BText   el_desc        ,/* O */
                             BInt4 * el_single_byte ,/* O */
                             BInt4 * el_num_byte    ,/* O */
                             BInt4 * el_num_dim     ,/* O */
                             BInt4 * el_dimens      )/* O */
{
  BUInt4  elm_num_dim = 0           ;
  BChar   elm_name    [MAX_NAME + 1];
  BChar   elm_type    [MAX_TYPE + 1];
  BChar   elm_quantity[MAX_NAME + 1];
  BChar   elm_unity   [MAX_NAME + 1];
  BChar   elm_desc    [MAX_DESC + 1];
  BUInt4  elm_dimens  [MAX_DIM     ];
  BUInt4  elm_single_byte = 0       ;
  BUInt8  elm_num_byte    = 0       ;
  BInt4   i                         ;

  nefis_errno = 0;

/*
 * Initialize variables
 */
  for ( i=0; i<MAX_NAME; i++ )
  {
    elm_name    [i]= ' ';
    elm_quantity[i]= ' ';
    elm_unity   [i]= ' ';
  }
  elm_name    [MAX_NAME]='\0';
  elm_quantity[MAX_NAME]='\0';
  elm_unity   [MAX_NAME]='\0';

  for ( i=0; i<MAX_TYPE; i++ )
  {
    elm_type    [i]= ' ';
  }
  elm_type    [MAX_TYPE]='\0';

  for ( i=0; i<MAX_DESC; i++ )
  {
    elm_desc    [i]= ' ';
  }
  elm_desc    [MAX_DESC]='\0';

  for ( i=0; i<MAX_DIM ; i++ )
  {
    el_dimens[i] = 1;
  }

  nefis_errno = GP_get_next_elm (*fd              , 0              ,
                                  elm_name        , elm_type       ,
                                  elm_quantity    , elm_unity      ,
                                  elm_desc        ,&elm_num_dim    ,
                                  elm_dimens      ,&elm_single_byte,
                                 &elm_num_byte    );

  if ( nefis_errno == 0 )
  {
    strncpy(el_name    , elm_name    , MAX_NAME);
    strncpy(el_quantity, elm_quantity, MAX_NAME);
    strncpy(el_unity   , elm_unity   , MAX_NAME);
    strncpy(el_desc    , elm_desc    , MAX_DESC);
    strncpy(el_type    , elm_type    , MAX_TYPE);
    el_name    [MAX_NAME] = '\0';
    el_quantity[MAX_NAME] = '\0';
    el_unity   [MAX_NAME] = '\0';
    el_desc    [MAX_DESC] = '\0';
    el_type    [MAX_TYPE] = '\0';

    *el_num_dim      = elm_num_dim     ;
    *el_num_byte     = (BInt4) elm_num_byte    ;
    *el_single_byte  = elm_single_byte ;

    for ( i=0; i<*el_num_dim; i++)
    {
      el_dimens[i] = elm_dimens[i];
    }
  }
/*
 * Do not fill extra places of el_dimens array,
 * probably the places does not exists
 */

  return nefis_errno;
}
/*==========================================================================*/
/*
 * Get next element on definition file
 * input:      fd        : NEFIS file set descriptor
 *
 * output:
 *
 *
 *
 * return:   0    No error occured
 *           !=0  Error occured
 */
DLLEXPORT BInt4 INQUIRE_NEXT_ELEMENT ( BInt4 * fd              ,/* I */
                             BText   el_name        ,/* O */
                             BText   el_type        ,/* O */
                             BText   el_quantity    ,/* O */
                             BText   el_unity       ,/* O */
                             BText   el_desc        ,/* O */
                             BInt4 * el_single_byte ,/* O */
                             BInt4 * el_num_byte    ,/* O */
                             BInt4 * el_num_dim     ,/* O */
                             BInt4 * el_dimens      )/* O */
{
  BUInt4  elm_num_dim = 0           ;
  BChar   elm_name    [MAX_NAME + 1];
  BChar   elm_type    [MAX_TYPE + 1];
  BChar   elm_quantity[MAX_NAME + 1];
  BChar   elm_unity   [MAX_NAME + 1];
  BChar   elm_desc    [MAX_DESC + 1];
  BUInt4  elm_dimens  [MAX_DIM     ];
  BUInt4  elm_single_byte = 0       ;
  BUInt8  elm_num_byte    = 0       ;
  BInt4   i                         ;

  nefis_errno = 0;

/*
 * Initialize variables
 */
  for ( i=0; i<MAX_NAME; i++ )
  {
    elm_name    [i]= ' ';
    elm_quantity[i]= ' ';
    elm_unity   [i]= ' ';
  }
  elm_name    [MAX_NAME]='\0';
  elm_quantity[MAX_NAME]='\0';
  elm_unity   [MAX_NAME]='\0';

  for ( i=0; i<MAX_TYPE; i++ )
  {
    elm_type    [i]= ' ';
  }
  elm_type    [MAX_TYPE]='\0';

  for ( i=0; i<MAX_DESC; i++ )
  {
    elm_desc    [i]= ' ';
  }
  elm_desc    [MAX_DESC]='\0';

  for ( i=0; i<MAX_DIM ; i++ )
  {
    el_dimens[i] = 1;
  }

  nefis_errno = GP_get_next_elm (*fd              , 1              ,
                                  elm_name        , elm_type       ,
                                  elm_quantity    , elm_unity      ,
                                  elm_desc        ,&elm_num_dim    ,
                                  elm_dimens      ,&elm_single_byte,
                                 &elm_num_byte    );

  if ( nefis_errno == 0 )
  {
    strncpy(el_name    , elm_name    , MAX_NAME);
    strncpy(el_quantity, elm_quantity, MAX_NAME);
    strncpy(el_unity   , elm_unity   , MAX_NAME);
    strncpy(el_desc    , elm_desc    , MAX_DESC);
    strncpy(el_type    , elm_type    , MAX_TYPE);
    el_name    [MAX_NAME] = '\0';
    el_quantity[MAX_NAME] = '\0';
    el_unity   [MAX_NAME] = '\0';
    el_desc    [MAX_DESC] = '\0';
    el_type    [MAX_TYPE] = '\0';
  }

  *el_num_dim      = elm_num_dim     ;
  *el_num_byte     = (BInt4) elm_num_byte    ;
  *el_single_byte  = elm_single_byte ;

  for ( i=0; i<*el_num_dim; i++)
  {
    el_dimens[i] = elm_dimens[i];
  }
/*
 * Do not fill extra places of el_dimens array,
 * probably the places does not exists
 */

  return nefis_errno;
}
/*==========================================================================*/
/*
 * Get first cell on definition file
 * input:      fd        : NEFIS file set descriptor
 *
 * output:
 *
 *
 *
 * return:   0    No error occured
 *           !=0  Error occured
 */
DLLEXPORT BInt4 INQUIRE_FIRST_CELL   ( BInt4 * fd             ,/* I */
                             BText   cl_name        ,/* O */
                             BInt4 * cl_num_dim     ,/* O */
                             BInt4 * cl_num_bytes   ,/* O */
                             BChar   el_names [][MAX_NAME+1]) /* O */
{
    BUInt8   cel_num_bytes = 0;
    BUInt4   cel_num_dim = 0           ;
    BChar    cel_name    [MAX_NAME + 1];
    BUInt4   i                         ;
    BText    elm_names;
    elm_names = NULL;

    nefis_errno = 0;

/*
 * Initialize variables
 */
    for ( i=0; i<MAX_NAME; i++ )
    {
        cel_name    [i]= ' ';
    }
    cel_name    [MAX_NAME]='\0';

    elm_names = NULL;

    nefis_errno = GP_get_next_cell(*fd              , 0              ,
                                    cel_name        ,&elm_names      ,
                                   &cel_num_dim     ,&cel_num_bytes  );
  if ( nefis_errno == 0 )
  {
    if ( cel_num_dim > (BUInt4) *cl_num_dim )
    {
        nefis_errcnt += 1;
        nefis_errno   = 1013;
        sprintf(error_text,
        "Inqfcl: User supplied array too small to contain Cell properties: \'%s\' %ld>%ld \n",
                    cel_name, cel_num_dim, *cl_num_dim);
        return nefis_errno;
    }

    *cl_num_dim   = (BUInt4) cel_num_dim;
    *cl_num_bytes = (BUInt4) cel_num_bytes   ;

    strncpy(cl_name    , cel_name    , MAX_NAME);
    cl_name    [MAX_NAME] = '\0';

    for ( i=0; i<cel_num_dim; i++ )
    {
        strncpy( &el_names[i][0], &elm_names[i*(MAX_NAME+1)], MAX_NAME );
        el_names[i][MAX_NAME] = '\0';
    }
  }

  return nefis_errno;
}
/*==========================================================================*/
/*
 * Get first cell on definition file
 * input:      fd        : NEFIS file set descriptor
 *
 * output:
 *
 *
 *
 * return:   0    No error occured
 *           !=0  Error occured
 */
DLLEXPORT BInt4 INQUIRE_FIRST_CELL2 (BInt4 *  fd          , /* I */
                                    BText    cl_name     , /* O */
                                    BInt4 *  cl_num_dim  , /* O */
                                    BInt4 *  cl_num_bytes, /* O */
                                    BText ** el_names    ) /* O */
{
    BUInt8   cel_num_bytes = 0;
    BUInt4   cel_num_dim = 0           ;
    BChar    cel_name    [MAX_NAME + 1];
    BUInt4   i                         ;
    BText    elm_names;
    elm_names = NULL;

    nefis_errno = 0;

/*
 * Initialize variables
 */

    nefis_errno = GP_get_next_cell(*fd              , 0              ,
                                    cel_name        ,&elm_names      ,
                                   &cel_num_dim     ,&cel_num_bytes  );
    if ( nefis_errno == 0 )
    {
        *cl_num_dim   = (BUInt4) cel_num_dim;
        *cl_num_bytes = (BUInt4) cel_num_bytes   ;

        strncpy(cl_name    , cel_name    , MAX_NAME);
        cl_name    [MAX_NAME] = '\0';

        {
            char *  b = (char  *) malloc( cel_num_dim * (MAX_NAME+1) * sizeof(char) );
            *el_names = (char **) malloc( cel_num_dim * sizeof(char*) );
            for ( i=0; i<cel_num_dim; i++ )
            {
                (*el_names)[i] = b + i*(MAX_NAME+1);
            }
            for ( i=0; i<cel_num_dim; i++ )
            {
                strncpy( (*el_names)[i], &elm_names[i*(MAX_NAME+1)], MAX_NAME );
                (*el_names)[i][MAX_NAME] = '\0';
            }
            //*el_names = a;
        }
    }

    return nefis_errno;
}
/*==========================================================================*/
/*
 * Get first cell on definition file
 * input:      fd        : NEFIS file set descriptor
 *
 * output:
 *
 *
 *
 * return:   0    No error occured
 *           !=0  Error occured
 */
DLLEXPORT BInt4 INQUIRE_FIRST_CELL3 (BInt4 *  fd          , /* I */
                                    BText    cl_name     , /* O */
                                    BInt4 *  cl_num_dim  , /* O */
                                    BInt4 *  cl_num_bytes, /* O */
                                    BText *   el_names    ) /* O */
{
    BUInt8   cel_num_bytes = 0;
    BUInt4   cel_num_dim = 0           ;
    BChar    cel_name    [MAX_NAME + 1];
    BUInt4   i                         ;
    BText    elm_names;

    elm_names = NULL;

    nefis_errno = 0;

/*
 * Initialize variables
 */

    nefis_errno = GP_get_next_cell(*fd              , 0              ,
                                    cel_name        ,&elm_names      ,
                                   &cel_num_dim     ,&cel_num_bytes  );
    if ( nefis_errno == 0 )
    {
        *cl_num_dim   = (BUInt4) cel_num_dim;
        *cl_num_bytes = (BUInt4) cel_num_bytes   ;

        strncpy(cl_name    , cel_name    , MAX_NAME);
        cl_name    [MAX_NAME] = '\0';

        *el_names = (char  *) malloc( cel_num_dim * (MAX_NAME+1) * sizeof(char) );
        for ( i=0; i<cel_num_dim; i++ )
        {
            strncpy(*el_names + i*(MAX_NAME + 1), &elm_names[i*(MAX_NAME + 1)], MAX_NAME);
            (*el_names + i*(MAX_NAME+1))[MAX_NAME] = '\0';
        }
    }

    return nefis_errno;
}
/*==========================================================================*/
/*
 * Get next cell on definition file
 * input:      fd        : NEFIS file set descriptor
 *
 * output:
 *
 *
 *
 * return:   0    No error occured
 *           !=0  Error occured
 */
DLLEXPORT BInt4 INQUIRE_NEXT_CELL   ( BInt4 * fd             ,/* I */
                             BText   cl_name        ,/* O */
                             BInt4 * cl_num_dim     ,/* O */
                             BInt4 * cl_num_bytes   ,/* O */
                             BChar   el_names [][MAX_NAME+1]) /* O */
{
    BUInt8   cel_num_bytes = 0;
    BUInt4   cel_num_dim = 0           ;
    BChar    cel_name    [MAX_NAME + 1];
    BUInt4   i                         ;
    BText    elm_names;
    elm_names = NULL;

    nefis_errno = 0;

/*
 * Initialize variables
 */
    for ( i=0; i<MAX_NAME; i++ )
    {
        cel_name    [i]= ' ';
    }
    cel_name    [MAX_NAME]='\0';

    elm_names = NULL;

    nefis_errno = GP_get_next_cell(*fd              , 1              ,
                                    cel_name        ,&elm_names      ,
                                   &cel_num_dim     ,&cel_num_bytes  );
    if ( nefis_errno == 0 )
    {
        if ( cel_num_dim > (BUInt4) *cl_num_dim )
        {
            nefis_errcnt += 1;
            nefis_errno   = 1013;
            sprintf(error_text,
            "Inqncl: User supplied array too small to contain Cell properties: \'%s\' %ld>%ld \n",
                        cel_name, cel_num_dim, *cl_num_dim);
            return nefis_errno;
        }

        *cl_num_dim   = (BUInt4) cel_num_dim;
        *cl_num_bytes = (BUInt4) cel_num_bytes   ;

        strncpy(cl_name    , cel_name    , MAX_NAME);
        cl_name    [MAX_NAME] = '\0';

        for ( i=0; i<cel_num_dim; i++ )
        {
            strncpy( &el_names[i][0], &elm_names[i*(MAX_NAME+1)], MAX_NAME );
            el_names[i][MAX_NAME] = '\0';
        }
    }

    return nefis_errno;
}
/*==========================================================================*/
/*
 * Get next cell on definition file
 * input:      fd        : NEFIS file set descriptor
 *
 * output:
 *
 *
 *
 * return:   0    No error occured
 *           !=0  Error occured
 */
DLLEXPORT BInt4 INQUIRE_NEXT_CELL2  (BInt4 *  fd          , /* I */
                                   BText    cl_name     , /* O */
                                   BInt4 *  cl_num_dim  , /* O */
                                   BInt4 *  cl_num_bytes, /* O */
                                   BText ** el_names    ) /* O */
{
    BUInt8   cel_num_bytes = 0;
    BUInt4   cel_num_dim = 0           ;
    BChar    cel_name    [MAX_NAME + 1];
    BUInt4   i                         ;
    BText    elm_names;

    elm_names = (BText) malloc((MAX_NAME+1) * *cl_num_dim);
    nefis_errno = 0;
/*
 * Initialize variables
 */
    for ( i=0; i<MAX_NAME; i++ )
    {
        cel_name    [i]= ' ';
    }
    cel_name    [MAX_NAME]='\0';

    elm_names= NULL;

    nefis_errno = GP_get_next_cell(*fd              , 1              ,
                                  cel_name        ,&elm_names      ,
                                 &cel_num_dim     ,&cel_num_bytes  );
    if ( nefis_errno == 0 )
    {
        *cl_num_dim   = (BUInt4) cel_num_dim;
        *cl_num_bytes = (BUInt4) cel_num_bytes   ;

        strncpy(cl_name    , cel_name    , MAX_NAME);
        cl_name    [MAX_NAME] = '\0';

        {
            char *  b = (char  *) malloc( cel_num_dim * (MAX_NAME+1) * sizeof(char) );
            *el_names = (char **) malloc( cel_num_dim * sizeof(char*) );
            for ( i=0; i<cel_num_dim; i++ )
            {
                (*el_names)[i] = b + i*(MAX_NAME+1);
            }
            for ( i=0; i<cel_num_dim; i++ )
            {
                strncpy( (*el_names)[i], &elm_names[i*(MAX_NAME+1)], MAX_NAME );
                (*el_names)[i][MAX_NAME] = '\0';
            }
        }
    }

    return nefis_errno;
}
/*==========================================================================*/
/*
 * Get next cell on definition file
 * input:      fd        : NEFIS file set descriptor
 *
 * output:
 *
 *
 *
 * return:   0    No error occured
 *           !=0  Error occured
 */
DLLEXPORT BInt4 INQUIRE_NEXT_CELL3  (BInt4 *  fd          , /* I */
                                   BText    cl_name     , /* O */
                                   BInt4 *  cl_num_dim  , /* O */
                                   BInt4 *  cl_num_bytes, /* O */
                                   BText *  el_names    ) /* O */
{
    BUInt8   cel_num_bytes = 0;
    BUInt4   cel_num_dim = 0           ;
    BChar    cel_name    [MAX_NAME + 1];
    BUInt4   i                         ;
    BText    elm_names;

    elm_names = (BText) malloc((MAX_NAME+1) * *cl_num_dim);
    nefis_errno = 0;
/*
 * Initialize variables
 */
    for ( i=0; i<MAX_NAME; i++ )
    {
        cel_name    [i]= ' ';
    }
    cel_name    [MAX_NAME]='\0';

    elm_names= NULL;

    nefis_errno = GP_get_next_cell(*fd              , 1              ,
                                  cel_name        ,&elm_names      ,
                                 &cel_num_dim     ,&cel_num_bytes  );
    if ( nefis_errno == 0 )
    {
        *cl_num_dim   = (BUInt4) cel_num_dim;
        *cl_num_bytes = (BUInt4) cel_num_bytes   ;

        strncpy(cl_name    , cel_name    , MAX_NAME);
        cl_name    [MAX_NAME] = '\0';

        *el_names = (char  *) malloc( cel_num_dim * (MAX_NAME+1) * sizeof(char) );
        for ( i=0; i<cel_num_dim; i++ )
        {
             strncpy( *el_names+i*(MAX_NAME+1), &elm_names[i*(MAX_NAME+1)], MAX_NAME );
            (*el_names + i*(MAX_NAME + 1))[MAX_NAME] = '\0';
        }
    }

    return nefis_errno;
}
/*==========================================================================*/
/*
 * Get first group definition on definition file
 * input:      fd        : NEFIS file set descriptor
 *
 * output:
 *
 *
 *
 * return:   0    No error occured
 *           !=0  Error occured
 */
DLLEXPORT BInt4 INQUIRE_FIRST_DEF_GROUP ( BInt4 * fd             ,/* I */
                                BText   gr_name        ,/* O */
                                BText   cl_name        ,/* O */
                                BInt4 * gr_num_dim     ,/* O */
                                BInt4 * gr_dimens      ,/* O */
                                BInt4 * gr_order       )/* O */
{
  BChar   cel_name    [MAX_NAME + 1];
  BInt4   grp_dimens  [MAX_DIM];
  BInt4   grp_num_dim = 0           ;
  BChar   grp_name    [MAX_NAME + 1];
  BInt4   grp_order   [MAX_DIM];
  BInt4   i                         ;

  nefis_errno = 0;

/*
 * Initialize variables
 */
  for ( i=0; i<MAX_NAME; i++ )
  {
    cel_name    [i]= ' ';
    grp_name    [i]= ' ';
  }
  cel_name    [MAX_NAME]='\0';
  grp_name    [MAX_NAME]='\0';

  nefis_errno = GP_get_next_def_grp(*fd              , 0              ,
                                     grp_name        , cel_name       ,
                                    &grp_num_dim     , grp_dimens     ,
                                     grp_order       );
  if ( nefis_errno == 0 )
  {
    if ( grp_num_dim > *gr_num_dim )
    {
      nefis_errcnt += 1;
      nefis_errno   = 1015;
      sprintf(error_text,
      "Inqfgr: User supplied array too small to contain group properties: \'%s\' %ld>%ld \n",
                    grp_name, grp_num_dim, *gr_num_dim);
      return nefis_errno;
    }

    strncpy(cl_name    , cel_name    , MAX_NAME);
    cl_name    [MAX_NAME] = '\0';

    strncpy(gr_name    , grp_name    , MAX_NAME);
    gr_name    [MAX_NAME] = '\0';

    *gr_num_dim   = grp_num_dim;
    for ( i=0; i<grp_num_dim; i++ )
    {
      gr_dimens[i] = grp_dimens[i];
      gr_order [i] = grp_order [i];
    }
  }

  return nefis_errno;
}
/*==========================================================================*/
/*
 * Get next group definition on definition file
 * input:      fd        : NEFIS file set descriptor
 *
 * output:
 *
 *
 *
 * return:   0    No error occured
 *           !=0  Error occured
 */
DLLEXPORT BInt4 INQUIRE_NEXT_DEF_GROUP  ( BInt4 * fd             ,/* I */
                                BText   gr_name        ,/* O */
                                BText   cl_name        ,/* O */
                                BInt4 * gr_num_dim     ,/* O */
                                BInt4 * gr_dimens      ,/* O */
                                BInt4 * gr_order       )/* O */
{
  BChar   cel_name    [MAX_NAME + 1];
  BInt4   grp_dimens  [MAX_DIM];
  BInt4   grp_num_dim = 0           ;
  BChar   grp_name    [MAX_NAME + 1];
  BInt4   grp_order   [MAX_DIM];
  BInt4   i                         ;

  nefis_errno = 0;

/*
 * Initialize variables
 */
  for ( i=0; i<MAX_NAME; i++ )
  {
    cel_name    [i]= ' ';
    grp_name    [i]= ' ';
  }
  cel_name    [MAX_NAME]='\0';
  grp_name    [MAX_NAME]='\0';

  nefis_errno = GP_get_next_def_grp(*fd              , 1              ,
                                     grp_name        , cel_name       ,
                                    &grp_num_dim     , grp_dimens     ,
                                     grp_order       );
  if ( nefis_errno == 0 )
  {
    if ( grp_num_dim > *gr_num_dim )
    {
      nefis_errcnt += 1;
      nefis_errno   = 1016;
      sprintf(error_text,
      "Inqngr: User supplied array too small to contain group properties: \'%s\' %ld>%ld \n",
                    grp_name, grp_num_dim, *gr_num_dim);
      return nefis_errno;
    }

    strncpy(cl_name    , cel_name    , MAX_NAME);
    cl_name    [MAX_NAME] = '\0';

    strncpy(gr_name    , grp_name    , MAX_NAME);
    gr_name    [MAX_NAME] = '\0';

    *gr_num_dim   = grp_num_dim;
    for ( i=0; i<grp_num_dim; i++ )
    {
      gr_dimens[i] = grp_dimens[i];
      gr_order [i] = grp_order [i];
    }
  }

  return nefis_errno;
}
/*==========================================================================*/
/*
 * Get first integer attribute on data file
 *
 * input:      fd        : NEFIS file set descriptor
 *           * gr_name   : first/next groupname on data file
 * output:   * at_name   : attribute name
 *           * at_value  : value of the attribute
 *
 * return:   0    No error occured
 *           !=0  Error occured
 */
DLLEXPORT BInt4 INQUIRE_FIRST_INTEGER( BInt4 * fd            ,
                             BText   gr_name       ,
                             BText   at_name       ,
                             BInt4 * at_value      )
{
  BChar   att_name[MAX_NAME+1];
  BChar   att_type[MAX_TYPE+1];
  BChar   grp_name[MAX_NAME+1];
  BInt4   i;

  nefis_errno = 0;

  C_Copy_text( grp_name    , gr_name    , MAX_NAME);

  for ( i=0; i<MAX_NAME; i++ )
  {
    att_name    [i]= ' ';
  }
  att_name    [MAX_NAME]='\0';

  strcpy(att_type, "INTEGER");

  nefis_errno = GP_get_attribute(*fd      , 1       , grp_name,
                                  att_name, at_value, att_type);

  if ( nefis_errno == 0 )
  {
    strncpy(at_name, att_name, MAX_NAME);
    at_name   [MAX_NAME] = '\0';
  }

  return nefis_errno;
}
/*==========================================================================*/
/*
 * Get first real attribute on data file
 *
 * input:      fd        : NEFIS file set descriptor
 *           * gr_name   : first/next groupname on data file
 * output:   * at_name   : attribute name
 *           * at_value  : value of the attribute
 *
 * return:   0    No error occured
 *           !=0  Error occured
 */
DLLEXPORT BInt4 INQUIRE_FIRST_REAL   ( BInt4 * fd            ,
                             BText   gr_name       ,
                             BText   at_name       ,
                             BRea4 * at_value      )
{
  BChar   att_name[MAX_NAME+1];
  BChar   att_type[MAX_TYPE+1];
  BChar   grp_name[MAX_NAME+1];
  BInt4   i;

  nefis_errno = 0;

  C_Copy_text( grp_name    , gr_name    , MAX_NAME);

  for ( i=0; i<MAX_NAME; i++ )
  {
    att_name    [i]= ' ';
  }
  att_name    [MAX_NAME]='\0';

  strcpy(att_type, "REAL");

  nefis_errno = GP_get_attribute(*fd      , 1       , grp_name,
                                  att_name, at_value, att_type);

  if ( nefis_errno == 0 )
  {
    strncpy(at_name, att_name, MAX_NAME);
    at_name   [MAX_NAME] = '\0';
  }

  return nefis_errno;
}
/*==========================================================================*/
/*
 * Get first string attribute on data file
 *
 * input:      fd        : NEFIS file set descriptor
 *           * gr_name   : first/next groupname on data file
 * output:   * at_name   : attribute name
 *           * at_value  : value of the attribute
 *
 * return:   0    No error occured
 *           !=0  Error occured
 */
DLLEXPORT BInt4 INQUIRE_FIRST_STRING ( BInt4 * fd            ,
                             BText   gr_name       ,
                             BText   at_name       ,
                             BText   at_value      )
{
  BChar   att_name [MAX_NAME+1];
  BChar   att_type [MAX_TYPE+1];
  BChar   grp_name [MAX_NAME+1];
  BInt4   i;

  nefis_errno = 0;

  C_Copy_text( grp_name    , gr_name    , MAX_NAME);

  for ( i=0; i<MAX_NAME; i++ )
  {
    att_name    [i]= ' ';
  }
  att_name    [MAX_NAME]='\0';

  strcpy(att_type, "CHARACTE");

  nefis_errno = GP_get_attribute(*fd      , 1       , grp_name,
                                  att_name, at_value, att_type);

  if ( nefis_errno == 0 )
  {
    strncpy(at_name, att_name, MAX_NAME);
    at_name   [MAX_NAME] = '\0';
  }

  return nefis_errno;
}
/*==========================================================================*/
/*
 * Read group definition from defintion file
 * input:      fd         : NEFIS file set descriptor
 *           * gr_defined : Groupname on definition file
 * output:   * cl_name    : celname
 *           * grp_num_dim: Number of dimensions of group
 *           * grp_dimens : Array with the dimensions
 *           * grp_order  : Array with the  group order on the file
 *
 * return:   0    No error occured
 *           !=0  Error occured
 */

DLLEXPORT BInt4 INQUIRE_GROUP       ( BInt4 * fd             ,
                            BText   gr_defined     ,
                            BText   cl_name        ,
                            BInt4 * gr_num_dim     ,
                            BInt4 * grp_dimens     ,
                            BInt4 * grp_order      )
{
  BChar   cel_name   [MAX_NAME+1];
  BChar   grp_defined[MAX_NAME+1];
  BUInt4  grp_num_dim;

  nefis_errno = 0;

  C_Copy_text( grp_defined , gr_defined , MAX_NAME);

  nefis_errno = GP_inquire_grp_def( *fd        ,
                                    grp_defined, cel_name ,&grp_num_dim  ,
                                    (BUInt4 *) grp_dimens , (BUInt4 *) grp_order);

  if ( nefis_errno == 0 )
  {
    strncpy( cl_name, cel_name, MAX_NAME);
    cl_name   [MAX_NAME] = '\0';

    *gr_num_dim = (BInt4) grp_num_dim;
  }

  return nefis_errno;
}
/*==========================================================================*/
/*
 * Determine maximum index of variable group
 * Input : * fd                 Nefis file set descriptor
 *           df_file            Data file name
 *           gr_name            Group name on data file (variable dimension)
 * Output:   -
 * Return:    0                 No error occured
 *            !=0               Error occured
 */

DLLEXPORT BInt4 INQUIRE_MAX_INDEX ( BInt4  * fd             ,
                          BText    gr_name        ,
                          BInt4  * max_index      )
{
  BChar   grp_name [ MAX_NAME+1 ];

  nefis_errno = 0;

  C_Copy_text( grp_name    , gr_name    , MAX_NAME);

  nefis_errno = GP_inquire_max ( *fd, grp_name, (BUInt4 *) max_index);

  return nefis_errno;
}
/*==========================================================================*/
/*
 * Get next data group on data file
 * input:     * fd        : NEFIS file set descriptor
 * output:    * gr_name   : first/next groupname on data file
 *              gr_defined: corresponding groupname (defined)
 *                           on definition file
 * return:   0    No error occured
 *           !=0  Error occured
 */
DLLEXPORT BInt4 INQUIRE_NEXT_DAT_GROUP   ( BInt4 * fd            ,
                                 BText   gr_name       ,
                                 BText   gr_defined    )
{
  BChar   grp_name   [MAX_NAME+1];
  BChar   grp_defined[MAX_NAME+1];
  BInt4   i      ;

  nefis_errno = 0;

  for ( i=0; i<MAX_NAME; i++ )
  {
    grp_name    [i]= ' ';
  }
  grp_name    [MAX_NAME]='\0';

  for ( i=0; i<MAX_NAME; i++ )
  {
    grp_defined [i]= ' ';
  }
  grp_defined [MAX_NAME]='\0';

  nefis_errno = GP_get_next_grp (*fd      , 1, grp_name, grp_defined);

/* TODO: check on length gr_name and grp_defined */
  if ( nefis_errno == 0 )
  {
    strncpy(gr_name   , grp_name   , MAX_NAME);
    strncpy(gr_defined, grp_defined, MAX_NAME);
    gr_name   [MAX_NAME] = '\0';
    gr_defined[MAX_NAME] = '\0';
  }

  return nefis_errno;
}
/*==========================================================================*/
/*
 * Get next integer attribute on data file
 *
 * input:  * fd        : NEFIS file set descriptor
 * output: * at_name   : attribute name
 *         * at_value  : value of the attribute
 * return:   0    No error occured
 *           !=0  Error occured
 */
DLLEXPORT BInt4 INQUIRE_NEXT_INTEGER ( BInt4 * fd            ,
                             BText   gr_name       ,
                             BText   at_name       ,
                             BInt4 * at_value      )
{
  BChar   att_name[MAX_NAME+1];
  BChar   att_type[MAX_TYPE+1];
  BChar   grp_name[MAX_NAME+1];
  BInt4   i;

  nefis_errno = 0;

  C_Copy_text( grp_name    , gr_name    , MAX_NAME);

  for ( i=0; i<MAX_NAME; i++ )
  {
    att_name    [i]= ' ';
  }
  att_name    [MAX_NAME]='\0';

  strcpy(att_type, "INTEGER");

  nefis_errno = GP_get_attribute(*fd      , 2       , grp_name,
                                  att_name, at_value, att_type);

  if ( nefis_errno == 0 )
  {
    strncpy(at_name, att_name, MAX_NAME);
    at_name   [MAX_NAME] = '\0';
  }

  return nefis_errno;
}
/*==========================================================================*/
/*
 * Get next real attribute on data file
 *
 * input:  * fd        : NEFIS file set descriptor
 * output: * at_name   : attribute name
 *         * at_value  : value of the attribute
 * return:   0           No error occured
 *           !=0         Error occured
 */
DLLEXPORT BInt4 INQUIRE_NEXT_REAL    ( BInt4 * fd            ,
                             BText   gr_name       ,
                             BText   at_name       ,
                             BRea4 * at_value      )
{
  BChar   att_name[MAX_NAME+1];
  BChar   att_type[MAX_TYPE+1];
  BChar   grp_name[MAX_NAME+1];
  BInt4   i;

  nefis_errno = 0;

  C_Copy_text( grp_name    , gr_name    , MAX_NAME);

  for ( i=0; i<MAX_NAME; i++ )
  {
    att_name    [i]= ' ';
  }
  att_name    [MAX_NAME]='\0';

  strcpy(att_type, "REAL");

  nefis_errno = GP_get_attribute(*fd      , 2       , grp_name,
                                  att_name, at_value, att_type);

  if ( nefis_errno == 0 )
  {
    strncpy(at_name, att_name, MAX_NAME);
    at_name   [MAX_NAME] = '\0';
  }

  return nefis_errno;
}
/*==========================================================================*/
/*
 * Get next string attribute on data file
 *
 * input:  * fd        : NEFIS file set descriptor
 * output: * at_name   : attribute name
 *         * at_value  : value of the attribute
 * return:   0           No error occured
 *           !=0         Error occured
 */
DLLEXPORT BInt4 INQUIRE_NEXT_STRING  ( BInt4 * fd            ,
                             BText   gr_name       ,
                             BText   at_name       ,
                             BText   at_value      )
{
  BChar   att_name[MAX_NAME+1];
  BChar   att_type[MAX_TYPE+1];
  BChar   grp_name[MAX_NAME+1];
  BInt4   i;

  nefis_errno = 0;

  C_Copy_text( grp_name    , gr_name    , MAX_NAME);

  for ( i=0; i<MAX_NAME; i++ )
  {
    att_name    [i]= ' ';
  }
  att_name    [MAX_NAME]='\0';

  strcpy(att_type, "CHARACTE");

  nefis_errno = GP_get_attribute(*fd      , 2       , grp_name,
                                  att_name, at_value, att_type);

  if ( nefis_errno == 0 )
  {
    strncpy(at_name, att_name, MAX_NAME);
    at_name   [MAX_NAME] = '\0';
  }

  return nefis_errno;
}
/*==========================================================================*/
/*
 * Print NEFIS error messages
 */
DLLEXPORT BInt4 NEFIS_ERROR ( BInt4 print_flag, BText err_string )
{
  BInt4 error;
  BChar error_string[LENGTH_ERROR_MESSAGE+1];

  error_string[0] = '\0';

  error = nefis_error(print_flag, error_string);

  strcpy( err_string  , error_string);

  return error;
}
/*==========================================================================*/
/*
 * Open data file
 * Input : * fd                 Nefis file set descriptor
 *           df_file            Data file name
 *           coding             Coding
 * Output:   -
 * Return:    0                 No error occured
 *            !=0               Error occured
 */

DLLEXPORT BInt4 OPEN_DAT_FILE   ( BInt4 * fd          ,
                        BText   dt_file     ,
                        BChar   coding      )
{
  BText    dat_file;
  BUInt4   file_length;

  nefis_errno = 0;

  file_length = (BUInt4) strlen(dt_file);
  dat_file = (BText) malloc ( sizeof(BChar) * (file_length+1) );
  strncpy( dat_file, dt_file, file_length);
  dat_file[file_length]='\0';

  nefis_errno = create_nefis_files( fd, dat_file, "", coding, 'U');

  free( (BData) dat_file    );

  return nefis_errno;
}
/*==========================================================================*/
/*
 * Open definiton file
 * Input : * fd                 Nefis file set descriptor
 *           df_file            Definition file name
 *           coding             Coding
 * Output:   -
 * Return:    0                 No error occured
 *            !=0               Error occured
 */
DLLEXPORT BInt4 OPEN_DEF_FILE   ( BInt4 * fd          ,
                        BText   df_file     ,
                        BChar   coding      )
{
  BText    def_file;
  BUInt4   file_length;

  nefis_errno = 0;

  file_length = (BUInt4) strlen(df_file);
  def_file = (BText) malloc ( sizeof(BChar) * (file_length+1) );
  strncpy( def_file, df_file, file_length);
  def_file[file_length]='\0';

  nefis_errno = create_nefis_files( fd, "", def_file, coding, 'U');

  free( (BData) def_file    );

  return nefis_errno;
}
/*==========================================================================*/
/*
 * Put character and alpha numeric element on data file
 * Input:  * fd                Fortran gives poBInt4er to deffds array
 * Output: -
 * func  : 0                   No error occured
 *         !=0                 Error occured
 */

DLLEXPORT BInt4 PUT_ELEMENT    ( BInt4 * fd             ,
                       BText   gr_name        ,
                       BText   el_name        ,
                       BInt4 * user_index     ,
                       BInt4 * user_order     ,
                       BData   getal          )
{
  BChar   elm_name [MAX_NAME + 1];
  BChar   grp_name [MAX_NAME + 1];
  BInt4   i        ;
  BInt4   j        ;
  BInt4   usr_index[MAX_DIM * DEFINE_STEPS];
  BInt4   usr_order[MAX_DIM    ];

  nefis_errno = 0;

  C_Copy_text( elm_name    , el_name    , MAX_NAME);
  C_Copy_text( grp_name    , gr_name    , MAX_NAME);

  for ( i=0; i<MAX_DIM; i++ )
  {
    usr_order[i] =user_order[i];
  }

  for ( i=0; i<MAX_DIM     ; i++ )
  {
    for ( j=0; j<DEFINE_STEPS; j++ )
    {
      usr_index[A2D(i,j)] = user_index[A2D(i,j)];
    }
  }

  nefis_errno = Put_element ( *fd, grp_name   , elm_name  , usr_index,
                                   usr_order  , getal     );

  return nefis_errno;
}
/*==========================================================================*/
/*
 * Put character element on data file
 * Output: -
 * func  : 0                   No error occured
 *         !=0                 Error occured
 */

DLLEXPORT BInt4 PUT_ELEMENT_STRING ( BInt4 * fd             ,
                           BText   gr_name        ,
                           BText   el_name        ,
                           BInt4 * user_index     ,
                           BInt4 * user_order     ,
                           BData   getal          )
{
  nefis_errno = 0;
  nefis_errno = PUT_ELEMENT ( fd             ,
                              gr_name        ,
                              el_name        ,
                              user_index     ,
                              user_order     ,
                              getal          );
  return nefis_errno;
}
/*==========================================================================*/
/*
 * Put integer attribute on data file
 * Input : * fd                 Nefis file set descriptor
 *           gr_name            Group name on data file
 *           at_name            Attribute name
 *           at_value           Attribute value
 * Output:   -
 * Return:    0                 No error occured
 *            !=0               Error occured
 */
DLLEXPORT BInt4 PUT_INT_ATTRIBUTE    ( BInt4 * fd            ,
                             BText   gr_name       ,
                             BText   at_name       ,
                             BInt4 * at_value      )
{
  BChar   att_name[MAX_NAME+1];
  BChar   att_type[MAX_TYPE+1];
  BData   att_value    ;
  BChar   grp_name[MAX_NAME+1];

  nefis_errno = 0;

  if ( ( (BInt4) strlen(gr_name) > (BInt4) MAX_NAME) ||
       ( (BInt4) strlen(at_name) > (BInt4) MAX_NAME)    )
  {
    nefis_errcnt += 1;
    nefis_errno   = 1019;
    sprintf(error_text,
      "Putiat: Groupname \'%s\' or\n        integer attribute name \'%s\' too long",
      gr_name, at_name);
    return nefis_errno;
  }

  C_Copy_text( grp_name    , gr_name    , MAX_NAME);
  C_Copy_text( att_name    , at_name    , MAX_NAME);

  strcpy(att_type, "INTEGER");
  att_value = (BData) at_value;

  nefis_errno = GP_put_attribute(*fd, grp_name, att_name, att_value, att_type);

  return nefis_errno;
}
/*==========================================================================*/
/*
 * Put integer attribute on data file
 * Input : * fd                 Nefis file set descriptor
 *           gr_name            Group name on data file
 *           at_name            Attribute name
 *           at_value           Attribute value
 * Output:   -
 * Return:    0                 No error occured
 *            !=0               Error occured
 */
DLLEXPORT BInt4 PUT_REAL_ATTRIBUTE   ( BInt4 * fd            ,
                             BText   gr_name       ,
                             BText   at_name       ,
                             BRea4 * at_value      )
{
  BChar   att_name[MAX_NAME+1];
  BChar   att_type[MAX_TYPE+1];
  BData   att_value    ;
  BChar   grp_name[MAX_NAME+1];

  nefis_errno = 0;

  if ( ((BInt4) strlen(gr_name) > (BInt4) MAX_NAME) ||
       ((BInt4) strlen(at_name) > (BInt4) MAX_NAME)    )
  {
    nefis_errcnt += 1;
    nefis_errno   = 1020;
    sprintf(error_text,
      "Putrat: Groupname \'%s\' or\n        real attribute name \'%s\' too long",
      gr_name, at_name);
    return nefis_errno;
  }

  C_Copy_text( grp_name    , gr_name    , MAX_NAME);
  C_Copy_text( att_name    , at_name    , MAX_NAME);

  strcpy(att_type, "REAL");
  att_value = (BData) at_value;

  nefis_errno = GP_put_attribute(*fd, grp_name, att_name, att_value, att_type);

  return nefis_errno;
}
/*==========================================================================*/
/*
 * Put integer attribute on data file
 * Input : * fd                 Nefis file set descriptor
 *           gr_name            Group name on data file
 *           at_name            Attribute name
 *           at_value           Attribute value
 * Output:   -
 * Return:    0                 No error occured
 *            !=0               Error occured
 */
DLLEXPORT BInt4 PUT_STRING_ATTRIBUTE ( BInt4 * fd            ,
                             BText   gr_name       ,
                             BText   at_name       ,
                             BText   at_value      )
{
  BChar   att_name [MAX_NAME+1];
  BChar   att_type [MAX_TYPE+1];
  BChar   att_value[MAX_NAME+1];
  BChar   grp_name [MAX_NAME+1];

  nefis_errno = 0;

  if ( ((BInt4) strlen(gr_name) > (BInt4) MAX_NAME) ||
       ((BInt4) strlen(at_name) > (BInt4) MAX_NAME) ||
       ((BInt4) strlen(at_value)> (BInt4) MAX_NAME)    )
  {
    nefis_errcnt += 1;
    nefis_errno   = 1021;
    sprintf(error_text,
      "Putsat: Groupname \'%s\' or\n        real attribute name \'%s\' or\n        attribute value \'%s\' too long",
      gr_name, at_name, at_value);
    return nefis_errno;
  }
  C_Copy_text( grp_name    , gr_name    , MAX_NAME);
  C_Copy_text( att_name    , at_name    , MAX_NAME);
  C_Copy_text( att_value   , at_value   , MAX_NAME);

  strcpy(att_type, "CHARACTE");

  nefis_errno = GP_put_attribute(*fd, grp_name, att_name, (BData) att_value, att_type);

  return nefis_errno;
}
/*==========================================================================*/
/*
 * Retrieve version number of the NEFIS library
 * Output: nefis_version       Nefis version string
 * Return: 0                   No error occured
 *         !=0                 Error occured
 */

DLLEXPORT BInt4 GET_NEFIS_VERSION  ( BText * nef_version)
{
  BText nefis_version;

  nefis_errno = 0;

  nefis_errno = OC_get_version(&nefis_version);

  if ( nefis_errno == 0 )
  {
    *nef_version = strdup(nefis_version);
  }
  return nefis_errno;
}
/*==========================================================================*/
/*
 * reset the NEFIS file version number
 * Input : * fd                Nefis file set descriptor
 *           file_version      NEFIS file version
 * Return: 0                   No error occured
 *         !=0                 Error occured
 */

DLLEXPORT BInt4 RESET_FILE_VERSION  ( BInt4 fd, BInt4 file_version)
{
  nefis_errno = 0;

  nefis_errno = OC_reset_file_version(fd, file_version);

  return nefis_errno;
}
