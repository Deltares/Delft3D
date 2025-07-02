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
 *   <f2c.c> - Functions related to the conversion Fortran to C
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
 *   This interface between Fortran and C is needed to translate the user
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
#  define strdup _strdup
#elif defined (salford32)
#  include <io.h>
#  include <windows.h>
#endif

/*
 * FTN_CAPITAL   : dvf6, salford
 * FTN_UNDERSCORE: sgi, sun, cygwin, linux
 * FTN_SMALL     : hp
 *
 * MIXED_STR_LEN_ARG: string length argument direct behind the string
 */

#if defined(linux)
#   include "config.h"
#   define FTN_CALL  /* nothing */
#   define CLOSE_DAT_FILE          FC_FUNC(cldtnf,CLDTNF)
#   define CLOSE_DEF_FILE          FC_FUNC(cldfnf,CLDFNF)
#   define CLOSE_FLUSH_DAT_FILE    FC_FUNC(clsdat,CLSDAT)
#   define CLOSE_FLUSH_DEF_FILE    FC_FUNC(clsdef,CLSDEF)
#   define CLOSE_NEFIS             FC_FUNC(clsnef,CLSNEF)
#   define CLOSE_ALL_NEFIS         FC_FUNC(clsanf,CLSANF)
#   define CREATE_NEFIS            FC_FUNC(crenef,CRENEF)
#   define DEFINE_CEL              FC_FUNC(defcel,DEFCEL)
#   define DEFINE_DATA             FC_FUNC(credat,CREDAT)
#   define DEFINE_ELEMENT          FC_FUNC(defelm,DEFELM)
#   define DEFINE_GROUP            FC_FUNC(defgrp,DEFGRP)
#   define FLUSH_DAT_FILE          FC_FUNC(flsdat,FLSDAT)
#   define FLUSH_DEF_FILE          FC_FUNC(flsdef,FLSDEF)
#   define GET_DAT_HEADER          FC_FUNC(gethdt,GETHDT)
#   define GET_DEF_HEADER          FC_FUNC(gethdf,GETHDF)
#   define GET_ELEMENT             FC_FUNC(getelt,GETELT)
#   define GET_ELEMENT_STRING      FC_FUNC(getels,GETELS)
#   define GET_INT_ATTRIBUTE       FC_FUNC(getiat,GETIAT)
#   define GET_NEFIS_VERSION       FC_FUNC(getnfv,GETNFV)
#   define GET_REAL_ATTRIBUTE      FC_FUNC(getrat,GETRAT)
#   define GET_STRING_ATTRIBUTE    FC_FUNC(getsat,GETSAT)
#   define INQUIRE_CEL             FC_FUNC(inqcel,INQCEL)
#   define INQUIRE_ELEMENT         FC_FUNC(inqelm,INQELM)
#   define INQUIRE_FIRST_ELEMENT   FC_FUNC(inqfel,INQFEL)
#   define INQUIRE_FIRST_CELL      FC_FUNC(inqfcl,INQFCL)
#   define INQUIRE_FIRST_DEF_GROUP FC_FUNC(inqfgr,INQFGR)
#   define INQUIRE_FIRST_DAT_GROUP FC_FUNC(inqfst,INQFST)
#   define INQUIRE_FIRST_INTEGER   FC_FUNC(inqfia,INQFIA)
#   define INQUIRE_FIRST_REAL      FC_FUNC(inqfra,INQFRA)
#   define INQUIRE_FIRST_STRING    FC_FUNC(inqfsa,INQFSA)
#   define INQUIRE_GROUP           FC_FUNC(inqgrp,INQGRP)
#   define INQUIRE_DATA_GROUP      FC_FUNC(inqdat,INQDAT)
#   define INQUIRE_MAX_INDEX       FC_FUNC(inqmxi,INQMXI)
#   define INQUIRE_NEXT_ELEMENT    FC_FUNC(inqnel,INQNEL)
#   define INQUIRE_NEXT_CELL       FC_FUNC(inqncl,INQNCL)
#   define INQUIRE_NEXT_DEF_GROUP  FC_FUNC(inqngr,INQNGR)
#   define INQUIRE_NEXT_DAT_GROUP  FC_FUNC(inqnxt,INQNXT)
#   define INQUIRE_NEXT_INTEGER    FC_FUNC(inqnia,INQNIA)
#   define INQUIRE_NEXT_REAL       FC_FUNC(inqnra,INQNRA)
#   define INQUIRE_NEXT_STRING     FC_FUNC(inqnsa,INQNSA)
#   define NEFIS_ERROR             FC_FUNC(neferr,NEFERR)
#   define OPEN_DAT_FILE           FC_FUNC(opndat,OPNDAT)
#   define OPEN_DEF_FILE           FC_FUNC(opndef,OPNDEF)
#   define PUT_ELEMENT             FC_FUNC(putelt,PUTELT)
#   define PUT_ELEMENT_STRING      FC_FUNC(putels,PUTELS)
#   define PUT_INT_ATTRIBUTE       FC_FUNC(putiat,PUTIAT)
#   define PUT_REAL_ATTRIBUTE      FC_FUNC(putrat,PUTRAT)
#   define PUT_STRING_ATTRIBUTE    FC_FUNC(putsat,PUTSAT)
#   define RESET_FILE_VERSION      FC_FUNC(resnfv,RESNFV)
#else
/* WIN32 or WIN64 */
#if defined(STDCALL)
#   define FTN_CALL __stdcall
#else
#   define FTN_CALL
#endif
#   define CLOSE_DAT_FILE          CLDTNF
#   define CLOSE_DEF_FILE          CLDFNF
#   define CLOSE_FLUSH_DAT_FILE    CLSDAT
#   define CLOSE_FLUSH_DEF_FILE    CLSDEF
#   define CLOSE_NEFIS             CLSNEF
#   define CLOSE_ALL_NEFIS         CLSANF
#   define CREATE_NEFIS            CRENEF
#   define DEFINE_CEL              DEFCEL
#   define DEFINE_DATA             CREDAT
#   define DEFINE_ELEMENT          DEFELM
#   define DEFINE_GROUP            DEFGRP
#   define FLUSH_DAT_FILE          FLSDAT
#   define FLUSH_DEF_FILE          FLSDEF
#   define GET_DAT_HEADER          GETHDT
#   define GET_DEF_HEADER          GETHDF
#   define GET_ELEMENT             GETELT
#   define GET_ELEMENT_STRING      GETELS
#   define GET_INT_ATTRIBUTE       GETIAT
#   define GET_NEFIS_VERSION       GETNFV
#   define GET_REAL_ATTRIBUTE      GETRAT
#   define GET_STRING_ATTRIBUTE    GETSAT
#   define INQUIRE_CEL             INQCEL
#   define INQUIRE_ELEMENT         INQELM
#   define INQUIRE_FIRST_ELEMENT   INQFEL
#   define INQUIRE_FIRST_CELL      INQFCL
#   define INQUIRE_FIRST_DEF_GROUP INQFGR
#   define INQUIRE_FIRST_DAT_GROUP INQFST
#   define INQUIRE_FIRST_INTEGER   INQFIA
#   define INQUIRE_FIRST_REAL      INQFRA
#   define INQUIRE_FIRST_STRING    INQFSA
#   define INQUIRE_GROUP           INQGRP
#   define INQUIRE_DATA_GROUP      INQDAT
#   define INQUIRE_MAX_INDEX       INQMXI
#   define INQUIRE_NEXT_ELEMENT    INQNEL
#   define INQUIRE_NEXT_CELL       INQNCL
#   define INQUIRE_NEXT_DEF_GROUP  INQNGR
#   define INQUIRE_NEXT_DAT_GROUP  INQNXT
#   define INQUIRE_NEXT_INTEGER    INQNIA
#   define INQUIRE_NEXT_REAL       INQNRA
#   define INQUIRE_NEXT_STRING     INQNSA
#   define NEFIS_ERROR             NEFERR
#   define OPEN_DAT_FILE           OPNDAT
#   define OPEN_DEF_FILE           OPNDEF
#   define PUT_ELEMENT             PUTELT
#   define PUT_ELEMENT_STRING      PUTELS
#   define PUT_INT_ATTRIBUTE       PUTIAT
#   define PUT_REAL_ATTRIBUTE      PUTRAT
#   define PUT_STRING_ATTRIBUTE    PUTSAT
#   define RESET_FILE_VERSION      RESNFV
#endif



#include "btps.h"
#include "nefis.h" /* needed for definition of LENGTH_ERROR_MESSAGE */
#include "nef-def.h"
#include "f2c.h"

#define A2D(m,n)  ((n)+DEFINE_STEPS*(m))



extern BInt4 nefis_errno;

/*==========================================================================*/
/* Start of C-functions                                                     */
/*==========================================================================*/
static BVoid F_Copy_text( BText name1       ,
                   BText name2       ,
                   BInt4 given_length,
                   BInt4 max_length  )
{
  BInt4 i       ;
  BInt4 max_copy;
  max_copy = min ( given_length, (BInt4)strlen(name2));
  max_copy = min ( max_copy, max_length);
  strncpy(name1, name2, max_copy);
  for ( i=max_copy; i<max_length; i++ )
  {
    name1[i]= ' ';
  }
  name1[max_length]='\0';
  return;
}

static char * strFcpy(char * str_1, int len)
{
    int m;
    char * str_2;
    m = min( len, (BInt4) strlen(str_1));
    str_2 = (char *) malloc( sizeof(char)*(m+1));
    strncpy(str_2, str_1, m);
    str_2[m] = '\0';
    return str_2;
}

static void RemoveTrailingBlanks(char * String)
{
  int i;
  i = (int) strlen(String)-1;
  while ( String[i] == ' '  ||
          String[i] == '\n' ||
          String[i] == '\t'    )
  {
    String[i] = '\0';
    i--;
  }
  return;
}

static int f2c_strlen(char * string, int length)
{
    /*
     * Determine the length of the string by
     * finding a \0 within a given number of characters
     * if no \0 is found the length is return
     */
    int i;
    int retval = length;
    for (i=0; i<length; i++)
    {
        if (string[i]=='\0')
        {
            retval = i;
            return retval;
        }
    }
    return retval;
}
/*==========================================================================*/
/* Start of NEFIS function calls                                            */
/*==========================================================================*/
/*
 * Close data file, and fluh hash buffer
 * Input : * set                Nefis file set descriptor
 * Output: * set                Nefis file set descriptor set to -1
 * Return:    0                 No error occured
 *            !=0               Error occured
 */

DLLEXPORT BInt4 FTN_CALL CLOSE_FLUSH_DAT_FILE ( BInt4  * set )
{
  nefis_errno = 0;

  nefis_errno = close_nefis_files( set );

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

DLLEXPORT BInt4 FTN_CALL CLOSE_ALL_NEFIS ( void )
{
  nefis_errno = 0;

  nefis_errno = OC_close_all_nefis_files();

  return nefis_errno;
}
/*==========================================================================*/
/*
 * Close definition file, and fluh hash buffer
 * Input : * set                Nefis file set descriptor
 * Output: * set                Nefis file set descriptor set to -1
 * Return:    0                 No error occured
 *            !=0               Error occured
 */

DLLEXPORT BInt4 FTN_CALL CLOSE_FLUSH_DEF_FILE ( BInt4  * set )
{
  nefis_errno = 0;

  nefis_errno = close_nefis_files( set );

  return nefis_errno;
}
/*==========================================================================*/
/*
 * Close data file, and do not flush hash buffer
 * Input : * set                Nefis file set descriptor
 * Output: * set                Nefis file set descriptor set to -1
 * Return:    0                 No error occured
 *            !=0               Error occured
 */

DLLEXPORT BInt4 FTN_CALL CLOSE_DAT_FILE ( BInt4  * set )
{
  BInt4 tmp_flush;

  nefis_errno = 0;
  nefis_flush = FALSE;
  tmp_flush   = nefis_flush;

  nefis_errno = close_nefis_files( set );

  nefis_flush = tmp_flush;

  return nefis_errno;
}
/*==========================================================================*/
/*
 * Close definition file, and do not flush hash buffer
 * Input : * set                Nefis file set descriptor
 * Output: * set                Nefis file set descriptor set to -1
 * Return:    0                 No error occured
 *            !=0               Error occured
 */

DLLEXPORT BInt4 FTN_CALL CLOSE_DEF_FILE ( BInt4  * set )
{
  BInt4 tmp_flush;

  nefis_errno = 0;
  nefis_flush = FALSE;
  tmp_flush   = nefis_flush;

  nefis_errno = close_nefis_files( set );

  nefis_flush = tmp_flush;

  return nefis_errno;
}
/*==========================================================================*/
/*
 * Close definition file and data file (flushing depends on open statement)
 * Input : * set                Nefis file set descriptor
 * Output: * set                Nefis file set descriptor set to -1
 * Return:    0                 No error occured
 *            !=0               Error occured
 */

DLLEXPORT BInt4 FTN_CALL CLOSE_NEFIS ( BInt4  * set )
{
  nefis_errno = 0;

  nefis_errno = close_nefis_files( set );

  return nefis_errno;
}
/*==========================================================================*/
/*
 * Define cel on definition file
 */
#if defined(MIXED_STR_LEN_ARG)
DLLEXPORT BInt4 FTN_CALL DEFINE_CEL  ( BInt4 * fd            ,
                            BText   cl_name        ,
                            BInt4   cl_name_length ,
                            BInt4 * cl_num_dim     ,
                            BText   el_names       ,
                            BInt4   el_names_length)
#else
DLLEXPORT BInt4 FTN_CALL DEFINE_CEL  ( BInt4 * fd             ,
                            BText   cl_name        ,
                            BInt4 * cl_num_dim     ,
                            BText   el_names       ,
                            BInt4   cl_name_length ,
                            BInt4   el_names_length)
#endif
{
  BChar   cel_name [ MAX_NAME + 1           ];
  BInt4   cel_num_dim                        ;
  BInt4   i;
  BInt4   j;
  BInt4   max_copy;
  BText   elm_names;

  elm_names = (BText) malloc(*cl_num_dim * (MAX_NAME+1));

  nefis_errno = 0;

  F_Copy_text (cel_name    , cl_name    , cl_name_length    , MAX_NAME);

  for ( i=0; i<((MAX_NAME+1)* *cl_num_dim); i++ )
  {
    elm_names[i]=' ';
  }
  for ( i=0; i<*cl_num_dim; i++ )
  {
    max_copy = f2c_strlen(el_names, el_names_length);
    max_copy = min ( max_copy        , MAX_NAME              );
    strncpy(&elm_names[i*(MAX_NAME+1)], &el_names[i*max_copy], max_copy);
    for ( j = i*(MAX_NAME+1) + max_copy;
          j < i*(MAX_NAME+1) + MAX_NAME;
          j++                           )
    {
      elm_names   [j]= ' ';
    }
    elm_names[i*(MAX_NAME+1) + MAX_NAME] = '\0';
  }

  cel_num_dim = *cl_num_dim;

  nefis_errno = Define_cel   ( *fd, cel_name,  cel_num_dim, elm_names);

  return nefis_errno;
}
/*==========================================================================*/
/*
 * Create space for data on data file
 */
#if defined(MIXED_STR_LEN_ARG)
DLLEXPORT BInt4 FTN_CALL DEFINE_DATA ( BInt4 * fd               ,
                            BText   gr_name          ,
                            BInt4   gr_name_length   ,
                            BText   gr_defined       ,
                            BInt4   gr_defined_length)
#else
DLLEXPORT BInt4 FTN_CALL DEFINE_DATA ( BInt4 * fd               ,
                            BText   gr_name          ,
                            BText   gr_defined       ,
                            BInt4   gr_name_length   ,
                            BInt4   gr_defined_length)
#endif
{
  BChar   grp_name   [MAX_NAME + 1];
  BChar   grp_defined[MAX_NAME + 1];

  nefis_errno = 0;

  F_Copy_text (grp_name    , gr_name    , gr_name_length    , MAX_NAME);
  F_Copy_text (grp_defined , gr_defined , gr_defined_length , MAX_NAME);

  nefis_errno = Define_data   ( *fd, grp_name, grp_defined);

  return nefis_errno;
}
/*==========================================================================*/
/*
 * Define element on definition file
 * Input:  * fd                Fortran gives pointer to deffds array
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
 *         !=0                 Error occured
 */
#if defined(MIXED_STR_LEN_ARG)
DLLEXPORT BInt4 FTN_CALL DEFINE_ELEMENT ( BInt4 * fd             ,
                               BText   el_name        ,
                               BInt4   el_name_length ,
                               BText   el_type        ,
                               BInt4   el_type_length ,
                               BInt4 * el_single_byte ,
                               BText   el_quantity    ,
                               BInt4   el_quantity_length,
                               BText   el_unity       ,
                               BInt4   el_unity_length,
                               BText   el_desc        ,
                               BInt4   el_desc_length ,
                               BInt4 * el_num_dim     ,
                               BInt4 * el_dimens      )
#else
DLLEXPORT BInt4 FTN_CALL DEFINE_ELEMENT ( BInt4 * fd             ,
                               BText   el_name        ,
                               BText   el_type        ,
                               BInt4 * el_single_byte ,
                               BText   el_quantity    ,
                               BText   el_unity       ,
                               BText   el_desc        ,
                               BInt4 * el_num_dim     ,
                               BInt4 * el_dimens      ,
                               BInt4   el_name_length ,
                               BInt4   el_type_length ,
                               BInt4   el_quantity_length,
                               BInt4   el_unity_length,
                               BInt4   el_desc_length )
#endif
{
  BInt4   elm_num_dim  ;
  BInt4   elm_single_byte;
  BChar   elm_name    [MAX_NAME + 1];
  BChar   elm_type    [MAX_TYPE + 1];
  BChar   elm_quantity[MAX_NAME + 1];
  BChar   elm_unity   [MAX_NAME + 1];
  BChar   elm_desc    [MAX_DESC + 1];
  BInt4   elm_dimens  [MAX_DIM     ];
  BInt4   i            ;

  nefis_errno = 0;

  F_Copy_text (elm_name    , el_name    , el_name_length    , MAX_NAME);
  F_Copy_text (elm_type    , el_type    , el_type_length    , MAX_TYPE);
  F_Copy_text (elm_quantity, el_quantity, el_quantity_length, MAX_NAME);
  F_Copy_text (elm_unity   , el_unity   , el_unity_length   , MAX_NAME);
  F_Copy_text (elm_desc    , el_desc    , el_desc_length    , MAX_DESC);

  elm_num_dim = *el_num_dim;
  for ( i=0; i<elm_num_dim; i++ )
  {
    elm_dimens[i] = el_dimens[i];
  }
  for ( i=elm_num_dim; i<MAX_DIM; i++ )
  {
    elm_dimens[i] = 1;
  }
  if ( elm_num_dim == 0 )
  {
    elm_num_dim   = 1;
  }
  elm_single_byte = *el_single_byte;

  nefis_errno = Define_element( *fd, elm_name   , elm_type  , elm_quantity   ,
                               elm_unity  , elm_desc  , elm_single_byte,
                               elm_num_dim, elm_dimens);

  return nefis_errno;
}
/*==========================================================================*/
/*
 * Define group on definition file
 */
#if defined(MIXED_STR_LEN_ARG)
DLLEXPORT BInt4 FTN_CALL DEFINE_GROUP ( BInt4 * fd              ,
                             BText   gr_name         ,
                             BInt4   gr_name_length  ,
                             BText   cl_name         ,
                             BInt4   cl_name_length  ,
                             BInt4 * gr_num_dim      ,
                             BInt4 * gr_dimens       ,
                             BInt4 * gr_order        )
#else
DLLEXPORT BInt4 FTN_CALL DEFINE_GROUP ( BInt4 * fd              ,
                             BText   gr_name         ,
                             BText   cl_name         ,
                             BInt4 * gr_num_dim      ,
                             BInt4 * gr_dimens       ,
                             BInt4 * gr_order        ,
                             BInt4   gr_name_length  ,
                             BInt4   cl_name_length  )
#endif
{
  BChar   cel_name  [MAX_NAME+1];
  BChar   grp_name  [MAX_NAME+1];
  BInt4   grp_dimens[MAX_DIM];
  BInt4   grp_num_dim;
  BInt4   grp_order [MAX_DIM];
  BInt4   i          ;

  nefis_errno = 0;

  F_Copy_text (cel_name    , cl_name    , cl_name_length    , MAX_NAME);
  F_Copy_text (grp_name    , gr_name    , gr_name_length    , MAX_NAME);

  grp_num_dim = *gr_num_dim;
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
 */
#if defined(MIXED_STR_LEN_ARG)
DLLEXPORT BInt4 FTN_CALL CREATE_NEFIS(
                            BInt4 * fd            ,
                            BText   dt_file       ,
                            BInt4   dt_file_length,
                            BText   df_file       ,
                            BInt4   df_file_length,
                            BText   cding         ,
                            BInt4   cding_length  ,
                            BText   acces_type    ,
                            BInt4   acces_length  )
#else
DLLEXPORT BInt4 FTN_CALL CREATE_NEFIS(
                            BInt4 * fd            ,
                            BText   dt_file       ,
                            BText   df_file       ,
                            BText   cding         ,
                            BText   acces_type    ,
                            BInt4   dt_file_length,
                            BInt4   df_file_length,
                            BInt4   cding_length  ,
                            BInt4   acces_length  )
#endif
{
  BText dat_file;
  BText def_file;
  BInt4 max_copy;
  BChar   coding;
  BChar  acctype;


  NOREFF(acces_length);
  NOREFF(cding_length);

  nefis_errno = 0;

  if ( dt_file_length > FILENAME_MAX-1 )
  {
    nefis_errno   = 2002;
    sprintf( error_text,
      "Crenef: Data filename too long ( length < %d )",FILENAME_MAX-1);
    return nefis_errno;
  }
  if ( df_file_length > FILENAME_MAX-1 )
  {
    nefis_errno   = 2003;
    sprintf( error_text,
      "Crenef: Definition filename too long ( length < %d )",FILENAME_MAX-1);
    return nefis_errno;
  }

  max_copy = f2c_strlen(dt_file, dt_file_length);
  dat_file = (BText  ) malloc ( sizeof(BChar) * (max_copy+1) );
  strncpy( dat_file, dt_file, max_copy);
  dat_file[max_copy]='\0';

  max_copy = f2c_strlen(df_file, df_file_length);
  def_file = (BText  ) malloc ( sizeof(BChar) * (max_copy+1) );
  strncpy( def_file, df_file, max_copy);
  def_file[max_copy]='\0';

  RemoveTrailingBlanks(dat_file);
  RemoveTrailingBlanks(def_file);

  coding = cding[0];
  acctype = acces_type[0];

  nefis_errno = create_nefis_files( fd, dat_file, def_file, coding, acctype);

  free( (BData) dat_file    );
  free( (BData) def_file    );

  return nefis_errno;
}
/*==========================================================================*/
/*
 * Flush hash buffer to data file
 */

DLLEXPORT BInt4 FTN_CALL FLUSH_DAT_FILE ( BInt4 * set )
{
  BInt4 dat_fds;

  nefis_errno = 0;
  if ( nefis[*set].one_file == TRUE )
  {
    dat_fds = nefis[*set].daf_fds;
  }
  else
  {
    dat_fds = nefis[*set].dat_fds;
  }

  nefis_errno = GP_flush_hash( dat_fds, *set);

  return nefis_errno;
}
/*==========================================================================*/
/*
 * Flush hash buffer to definition file
 */

DLLEXPORT BInt4 FTN_CALL FLUSH_DEF_FILE ( BInt4 * set )
{
  BInt4 def_fds;

  nefis_errno = 0;
  if ( nefis[*set].one_file == TRUE )
  {
    def_fds = nefis[*set].daf_fds;
  }
  else
  {
    def_fds = nefis[*set].def_fds;
  }

  nefis_errno = GP_flush_hash( def_fds, *set);

  return nefis_errno;
}
/*==========================================================================*/
/*
 * Get header from data file
 */
DLLEXPORT BInt4 FTN_CALL GET_DAT_HEADER( BInt4 * set          ,
                      BText   header       ,
                      BInt4   header_length)
{
  BUInt8 n_read = 0;

  nefis_errno = 0;

  if ( header_length < LHDRDT )
  {
    nefis_errno   = 2004;
    sprintf( error_text,
      "Gethdt: Supplied character string too small for header: %ld<%ld\n",
      header_length, LHDRDT);
    return nefis_errno;
  }

  n_read = GP_read_file ( nefis[*set].dat_fds, header, 0, LHDRDT);
  if ( n_read == -1 )
  {
    nefis_errno   = 2005;
    sprintf( error_text,
      "Gethdt: Unable to read data file header (file write only?).");
  }
  if ( n_read > LHDRDT )
  {
    nefis_errno   = 2006;
    sprintf( error_text,
      "Gethdt: During reading of data file header.");
  }

  return nefis_errno;
}
/*==========================================================================*/
/*
 * Get header from definition file
 */

DLLEXPORT BInt4 FTN_CALL GET_DEF_HEADER( BInt4 * set          ,
                      BText   header       ,
                      BInt4   header_length)
{
  BUInt8 n_read = 0;

  nefis_errno = 0;

  if ( header_length < LHDRDF )
  {
    nefis_errno   = 2007;
      sprintf( error_text,
        "Gethdf: Supplied character string too small for header %ld<%ld\n",
        header_length, LHDRDF);
    return nefis_errno;
  }

  n_read = GP_read_file ( nefis[*set].def_fds, header, 0, LHDRDF);
  if ( n_read == -1 )
  {
    nefis_errno   = 2008;
    sprintf( error_text,
      "Gethdf: Unable to read definition file header (file write only?).");
    return nefis_errno;
  }
  if ( n_read > LHDRDF )
  {
    nefis_errno   = 2009;
    sprintf( error_text,
      "Gethdf: During reading of definition file header.");
    return nefis_errno;
  }

  return nefis_errno;
}
/*==========================================================================*/
/*
 * Retrieve element from data file
 * Input:  * fd                Fortran gives pointer to deffds array
 *         gr_name             Group name on data file
 *         el_name             Element name to write data to
 *         usr_index
 *         usr_order
 *         getal
 * Output: -
 * Return: 0                   No error occured
 *         !=0                 Error occured
 */
#if defined(MIXED_STR_LEN_ARG)
DLLEXPORT BInt4 FTN_CALL GET_ELEMENT( BInt4 * fd               ,
                           BText   gr_name          ,
                           BInt4   gr_name_length   ,
                           BText   el_name          ,
                           BInt4   el_name_length   ,
                           BInt4 * user_index       ,
                           BInt4 * user_order       ,
                           BInt4 * buffer_length    ,
                           BData   buffer           )

#else
DLLEXPORT BInt4 FTN_CALL GET_ELEMENT( BInt4 * fd               ,
                           BText   gr_name          ,
                           BText   el_name          ,
                           BInt4 * user_index       ,
                           BInt4 * user_order       ,
                           BInt4 * buffer_length    ,
                           BData   buffer           ,
                           BInt4   gr_name_length   ,
                           BInt4   el_name_length   )
#endif
{
  BChar   elm_name[MAX_NAME + 1];
  BChar   grp_name[MAX_NAME + 1];
  BInt4   i           ;
  BInt4   j           ;
  BInt4   usr_index[MAX_DIM * DEFINE_STEPS];
  BInt4   usr_order[MAX_DIM    ];

  nefis_errno = 0;

  F_Copy_text (elm_name    , el_name    , el_name_length    , MAX_NAME);
  F_Copy_text (grp_name    , gr_name    , gr_name_length    , MAX_NAME);

  for ( i=0; i<MAX_DIM     ; i++ )
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

  nefis_errno = Get_element ( *fd, grp_name   , elm_name     , usr_index,
                                   usr_order  ,*buffer_length, buffer   );

  return nefis_errno;
}
/*==========================================================================*/
/*
 * Retrieve element from data file
 * Output: -
 * Return: 0                   No error occured
 *         !=0                 Error occured
 */
#if defined(MIXED_STR_LEN_ARG)
DLLEXPORT BInt4 FTN_CALL GET_ELEMENT_STRING ( BInt4 * fd               ,
                                   BText   gr_name          ,
                                   BInt4   gr_name_length   ,
                                   BText   el_name          ,
                                   BInt4   el_name_length   ,
                                   BInt4 * user_index       ,
                                   BInt4 * user_order       ,
                                   BInt4 * buffer_length    ,
                                   BData   buffer           ,
                                   BInt4   bffr_length      )
#else
DLLEXPORT BInt4 FTN_CALL GET_ELEMENT_STRING ( BInt4 * fd               ,
                                   BText   gr_name          ,
                                   BText   el_name          ,
                                   BInt4 * user_index       ,
                                   BInt4 * user_order       ,
                                   BInt4 * buffer_length    ,
                                   BData   buffer           ,
                                   BInt4   gr_name_length   ,
                                   BInt4   el_name_length   ,
                                   BInt4   bffr_length      )
#endif
{
  nefis_errno = 0;
  NOREFF(bffr_length);

#if defined(MIXED_STR_LEN_ARG)
  nefis_errno = GET_ELEMENT       (  fd               ,
                                     gr_name          ,
                                     gr_name_length   ,
                                     el_name          ,
                                     el_name_length   ,
                                     user_index       ,
                                     user_order       ,
                                     buffer_length    ,
                                     buffer           );
#else
  nefis_errno = GET_ELEMENT        ( fd               ,
                                     gr_name          ,
                                     el_name          ,
                                     user_index       ,
                                     user_order       ,
                                     buffer_length    ,
                                     buffer           ,
                                     gr_name_length   ,
                                     el_name_length   );
#endif

  return nefis_errno;
}
/*==========================================================================*/
/*
 * Get integer attribute on data file
 */
#if defined(MIXED_STR_LEN_ARG)
DLLEXPORT BInt4 FTN_CALL GET_INT_ATTRIBUTE    ( BInt4 * fd            ,
                                     BText   gr_name       ,
                                     BInt4   gr_name_length,
                                     BText   at_name       ,
                                     BInt4   at_name_length,
                                     BInt4 * at_value      )
#else
DLLEXPORT BInt4 FTN_CALL GET_INT_ATTRIBUTE    ( BInt4 * fd            ,
                                     BText   gr_name       ,
                                     BText   at_name       ,
                                     BInt4 * at_value      ,
                                     BInt4   gr_name_length,
                                     BInt4   at_name_length)
#endif
{
  BChar   att_name[MAX_NAME+1];
  BChar   att_type[MAX_TYPE+1];
  BChar   grp_name[MAX_NAME+1];
  BInt4   next=0  ;

  nefis_errno = 0;

  F_Copy_text (grp_name    , gr_name    , gr_name_length    , MAX_NAME);
  F_Copy_text (att_name    , at_name    , at_name_length    , MAX_NAME);

  strcpy(att_type, "INTEGER");

  nefis_errno = GP_get_attribute(*fd      , next    , grp_name,
                                  att_name, at_value, att_type);

  return nefis_errno;
}
/*==========================================================================*/
/*
 * Get integer attribute on data file
 */
#if defined(MIXED_STR_LEN_ARG)
DLLEXPORT BInt4 FTN_CALL GET_REAL_ATTRIBUTE   ( BInt4 * fd             ,
                                     BText   gr_name        ,
                                     BInt4   gr_name_length ,
                                     BText   at_name        ,
                                     BInt4   at_name_length ,
                                     BRea4 * at_value       )
#else
DLLEXPORT BInt4 FTN_CALL GET_REAL_ATTRIBUTE   ( BInt4 * fd             ,
                                     BText   gr_name        ,
                                     BText   at_name        ,
                                     BRea4 * at_value       ,
                                     BInt4   gr_name_length ,
                                     BInt4   at_name_length )
#endif
{
  BChar   att_name[MAX_NAME+1];
  BChar   att_type[MAX_TYPE+1];
  BChar   grp_name[MAX_NAME+1];
  BInt4   next=0  ;

  nefis_errno = 0;

  F_Copy_text (grp_name    , gr_name    , gr_name_length    , MAX_NAME);
  F_Copy_text (att_name    , at_name    , at_name_length    , MAX_NAME);

  strcpy(att_type, "REAL");

  nefis_errno = GP_get_attribute(*fd      , next    , grp_name,
                                  att_name, at_value, att_type);

  return nefis_errno;
}
/*==========================================================================*/
/*
 * Get integer attribute on data file
 */
#if defined(MIXED_STR_LEN_ARG)
DLLEXPORT BInt4 FTN_CALL GET_STRING_ATTRIBUTE ( BInt4 * fd             ,
                                     BText   gr_name        ,
                                     BInt4   gr_name_length ,
                                     BText   at_name        ,
                                     BInt4   at_name_length ,
                                     BText   at_value       ,
                                     BInt4   at_value_length)
#else
DLLEXPORT BInt4 FTN_CALL GET_STRING_ATTRIBUTE ( BInt4 * fd             ,
                                     BText   gr_name        ,
                                     BText   at_name        ,
                                     BText   at_value       ,
                                     BInt4   gr_name_length ,
                                     BInt4   at_name_length ,
                                     BInt4   at_value_length)
#endif
{
  BChar   att_name [MAX_NAME+1];
  BChar   att_type [MAX_TYPE+1];
  BChar   grp_name [MAX_NAME+1];
  BInt4   next=0  ;

  nefis_errno = 0;

  if ( at_value_length < MAX_NAME )
  {
    nefis_errcnt += 1;
    nefis_errno   = 2011;
    sprintf( error_text,
      "Getsat: User supplied attribute string too small %ld<%ld\n",
      at_value_length, MAX_NAME);
    return nefis_errno;
  }

  F_Copy_text (grp_name    , gr_name    , gr_name_length    , MAX_NAME);
  F_Copy_text (att_name    , at_name    , at_name_length    , MAX_NAME);

  strcpy(att_type, "CHARACTE");

  nefis_errno = GP_get_attribute(*fd      , next    , grp_name,
                                  att_name, at_value, att_type);

  return nefis_errno;
}
/*==========================================================================*/
/*
 * Read cel definition from defintion file
 */

#if defined(MIXED_STR_LEN_ARG)
DLLEXPORT BInt4 FTN_CALL INQUIRE_CEL         ( BInt4 * fd              ,
                                    BText   cl_name        ,
                                    BInt4   cl_name_length ,
                                    BInt4 * cl_num_dim     ,
                                    BText   el_names       ,
                                    BInt4   el_names_length)
#else
DLLEXPORT BInt4 FTN_CALL INQUIRE_CEL         ( BInt4 * fd              ,
                                    BText   cl_name        ,
                                    BInt4 * cl_num_dim     ,
                                    BText   el_names       ,
                                    BInt4   cl_name_length ,
                                    BInt4   el_names_length)
#endif
{
  BChar   cel_name  [ MAX_NAME+1 ];
  BUInt8  cel_num_bytes=0;
  BUInt4  cel_num_dim  = MAX_DIM;
  BText   elm_names;
  BUInt4  i       ;

  nefis_errno = 0;
  elm_names = NULL;

  F_Copy_text (cel_name    , cl_name    , cl_name_length    , MAX_NAME);

/* TODO: how to check, the supplied memory is enough */

  if ( el_names_length < MAX_NAME )
  {
    nefis_errcnt += 1;
    nefis_errno   = 2012;
    sprintf(error_text,
      "Inqcel: Supplied array too small to contain all element names: \'%s\' %ld<%d\n",
                    cel_name, el_names_length, MAX_NAME);
    return nefis_errno;
  }

  nefis_errno = GP_inquire_cel(*fd       , cel_name     ,&cel_num_dim,
                               &elm_names,&cel_num_bytes);
  if ( nefis_errno == 0 )
  {
    if ( (BUInt4) *cl_num_dim < cel_num_dim )
    {
    nefis_errcnt += 1;
      nefis_errno   = 2013;
      sprintf(error_text,
      "Inqcel: User supplied array too small to contain Cell properties: \'%s\' %ld<%ld \n",
                    cel_name, *cl_num_dim, cel_num_dim);
      return nefis_errno;
    }

    for ( i=0; i<cel_num_dim; i++ )
    {
      strncpy( &el_names[i*MAX_NAME], &elm_names[i*(MAX_NAME+1)], MAX_NAME);
    }
    *cl_num_dim = cel_num_dim;
  }

  return nefis_errno;
}
/*==========================================================================*/
/*
 * Read group name from data file
 */

#if defined(MIXED_STR_LEN_ARG)
DLLEXPORT BInt4 FTN_CALL INQUIRE_DATA_GROUP  ( BInt4 * fd               ,
                                    BText   gr_name          ,
                                    BInt4   gr_name_length   ,
                                    BText   gr_defined       ,
                                    BInt4   gr_defined_length)
#else
DLLEXPORT BInt4 FTN_CALL INQUIRE_DATA_GROUP  ( BInt4 * fd               ,
                                    BText   gr_name          ,
                                    BText   gr_defined       ,
                                    BInt4   gr_name_length   ,
                                    BInt4   gr_defined_length)
#endif
{
  BUInt8  grp_pointer = NIL;
  BChar   grp_name   [MAX_NAME+1];
  BChar   grp_defined[MAX_NAME+1];

  nefis_errno = 0;

  if ( gr_defined_length < MAX_NAME )
  {
    nefis_errcnt += 1;
    nefis_errno   = 2014;
    sprintf(error_text,
        "Inqdat: User supplied array to store group definition too small: %ld<%ld \n",
                     gr_defined_length, MAX_NAME);
    return nefis_errno;
  }

  F_Copy_text (grp_name    , gr_name    , gr_name_length    , MAX_NAME);

  nefis_errno = GP_inquire_dat(*fd      , &grp_pointer,
                                grp_name,  grp_defined);

  if ( nefis_errno == 0 )
  {
    strncpy(gr_defined , grp_defined, MAX_NAME);
  }

  return nefis_errno;
}
/*==========================================================================*/
/*
 * Read element definition from defintion file
 */

#if defined(MIXED_STR_LEN_ARG)
DLLEXPORT BInt4 FTN_CALL INQUIRE_ELEMENT     ( BInt4 * fd             ,   /* I */
                                    BText   el_name        ,   /* I */
                                    BUInt4  el_name_length ,   /* I */
                                    BText   el_type        ,   /* O */
                                    BUInt4  el_type_length ,   /* I */
                                    BInt4 * el_single_bytes,   /* O */
                                    BText   el_quantity    ,   /* O */
                                    BUInt4  el_quantity_length,/* I */
                                    BText   el_unity       ,   /* O */
                                    BUInt4  el_unity_length,   /* I */
                                    BText   el_desc        ,   /* O */
                                    BUInt4  el_desc_length ,   /* I */
                                    BInt4 * el_num_dim     ,   /* O */
                                    BInt4 * el_dimens      )   /* O */
#else
DLLEXPORT BInt4 FTN_CALL INQUIRE_ELEMENT     ( BInt4 * fd             ,   /* I */
                                    BText   el_name        ,   /* I */
                                    BText   el_type        ,   /* O */
                                    BInt4 * el_single_bytes,   /* O */
                                    BText   el_quantity    ,   /* O */
                                    BText   el_unity       ,   /* O */
                                    BText   el_desc        ,   /* O */
                                    BInt4 * el_num_dim     ,   /* O */
                                    BInt4 * el_dimens      ,   /* O */
                                    BUInt4  el_name_length ,   /* I */
                                    BUInt4  el_type_length ,   /* I */
                                    BUInt4  el_quantity_length,/* I */
                                    BUInt4  el_unity_length,   /* I */
                                    BUInt4  el_desc_length )   /* I */
#endif
{
  BChar   elm_name    [MAX_NAME+1];
  BText   elm_name_tmp            ;
  BChar   elm_type    [MAX_TYPE+1];
  BUInt4  elm_single_bytes;
  BChar   elm_quantity[MAX_NAME+1];
  BChar   elm_unity   [MAX_NAME+1];
  BChar   elm_desc    [MAX_DESC+1];
  BInt4   elm_dimens  [MAX_DIM]   ;
  BUInt8  elm_num_bytes=0;
  BUInt4  elm_num_dim    ;
  BUInt4  i       ;

  nefis_errno = 0;

  if ( ( el_type_length     < MAX_TYPE ) ||
       ( el_quantity_length < MAX_NAME ) ||
       ( el_unity_length    < MAX_NAME ) ||
       ( el_desc_length     < MAX_DESC )    )
  {
    nefis_errcnt += 1;
    nefis_errno   = 2015;
    sprintf(error_text,
        "Inqelm: User supplied array's to store element definition too small: %s, %ld, %ld<%ld, %ld<%ld, %ld<%ld, %ld<%ld\n",
        el_name, el_name_length, el_type_length, MAX_TYPE, el_quantity_length, MAX_NAME, el_unity_length, MAX_NAME, el_desc_length, MAX_DESC);
    return nefis_errno;
  }
  if ( el_name_length   > MAX_NAME    )
  {
    elm_name_tmp = strFcpy(el_name, el_name_length);

    nefis_errcnt += 1;
    nefis_errno   = 2016;
    sprintf(error_text,
      "Inqelm: Element name \'%s\' too long", elm_name_tmp);
    free( elm_name_tmp );
    return nefis_errno;
  }

  F_Copy_text (elm_name    , el_name    , el_name_length    , MAX_NAME);

  nefis_errno = GP_inquire_elm(*fd           , elm_name   , elm_type        ,
                                elm_quantity , elm_unity  , elm_desc        ,
                               &elm_num_dim  , (BUInt4 *) elm_dimens ,&elm_single_bytes,
                               &elm_num_bytes);

  if ( nefis_errno == 0 )
  {
    strncpy( el_type    , elm_type    , MAX_TYPE);
    for (i=MAX_TYPE; i<el_type_length    ; i++) { el_type[i]     = ' '; }

    strncpy( el_quantity, elm_quantity, MAX_NAME);
    for (i=MAX_NAME; i<el_quantity_length; i++) { el_quantity[i] = ' '; }

    strncpy( el_unity   , elm_unity   , MAX_NAME);
    for (i=MAX_NAME; i<el_unity_length   ; i++) { el_unity[i]    = ' '; }

    strncpy( el_desc    , elm_desc    , MAX_DESC);
    for (i=MAX_DESC; i<el_desc_length    ; i++) { el_desc[i]     = ' '; }

    if ( (BUInt4) *el_num_dim < elm_num_dim)
    {
      nefis_errcnt += 1;
      nefis_errno   = 2017;
      sprintf(error_text,
        "Inqelm: User supplied array to contain element dimensions too small (check value of elmndm): %ld<%ld \n",
                    *el_num_dim, elm_num_dim);
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
 * Get first integer attribute on data file
 * input
 * fd     : NEFIS file set descriptor
 *
 * output
 * gr_name   : first/next groupname on data file
 * gr_defined: corresponding groupname (defined) on definition file
 *
 * return values:   0    No error occured
 *                  !=0  Error occured
 */
#if defined(MIXED_STR_LEN_ARG)
DLLEXPORT BInt4 FTN_CALL INQUIRE_FIRST_DAT_GROUP  ( BInt4 * fd            ,
                                         BText   gr_name       ,
                                         BInt4   gr_name_length,
                                         BText   gr_defined    ,
                                         BInt4   gr_defined_length)
#else
DLLEXPORT BInt4 FTN_CALL INQUIRE_FIRST_DAT_GROUP  ( BInt4 * fd            ,
                                         BText   gr_name       ,
                                         BText   gr_defined    ,
                                         BInt4   gr_name_length,
                                         BInt4   gr_defined_length)
#endif
{
  BChar   grp_name   [MAX_NAME+1];
  BChar   grp_defined[MAX_NAME+1];
  BInt4   i      ;

  nefis_errno = 0;

  if ( ( gr_name_length    < MAX_NAME ) ||
       ( gr_defined_length < MAX_NAME )    )
  {
    nefis_errcnt += 1;
    nefis_errno   = 2018;
    sprintf(error_text,
      "Inqfst: User supplied array to contain names too small: %ld<%ld, %ld<%ld\n",
       gr_name_length, MAX_NAME, gr_defined_length, MAX_NAME );
    return nefis_errno;
  }

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
#if defined(MIXED_STR_LEN_ARG)
DLLEXPORT BInt4 FTN_CALL INQUIRE_FIRST_ELEMENT( BInt4 * fd                 ,/* I */
                                     BText   el_name            ,/* O */
                                     BInt4   el_name_length     ,/* I */
                                     BText   el_type            ,/* O */
                                     BInt4   el_type_length     ,/* I */
                                     BText   el_quantity        ,/* O */
                                     BInt4   el_quantity_length ,/* I */
                                     BText   el_unity           ,/* O */
                                     BInt4   el_unity_length    ,/* I */
                                     BText   el_desc            ,/* O */
                                     BInt4   el_desc_length     ,/* I */
                                     BInt4 * el_single_bytes    ,/* O */
                                     BInt4 * el_num_bytes       ,/* O */
                                     BInt4 * el_num_dim         ,/* O */
                                     BInt4 * el_dimens          )/* O */
#else
DLLEXPORT BInt4 FTN_CALL INQUIRE_FIRST_ELEMENT( BInt4 * fd                 ,/* I */
                                     BText   el_name            ,/* O */
                                     BText   el_type            ,/* O */
                                     BText   el_quantity        ,/* O */
                                     BText   el_unity           ,/* O */
                                     BText   el_desc            ,/* O */
                                     BInt4 * el_single_bytes    ,/* O */
                                     BInt4 * el_num_bytes       ,/* O */
                                     BInt4 * el_num_dim         ,/* O */
                                     BInt4 * el_dimens          ,/* O */
                                     BInt4   el_name_length     ,/* I */
                                     BInt4   el_type_length     ,/* I */
                                     BInt4   el_quantity_length ,/* I */
                                     BInt4   el_unity_length    ,/* I */
                                     BInt4   el_desc_length     )/* I */
#endif
{
  BChar   elm_name    [MAX_NAME+1];
  BChar   elm_type    [MAX_TYPE+1];
  BInt4   elm_single_bytes = 0    ;
  BChar   elm_quantity[MAX_NAME+1];
  BChar   elm_unity   [MAX_NAME+1];
  BChar   elm_desc    [MAX_DESC+1];
  BInt4   elm_dimens  [MAX_DIM]   ;
  BInt4   elm_num_bytes = 0       ;
  BInt4   elm_num_dim   = 0       ;
  BInt4   i       ;

  nefis_errno = 0;

  if ( ( el_name_length     < MAX_NAME ) ||
       ( el_type_length     < MAX_TYPE ) ||
       ( el_quantity_length < MAX_NAME ) ||
       ( el_unity_length    < MAX_NAME ) ||
       ( el_desc_length     < MAX_DESC )    )
  {
    nefis_errcnt += 1;
    nefis_errno   = 2019;
    sprintf(error_text,
        "Inqelm: User supplied array's to store element definition too small: %ld<%ld, %ld<%ld, %ld<%ld, %ld<%ld, %ld<%ld \n",
            el_name_length,MAX_NAME, el_type_length, MAX_TYPE, el_quantity_length, MAX_NAME, el_unity_length, MAX_NAME, el_desc_length, MAX_DESC);
    return nefis_errno;
  }
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

  nefis_errno = GP_get_next_elm (*fd              , 0               ,
                                  elm_name        , elm_type        ,
                                  elm_quantity    , elm_unity       ,
                                  elm_desc        ,&elm_num_dim     ,
                                  elm_dimens      ,&elm_single_bytes,
                                 &elm_num_bytes   );

  if ( nefis_errno == 0 )
  {
    strncpy( el_name    , elm_name    , MAX_NAME);
    strncpy( el_type    , elm_type    , MAX_TYPE);
    strncpy( el_quantity, elm_quantity, MAX_NAME);
    strncpy( el_unity   , elm_unity   , MAX_NAME);
    strncpy( el_desc    , elm_desc    , MAX_DESC);

    if ( *el_num_dim < elm_num_dim)
    {
      nefis_errcnt += 1;
      nefis_errno   = 2020;
      sprintf(error_text,
        "Inqfel: User supplied array to contain element names too small: %ld<%ld \n",
            *el_num_dim, elm_num_dim);
      return nefis_errno;
    }

    *el_single_bytes = elm_single_bytes;
    *el_num_bytes    = elm_num_bytes   ;
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
/*==========================================================================*/
#if defined(MIXED_STR_LEN_ARG)
DLLEXPORT BInt4 FTN_CALL INQUIRE_NEXT_ELEMENT ( BInt4 * fd                 ,/* I */
                                     BText   el_name            ,/* O */
                                     BInt4   el_name_length     ,/* I */
                                     BText   el_type            ,/* O */
                                     BInt4   el_type_length     ,/* I */
                                     BText   el_quantity        ,/* O */
                                     BInt4   el_quantity_length ,/* I */
                                     BText   el_unity           ,/* O */
                                     BInt4   el_unity_length    ,/* I */
                                     BText   el_desc            ,/* O */
                                     BInt4   el_desc_length     ,/* I */
                                     BInt4 * el_single_bytes    ,/* O */
                                     BInt4 * el_num_bytes       ,/* O */
                                     BInt4 * el_num_dim         ,/* O */
                                     BInt4 * el_dimens          )/* O */
#else
DLLEXPORT BInt4 FTN_CALL INQUIRE_NEXT_ELEMENT ( BInt4 * fd                 ,/* I */
                                     BText   el_name            ,/* O */
                                     BText   el_type            ,/* O */
                                     BText   el_quantity        ,/* O */
                                     BText   el_unity           ,/* O */
                                     BText   el_desc            ,/* O */
                                     BInt4 * el_single_bytes    ,/* O */
                                     BInt4 * el_num_bytes       ,/* O */
                                     BInt4 * el_num_dim         ,/* O */
                                     BInt4 * el_dimens          ,/* O */
                                     BInt4   el_name_length     ,/* I */
                                     BInt4   el_type_length     ,/* I */
                                     BInt4   el_quantity_length ,/* I */
                                     BInt4   el_unity_length    ,/* I */
                                     BInt4   el_desc_length     )/* I */
#endif
{
  BChar   elm_name    [MAX_NAME+1];
  BChar   elm_type    [MAX_TYPE+1];
  BInt4   elm_single_bytes = 0    ;
  BChar   elm_quantity[MAX_NAME+1];
  BChar   elm_unity   [MAX_NAME+1];
  BChar   elm_desc    [MAX_DESC+1];
  BInt4   elm_dimens  [MAX_DIM]   ;
  BInt4   elm_num_bytes = 0       ;
  BInt4   elm_num_dim   = 0       ;
  BInt4   i       ;

  nefis_errno = 0;

  if ( ( el_name_length     < MAX_NAME ) ||
       ( el_type_length     < MAX_TYPE ) ||
       ( el_quantity_length < MAX_NAME ) ||
       ( el_unity_length    < MAX_NAME ) ||
       ( el_desc_length     < MAX_DESC )    )
  {
    nefis_errcnt += 1;
    nefis_errno   = 2021;
    sprintf(error_text,
        "Inqelm: User supplied array's to store element definition too small: %ld<%ld, %ld<%ld, %ld<%ld, %ld<%ld, %ld<%ld \n",
        el_name_length,MAX_NAME, el_type_length, MAX_TYPE, el_quantity_length, MAX_NAME, el_unity_length, MAX_NAME, el_desc_length, MAX_DESC);
    return nefis_errno;
  }
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

  nefis_errno = GP_get_next_elm (*fd              , 1               ,
                                  elm_name        , elm_type        ,
                                  elm_quantity    , elm_unity       ,
                                  elm_desc        ,&elm_num_dim     ,
                                  elm_dimens      ,&elm_single_bytes,
                                 &elm_num_bytes   );

  if ( nefis_errno == 0 )
  {
    strncpy( el_name    , elm_name    , MAX_NAME);
    strncpy( el_type    , elm_type    , MAX_TYPE);
    strncpy( el_quantity, elm_quantity, MAX_NAME);
    strncpy( el_unity   , elm_unity   , MAX_NAME);
    strncpy( el_desc    , elm_desc    , MAX_DESC);

    if ( *el_num_dim < elm_num_dim)
    {
        nefis_errcnt += 1;
        nefis_errno   = 2022;
        sprintf(error_text,
            "Inqfel: User supplied array to contain element names too small: %ld<%ld \n",
            *el_num_dim, elm_num_dim);
        return nefis_errno;
    }

    *el_single_bytes = elm_single_bytes;
    *el_num_bytes    = elm_num_bytes   ;
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
#if defined(MIXED_STR_LEN_ARG)
DLLEXPORT BInt4 FTN_CALL INQUIRE_FIRST_CELL   ( BInt4 * fd                     , /* I */
                                     BText   cl_name                , /* O */
                                     BInt4   cl_name_length         , /* I */
                                     BInt4 * cl_num_dim             , /* O */
                                     BInt4 * cl_num_bytes           , /* O */
                                     BChar   el_names [][MAX_NAME+1], /* O */
                                     BInt4   el_names_length        ) /* I */
#else
DLLEXPORT BInt4 FTN_CALL INQUIRE_FIRST_CELL   ( BInt4 * fd                     , /* I */
                                     BText   cl_name                , /* O */
                                     BInt4 * cl_num_dim             , /* O */
                                     BInt4 * cl_num_bytes           , /* O */
                                     BChar   el_names [][MAX_NAME+1], /* O */
                                     BInt4   cl_name_length         , /* I */
                                     BInt4   el_names_length        ) /* I */
#endif
{
  BUInt8  cel_num_bytes = 0;
  BUInt4  cel_num_dim   = 0;
  BChar   cel_name    [MAX_NAME + 1];
  BText   elm_names;
  BUInt4  i;

  NOREFF(cl_name_length);

  nefis_errno = 0;
  elm_names = NULL;

  if ( el_names_length < MAX_NAME )
  {
    nefis_errcnt += 1;
    nefis_errno   = 2023;
    sprintf(error_text,
      "Inqfcl: Supplied array too small to contain all element names: \'%s\' %ld>%d \n",
                    cl_name, el_names_length, MAX_NAME);
    return nefis_errno;
  }
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
                                  cel_name        , elm_names      ,
                                 &cel_num_dim     ,&cel_num_bytes  );
  if ( nefis_errno == 0 )
  {
    if ( cel_num_dim > (BUInt4) *cl_num_dim )
    {
      nefis_errcnt += 1;
      nefis_errno   = 2024;
      sprintf(error_text,
      "Inqfcl: User supplied array too small to contain Cell properties: \'%s\' %ld>%ld \n",
                    cel_name, cel_num_dim, *cl_num_dim);
      return nefis_errno;
    }

    *cl_num_dim   = (BUInt4) cel_num_dim;
    *cl_num_bytes = (BUInt4) cel_num_bytes   ;

    strncpy(cl_name    , cel_name    , MAX_NAME);
    cel_name    [MAX_TYPE] = '\0';

    for ( i=0; i<cel_num_dim; i++ )
    {
      strcpy( &el_names[i][MAX_NAME+1], &elm_names[i*(MAX_NAME+1)] );
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
#if defined(MIXED_STR_LEN_ARG)
DLLEXPORT BInt4 FTN_CALL INQUIRE_NEXT_CELL    ( BInt4 * fd                     , /* I */
                                     BText   cl_name                , /* O */
                                     BInt4   cl_name_length         , /* I */
                                     BInt4 * cl_num_dim             , /* O */
                                     BInt4 * cl_num_bytes           , /* O */
                                     BChar   el_names [][MAX_NAME+1], /* O */
                                     BInt4   el_names_length)         /* I */
#else
DLLEXPORT BInt4 FTN_CALL INQUIRE_NEXT_CELL    ( BInt4 * fd                     , /* I */
                                     BText   cl_name                , /* O */
                                     BInt4 * cl_num_dim             , /* O */
                                     BInt4 * cl_num_bytes           , /* O */
                                     BChar   el_names [][MAX_NAME+1], /* O */
                                     BInt4   cl_name_length         , /* I */
                                     BInt4   el_names_length)         /* I */
#endif
{
  BUInt8  cel_num_bytes = 0;
  BUInt4  cel_num_dim = 0           ;
  BChar   cel_name    [MAX_NAME + 1];
  BText   elm_names                 ;
  BUInt4  i                         ;

  NOREFF(cl_name_length);

  nefis_errno = 0;
  elm_names   = NULL;

  if ( el_names_length < MAX_NAME )
  {
    nefis_errcnt += 1;
    nefis_errno   = 2025;
    sprintf(error_text,
      "Inqfcl: Supplied array too small to contain all element names: \'%s\' %ld>%d \n",
                    cl_name, el_names_length, MAX_NAME);
    return nefis_errno;
  }
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
                                  cel_name        , elm_names      ,
                                 &cel_num_dim     ,&cel_num_bytes  );
  if ( nefis_errno == 0 )
  {
    if ( cel_num_dim > (BUInt4) *cl_num_dim )
    {
      nefis_errcnt += 1;
      nefis_errno   = 2026;
      sprintf(error_text,
      "Inqncl: User supplied array too small to contain Cell properties: \'%s\' %ld>%ld \n",
                    cel_name, cel_num_dim, *cl_num_dim);
      return nefis_errno;
    }

    *cl_num_dim   = (BUInt4) cel_num_dim;
    *cl_num_bytes = (BUInt4) cel_num_bytes   ;

    strncpy(cl_name    , cel_name    , MAX_NAME);
    cel_name    [MAX_TYPE] = '\0';

    for ( i=0; i<cel_num_dim; i++ )
    {
      strcpy( &el_names[i][MAX_NAME+1], &elm_names[i*(MAX_NAME+1)] );
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
#if defined(MIXED_STR_LEN_ARG)
DLLEXPORT BInt4 FTN_CALL INQUIRE_FIRST_DEF_GROUP ( BInt4 * fd             ,/* I */
                                        BText   gr_name        ,/* O */
                                        BInt4   gr_name_length ,/* I */
                                        BText   cl_name        ,/* O */
                                        BInt4   cl_name_length ,/* I */
                                        BInt4 * gr_num_dim     ,/* O */
                                        BInt4 * gr_dimens      ,/* O */
                                        BInt4 * gr_order       )/* O */
#else
DLLEXPORT BInt4 FTN_CALL INQUIRE_FIRST_DEF_GROUP ( BInt4 * fd             ,/* I */
                                        BText   gr_name        ,/* O */
                                        BText   cl_name        ,/* O */
                                        BInt4 * gr_num_dim     ,/* O */
                                        BInt4 * gr_dimens      ,/* O */
                                        BInt4 * gr_order       ,/* O */
                                        BInt4   gr_name_length ,/* I */
                                        BInt4   cl_name_length )/* I */
#endif
{
  BChar   cel_name    [MAX_NAME + 1];
  BInt4   grp_dimens  [MAX_DIM];
  BInt4   grp_num_dim = 0           ;
  BChar   grp_name    [MAX_NAME + 1];
  BInt4   grp_order   [MAX_DIM];
  BInt4   i                         ;

  nefis_errno = 0;

  if ( ( gr_name_length < MAX_NAME ) ||
       ( cl_name_length < MAX_NAME )    )
  {
    nefis_errcnt += 1;
    nefis_errno   = 2027;
    sprintf(error_text,
        "Inqfgr: User supplied array to contain names too small: %ld<%ld, %ld<%ld \n",
         gr_name_length, MAX_NAME, cl_name_length, MAX_NAME);
    return nefis_errno;
  }
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
      nefis_errno   = 2028;
      sprintf(error_text,
      "Inqfgr: User supplied array too small to contain group properties: \'%s\' %ld>%ld \n",
                    grp_name, grp_num_dim, *gr_num_dim);
      return nefis_errno;
    }

    strncpy(cl_name    , cel_name    , MAX_NAME);
    cel_name    [MAX_TYPE] = '\0';

    strncpy(gr_name    , grp_name    , MAX_NAME);
    grp_name    [MAX_TYPE] = '\0';

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
#if defined(MIXED_STR_LEN_ARG)
DLLEXPORT BInt4 FTN_CALL INQUIRE_NEXT_DEF_GROUP  ( BInt4 * fd             ,/* I */
                                        BText   gr_name        ,/* O */
                                        BInt4   gr_name_length ,/* I */
                                        BText   cl_name        ,/* O */
                                        BInt4   cl_name_length ,/* I */
                                        BInt4 * gr_num_dim     ,/* O */
                                        BInt4 * gr_dimens      ,/* O */
                                        BInt4 * gr_order       )/* O */
#else
DLLEXPORT BInt4 FTN_CALL INQUIRE_NEXT_DEF_GROUP  ( BInt4 * fd             ,/* I */
                                        BText   gr_name        ,/* O */
                                        BText   cl_name        ,/* O */
                                        BInt4 * gr_num_dim     ,/* O */
                                        BInt4 * gr_dimens      ,/* O */
                                        BInt4 * gr_order       ,/* O */
                                        BInt4   gr_name_length ,/* I */
                                        BInt4   cl_name_length )/* I */
#endif
{
  BChar   cel_name    [MAX_NAME + 1];
  BInt4   grp_dimens  [MAX_DIM];
  BInt4   grp_num_dim = 0           ;
  BChar   grp_name    [MAX_NAME + 1];
  BInt4   grp_order   [MAX_DIM];
  BInt4   i                         ;

  nefis_errno = 0;

  if ( ( gr_name_length < MAX_NAME ) ||
       ( cl_name_length < MAX_NAME )    )
  {
    nefis_errcnt += 1;
    nefis_errno   = 2029;
    sprintf(error_text,
      "Inqfgr: User supplied array to contain names too small: %ld<%ld, %ld<%ld \n",
        gr_name_length, MAX_NAME, cl_name_length, MAX_NAME);
    return nefis_errno;
  }
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
      nefis_errno   = 2030;
      sprintf(error_text,
      "Inqngr: User supplied array too small to contain group properties: \'%s\' %ld>%ld \n",
                    grp_name, grp_num_dim, *gr_num_dim);
      return nefis_errno;
    }

    strncpy(cl_name    , cel_name    , MAX_NAME);
    cel_name    [MAX_TYPE] = '\0';

    strncpy(gr_name    , grp_name    , MAX_NAME);
    grp_name    [MAX_TYPE] = '\0';

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
 *             gr_defined: corresponding groupname (defined)
 *                         on definition file
 * output:   * at_value  : value of the attribute
 *
 * return:   0    No error occured
 *           !=0  Error occured
 */
#if defined(MIXED_STR_LEN_ARG)
DLLEXPORT BInt4 FTN_CALL INQUIRE_FIRST_INTEGER( BInt4 * fd            ,
                                     BText   gr_name       ,
                                     BInt4   gr_name_length,
                                     BText   at_name       ,
                                     BInt4   at_name_length,
                                     BInt4 * at_value      )
#else
DLLEXPORT BInt4 FTN_CALL INQUIRE_FIRST_INTEGER( BInt4 * fd            ,
                                     BText   gr_name       ,
                                     BText   at_name       ,
                                     BInt4 * at_value      ,
                                     BInt4   gr_name_length,
                                     BInt4   at_name_length)
#endif
{
  BChar   att_name[MAX_NAME+1];
  BChar   att_type[MAX_TYPE+1];
  BChar   grp_name[MAX_NAME+1];
  BInt4   i;

  nefis_errno = 0;

  if ( at_name_length    < MAX_NAME )
  {
    nefis_errcnt += 1;
    nefis_errno   = 2031;
    sprintf(error_text,
      "Inqfia: User supplied array to contain integer attribute names too small: %ld<%ld\n",
      at_name_length, MAX_NAME);
    return nefis_errno;
  }

  F_Copy_text (grp_name    , gr_name    , gr_name_length    , MAX_NAME);

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
  }

  return nefis_errno;
}
/*==========================================================================*/
/*
 * Get first real attribute on data file
 *
 * input:      fd        : NEFIS file set descriptor
 *           * gr_name   : first/next groupname on data file
 *             gr_defined: corresponding groupname (defined)
 *                         on definition file
 * output:   * at_value  : value of the attribute
 *
 * return:   0    No error occured
 *           !=0  Error occured
 */
#if defined(MIXED_STR_LEN_ARG)
DLLEXPORT BInt4 FTN_CALL INQUIRE_FIRST_REAL   ( BInt4 * fd            ,
                                     BText   gr_name       ,
                                     BInt4   gr_name_length,
                                     BText   at_name       ,
                                     BInt4   at_name_length,
                                     BRea4 * at_value      )
#else
DLLEXPORT BInt4 FTN_CALL INQUIRE_FIRST_REAL   ( BInt4 * fd            ,
                                     BText   gr_name       ,
                                     BText   at_name       ,
                                     BRea4 * at_value      ,
                                     BInt4   gr_name_length,
                                     BInt4   at_name_length)
#endif
{
  BChar   att_name[MAX_NAME+1];
  BChar   att_type[MAX_TYPE+1];
  BChar   grp_name[MAX_NAME+1];
  BInt4   i;

  nefis_errno = 0;

  if ( at_name_length    < MAX_NAME )
  {
    nefis_errcnt += 1;
    nefis_errno   = 2032;
    sprintf(error_text,
      "Inqfra: User supplied array to contain real attribute names too small: %ld<%ld\n",
      at_name_length, MAX_NAME);
    return nefis_errno;
  }
  F_Copy_text (grp_name    , gr_name    , gr_name_length    , MAX_NAME);

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
  }

  return nefis_errno;
}
/*==========================================================================*/
/*
 * Get first string attribute on data file
 *
 * input:      fd        : NEFIS file set descriptor
 *           * gr_name   : first/next groupname on data file
 *             gr_defined: corresponding groupname (defined)
 *                         on definition file
 * output:   * at_value  : value of the attribute
 *
 * return:   0    No error occured
 *           !=0  Error occured
 */
#if defined(MIXED_STR_LEN_ARG)
DLLEXPORT BInt4 FTN_CALL INQUIRE_FIRST_STRING ( BInt4 * fd            ,
                                     BText   gr_name       ,
                                     BInt4   gr_name_length,
                                     BText   at_name       ,
                                     BInt4   at_name_length,
                                     BText   at_value      ,
                                     BInt4   at_value_length)
#else
DLLEXPORT BInt4 FTN_CALL INQUIRE_FIRST_STRING ( BInt4 * fd            ,
                                     BText   gr_name       ,
                                     BText   at_name       ,
                                     BText   at_value      ,
                                     BInt4   gr_name_length,
                                     BInt4   at_name_length,
                                     BInt4   at_value_length)
#endif
{
  BChar   att_name [MAX_NAME+1];
  BChar   att_type [MAX_TYPE+1];
  BChar   att_value[MAX_NAME+1];
  BChar   grp_name [MAX_NAME+1];
  BInt4   i;

  nefis_errno = 0;

  if ( ( at_name_length  < MAX_NAME ) ||
       ( at_value_length < MAX_NAME )    )
  {
    nefis_errcnt += 1;
    nefis_errno   = 2033;
    sprintf(error_text,
      "Inqfsa: User supplied array to contain string attribute names too small: %ld<%ld, %ld<%ld\n",
      at_name_length, MAX_NAME, at_value_length, MAX_NAME);
    return nefis_errno;
  }

  F_Copy_text (grp_name    , gr_name    , gr_name_length    , MAX_NAME);

  for ( i=0; i<MAX_NAME; i++ )
  {
    att_name    [i]= ' ';
    att_value   [i]= ' ';
  }
  att_name    [MAX_NAME]='\0';
  att_value   [MAX_NAME]='\0';

  strcpy(att_type, "CHARACTE");

  nefis_errno = GP_get_attribute(*fd      , 1        , grp_name,
                                  att_name, att_value, att_type);

  if ( nefis_errno == 0 )
  {
    strncpy(at_name , att_name , MAX_NAME);
    strncpy(at_value, att_value, MAX_NAME);
  }

  return nefis_errno;
}
/*==========================================================================*/
/*
 * Read group definition from defintion file
 */

#if defined(MIXED_STR_LEN_ARG)
DLLEXPORT BInt4 FTN_CALL INQUIRE_GROUP       ( BInt4 * fd              ,
                                    BText   gr_defined      ,
                                    BInt4   gr_defined_length,
                                    BText   cl_name         ,
                                    BInt4   cl_name_length  ,
                                    BInt4 * gr_num_dim      ,
                                    BInt4 * gr_dimens       ,
                                    BInt4 * gr_order        )
#else
DLLEXPORT BInt4 FTN_CALL INQUIRE_GROUP       ( BInt4 * fd              ,
                                    BText   gr_defined      ,
                                    BText   cl_name         ,
                                    BInt4 * gr_num_dim      ,
                                    BInt4 * gr_dimens       ,
                                    BInt4 * gr_order        ,
                                    BInt4   gr_defined_length,
                                    BInt4   cl_name_length  )
#endif
{
  BChar   cel_name   [MAX_NAME+1];
  BChar   grp_defined[MAX_NAME+1];
  BText   grp_defined_tmp;
  BUInt4  grp_num_dim = 0        ;
  BUInt4  grp_dimens[MAX_DIM]    ;
  BUInt4  grp_order [MAX_DIM]    ;
  BUInt4  i                      ;

  NOREFF(cl_name_length);

  nefis_errno = 0;

  if ( gr_defined_length > MAX_NAME )
  {
    grp_defined_tmp =  strFcpy(gr_defined, gr_defined_length);

    nefis_errcnt += 1;
    nefis_errno   = 2034;
    sprintf(error_text,
      "Inqgrp: Group name too long \'%s\'", grp_defined_tmp);
    free( grp_defined_tmp );
    return nefis_errno;
  }

  F_Copy_text (grp_defined , gr_defined , gr_defined_length , MAX_NAME);

  nefis_errno = GP_inquire_grp_def( *fd        ,
                                    grp_defined, cel_name ,&grp_num_dim  ,
                                    (BUInt4 *) grp_dimens , (BUInt4 *) grp_order);

  if ( nefis_errno == 0 )
  {
    strncpy( cl_name, cel_name, MAX_NAME);

    if ( *gr_num_dim < (BInt4) grp_num_dim )
    {
      nefis_errcnt += 1;
      nefis_errno   = 2035;
      sprintf(error_text,
        "Inqgrp: User supplied array to contain group dimensions too small: %ld<%ld\n",
        *gr_num_dim, grp_num_dim);
      return nefis_errno;
    }
    *gr_num_dim = grp_num_dim;
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
 * Determine maximum index of variable group
 */

#if defined(MIXED_STR_LEN_ARG)
DLLEXPORT BInt4 FTN_CALL INQUIRE_MAX_INDEX   ( BInt4 * fd               ,
                                    BText   gr_name          ,
                                    BInt4   gr_name_length   ,
                                    BInt4 * max_index        )
#else
DLLEXPORT BInt4 FTN_CALL INQUIRE_MAX_INDEX   ( BInt4 * fd               ,
                                    BText   gr_name          ,
                                    BInt4 * max_index        ,
                                    BInt4   gr_name_length   )
#endif
{
  BChar   grp_name   [MAX_NAME+1];

  nefis_errno = 0;

  F_Copy_text (grp_name    , gr_name    , gr_name_length    , MAX_NAME);

  nefis_errno = GP_inquire_max (*fd, grp_name, (BUInt4 *) max_index);

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
#if defined(MIXED_STR_LEN_ARG)
DLLEXPORT BInt4 FTN_CALL INQUIRE_NEXT_DAT_GROUP   ( BInt4 * fd            ,
                                         BText   gr_name       ,
                                         BInt4   gr_name_length,
                                         BText   gr_defined    ,
                                         BInt4   gr_defined_length)
#else
DLLEXPORT BInt4 FTN_CALL INQUIRE_NEXT_DAT_GROUP   ( BInt4 * fd            ,
                                         BText   gr_name       ,
                                         BText   gr_defined    ,
                                         BInt4   gr_name_length,
                                         BInt4   gr_defined_length)
#endif
{
  BChar   grp_name   [MAX_NAME+1];
  BChar   grp_defined[MAX_NAME+1];
  BInt4   i      ;

  nefis_errno = 0;

  if ( ( gr_name_length    < MAX_NAME ) ||
       ( gr_defined_length < MAX_NAME )    )
  {
    nefis_errcnt += 1;
    nefis_errno   = 2037;
    sprintf(error_text,
        "Inqnxt: User supplied array to contain names too small: %ld<%ld, %ld<%ld\n",
        gr_name_length, MAX_NAME, gr_defined_length, MAX_NAME);
    return nefis_errno;
  }

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
  }

  return nefis_errno;
}
/*==========================================================================*/
/*
 * Get next integer attribute on data file
 */
#if defined(MIXED_STR_LEN_ARG)
DLLEXPORT BInt4 FTN_CALL INQUIRE_NEXT_INTEGER ( BInt4 * fd            ,
                                     BText   gr_name       ,
                                     BInt4   gr_name_length,
                                     BText   at_name       ,
                                     BInt4   at_name_length,
                                     BInt4 * at_value      )
#else
DLLEXPORT BInt4 FTN_CALL INQUIRE_NEXT_INTEGER ( BInt4 * fd            ,
                                     BText   gr_name       ,
                                     BText   at_name       ,
                                     BInt4 * at_value      ,
                                     BInt4   gr_name_length,
                                     BInt4   at_name_length)
#endif
{
  BChar   att_name[MAX_NAME+1];
  BChar   att_type[MAX_TYPE+1];
  BChar   grp_name[MAX_NAME+1];
  BInt4   i;

  nefis_errno = 0;

  if ( at_name_length    < MAX_NAME )
  {
    nefis_errcnt += 1;
    nefis_errno   = 2038;
    sprintf(error_text,
        "Inqnia: User supplied array to contain integer attribute names too small: %ld<%ld\n",
        at_name_length, MAX_NAME);
    return nefis_errno;
  }

  F_Copy_text (grp_name    , gr_name    , gr_name_length    , MAX_NAME);

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
  }

  return nefis_errno;
}
/*==========================================================================*/
/*
 * Get next real attribute on data file
 */
#if defined(MIXED_STR_LEN_ARG)
DLLEXPORT BInt4 FTN_CALL INQUIRE_NEXT_REAL    ( BInt4 * fd            ,
                                     BText   gr_name       ,
                                     BInt4   gr_name_length,
                                     BText   at_name       ,
                                     BInt4   at_name_length,
                                     BRea4 * at_value      )
#else
DLLEXPORT BInt4 FTN_CALL INQUIRE_NEXT_REAL    ( BInt4 * fd            ,
                                     BText   gr_name       ,
                                     BText   at_name       ,
                                     BRea4 * at_value      ,
                                     BInt4   gr_name_length,
                                     BInt4   at_name_length)
#endif
{
  BChar   att_name[MAX_NAME+1];
  BChar   att_type[MAX_TYPE+1];
  BChar   grp_name[MAX_NAME+1];
  BInt4   i;

  nefis_errno = 0;

  if ( at_name_length    < MAX_NAME )
  {
    nefis_errcnt += 1;
    nefis_errno   = 2039;
    sprintf(error_text,
      "Inqnra: User supplied array to contain real attribute names too small: %ld<%ld\n",
        at_name_length, MAX_NAME);
    return nefis_errno;
  }

  F_Copy_text (grp_name    , gr_name    , gr_name_length    , MAX_NAME);

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
  }

  return nefis_errno;
}
/*==========================================================================*/
/*
 * Get next string attribute on data file
 */
#if defined(MIXED_STR_LEN_ARG)
DLLEXPORT BInt4 FTN_CALL INQUIRE_NEXT_STRING  ( BInt4 * fd            ,
                                     BText   gr_name       ,
                                     BInt4   gr_name_length,
                                     BText   at_name       ,
                                     BInt4   at_name_length,
                                     BText   at_value      ,
                                     BInt4   at_value_length)
#else
DLLEXPORT BInt4 FTN_CALL INQUIRE_NEXT_STRING  ( BInt4 * fd            ,
                                     BText   gr_name       ,
                                     BText   at_name       ,
                                     BText   at_value      ,
                                     BInt4   gr_name_length,
                                     BInt4   at_name_length,
                                     BInt4   at_value_length)
#endif
{
  BChar   att_name [MAX_NAME+1];
  BChar   att_type [MAX_TYPE+1];
  BChar   att_value[MAX_NAME+1];
  BChar   grp_name [MAX_NAME+1];
  BInt4   i;

  nefis_errno = 0;

  if ( ( at_name_length    < MAX_NAME ) ||
       ( at_value_length   < MAX_NAME )    )
  {
    nefis_errcnt += 1;
    nefis_errno   = 2040;
    sprintf(error_text,
      "Inqnra: User supplied array to contain string attributes (name/value) too small: %ld<%ld,  %ld<%ld\n",
        at_name_length, MAX_NAME, at_value_length, MAX_NAME);
    return nefis_errno;
  }

  F_Copy_text (grp_name    , gr_name    , gr_name_length    , MAX_NAME);

  for ( i=0; i<MAX_NAME; i++ )
  {
    att_name    [i]= ' ';
    att_value   [i]= ' ';
  }
  att_name    [MAX_NAME]='\0';
  att_value   [MAX_NAME]='\0';

  strcpy(att_type, "CHARACTE");

  nefis_errno = GP_get_attribute(*fd      , 2        , grp_name,
                                  att_name, att_value, att_type);

  if ( nefis_errno == 0 )
  {
    strncpy(at_name , att_name , MAX_NAME);
    strncpy(at_value, att_value, MAX_NAME);
  }

  return nefis_errno;
}
/*==========================================================================*/
/*
 * NEFIS error messages
 */
DLLEXPORT BInt4 FTN_CALL NEFIS_ERROR ( BInt4 * print_stderr,
                    BText   err_string  ,
                    BInt4   err_string_length)
{
  BInt4  error;
  BChar  error_string[LENGTH_ERROR_MESSAGE+1];
  BInt4  i;
  BInt4  max_copy;
/*
 * clean up supplied character string (contains old messages?)
 */
  for ( i=0; i<err_string_length; i++ )
  {
    err_string[i] = ' ';
  }

  error = nefis_error( *print_stderr, error_string );

  max_copy = f2c_strlen(error_string, err_string_length);
  strncpy( err_string  , error_string, max_copy);

  return error;
}
/*==========================================================================*/
/*
 * Open data file
 */

#if defined(MIXED_STR_LEN_ARG)
DLLEXPORT BInt4 FTN_CALL OPEN_DAT_FILE   ( BInt4 * fd          ,
                                BText   dt_file        ,
                                BInt4   dt_file_length ,
                                BText   cding          ,
                                BInt4   cding_length   )
#else
DLLEXPORT BInt4 FTN_CALL OPEN_DAT_FILE   ( BInt4 * fd          ,
                                BText   dt_file        ,
                                BText   cding          ,
                                BInt4   dt_file_length ,
                                BInt4   cding_length   )
#endif
{
  BChar   access_type='U';
  BText   dat_file;
  BInt4   max_copy;
  BChar   coding;

  NOREFF(cding_length);

  nefis_errno = 0;

  max_copy = f2c_strlen(dt_file, dt_file_length);
  dat_file = (BText  ) malloc ( sizeof(BChar) * (max_copy+1) );
  strncpy( dat_file, dt_file, dt_file_length);
  dat_file[dt_file_length]='\0';

  RemoveTrailingBlanks(dat_file);

  coding = cding[0];

  nefis_errno = create_nefis_files( fd, dat_file, "", coding, access_type);

  free( (BData) dat_file    );

  return nefis_errno;
}
/*==========================================================================*/
/*
 * Open definiton file
 */
#if defined(MIXED_STR_LEN_ARG)
DLLEXPORT BInt4 FTN_CALL OPEN_DEF_FILE   ( BInt4 * fd          ,
                                BText   df_file        ,
                                BInt4   df_file_length ,
                                BText   cding          ,
                                BInt4   cding_length   )
#else
DLLEXPORT BInt4 FTN_CALL OPEN_DEF_FILE   ( BInt4 * fd          ,
                                BText   df_file        ,
                                BText   cding          ,
                                BInt4   df_file_length ,
                                BInt4   cding_length   )
#endif
{
  BChar   access_type='U';
  BText   def_file;
  BChar   coding;
  BInt4   max_copy;


  NOREFF(cding_length);

  nefis_errno = 0;

  max_copy = f2c_strlen(df_file, df_file_length);
  def_file = (BText  ) malloc ( sizeof(BChar) * (max_copy+1) );
  strncpy( def_file, df_file, max_copy);
  def_file[max_copy]='\0';
  RemoveTrailingBlanks(def_file);


  coding = cding[0];

  nefis_errno = create_nefis_files( fd, "", def_file, coding, access_type);

  free( (BData) def_file    );


  return nefis_errno;
}
/*==========================================================================*/
/*
 * Define element on definition file
 * Input:  * fd                Fortran gives pointer to deffds array
 *         gr_name             Group name on data file
 *         el_name             Element name to write data to
 *         usr_index
 *         usr_order
 *         getal
 * Output: -
 * func  : 0                   No error occured
 *         !=0                 Error occured
 */
#if defined(MIXED_STR_LEN_ARG)
DLLEXPORT BInt4 FTN_CALL PUT_ELEMENT_STRING ( BInt4 * fd             ,
                                   BText   gr_name        ,
                                   BInt4   gr_name_length ,
                                   BText   el_name        ,
                                   BInt4   el_name_length ,
                                   BInt4 * user_index     ,
                                   BInt4 * user_order     ,
                                   BData   getal          ,
                                   BInt4   getal_length   )
#else
DLLEXPORT BInt4 FTN_CALL PUT_ELEMENT_STRING ( BInt4 * fd             ,
                                   BText   gr_name        ,
                                   BText   el_name        ,
                                   BInt4 * user_index     ,
                                   BInt4 * user_order     ,
                                   BData   getal          ,
                                   BInt4   gr_name_length ,
                                   BInt4   el_name_length ,
                                   BInt4   getal_length   )
#endif
{
  BChar   elm_name [MAX_NAME + 1];
  BChar   grp_name [MAX_NAME + 1];
  BInt4   i        ;
  BInt4   j        ;
  BInt4   usr_index[MAX_DIM * DEFINE_STEPS];
  BInt4   usr_order[MAX_DIM    ];

  NOREFF(getal_length);

  nefis_errno = 0;

  F_Copy_text (elm_name    , el_name    , el_name_length    , MAX_NAME);
  F_Copy_text (grp_name    , gr_name    , gr_name_length    , MAX_NAME);

  for ( i=0; i<MAX_DIM     ; i++ )
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
 * Define element on definition file
 * Input:  * fd                Fortran gives pointer to deffds array
 * Output: -
 * func  : 0                   No error occured
 *         !=0                 Error occured
 */
#if defined(MIXED_STR_LEN_ARG)
DLLEXPORT BInt4 FTN_CALL PUT_ELEMENT( BInt4 * fd             ,
                           BText   gr_name        ,
                           BInt4   gr_name_length ,
                           BText   el_name        ,
                           BInt4   el_name_length ,
                           BInt4 * user_index     ,
                           BInt4 * user_order     ,
                           BData   getal          )
#else
DLLEXPORT BInt4 FTN_CALL PUT_ELEMENT( BInt4 * fd             ,
                           BText   gr_name        ,
                           BText   el_name        ,
                           BInt4 * user_index     ,
                           BInt4 * user_order     ,
                           BData   getal          ,
                           BInt4   gr_name_length ,
                           BInt4   el_name_length )
#endif
{
  BInt4 getal_length;

  nefis_errno  = 0;
  getal_length = (BInt4) sizeof(getal);

#if defined(MIXED_STR_LEN_ARG)
    nefis_errno = PUT_ELEMENT_STRING    ( fd             ,
                                          gr_name        ,
                                          gr_name_length ,
                                          el_name        ,
                                          el_name_length ,
                                          user_index     ,
                                          user_order     ,
                                          getal          ,
                                          getal_length   );
#else
    nefis_errno = PUT_ELEMENT_STRING    ( fd             ,
                                          gr_name        ,
                                          el_name        ,
                                          user_index     ,
                                          user_order     ,
                                          getal          ,
                                          gr_name_length ,
                                          el_name_length ,
                                          getal_length   );
#endif

  return nefis_errno;
}

/*==========================================================================*/
/*
 * Put integer attribute on data file
 */
#if defined(MIXED_STR_LEN_ARG)
DLLEXPORT BInt4 FTN_CALL PUT_INT_ATTRIBUTE    ( BInt4 * fd            ,
                                     BText   gr_name       ,
                                     BInt4   gr_name_length,
                                     BText   at_name       ,
                                     BInt4   at_name_length,
                                     BInt4 * at_value      )
#else
DLLEXPORT BInt4 FTN_CALL PUT_INT_ATTRIBUTE    ( BInt4 * fd            ,
                                     BText   gr_name       ,
                                     BText   at_name       ,
                                     BInt4 * at_value      ,
                                     BInt4   gr_name_length,
                                     BInt4   at_name_length)
#endif
{
  BChar   att_name [MAX_NAME+1];
  BText   att_name_tmp;
  BChar   att_type [MAX_TYPE+1];
  BChar   grp_name [MAX_NAME+1];
  BText   grp_name_tmp;

  nefis_errno = 0;

  if ( (gr_name_length  > MAX_NAME) ||
       (at_name_length  > MAX_NAME)    )
  {
    grp_name_tmp = strFcpy(gr_name, gr_name_length);
    att_name_tmp = strFcpy(at_name, at_name_length);

    nefis_errcnt += 1;
    nefis_errno   = 2042;
    sprintf(error_text,
      "Putiat: Groupname \'%s\' or\n        integer attribute name \'%s\' too long",
      grp_name_tmp,att_name_tmp);
    free(grp_name_tmp);
    free(att_name_tmp);

    return nefis_errno;
  }

  F_Copy_text (grp_name    , gr_name    , gr_name_length    , MAX_NAME);
  F_Copy_text (att_name    , at_name    , at_name_length    , MAX_NAME);

  strcpy(att_type, "INTEGER");

  nefis_errno = GP_put_attribute(*fd, grp_name, att_name, (BData) at_value, att_type);

  return nefis_errno;
}
/*==========================================================================*/
/*
 * Put integer attribute on data file
 */
#if defined(MIXED_STR_LEN_ARG)
DLLEXPORT BInt4 FTN_CALL PUT_REAL_ATTRIBUTE   ( BInt4 * fd            ,
                                     BText   gr_name       ,
                                     BInt4   gr_name_length,
                                     BText   at_name       ,
                                     BInt4   at_name_length,
                                     BRea4 * at_value      )
#else
DLLEXPORT BInt4 FTN_CALL PUT_REAL_ATTRIBUTE   ( BInt4 * fd            ,
                                     BText   gr_name       ,
                                     BText   at_name       ,
                                     BRea4 * at_value      ,
                                     BInt4   gr_name_length,
                                     BInt4   at_name_length)
#endif
{
  BChar   att_name [MAX_NAME+1];
  BText   att_name_tmp;
  BChar   att_type [MAX_TYPE+1];
  BChar   grp_name [MAX_NAME+1];
  BText   grp_name_tmp;

  nefis_errno = 0;

  if ( (gr_name_length  > MAX_NAME) ||
       (at_name_length  > MAX_NAME)    )
  {
    grp_name_tmp = strFcpy(gr_name, gr_name_length);
    att_name_tmp = strFcpy(at_name, at_name_length);

    nefis_errcnt += 1;
    nefis_errno   = 2043;
    sprintf(error_text,
      "Putrat: Groupname \'%s\' or\n        real attribute name \'%s\' too long",
      grp_name_tmp, att_name_tmp);
    free( grp_name_tmp );
    free( att_name_tmp );
    return nefis_errno;
  }

  F_Copy_text (grp_name    , gr_name    , gr_name_length    , MAX_NAME);
  F_Copy_text (att_name    , at_name    , at_name_length    , MAX_NAME);

  strcpy(att_type, "REAL");

  nefis_errno = GP_put_attribute(*fd, grp_name, att_name, (BData) at_value, att_type);

  return nefis_errno;
}
/*==========================================================================*/
/*
 * Put integer attribute on data file
 */
#if defined(MIXED_STR_LEN_ARG)
DLLEXPORT BInt4 FTN_CALL PUT_STRING_ATTRIBUTE ( BInt4 * fd             ,
                                     BText   gr_name        ,
                                     BInt4   gr_name_length ,
                                     BText   at_name        ,
                                     BInt4   at_name_length ,
                                     BText   at_value       ,
                                     BInt4   at_value_length)
#else
DLLEXPORT BInt4 FTN_CALL PUT_STRING_ATTRIBUTE ( BInt4 * fd             ,
                                     BText   gr_name        ,
                                     BText   at_name        ,
                                     BText   at_value       ,
                                     BInt4   gr_name_length ,
                                     BInt4   at_name_length ,
                                     BInt4   at_value_length)
#endif
{
  BChar   att_name [MAX_NAME+1];
  BText   att_name_tmp;
  BChar   att_type [MAX_TYPE+1];
  BChar   att_value[MAX_NAME+1];
  BText   att_value_tmp;
  BChar   grp_name [MAX_NAME+1];
  BText   grp_name_tmp;

  nefis_errno = 0;

  if ( (gr_name_length  > MAX_NAME) ||
       (at_name_length  > MAX_NAME) ||
       (at_value_length > MAX_NAME)    )
  {
    grp_name_tmp  = strFcpy(gr_name , gr_name_length);
    att_name_tmp  = strFcpy(at_name , at_name_length);
    att_value_tmp = strFcpy(at_value, at_value_length);

    nefis_errcnt += 1;
    nefis_errno   = 2044;
    sprintf(error_text,
      "Putsat: Groupname \'%s\' or\n        string attribute name \'%s\' or\n        attribute value \'%s\' too long",
      grp_name_tmp, att_name_tmp, att_value_tmp);
    free( grp_name_tmp );
    free( att_name_tmp );
    free( att_value_tmp );
    return nefis_errno;
  }
  F_Copy_text (grp_name    , gr_name    , gr_name_length    , MAX_NAME);
  F_Copy_text (att_name    , at_name    , at_name_length    , MAX_NAME);
  F_Copy_text (att_value   , at_value   , at_value_length   , MAX_NAME);

  strcpy(att_type, "CHARACTE");

  nefis_errno = GP_put_attribute(*fd, grp_name, att_name, att_value, att_type);

  return nefis_errno;
}
/*==========================================================================*/
/*
 * Retrieve version number of the NEFIS library
 * Return: 0                   No error occured
 *         !=0                 Error occured
 */

DLLEXPORT BInt4 FTN_CALL GET_NEFIS_VERSION  ( BText nef_version, BInt4   version_length )
{
  BText nefis_version;
  BInt4 min_length;
  BInt4 i;

  nefis_errno = 0;

  nefis_errno = OC_get_version(&nefis_version);

  min_length = min(version_length, (BInt4) strlen(nefis_version));

  if ( nefis_errno == 0 )
  {
      strncpy( nef_version, nefis_version, min_length);
      for (i=min_length; i<version_length; i++)
      {
          nef_version[i] = ' ';
      }
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

DLLEXPORT BInt4 FTN_CALL RESET_FILE_VERSION  ( BInt4 fd, BInt4 file_version)
{
  nefis_errno = 0;

  nefis_errno = OC_reset_file_version(fd, file_version);

  return nefis_errno;
}

