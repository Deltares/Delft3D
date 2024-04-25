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
/*
 *   <rt.c> - Retrieve information about group - element from file or
 *            from structure with retrieved elements
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
 *   RT_retrieve_file :  Retrieve information from file
 *   RT_retrieve_array:  Retrieve information from array
 *   RT_streql        :  Compare two strings
 */
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <limits.h>

#include "btps.h"
#include "nefis.h" /* needed for definition of LENGTH_ERROR_MESSAGE */
#include "nef-def.h"
#include "rt.h"

/*
 * Parameter "index_copy" is used as a workaround for a consistent crash
 * when using the Intel compilers version 11.0, 64-bit
 */
static BInt4* index_copy;

static BInt4 RT_update_var_array( BInt4    ,
                                  BInt4    ,
                                  BUInt8 * );
BInt4 RT_update_var_index_array ( BInt4    ,
                                  BInt4    ,
                                  BUInt8   ,
                                  BInt4    ,
                                  BInt4    ,
                                  BUInt4 * );
static retrieve * Find ( retrieve *,
                         BText     ,
                         BText      );
static retrieve * Tree ( retrieve *,
                         BText     ,
                         BText     ,
                         BUInt8 *  ,
                         BUInt4 *  ,
                         BUInt4 *  ,
                         BUInt4 *  ,
                         BUInt4 *  ,
                         BText     ,
                         BUInt4 *  ,
                         BUInt4 *  ,
                         BUInt4 *  ,
                         BUInt8 *  ,
                         BUInt8 *  );
static BInt4 RT_retrieve_struc(
                         BInt4    ,
                         BText    ,
                         BText    ,
                         BUInt8 * ,
                         BUInt4 * ,
                         BUInt4 * ,
                         BUInt4 * ,
                         BUInt4 * ,
                         BText    ,
                         BUInt4 * ,
                         BUInt4 * ,
                         BUInt4 * ,
                         BUInt8 * ,
                         BUInt8 * );
static BInt4 RT_retrieve_file(
                         BInt4    ,
                         BText    ,
                         BText    ,
                         BUInt8 * ,
                         BUInt4 * ,
                         BUInt4 * ,
                         BUInt4 * ,
                         BUInt4 * ,
                         BText    ,
                         BUInt4 * ,
                         BUInt4 * ,
                         BUInt4 * ,
                         BUInt8 * ,
                         BUInt8 * );
static BInt4 RT_update_struc(
                         BInt4    ,
                         BText    ,
                         BText    ,
                         BUInt8 * ,
                         BUInt4 * ,
                         BUInt4 * ,
                         BUInt4 * ,
                         BUInt4 * ,
                         BText    ,
                         BUInt4 * ,
                         BUInt4 * ,
                         BUInt4 * ,
                         BUInt8 * ,
                         BUInt8 * );

/*==========================================================================*/
/*
 * Compare two strings
 * return value:  NULL  strings are the same
 *               !NULL  strigs are not the same
 */
/*==========================================================================*/
BText RT_streql(BText cs,
                BText ct,
                BInt4 max)
{
  cs[max] = '\0';
  ct[max] = '\0';
  if ( strcmp(cs,ct) < 0 )
  {
    return  strstr(cs,ct);
  }
  return  strstr(ct,cs);
}

/*==========================================================================*/
/*     * * * * * * * * * * * * * DESCRIPTION * * * * * * * * * * * * *
 *
 *    - Retrieve information about group - element from file or
 *      from structure with retrieved elements. if data is retrieved from
 *      file the structure is updated
 */
/*==========================================================================*/
BInt4 RT_retrieve      ( BInt4    set           ,
                         BText    grp_name      ,
                         BText    elm_name      ,
                         BUInt8 * cel_num_bytes ,
                         BUInt4 * elm_dimens    ,
                         BUInt4 * elm_num_dim   ,
                         BUInt4 * elm_offset    ,
                         BUInt4 * elm_single_bytes,
                         BText    elm_type      ,
                         BUInt4 * grp_dimens    ,
                         BUInt4 * grp_num_dim   ,
                         BUInt4 * grp_order     ,
                         BUInt8 * grp_pointer   ,
                         BUInt8 * write_bytes   )
{
  BInt4 error = 0;

  error = RT_retrieve_struc  ( set            , grp_name        , elm_name    ,
                               cel_num_bytes  , elm_dimens      , elm_num_dim ,
                               elm_offset     , elm_single_bytes, elm_type    ,
                               grp_dimens     , grp_num_dim     , grp_order   ,
                               grp_pointer    , write_bytes     );
/*
 * If error occured in previous function then the assumption is that the
 * groupname is not in the structure so read it from file
 */
  if ( error != 0 || nefis[set].file_retrieve)
  {
    nefis_errno = RT_retrieve_file ( set            , grp_name        , elm_name    ,
                                     cel_num_bytes  , elm_dimens      , elm_num_dim ,
                                     elm_offset     , elm_single_bytes, elm_type    ,
                                     grp_dimens     , grp_num_dim     , grp_order   ,
                                     grp_pointer    , write_bytes     );

    if ( nefis_errno == 0 )
    {
      nefis_errno = RT_update_struc ( set            , grp_name        , elm_name    ,
                                      cel_num_bytes  , elm_dimens      , elm_num_dim ,
                                      elm_offset     , elm_single_bytes, elm_type    ,
                                      grp_dimens     , grp_num_dim     , grp_order   ,
                                      grp_pointer    , write_bytes     );
    }
    nefis[set].file_retrieve = FALSE;

  }

  return nefis_errno;
}
/*==========================================================================*/
/*     * * * * * * * * * * * * * DESCRIPTION * * * * * * * * * * * * *
 *
 *    - Retrieve information about group - element from
 *      from structure with retrieved elements
 */
/*==========================================================================*/
static BInt4 RT_retrieve_struc(
                         BInt4    set             ,
                         BText    grp_name        ,
                         BText    elm_name        ,
                         BUInt8 * cel_num_bytes   ,
                         BUInt4 * elm_dimens      ,
                         BUInt4 * elm_num_dim     ,
                         BUInt4 * elm_offset      ,
                         BUInt4 * elm_single_bytes,
                         BText    elm_type        ,
                         BUInt4 * grp_dimens      ,
                         BUInt4 * grp_num_dim     ,
                         BUInt4 * grp_order       ,
                         BUInt8 * grp_pointer     ,
                         BUInt8 * write_bytes     )
{
  BInt4 error = 1;
  static BInt4 first = TRUE;
  BInt4 i;
  static retrieve * found;

  if ( first == TRUE  )
  {
    for ( i=0; i<MAX_NEFIS_FILES; i++)
    {
      array_retrieve_ptrs[i] = NULL;
    }
    first = FALSE;
  }
  nefis[set].array_retrieve_ptrs = TRUE;

  found = Find(array_retrieve_ptrs[set], grp_name        , elm_name    );

  if ( found != NULL )
  {
    error = 0;

    *cel_num_bytes    = found->cel_num_bytes;
    *elm_num_dim      = found->elm_num_dim;
    *elm_offset       = found->elm_offset ;
    *elm_single_bytes = found->elm_single_bytes;
    *grp_num_dim      = found->grp_num_dim;
    *grp_pointer      = found->grp_pointer;
    *write_bytes      = found->write_bytes;
    strncpy(elm_type,found->elm_type,MAX_TYPE);

    for ( i=0; i<MAX_DIM; i++ )
    {
      elm_dimens[i] = found->elm_dimens[i] ;
      grp_dimens[i] = found->grp_dimens[i] ;
      grp_order [i] = found->grp_order [i] ;
    }
  }

  return error;
}
/*==========================================================================*/
static retrieve * Find ( retrieve * p          ,
                         BText   grp_name      ,
                         BText   elm_name      )
{
  BInt4   a = 0;

  while ( p != NULL )
  {
    a=0;
    if ( RT_streql( p->elm_name, elm_name, MAX_NAME) == NULL )
    {
      a++;
    }
    if ( RT_streql( p->grp_name, grp_name, MAX_NAME) == NULL )
    {
      a++;
    }
    if ( a > 0 )
    {
      if (strncmp(elm_name,p->elm_name,MAX_NAME) <0 )
      {
        p = p->left;         /* follow left path  */
      }
      else
      {
        p = p->right;        /* follow right path */
      }
    }
    else
    {
      break;
    }
  }
  return (p);
}
/*==========================================================================*/
/*     * * * * * * * * * * * * * DESCRIPTION * * * * * * * * * * * * *
 *
 *    - Set information about group - element into structure
 */
/*==========================================================================*/
static BInt4 RT_update_struc(
                         BInt4    set           ,
                         BText    grp_name      ,
                         BText    elm_name      ,
                         BUInt8 * cel_num_bytes ,
                         BUInt4 * elm_dimens    ,
                         BUInt4 * elm_num_dim   ,
                         BUInt4 * elm_offset    ,
                         BUInt4 * elm_single_bytes,
                         BText    elm_type      ,
                         BUInt4 * grp_dimens    ,
                         BUInt4 * grp_num_dim   ,
                         BUInt4 * grp_order     ,
                         BUInt8 * grp_pointer   ,
                         BUInt8 * write_bytes   )
{
  BInt4   error=1;

  array_retrieve_ptrs[set] =
        Tree(array_retrieve_ptrs[set], grp_name, elm_name    ,
             cel_num_bytes  , elm_dimens      , elm_num_dim ,
             elm_offset     , elm_single_bytes, elm_type    ,
             grp_dimens     , grp_num_dim     , grp_order   ,
             grp_pointer    , write_bytes     );

  if ( array_retrieve_ptrs[set] != NULL )
  {
    error = 0;
  }

  return error;
}
/*==========================================================================*/
static retrieve * Tree ( retrieve * p           ,
                         BText    grp_name      ,
                         BText    elm_name      ,
                         BUInt8 * cel_num_bytes ,
                         BUInt4 * elm_dimens    ,
                         BUInt4 * elm_num_dim   ,
                         BUInt4 * elm_offset    ,
                         BUInt4 * elm_single_bytes,
                         BText    elm_type      ,
                         BUInt4 * grp_dimens    ,
                         BUInt4 * grp_num_dim   ,
                         BUInt4 * grp_order     ,
                         BUInt8 * grp_pointer   ,
                         BUInt8 * write_bytes   )
{
  BInt4 i;

  if ( p == NULL )          /* end of search path? */
  {
    p = (retrieve *)malloc(sizeof(retrieve));
    p->left  = NULL;
    p->right = NULL;
/*
 *  put group and element information into structure
 */
    strncpy(p->grp_name,grp_name,MAX_NAME);
            p->grp_name[MAX_NAME] = '\0';
    strncpy(p->elm_name,elm_name,MAX_NAME);
            p->elm_name[MAX_NAME] = '\0';
    p->cel_num_bytes      = *cel_num_bytes;
    p->elm_num_dim        = *elm_num_dim  ;
    p->elm_offset         = *elm_offset   ;
    p->elm_single_bytes   = *elm_single_bytes;
    strncpy(p->elm_type,elm_type,MAX_TYPE);
            p->elm_type[MAX_TYPE] = '\0';
    p->grp_num_dim        = *grp_num_dim  ;
    p->grp_pointer        = *grp_pointer  ;
    p->write_bytes        = *write_bytes  ;

    for (i=0; i<MAX_DIM; i++ )
    {
      p->elm_dimens[i] = elm_dimens[i];
      p->grp_dimens[i] = grp_dimens[i];
      p->grp_order [i] = grp_order [i];
    }
  }
  else
  {
    if ( strncmp(elm_name,p->elm_name,MAX_NAME) <0 ) { /* to the left?     */
      p->left = Tree(p->left        , grp_name        , elm_name     ,
                     cel_num_bytes  , elm_dimens      , elm_num_dim ,
                     elm_offset     , elm_single_bytes, elm_type    ,
                     grp_dimens     , grp_num_dim     , grp_order   ,
                     grp_pointer    , write_bytes     );
    }
    else
    {                                            /* to the right?    */
      p->right = Tree(p->right       , grp_name        , elm_name    ,
                      cel_num_bytes  , elm_dimens      , elm_num_dim ,
                      elm_offset     , elm_single_bytes, elm_type    ,
                      grp_dimens     , grp_num_dim     , grp_order   ,
                      grp_pointer    , write_bytes     );
    }
  }
  return (p);
}
/*==========================================================================*/
retrieve * free_retrieve_ptrs( retrieve * p )
{
    if (p->left  != NULL)
    {
        p->left  = free_retrieve_ptrs( p->left  );
    }
    if (p->right != NULL)
    {
        p->right = free_retrieve_ptrs( p->right );
    }
    free(p);
    p = NULL;

    return p;
}
/*==========================================================================*/
/*     * * * * * * * * * * * * * DESCRIPTION * * * * * * * * * * * * *
 *
 *    - Retrieve pointer to data for variable group dimension
 *
 * Three added to var_index to get the right pointer for the
 * retrieve_var array
 * first  dimension: nefis file set descriptor
 * second dimension: counts number of groups with variable dimension
 * thirth dimension: see description below
 * retrieve_var[set][i][0] = grp_pointer
 * retrieve_var[set][i][1] = max_index
 * retrieve_var[set][i][2] = NULL dimension, occupies only space
 * retrieve_var[set][i][3] = pointer for first index
 * retrieve_var[set][i][4] = pointer for second index
 */
/*==========================================================================*/
BInt4 RT_retrieve_var  ( BInt4    set           ,
                         BUInt8 * grp_pointer   ,
                         BInt4    var_index     ,
                         BUInt8 * var_pointer   )
{
  BInt4   error =  1;
  BInt4   fgd   = -1;    /* free group descriptor */
  BInt4   gd    = -1;    /*      group descriptor */
  BInt4   i         ;

  *var_pointer = (BUInt8) BUINT8_MAX;

/*
 *  search for first free group descriptor and for group descriptor if
 *  already in retrieve_var array
 */

  for ( i=0; i<MAX_VAR_GROUPS; i++ )
  {
    if ( *grp_pointer == retrieve_var[set][i][0] )
    {
      gd = i;
      break;
    }
    if ( ( fgd == -1                            ) &&
         ( retrieve_var[set][i][0] == BUINT8_MAX )    )
    {
      fgd = i;
    }
  }

/*
 * no group descriptor found and no free group descriptors available
 */

  if ( ( gd  == -1 ) &&
       ( fgd == -1 )    )
  {
    nefis_errno   = 9001;
    nefis_errcnt += 1;
    sprintf( error_text,
     "The variable MAX_VAR_GROUPS needs to be increased.\nContact Deltares\n");
    return nefis_errno;
    //in crease max_var_groups with a factor 4 (=2x2)
  }

  if (gd == -1)
  {
      gd = fgd;
  }

  error = RT_update_var_array( set, gd, grp_pointer );
  if ( error == 0 && var_index < retrieve_var[set][gd][1] )
  {
    *var_pointer = retrieve_var[set][gd][var_index+3];
  }
  else
  {
      error = 1; /*  var_index (c-numbering; retrieve_var (fortran-numbering) */
  }

  return error;
}
/*==========================================================================*/
/*     * * * * * * * * * * * * * DESCRIPTION * * * * * * * * * * * * *
 *
 *    - Update variable-pointer array to data of variable group dimension
 */
/*==========================================================================*/
static BInt4 RT_update_var_array( BInt4    set         ,
                                  BInt4    gd          ,
                                  BUInt8 * grp_pointer )
{
    BInt4   error     =  1;
    BInt4   level     =  0;
    BInt4   index     =  0;
    BUInt4  max_index = (BUInt4) BUINT8_MAX;
    BUInt8  start_table;

    start_table = *grp_pointer+(BUInt8)SIZE_DAT_BUF+SIZE_BINT8;
    if (nefis[set].file_version == Version_1)
    {
        start_table = *grp_pointer+(BUInt8)SIZE_DAT_BUF-3*SIZE_BINT4+SIZE_BINT4;
    }

    index_copy = (BInt4 *) malloc(sizeof(BInt4));

    error = RT_update_var_index_array( set  , gd   , start_table,
                                       level, index, &max_index  );
    free(index_copy);
    if ( error == 0 )
    {
        retrieve_var[set][gd][0] = *grp_pointer;
        retrieve_var[set][gd][1] = (BUInt8) max_index;
    }

    return error;
}
/*==========================================================================*/
BInt4 RT_update_var_index_array( BInt4    set          ,
                                 BInt4    gd           ,
                                 BUInt8   table_pointer,
                                 BInt4    level        ,
                                 BInt4    index        ,
                                 BUInt4 * max_index    )
{
  static  BInt4 found[MAX_VAR_GROUPS];

    BText   cp       ;
    BInt4   error = 0;
    BInt4   dat_fds  ;
    BInt4   from_xdr ;
    BInt4   i        ;
    BInt4   k        ;
    BData   vp       ;

    union _pointer
    {
        BChar  st  [2048];
        BUInt8 ptr  [256];
    } data;

    if ( nefis[set].one_file == TRUE )
    {
        dat_fds = nefis[set].daf_fds;
    }
    else
    {
        dat_fds = nefis[set].dat_fds;
    }

    found[gd] = 0;

    (BVoid) GP_read_file_pointers ( dat_fds       , data.st  ,
                                    table_pointer , SIZE_BINT8*256, set);

    if ( nefis[set].daf_neutral == TRUE ||
         nefis[set].dat_neutral == TRUE    )
    {
        from_xdr = 1;
        vp = (BData) malloc ( SIZE_BINT8*256 );
        cp = (BText) data.st;
        error = convert_ieee( &vp, &cp, SIZE_BINT8*256, SIZE_BINT8, "INTEGER", from_xdr);
        for ( i=0; i<256; i++ )
        {
            data.ptr[i] = *((BUInt8 *)vp + (BUInt8)i);
        }
        free( (BData) vp );
    }
    else
    {
        /* nothing */
    }
    index *= 256;

/*
 *  Loop backward through the pointer table, so find the highest index first
 */
    for ( k=255; k>-1; k-- )
    {
/*
 *  Look for the first valid file pointer value which points to a data block
 */
        if ( ( data.ptr[k] != BUINT8_MAX) &&
             ( found[gd]  == 0         )   )
        {
            index += k;
            if ( level != NR_VAR_TABLES-1 ) /*TODO */
            {
                error = RT_update_var_index_array( set    , gd   , data.ptr[k],
                                                   level+1, index, max_index );
            }
            else
            {
                /*
                 *  retrieve_var declared in fuction open_nefis_files,
                 *  will be freed in fuction close_nefis_files.
                 */
                *index_copy=index;
                found[gd] = 1;

                if (*max_index == -1 )
                {
                    *max_index = index;
                    retrieve_var[set][gd] =
                    (BUInt8 *) realloc( retrieve_var[set][gd], (index+3) * sizeof(BUInt8) );
                }
                /*
                 * When using the Intel compilers version 11.0, 64-bit,
                 * parameter "index" has the wrong value when used in the for-statement below.
                 * It seems to be an optimization problem when using recursive functions.
                 * Workaround: use "index_copy"
                 */
                for ( i=0; i<*index_copy+3; i++ )
                {
                    retrieve_var[set][gd][i] = (BUInt8) BUINT8_MAX;
                }
                retrieve_var[set][gd][index+2] = data.ptr[k];
            }
        }
        else if ( found[gd] == 1 )
        {
            index--;
            if ( level != NR_VAR_TABLES-1 )  /*TODO: */
            {
                error = RT_update_var_index_array( set    , gd   , data.ptr[k],
                                                   level+1, index, max_index        );
            }
            else
            {
                if ( index > 0 )
                {
                    retrieve_var[set][gd][index+2] = data.ptr[k];
                }
                else if ( index == 0 )
                {
                    retrieve_var[set][gd][index+2] = (BUInt8) BUINT8_MAX;
                }
            }
        }
/*
    else
    {
       first entry not yet found ( found == 0 ), check next entry
    }
 */
    }

  return error;
}

/*==========================================================================*/
/*     * * * * * * * * * * * * * DESCRIPTION * * * * * * * * * * * * *
 *
 *    - Retrieve information about group - element from file
 */
/*==========================================================================*/
static BInt4 RT_retrieve_file (
                         BInt4    set           ,
                         BText    grp_name      ,
                         BText    look_elm_name,
                         BUInt8 * cel_num_bytes ,
                         BUInt4 * elm_dimens    ,
                         BUInt4 * elm_num_dim   ,
                         BUInt4 * elm_offset    ,
                         BUInt4 * elm_single_bytes,
                         BText    elm_type      ,
                         BUInt4 * grp_dimens    ,
                         BUInt4 * grp_num_dim   ,
                         BUInt4 * grp_order     ,
                         BUInt8 * grp_pointer   ,
                         BUInt8 * write_bytes   )
{
  BChar   cel_name    [ MAX_NAME+1 ];
  BUInt4  cel_num_dim   ;
  BChar   elm_desc    [ MAX_DESC+1 ];
  BChar   elm_name    [ MAX_NAME+1 ];
  BText   elm_names   ;
  BUInt8  elm_num_bytes;
  BChar   elm_quantity[ MAX_NAME+1 ];
  BChar   elm_unity   [ MAX_NAME+1 ];
  BChar   grp_defined [ MAX_NAME+1 ];
  BUInt4  i       ;
  BUInt8  numbytes;
  BInt4   max_comp;

  cel_num_dim = 0;
  i = -1;
  
  cel_name    [MAX_NAME] = '\0';
  elm_desc    [MAX_DESC] = '\0';
  elm_name    [MAX_NAME] = '\0';
  elm_names              = NULL;
  elm_quantity[MAX_NAME] = '\0';
  elm_unity   [MAX_NAME] = '\0';
  grp_defined [MAX_NAME] = '\0';

/*
 * get info about group on data file
 */
  nefis_errno = GP_inquire_dat( set , grp_pointer, grp_name   ,  grp_defined);
  if ( nefis_errno != 0 )
  {
    return nefis_errno;
  }

/*
 * get info about group on definition file
 */

  nefis_errno = GP_inquire_grp_def( set        ,
                                    grp_defined, cel_name  ,  grp_num_dim ,
                                    grp_dimens , grp_order );
  if ( nefis_errno != 0 )
  {
    return nefis_errno;
  }

/*
 * get info about cel on definition file
 */
  elm_names = NULL;
  nefis_errno = GP_inquire_cel ( set      , cel_name   , &cel_num_dim,
                                 &elm_names,&numbytes);
  *cel_num_bytes = numbytes;
  if ( nefis_errno != 0 )
  {
    return nefis_errno;
  }

/*
 * get info about elements on definition file
 * If write all elements, then do not check element names but get the element
 * type from the first element in the cel (all elements have the same type)
 */

  *elm_offset     = 0;
  if ( strncmp("*"     , look_elm_name, 1) == 0 )
  {
    strncpy( elm_name, elm_names, MAX_NAME);
    nefis_errno = GP_inquire_elm ( set      , elm_name   , elm_type   , elm_quantity,
                                   elm_unity, elm_desc   , elm_num_dim, elm_dimens  ,
                                   elm_single_bytes,&elm_num_bytes);
    if ( nefis_errno != 0 )
    {
      return nefis_errno;
    }
    *write_bytes = *cel_num_bytes;
  }
  else
  {
    for ( i=0; i<cel_num_dim; i++ )
    {
      strncpy( elm_name, elm_names+i*(MAX_NAME+1), MAX_NAME);
/*
 * You have to read all previous elements to compute offset in the cel
 */
      nefis_errno = GP_inquire_elm ( set      , elm_name   , elm_type   , elm_quantity,
                                     elm_unity, elm_desc   , elm_num_dim, elm_dimens  ,
                                     elm_single_bytes,&elm_num_bytes);
      if ( nefis_errno != 0 )
      {
        return nefis_errno;
      }
      *write_bytes = elm_num_bytes;
      max_comp = min( (BInt4)strlen(look_elm_name), MAX_NAME);
      if ( strncmp(elm_name, look_elm_name, max_comp) == 0 )
      {
        break;
      }
      *elm_offset += (BUInt4)elm_num_bytes;
    }
  }
  if (i == cel_num_dim)
  {
    nefis_errno   = 9002;
    nefis_errcnt += 1;
    sprintf( error_text,
     "Element \"%s\" of group \"%s\" not found on file \"%s\"\n",
      look_elm_name, grp_name, nefis[set].dat_name );
    return nefis_errno;
  }

  return nefis_errno;
}
