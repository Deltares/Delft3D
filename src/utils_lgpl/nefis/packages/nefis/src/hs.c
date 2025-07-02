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
 *   <hs.c> - Functions related to de HASH table used by NEFiS
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
 */
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>
#include <limits.h>

#if defined(_WIN32) || defined(GNU_PC)
#  include <io.h>
#elif defined(salford32)
#  include <io.h>
#endif

#include "btps.h"
#include "nefis.h" /* needed for definition of LENGTH_ERROR_MESSAGE */
#include "nef-def.h"
#include "hs.h"


static BInt4 HS_hash_key  ( BText, BUInt4 );


/*==========================================================================*/
/*
 *  Check existence of element, cel or group
 *
 *  input:         key            key to be checked
 *                 hashtb         address to local hashtable
 *  output:        pnt_hash       pointer to key on file
 *                 pnt_bucket     pointer in hashtable
 *                 new_entry      0  key already exist
 *                                1  new entry available
 *  return values  0     No error occured
 *                 != 0  Error occured
 */
BInt4 HS_check_ecg (BInt4    set       ,
                    BInt4    fds       ,
                    BText    key       ,
                    BUInt8   nil       ,
                    BUInt8 * hashtb    ,
                    BInt4    lhshtb    ,
                    BUInt8 * pnt_hash  ,
                    BUInt4 * pnt_bucket,
                    BUInt4 * new_entry )
{
    union pntr {
        BChar   st [ 3*SIZE_BINT8+MAX_NAME ];
        BUInt8  ptr[(3*SIZE_BINT8+MAX_NAME)/SIZE_BINT8];
    } nxt_pnt;

    BUInt4  bucket;
    BText   cp;
    BUInt8  current_pointer;
    BInt4   from_xdr;
    BInt4   found = 0;
    BUInt4  i;
    BUInt8  n_read = 0;
    BChar   next_key[MAX_NAME+1];
    BUInt8  next_pointer;
    BUInt4  offset[4];
    BUInt8  size;
    BData   vp;

    for ( i=0; i<MAX_NAME; i++ )
    {
        next_key[i] = ' ';
    }
    next_key[MAX_NAME] = '\0';

    *pnt_hash   = 0UL;
    *pnt_bucket = (BUInt4) BUINT8_MAX;

    offset[ 0] = 0;
    offset[ 1] = offset[ 0] + SIZE_BINT8;
    offset[ 2] = offset[ 1] + SIZE_BINT8;
    offset[ 3] = offset[ 2] + 8;
    size       = offset[ 3] + MAX_NAME;
    if (nefis[set].file_version ==  Version_1)
    {
        size       = 2*SIZE_BINT4+4 + MAX_NAME;
    }

/*
 * Determine result of Hash-function, 0 <= bucket <= lhshtb
 */
    bucket = HS_hash_key(key, lhshtb);
    if ( bucket == -1 )
    {
        nefis_errcnt += 1;
        nefis_errno   = 7001;
        sprintf( error_text,
          "No entry found in hash table for key \'%s\'", key);
        return  nefis_errno;
    }
    current_pointer = *(hashtb+bucket);
    if ( current_pointer == nil )
    {

/*
 *  no entry in hash-table for given key, so create one
 */
        *pnt_bucket = bucket;
        *pnt_hash   = nil   ;
        *new_entry  = 1     ;
    }
    else
    {

/*
 *  there is already an entry in the hash-table for this key
 *  so find out if this key is already defined
 *
 *  get next_key, next_pointer from NEFIS file
 */
        while ( found == 0 ) {

/*
 *    read next_key and next_pointer
 */
        n_read = GP_read_file (fds, nxt_pnt.st, current_pointer, size);
        if ( n_read == -1 )
        {
            nefis_errcnt += 1;
            nefis_errno   = 7002;
            sprintf(error_text,
            "Unable to read next pointer (file write only?)");
            return nefis_errno;
        }
        if ( n_read > (BInt4) sizeof(nxt_pnt.st) )
        {
            nefis_errcnt += 1;
            nefis_errno   = 7003;
            sprintf(error_text,
            "During reading of next pointer");
            return nefis_errno;
        }

        from_xdr = 1;
        if ( nefis[set].daf_neutral == TRUE ||
             nefis[set].dat_neutral == TRUE ||
             nefis[set].def_neutral == TRUE    )
        {
            if (nefis[set].file_version ==  Version_1)
            {
                MCR_shift_string_forward(nxt_pnt.st+12, sizeof(nxt_pnt.st)-12-12, 12);
                MCR_B_from_int4_to_int8 ((BInt4 *) nxt_pnt.st, 12/4);
            }
            vp = (BData) malloc( sizeof(BUInt8) * 1);
            cp = (BText) &nxt_pnt.ptr[ 0];
            nefis_errno = convert_ieee( &vp, &cp,  1*SIZE_BINT8, SIZE_BINT8, "INTEGER", from_xdr);
            nxt_pnt.ptr[ 0]  = *((BUInt8 *) vp);
            free( (BData) vp );
        }
        else
        {
            if (nefis[set].file_version ==  Version_1)
            {
                MCR_shift_string_forward(nxt_pnt.st+12, sizeof(nxt_pnt.st)-12-12, 12);
                MCR_L_from_int4_to_int8 ((BInt4 *) nxt_pnt.st, 12/4);
            }
        }
        next_pointer = nxt_pnt.ptr[ 0];
        /* ......... = nxt_pnt.ptr[1]; not used */
        /* ......... = nxt_pnt.st+offset[2]; not used */

        strncpy (next_key, nxt_pnt.st+offset[3], MAX_NAME);

        for ( i=(BInt4)strlen(next_key); i<MAX_NAME; i++ )
        {
             next_key[i] = ' ';
        }

        if ( strncmp(next_key, key, MAX_NAME) == 0 )
        {
            *pnt_hash   = current_pointer;
            *pnt_bucket = bucket;
            *new_entry  = 0;
            found       = 1;
        }
        else if ( next_pointer == nil )
        {

/*
 *      entry not in hash table so add one for this key
 *      and terminate while loop
 */
            *pnt_bucket = bucket;
            *pnt_hash   = nil;
            *new_entry  = 1;
            found       = 1;
            }
            else
            {
                *pnt_hash   = current_pointer;
            }
            current_pointer = next_pointer;
        }
    }

    return nefis_errno;
}
/*==========================================================================*/
BInt4 HS_get_cont_cel (BInt4    set          ,
                       BUInt8   pointer      ,
                       BUInt8 * next_pointer ,
                       BText    cel_name     ,
                       BText  * elm_names    ,
                       BUInt4 * cel_num_dim  ,
                       BUInt8 * cel_num_bytes)
{
    union cel
    {
      BChar   * st ;
      BUInt4  * in ;
      BUInt8  * ptr;
    };

    BText   cp      ;
    BInt4   fds     ;
    BInt4   from_xdr;
    BUInt4  i       ;
    BUInt4  index;
    BUInt8  n_read = 0;
    BUInt4  offset[7] ;
    BUInt8  size_cel_buf;
    BData   vp      ;
    union cel * cel_buf ;
    BInt4 size_cel_union;

    if ( nefis[set].one_file == TRUE )
    {
        fds  = nefis[set].daf_fds;
    }
    else
    {
        fds  = nefis[set].def_fds;
    }

    from_xdr = 1;

    offset[ 0] = 0;
    offset[ 1] = offset[ 0] + SIZE_BINT8;
    offset[ 2] = offset[ 1] + SIZE_BINT8;
    offset[ 3] = offset[ 2] + 8;
    offset[ 4] = offset[ 3] + MAX_NAME;
    offset[ 5] = offset[ 4] + SIZE_BINT8;
    offset[ 6] = offset[ 5] + SIZE_BINT4;
/*
 * Retrieve first the total number of elements in this cel
 */
    size_cel_buf = SIZE_CEL_BUF;
    if (nefis[set].file_version ==  Version_1)
    {
        size_cel_buf = size_cel_buf - 16; /* four numbers of 4-byte */
    }

    cel_buf = (union cel *) malloc(sizeof(char *));
    cel_buf->st = (char *) malloc(SIZE_CEL_BUF); /* larger size needed, 4 byte integers to 8 byte integers */

    n_read = GP_read_file (fds, cel_buf->st, pointer, size_cel_buf);

    if (nefis[set].file_version ==  Version_1)
    {
        MCR_shift_string_forward(cel_buf->st+12, 24, 12); /* make room for three integers */
        MCR_B_from_int4_to_int8 (&cel_buf->in[0], 12/4);
        MCR_shift_string_forward(cel_buf->st+40, 8, 4); /* make room for one integer */
        MCR_B_from_int4_to_int8 (cel_buf->st+40, 1);
    }

    index  = offset[5]/SIZE_BINT4;
    if ( nefis[set].daf_neutral == TRUE ||
         nefis[set].def_neutral == TRUE    )
    {
        vp = (BData) malloc ( sizeof(BUInt4) * 1 );
        cp = (BText) &cel_buf->in[index];
        nefis_errno = convert_ieee(&vp , &cp,  1*SIZE_BINT4, SIZE_BINT4, "INTEGER", from_xdr);
        cel_buf->in[index] = *( (BInt4 *) vp);
        free( (BData) vp );
    }
    *cel_num_dim = cel_buf->in[index];
/*
 * Determine the exact size of the cell buffer
 */
    size_cel_union = sizeof(char) * (SIZE_CEL_BUF + *cel_num_dim*MAX_NAME);
    free(cel_buf->st);
    cel_buf->st = (char *) malloc(size_cel_union);

    size_cel_buf = SIZE_CEL_BUF+*cel_num_dim*MAX_NAME;
    if (nefis[set].file_version ==  Version_1)
    {
        size_cel_buf = size_cel_buf - 16; /* four numbers are 4-byte */
    }
/*
 *  read contents of cell
 */
    n_read = GP_read_file (fds, cel_buf->st, pointer, size_cel_buf);

    if ( n_read > (BInt4) size_cel_buf )
    {
        nefis_errcnt += 1;
        nefis_errno = 7004;
        sprintf(error_text,
        "During reading of cell structure.");
        return nefis_errno;
    }

    if ( nefis[set].daf_neutral == TRUE ||
         nefis[set].def_neutral == TRUE    )
    {
        if (nefis[set].file_version ==  Version_1)
        {
            MCR_shift_string_forward(cel_buf->st+12, size_cel_buf-12, 12);
            MCR_B_from_int4_to_int8 (&cel_buf->in[0], 12/4);
        }
        vp = (BData) malloc ( sizeof(BUInt8 ) * 2 );
        cp = (BText) &cel_buf->ptr[0];
        nefis_errno = convert_ieee(&vp , &cp, 2*SIZE_BINT8, SIZE_BINT8, "INTEGER", from_xdr);
        for (i=0; i<2; i++ )
        {
            cel_buf->ptr[i] = *( (BUInt8 *) vp+i );
        }
        free( (BData) vp );
    }
    else
    {
        if (nefis[set].file_version ==  Version_1)
        {
          MCR_shift_string_forward(cel_buf->st+12, 24, 12);
          MCR_L_from_int4_to_int8 (&cel_buf->in[0], 12/4);
        }
    }
    *next_pointer = (BUInt8) cel_buf->ptr[0];
    /* .......... = cel.ptr[1]; not used */
    /* .......... = cel.st+offset[2]; not used */

    strncpy(cel_name    ,cel_buf->st+offset[3], MAX_NAME);

    index  = offset[4]/SIZE_BINT8;
    if ( nefis[set].daf_neutral == TRUE ||
         nefis[set].def_neutral == TRUE    )
    {
        if ( nefis[set].file_version == Version_1 )
        {
            MCR_shift_string_forward(cel_buf->st+offset[4],size_cel_union-offset[4]-4, 4)
            MCR_B_from_int4_to_int8 ((BInt4 *) (cel_buf->st+offset[4]), 1);
        }
        vp = (BData) malloc ( sizeof(BUInt8 ) * 1 );
        cp = (BText) &cel_buf->ptr[index];
        nefis_errno = convert_ieee(&vp , &cp, 1*SIZE_BINT8, SIZE_BINT8, "INTEGER", from_xdr);
        cel_buf->ptr[index] = *( (BUInt8 *) vp );
        free( (BData) vp );
    }
    else
    {
        if (nefis[set].file_version ==  Version_1)
        {
            MCR_shift_string_forward(cel_buf+offset[4],SIZE_CEL_BUF+*cel_num_dim*MAX_NAME-offset[4]-4, 4)
            MCR_L_from_int4_to_int8 ((BInt4 *) (cel_buf+offset[4]), 1);
        }
    }
    *cel_num_bytes = cel_buf->ptr[index];
/*
 * Retrieve the element names from the cell buffer
 */
    if (*cel_num_dim > 0)
    {
        *elm_names = (BText) malloc(*cel_num_dim * (MAX_NAME+1));
        for (i=0; i<*cel_num_dim; i++ )
        {
            strncpy(&elm_names[0][i*(MAX_NAME+1)], cel_buf->st+offset[6]+i*MAX_NAME, MAX_NAME);
            elm_names[0][(i+1)*(MAX_NAME+1)-1] = '=';
        }
    }
    free(cel_buf->st);
    free(cel_buf);
    return nefis_errno;
}
/*==========================================================================*/
BInt4 HS_get_cont_elm (BInt4    set             ,
                       BUInt8   pointer         ,
                       BUInt8 * next_pointer    ,
                       BText    elm_name        ,
                       BText    elm_type        ,
                       BText    elm_quantity    ,
                       BText    elm_unity       ,
                       BText    elm_desc        ,
                       BUInt4 * elm_num_dim     ,
                       BUInt4 * elm_dimens      ,
                       BUInt4 * elm_single_bytes,
                       BUInt8 * elm_num_bytes   )
{
    union   element
    {
        BChar   st [SIZE_ELM_BUF];
        BUInt4  in [SIZE_ELM_BUF/SIZE_BINT4];
        BUInt8  ptr[SIZE_ELM_BUF/SIZE_BINT8];
    } elm;
    BText   cp      ;
    BInt4   fds     ;
    BInt4   from_xdr;
    BUInt4  i       ;
    BUInt4  index   ;
    BUInt8  n_read  ;
    BUInt4  offset[11];
    BUInt8  size;
    BData   vp      ;

    if ( nefis[set].one_file == TRUE )
    {
        fds  = nefis[set].daf_fds;
    }
    else
    {
        fds  = nefis[set].def_fds;
    }

    offset[ 0] = 0;
    offset[ 1] = offset[ 0] + SIZE_BINT8;
    offset[ 2] = offset[ 1] + SIZE_BINT8;
    offset[ 3] = offset[ 2] + 8;
    offset[ 4] = offset[ 3] + MAX_NAME;
    offset[ 5] = offset[ 4] + MAX_TYPE;
    offset[ 6] = offset[ 5] + SIZE_BINT8;
    offset[ 7] = offset[ 6] + SIZE_BINT4;
    offset[ 8] = offset[ 7] + MAX_NAME;
    offset[ 9] = offset[ 8] + MAX_NAME;
    offset[10] = offset[ 9] + MAX_DESC;
    size = sizeof(elm.st);
    if (nefis[set].file_version ==  Version_1)
    {
        size = sizeof(elm.st) - 16;
    }

/*
 *  read contents of element
 */
    n_read = GP_read_file (fds, elm.st, pointer, size);

    if ( n_read > (BInt4) sizeof(elm.st) )
    {
        nefis_errcnt += 1;
        nefis_errno   = 7005;
        sprintf(error_text,
        "During reading of element structure");
        return nefis_errno;
    }


    from_xdr = 1;
    if ( nefis[set].daf_neutral == TRUE ||
         nefis[set].def_neutral == TRUE    )
    {
        if (nefis[set].file_version ==  Version_1)
        {
            MCR_shift_string_forward(elm.st+12, sizeof(elm.st)-12-12, 12);
            MCR_B_from_int4_to_int8 ((BInt4 *) elm.st, 12/4);
        }
        vp = (BData) malloc ( sizeof(BUInt8) * 2 );
        cp = (BText) &elm.ptr[ 0];
        nefis_errno = convert_ieee(&vp , &cp, 2*SIZE_BINT8, SIZE_BINT8, "INTEGER", from_xdr);
        for ( i=0; i<2; i++ )
        {
            elm.ptr[ 0+i] = *( (BUInt8 *) vp+i );
        }
        free( (BData) vp );
    }
    else
    {
        if (nefis[set].file_version ==  Version_1)
        {
            MCR_shift_string_forward(elm.st+12, sizeof(elm.st)-12-12, 12);
            MCR_L_from_int4_to_int8 ((BInt4 *) elm.st, 12/4);
        }
    }
    *next_pointer = elm.ptr[0];
   /* ........... = elm.ptr[1]; not used */
   /* ........... = elm.st+offset[2]; not used */

    strncpy(elm_name    ,elm.st+offset[3],  MAX_NAME);
    strncpy(elm_type    ,elm.st+offset[4],  MAX_TYPE);

    index  = offset[5]/SIZE_BINT8;
    if ( nefis[set].daf_neutral == TRUE ||
         nefis[set].def_neutral == TRUE    )
    {
        if (nefis[set].file_version ==  Version_1)
        {
            MCR_shift_string_forward(elm.st+offset[5], sizeof(elm.st)-offset[5]-4, 4);
            MCR_B_from_int4_to_int8 ((BInt4 *) (elm.st+offset[5]), 1);
        }
        vp = (BData) malloc ( sizeof(BUInt8 ) * 1 );
        cp = (BText) &elm.ptr[index];
        nefis_errno = convert_ieee(&vp , &cp, 1*SIZE_BINT8, SIZE_BINT8, "INTEGER", from_xdr);
        elm.ptr[index] = *( (BUInt8 *) vp );
        free( (BData) vp );
    }
    else
    {
        if (nefis[set].file_version ==  Version_1)
        {
            MCR_shift_string_forward(elm.st+offset[5], sizeof(elm.st)-offset[5]-4, 4);
            MCR_L_from_int4_to_int8 ((BInt4 *) (elm.st+offset[5]), 1);
        }
    }
    *elm_num_bytes = elm.ptr[index];

    index  = offset[6]/SIZE_BINT4;
    if ( nefis[set].daf_neutral == TRUE ||
         nefis[set].def_neutral == TRUE    )
    {
        vp = (BData) malloc ( sizeof(BInt4 ) * 1 );
        cp = (BText) &elm.in[index];
        nefis_errno = convert_ieee(&vp , &cp,  1*SIZE_BINT4, SIZE_BINT4, "INTEGER", from_xdr);
        elm.in [index] = *( (BInt4 *) vp );
        free( (BData) vp );
    }
    *elm_single_bytes = elm.in [index];

    strncpy(elm_quantity,elm.st+offset[7],  MAX_NAME);

    strncpy(elm_unity   ,elm.st+offset[8],  MAX_NAME);

    strncpy(elm_desc    ,elm.st+offset[9],  MAX_DESC);

    index  = offset[10]/SIZE_BINT4;
    if ( nefis[set].daf_neutral == TRUE ||
         nefis[set].def_neutral == TRUE    )
    {
        vp = (BData) malloc ( sizeof(BInt4 ) * 6 );
        cp = (BText) &elm.in[index];
        nefis_errno = convert_ieee(&vp , &cp,  6*SIZE_BINT4, SIZE_BINT4, "INTEGER", from_xdr);
        for ( i=0; i<MAX_DIM+1; i++ )
        {
            elm.in[index+i] = *( (BUInt4 *) vp+i );
        }
        free( (BData) vp );
    }
    *elm_num_dim  = elm.in[index];
    for ( i=0; i<MAX_DIM; i++ )
    {
        elm_dimens[i] = elm.in[index+1+i];
    }

    return nefis_errno;
}
/*==========================================================================*/
BInt4 HS_get_cont_grp ( BInt4    set         ,
                        BUInt8   pointer     ,
                        BUInt8 * next_pointer,
                        BText    grp_name    ,
                        BText    cel_name    ,
                        BUInt4 * grp_num_dim ,
                        BUInt4 * grp_dimens  ,
                        BUInt4 * grp_order   )
{
    union     group
    {
        BChar   st [SIZE_GRP_BUF];
        BUInt4  in [SIZE_GRP_BUF/SIZE_BINT4];
        BUInt8  ptr[SIZE_GRP_BUF/SIZE_BINT8];
    } grp;
    BText   cp       ;
    BInt4   fds     ;
    BInt4   from_xdr ;
    BUInt4  i        ;
    BUInt4  index    ;
    BUInt8  n_read   ;
    BUInt4  offset[6];
    BUInt8  size;
    BData   vp       ;

    grp_name[MAX_NAME] ='\0';

    if ( nefis[set].one_file == TRUE )
    {
        fds  = nefis[set].daf_fds;
    }
    else
    {
        fds  = nefis[set].def_fds;
    }

    offset[ 0] = 0;
    offset[ 1] = offset[ 0] + SIZE_BINT8;
    offset[ 2] = offset[ 1] + SIZE_BINT8;
    offset[ 3] = offset[ 2] + 8;
    offset[ 4] = offset[ 3] + MAX_NAME;
    offset[ 5] = offset[ 4] + MAX_NAME;
    size = sizeof(grp.st);
    if (nefis[set].file_version ==  Version_1)
    {
        size = sizeof(grp.st) - 12;
    }

/*
 *  read contents of group
 */
    n_read = GP_read_file (fds, grp.st, pointer, size);

    if ( n_read > (BInt4) size )
    {
        nefis_errcnt += 1;
        nefis_errno   = 7006;
        sprintf(error_text,
        "During reading of group structure");
        return nefis_errno;
    }


    from_xdr = 1;
    if ( nefis[set].daf_neutral == TRUE ||
         nefis[set].def_neutral == TRUE    )
    {
        if (nefis[set].file_version ==  Version_1)
        {
            MCR_shift_string_forward(grp.st+12, sizeof(grp.st)-12-12, 12);
            MCR_B_from_int4_to_int8 ((BInt4 *) grp.st, 12/4);
        }
        vp = (BData) malloc ( sizeof(BUInt8) * 1);
        cp = (BText) &grp.ptr[0];
        nefis_errno = convert_ieee(&vp , &cp,  1*SIZE_BINT8, SIZE_BINT8, "INTEGER", from_xdr);
        grp.ptr[0] = *( (BUInt8 *) vp );
        free( (BData) vp );
    }
    else
    {
        if (nefis[set].file_version ==  Version_1)
        {
            MCR_shift_string_forward(grp.st+12, sizeof(grp.st)-12-12, 12);
            MCR_L_from_int4_to_int8 ((BInt4 *) grp.st, 12/4);
        }
    }
    *next_pointer  = grp.ptr[ 0];
    /* ........... = grp.ptr[1]; not used */
    /* ........... = grp.st+ofset[2]; not used */

    strncpy(grp_name    ,grp.st+offset[3],  MAX_NAME);

    strncpy(cel_name    ,grp.st+offset[4],  MAX_NAME);

    index  = offset[5]/SIZE_BINT4;
    if ( nefis[set].daf_neutral == TRUE ||
         nefis[set].def_neutral == TRUE    )
    {
        vp = (BData) malloc ( SIZE_BINT4 * (2*MAX_DIM+1));
        cp = (BText) &grp.in[index];
        nefis_errno = convert_ieee(&vp , &cp, (2*MAX_DIM+1)*SIZE_BINT4, SIZE_BINT4, "INTEGER", from_xdr);
        for ( i=0; i<(2*MAX_DIM+1); i++ )
        {
            grp.in[index+i] = *( (BInt4 *) vp +i);
        }
        free( (BData) vp );
    }
    *grp_num_dim   = grp.in [index];
    for ( i=0; i<MAX_DIM; i++ )
    {
        grp_dimens[i] = grp.in[index+1+i];
    }
    for ( i=0; i<MAX_DIM; i++ )
    {
        grp_order [i] = grp.in[index+1+MAX_DIM+i];
    }

    return nefis_errno;
}
/*==========================================================================*/
/*
 * Return value
 *        0       Succesful
 *        1       Not succesful
 */
BInt4 HS_get_cont_dat ( BInt4    set         ,
                        BUInt8   pointer     ,
                        BUInt8 * next_pointer,
                        BText    grp_name    ,
                        BText    grp_defined )
{
    union     dat_union{
        BChar   st [ 3*SIZE_BINT8+2*MAX_NAME];
        BUInt4  in [(3*SIZE_BINT8+2*MAX_NAME)/SIZE_BINT4];
        BUInt8  ptr[(3*SIZE_BINT8+2*MAX_NAME)/SIZE_BINT8];
    } data;
    BText   cp       ;
    BInt4   fds      ;
    BInt4   from_xdr ;
    BUInt8  i        ;
    BUInt8  n_read   ;
    BUInt8  offset[6];
    BUInt8  size     ;
    BData   vp       ;

    if ( nefis[set].one_file == TRUE )
    {
        fds  = nefis[set].daf_fds;
    }
    else
    {
        fds  = nefis[set].dat_fds;
    }

    offset[ 0] = 0;
    offset[ 1] = offset[ 0] + SIZE_BINT8;
    offset[ 2] = offset[ 1] + SIZE_BINT8;
    offset[ 3] = offset[ 2] + 8;
    offset[ 4] = offset[ 3] + MAX_NAME;
    offset[ 5] = offset[ 4] + MAX_NAME;
    size = sizeof(data.st);
    if (nefis[set].file_version ==  Version_1)
    {
        size = sizeof(data.st) - 2*SIZE_BINT4 - 4;
    }

/*
 *  read contents of group
 */
    n_read = GP_read_file (fds, data.st, pointer, size);

    if ( n_read > (BInt4) sizeof(data.st) )
    {
        nefis_errcnt += 1;
        nefis_errno   = 7007;
        sprintf(error_text,
        "During reading of data structure");
        return nefis_errno;
    }

    from_xdr = 1;
    if ( nefis[set].daf_neutral == TRUE ||
         nefis[set].dat_neutral == TRUE    )
    {
        if (nefis[set].file_version ==  Version_1)
        {
          MCR_shift_string_forward(data.st+12, sizeof(data.st)-12-12, 12);
          MCR_B_from_int4_to_int8 ((BInt4 *) data.st, 12/4);
        }
        vp = (BData) malloc ( sizeof(BUInt8) * 2 );
        cp = (BText) &data.ptr[0];
        nefis_errno = convert_ieee(&vp , &cp, 2*SIZE_BINT8, SIZE_BINT8, "INTEGER", from_xdr);
        for ( i=0; i<2; i++ )
        {
            data.ptr[ 0+i] = *( (BUInt8 *) vp +i);
        }
        free( (BData) vp );
    }
    else
    {
        if (nefis[set].file_version ==  Version_1)
        {
            MCR_shift_string_forward(data.st+12, sizeof(data.st)-12-12, 12);
            MCR_L_from_int4_to_int8 ((BInt4 *) data.st, 12/4);
        }
    }
    *next_pointer = data.ptr[0];
    /* .......... = data.ptr[1]; not used */
    /* .......... = data.st+ofset[2]; not used */

    strncpy(grp_name    ,data.st+offset[3],  MAX_NAME);

    strncpy(grp_defined ,data.st+offset[4],  MAX_NAME);

    return nefis_errno;
}
/*==========================================================================*/
static BInt4 HS_hash_key  ( BText  key           ,
                             BUInt4 len_hash_table)
{
/*
 * Computes hash_key [0,len_hash_table-1]
 */
/*
 * Return value
 *        -1      No entry in hash table found
 *        others  entry in hash table
 */

  BUInt4  hash_code = 0;
  BInt4   i;
  BUInt4  tmp=1;

  if ( (BInt4)strlen(key) == 0 )
  {
    return -1;
  }

  for ( i=0; i<MAX_NAME; i++ )
  {
    if ( i<(BInt4)strlen(key) )
    {
      tmp = tmp * (key[i] + 1);
    }
    else
    {
      tmp = tmp * ( ' ' + 1 );
    }
    if ( (BInt4) (i+1)%4 == 0 )
    {
      hash_code = hash_code + tmp;
      tmp = 1;
    }
  }

  hash_code = hash_code % len_hash_table;

  return hash_code;
}
/*==========================================================================*/
