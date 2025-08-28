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
 *   <df.c> - Functions related to define the element, cel group and data set
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
 *   Define_element:  Defines an element on definition file
 *   Define_cel    :  Defines a  cel on definition file
 *   Define_group  :  Defines a group on definition file
 *   Define_data   :  Defines a group on the data file
 */
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <limits.h>
#include <ctype.h>

#if defined(_WIN32) || defined(GNU_PC)
#  include <io.h>
#elif defined(salford32)
#  include <io.h>
#  include <windows.h>
#endif

#include "btps.h"
#include "nefis.h" /* needed for definition of LENGTH_ERROR_MESSAGE */
#include "nef-def.h"
#include "df.h"

BInt4 DF_check_bytes_elm(BUInt8 *, BText, BUInt4, BUInt4);

/*==========================================================================*/
/*     * * * * * * * * * * * * * DESCRIPTION * * * * * * * * * * * * *
 *
 *    - This function defines a new element, identified by its
 *      element name, on a NEFIS definition file by specifying the
 *      element type, description, etc.
 */
/*
 *      return: 0    no error occured
 *              !=0  error occured
 */
/*
 *  8 byte pointers (Nefis5)
 *
 *  elm_buf.s[  0-  7] = elm_buf.p[ 0] = next_pnt_hash
 *  elm_buf.s[  8- 15] = elm_buf.p[ 1] = rest_length (174-16=168)
 *  elm_buf.s[ 16- 23] = "       1"
 *  elm_buf.s[ 24- 47] = element name        (16 bytes)
 *  elm_buf.s[ 40- 45] = element type        ( 8 bytes)
 *  elm_buf.s[ 48- 55] = elm_buf.p[ 6] = size of element in bytes
 *  elm_buf.s[ 56- 59] = elm_buf.i[14] = size of single element in bytes
 *  elm_buf.s[ 60- 75] = element quantity    (16 bytes)
 *  elm_buf.s[ 76- 91] = element unity       (16 bytes)
 *  elm_buf.s[ 92-155] = element description (64 bytes)
 *  elm_buf.s[156-159] = elm_buf.i[39] = elm_dimens       (N<=5)
 *  elm_buf.s[160-163] = elm_buf.i[40] = elm_num_dim[0]
 *  elm_buf.s[164-167] = elm_buf.i[41] = elm_num_dim[1]
 *  elm_buf.s[168-171] = elm_buf.i[42] = elm_num_dim[2]
 *  elm_buf.s[172-175] = elm_buf.i[43] = elm_num_dim[3]
 *  elm_buf.s[176-179] = elm_buf.i[44] = elm_num_dim[4]
 */
/*==========================================================================*/
BInt4 Define_element ( BInt4    set         ,
                       BText    elm_name    ,
                       BText    elm_type    ,
                       BText    elm_quantity,
                       BText    elm_unity   ,
                       BText    elm_desc    ,
                       BUInt4   elm_single_bytes,
                       BUInt4   elm_num_dim ,
                       BUInt4 * elm_dimens  )
{
    BText    cp           ;
    BUInt8 * eof_ptr      ;
    BInt4    fds          ;
    BInt4    from_xdr     ;
    BUInt8 * hashtb       ;
    BUInt4   i            ;
    BUInt4   index        ;
    BUInt4   max_copy     ;
    BUInt8   n_written = 0;
    BUInt4   new_entry    ;
    BUInt8   prv_pnt_hash ;
    BUInt8   next_pnt_hash;
    BUInt8   num_array_elem;
    BUInt8   num_bytes  ;
    BUInt4   offset[11]   ;
    BUInt4   pnt_bucket   ;
    BUInt8   pnt_hash     ;
    BUInt8   rest_length  ;
    BUInt4   size_elm_buf = SIZE_ELM_BUF;
    BData    vp           ;
/*
 * Remark: Character string does not contain \0 at end of elm_buf.st
 */
    for ( i=0; i<size_elm_buf; i++ )
    {
        elm_buf.st[i]= ' ';
    }
/*
 * Number of dimensions must be [1,MAX_DIM]
 */
    if (  ( elm_num_dim < 1       ) ||
          ( elm_num_dim > MAX_DIM )    )
    {
        nefis_errcnt += 1;
        nefis_errno   = 5001;
        sprintf(error_text,
        "Number of dimensions not within the range [1,%d]\n      Element \'%s\' has dimension %ld",
        MAX_DIM, elm_name, elm_num_dim);
        return nefis_errno;
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

    if ( nefis[set].one_file == TRUE )
    {
        fds     =  nefis[set].daf_fds;
        eof_ptr = &nefis[set].daf.fds[1];
        hashtb  = &nefis[set].daf.fds[2];
    }
    else
    {
        fds     =  nefis[set].def_fds;
        eof_ptr = &nefis[set].def.fds[1];
        hashtb  = &nefis[set].def.fds[2];
    }
    if ( nefis[set].file_version == Version_1 )
    {
        size_elm_buf -= 16;    /* correction for 32-bits version (4 pointers) */
    }
    for ( i=0; i<(BInt4)strlen(elm_type); i++ )
    {
        elm_type[i] = (BChar) toupper( (BInt4) elm_type[i] );
    }

    elm_buf.ptr[0] = NIL;
    elm_buf.ptr[1] = NIL;

    strncpy(elm_buf.st+offset[2], "       1"      , 8);

    max_copy = min( MAX_NAME, (BUInt4) strlen(elm_name    ) );
    strncpy(elm_buf.st+offset[3], elm_name    , max_copy);

    max_copy = min( MAX_TYPE, (BUInt4) strlen(elm_type    ) );
    strncpy(elm_buf.st+offset[4], elm_type    , max_copy);

    nefis_errno = DF_check_bytes_elm(&num_bytes, elm_type, elm_single_bytes, elm_num_dim);
    if (nefis_errno != 0)
    {
        return nefis_errno;
    }

    num_array_elem = 1;
    for (i=0; i<elm_num_dim; i++ )
    {
        num_array_elem = num_array_elem * (BUInt8) elm_dimens[i];
    }
    index  = offset[5]/SIZE_BINT8;
    elm_buf.ptr[index] = num_bytes  * num_array_elem;

    index  = offset[6]/SIZE_BINT4;
    elm_buf.in [index] = (BUInt4) num_bytes;

    max_copy = min( MAX_NAME, (BUInt4) strlen(elm_quantity) );
    strncpy(elm_buf.st+offset[7], elm_quantity, max_copy);

    max_copy = min( MAX_NAME, (BUInt4) strlen(elm_unity   ) );
    strncpy(elm_buf.st+offset[8], elm_unity   , max_copy);

    max_copy = min( MAX_DESC, (BUInt4) strlen(elm_desc    ) );
    strncpy(elm_buf.st+offset[9], elm_desc    , max_copy);

    index = offset[10]/SIZE_BINT4;
    elm_buf.in[index] = elm_num_dim;
    for ( i=0; i<MAX_DIM; i++ )
    {
        elm_buf.in[index+1+i] = elm_dimens[i];
    }

/*
 *  Check for element name
 */
    nefis_errno = HS_check_ecg (set   , fds      , elm_name   , NIL       , hashtb,
                                LHSHEL, &pnt_hash, &pnt_bucket, &new_entry);
    if ( new_entry == 0 )
    {
        nefis_errcnt += 1;
        nefis_errno   = 5007;
        sprintf(error_text,
        "Element \'%s\' already exists\n", elm_name);
    }
    if ( nefis_errno != 0 )
    {
        return nefis_errno;
    }

    if ( pnt_hash == NIL )
    {
/*
 *    New element will be created at end-of-file, point to end of file
 */
        pnt_hash      = *eof_ptr;
        next_pnt_hash = *(hashtb+pnt_bucket);

        if ( nefis[set].file_version == Version_1 )
        {
            rest_length = (BUInt8) (size_elm_buf - 2*SIZE_BINT4);
        }
        else
        {
            rest_length = (BUInt8) (size_elm_buf - 2*SIZE_BINT8);
        }

        elm_buf.ptr[0] = next_pnt_hash;
        elm_buf.ptr[1] = rest_length ;
/*
 *    Set pointer to end-of-file and write buffer
 */
        if ( nefis[set].daf_neutral == TRUE ||
             nefis[set].def_neutral == TRUE    )
        {
            from_xdr = 0;
            cp = (BText) malloc  (sizeof(elm_buf.st) );

            vp = (BData) &elm_buf.ptr[ 0];
            nefis_errno = convert_ieee( &vp, &cp, 2*SIZE_BINT8, SIZE_BINT8, "INTEGER", from_xdr);
            for ( i=0; i<2; i++ )
            {
                elm_buf.ptr[ 0+i] = *( (BUInt8 *) cp + i);
            }

            index = offset[5]/SIZE_BINT8;
            vp = (BData) &elm_buf.ptr[index];
            nefis_errno = convert_ieee( &vp, &cp, 1*SIZE_BINT8, SIZE_BINT8, "INTEGER", from_xdr);
            elm_buf.ptr[index] = *( (BUInt8 *) cp );

            index = offset[6]/SIZE_BINT4;
            vp = (BData) &elm_buf.in[index];
            nefis_errno = convert_ieee( &vp, &cp, 1*SIZE_BINT4, SIZE_BINT4, "INTEGER", from_xdr);
            elm_buf.in[ index] = *( (BUInt4 *) cp );

            index = offset[10]/SIZE_BINT4;
            vp = (BData) &elm_buf.in[index];
            nefis_errno = convert_ieee( &vp, &cp, 6*SIZE_BINT4, SIZE_BINT4, "INTEGER", from_xdr);
            for ( i=0; i<6; i++ )
            {
                elm_buf.in[index+i] = *( (BUInt4 *) cp + i);
            }
            free( (BData) cp         );
        }

        if ( nefis[set].file_version == Version_1 )
        {
            if ( nefis[set].daf_neutral == TRUE ||
                 nefis[set].def_neutral == TRUE    )
            {
                MCR_B_from_int8_to_int4 ((BInt4 *) elm_buf.st, 2);
                MCR_B_from_int8_to_int4 ((BInt4 *) (elm_buf.st+offset[5]),1);
                MCR_shift_string_backward(elm_buf.st+offset[6]  , sizeof(elm_buf.st)-offset[6]  , 4);
                MCR_shift_string_backward(elm_buf.st+offset[3]-4, sizeof(elm_buf.st)-offset[3]+4, 12);
            }
            else
            {
                MCR_L_from_int8_to_int4 ((BInt4 *) elm_buf.st, 2);
                MCR_L_from_int8_to_int4 ((BInt4 *) (elm_buf.st+offset[5]), 1);
                MCR_shift_string_backward(elm_buf.st+offset[6]  , sizeof(elm_buf.st)-offset[6]  , 4);
                MCR_shift_string_backward(elm_buf.st+offset[3]-4, sizeof(elm_buf.st)-offset[3]+4, 12);
            }
        }
        n_written = GP_write_file( fds, elm_buf.st, *eof_ptr, size_elm_buf);

        if (n_written != size_elm_buf )
        {
            nefis_errcnt += 1;
            nefis_errno   = 5008;
            sprintf(error_text,
            "Error writing element \'%s\' to definition file\n", elm_name);
            return nefis_errno;
        }

        prv_pnt_hash  = *eof_ptr;
        next_pnt_hash = prv_pnt_hash + n_written;
        if ( next_pnt_hash < prv_pnt_hash )
        {
            nefis_errcnt += 1;
            nefis_errno   = 5024;
            if ( nefis[set].one_file == TRUE )
            {
                sprintf(error_text,
                   "Maximum size reached in DataDefinition file \"%s\"\n",nefis[set].daf_name);
            }
            else
            {
                sprintf(error_text,
                   "Maximum size reached in definition file\"%s\"\n",nefis[set].def_name);
            }
            return nefis_errno;
        }
        *eof_ptr = next_pnt_hash;
        *(hashtb+pnt_bucket) = pnt_hash;
    }
    return nefis_errno;
}
/*==========================================================================*/
/*     * * * * * * * * * * * * * DESCRIPTION * * * * * * * * * * * * *
 *
 *    - - This function defines a new cel, identified by its
 *        cel name, on a NEFIS definition file by specifying
 *        an array of pre-defined element names
 */
/*
 *      return: 0    no error occured
 *              !=0  error occured
 */
/*
 *  8 byte pointers (Nefis5)
 *
 *  cel_buf.s[  0-  7] = cel_buf.p[ 0] = next_pnt_hash
 *  cel_buf.s[  8- 15] = cel_buf.p[ 1] = rest_length
 *  cel_buf.s[ 16- 23] = "       2"
 *  cel_buf.s[ 24- 39] = cel name            (16 bytes)
 *  cel_buf.s[ 40- 47] = cel_buf.p[ 5] = size of cel in bytes
 *  cel_buf.s[ 48- 51] = cel_buf.i[12] = cel dimension
 *
 *  cel_buf.s[ 52- 67] = 1st element name    (16 bytes)
 *  cel_buf.s[ 68- 83] = 2nd element name    (16 bytes)
 *                ...  = ...
 *                ...  = ...
 */
/*==========================================================================*/

BInt4 Define_cel ( BInt4   set       ,
                   BText   cel_name  ,
                   BUInt4  cel_dimens,
                   BText   elm_names )
{
    BText    cp         ;
    BChar    elm_desc    [MAX_DESC+1];
    BUInt4   elm_dimens  [MAX_DIM   ];
    BChar    elm_name    [MAX_NAME+1];
    BUInt8   elm_num_bytes;
    BUInt4   elm_num_dim;
    BChar    elm_quantity[MAX_NAME+1];
    BUInt4   elm_single_bytes;
    BChar    elm_type    [MAX_TYPE+1];
    BChar    elm_unity   [MAX_NAME+1];
    BUInt8 * eof_ptr    ;
    BInt4    fds        ;
    BInt4    from_xdr   ;
    BUInt8 * hashtb     ;
    BUInt4   i          ;
    BUInt4   index      ;
    BUInt4   max_copy   ;
    BUInt8   n_written=0;
    BUInt4   new_entry  ;
    BUInt8   prv_pnt_hash ;
    BUInt8   next_pnt_hash;
    BUInt8   next_pointer ;
    BUInt4   offset[7]  ;
    BUInt4   pnt_bucket ;
    BUInt8   pnt_hash   ;
    BUInt8   rest_length;
    BInt4    size_cel_buf = SIZE_CEL_BUF;
    BData    vp         ;
    union cel * cel_buf ;
    BInt4 size_cel_union;
/*
 * Initialize local variables
 */
    elm_name    [MAX_NAME] = '\0';
    elm_type    [MAX_TYPE] = '\0';
    elm_quantity[MAX_NAME] = '\0';
    elm_unity   [MAX_NAME] = '\0';
    elm_desc    [MAX_DESC] = '\0';

    offset[ 0] = 0;
    offset[ 1] = offset[ 0] + SIZE_BINT8;
    offset[ 2] = offset[ 1] + SIZE_BINT8;
    offset[ 3] = offset[ 2] + 8;
    offset[ 4] = offset[ 3] + MAX_NAME;
    offset[ 5] = offset[ 4] + SIZE_BINT8;
    offset[ 6] = offset[ 5] + SIZE_BINT4;

    if ( nefis[set].one_file == TRUE )
    {
        fds     =  nefis[set].daf_fds;
        eof_ptr = &nefis[set].daf.fds[1];
        hashtb  = &nefis[set].daf.fds[2];
    }
    else
    {
        fds     =  nefis[set].def_fds;
        eof_ptr = &nefis[set].def.fds[1];
        hashtb  = &nefis[set].def.fds[2];
    }
    if ( nefis[set].file_version == Version_1 )
    {
        size_cel_buf -= 4*SIZE_BINT4;   /* correction for 32-bits file */
    }
/*
 * Remark: Character string does not contain \0 at end of cel_buf.st
 */
    size_cel_union = sizeof(char) * (SIZE_CEL_BUF + cel_dimens*MAX_NAME);
    cel_buf = (union cel *) malloc(sizeof(char *));
    cel_buf->st = (char *) malloc(size_cel_union);
    for ( i=0; i<SIZE_CEL_BUF+cel_dimens*MAX_NAME; i++ )
    {
        cel_buf->st[i] = ' ';
    }

    cel_buf->ptr[0] = NIL;
    cel_buf->ptr[1] = NIL;

    strncpy(cel_buf->st+offset[2], "       2"      , 8);

    max_copy = min( MAX_NAME, (BInt4)strlen(cel_name    ) );
    strncpy(cel_buf->st+offset[3], cel_name    , max_copy);

    index  = offset[4]/SIZE_BINT8;
    cel_buf->ptr[index] = 0;

    index  = offset[5]/SIZE_BINT4;
    cel_buf->in [index] = cel_dimens;

/*
 *  Loop over all elements contained in this cel
 */
    index  = offset[4]/SIZE_BINT8;
    for ( i=0; i<cel_dimens; i++ )
    {
        max_copy = min( MAX_NAME, (BUInt4) strlen(elm_names+i*(MAX_NAME+1)) );
        strncpy( elm_name, elm_names + i*(MAX_NAME+1), max_copy );
        elm_name[max_copy] = '\0';

/*
 *    Check for existence element name
 */
        nefis_errno = HS_check_ecg (set   , fds       , elm_name   , NIL       , hashtb ,
                                    LHSHEL, &pnt_hash , &pnt_bucket, &new_entry);
        if ( nefis_errno != 0 )
        {
            return nefis_errno;
        }

        if ( new_entry == 0 )
        {
            /*
             *  element found on definition file
             *  put element name in cel-buffer
             *  and add element length to cel size
             */
            nefis_errno = HS_get_cont_elm( set       , pnt_hash  ,&next_pointer,
                                           elm_name  , elm_type  , elm_quantity,
                                           elm_unity , elm_desc  ,&elm_num_dim ,
                                           elm_dimens, &elm_single_bytes, &elm_num_bytes);
            if ( nefis_errno != 0 )
            {
                return nefis_errno;
            }

            max_copy = min( MAX_NAME, (BUInt4) strlen(elm_name) );
            strncpy( cel_buf->st+offset[6]+MAX_NAME*i, elm_name, max_copy );
            size_cel_buf += MAX_NAME;
            /*
             * Align the element size to macro ALIGNMENT
             */
            if ( elm_num_bytes%ALIGNMENT != 0 )
            {
                elm_num_bytes += (ALIGNMENT - elm_num_bytes%ALIGNMENT);
            }
            cel_buf->ptr[index] += elm_num_bytes;
        }
        else
        {
            nefis_errcnt += 1;
            nefis_errno   = 5009;
            sprintf(error_text,
              "Element \'%s\' does not exist\n", elm_name);
            return nefis_errno;
        }
    }
/*
 *  Check for existence cel name
 */
    nefis_errno = HS_check_ecg (set   , fds      , cel_name   , NIL      , hashtb+LHSHEL ,
                                LHSHCL, &pnt_hash, &pnt_bucket, &new_entry);
    if ( new_entry == 0 )
    {
        nefis_errcnt += 1;
        nefis_errno   = 5010;
        sprintf(error_text,
        "Cel \'%s\' already exists\n", cel_name);
    }
    if ( nefis_errno != 0 )
    {
      return nefis_errno;
    }

    if ( pnt_hash == NIL )
    {
/*
 *    New cel, point to end of file
 */
        pnt_hash      = *eof_ptr;
        next_pnt_hash = *(hashtb+LHSHEL+pnt_bucket);

        if ( nefis[set].file_version == Version_1 )
        {
            rest_length   = (BUInt8) (size_cel_buf - 2*SIZE_BINT4);
        }
        else
        {
            rest_length   = (BUInt8) (size_cel_buf - 2*SIZE_BINT8);
        }

        cel_buf->ptr[0] = next_pnt_hash;
        cel_buf->ptr[1] = rest_length  ;
/*
 *    Set pointer to end-of-file and write buffer
 */
        if ( nefis[set].def_neutral == TRUE ||
             nefis[set].daf_neutral == TRUE    )
        {
            from_xdr = 0;
            cp = (BText) malloc (size_cel_union);

            vp = (BData) &cel_buf->ptr[0];
            nefis_errno = convert_ieee( &vp, &cp, 2*SIZE_BINT8, SIZE_BINT8, "INTEGER", from_xdr);
            for ( i=0; i<2; i++ )
            {
                cel_buf->ptr[   i] = *( (BUInt8 *) cp + i);
            }

            /* size of cel in bits */
            index = offset[4]/SIZE_BINT8;
            vp = (BData) &cel_buf->ptr[index];
            nefis_errno = convert_ieee( &vp, &cp, 1*SIZE_BINT8, SIZE_BINT8, "INTEGER", from_xdr);
            cel_buf->ptr[index] = *( (BUInt8 *) cp );

            index  = offset[5]/SIZE_BINT4;
            vp = (BData) &cel_buf->in[index];
            nefis_errno = convert_ieee( &vp, &cp, 1*SIZE_BINT4, SIZE_BINT4, "INTEGER", from_xdr);
            cel_buf->in[index] = *( (BUInt4 *) cp );

            free( (BData) cp );
        }

        if ( nefis[set].file_version == Version_1 )
        {
            if ( nefis[set].daf_neutral == TRUE ||
                 nefis[set].def_neutral == TRUE    )
            {
                MCR_B_from_int8_to_int4 ((BInt4 *) cel_buf->st, 2);
                MCR_B_from_int8_to_int4 ((BInt4 *) (cel_buf->st+offset[4]),1);
                MCR_shift_string_backward(cel_buf->st+offset[5]  , size_cel_union-offset[5]  , 4);
                MCR_shift_string_backward(cel_buf->st+offset[3]-4, size_cel_union-offset[3]+4, 12);
            }
            else
            {
                MCR_L_from_int8_to_int4 ((BInt4 *) cel_buf->st, 2);
                MCR_L_from_int8_to_int4 ((BInt4 *) (cel_buf->st+offset[5]), 1);
                MCR_shift_string_backward(cel_buf->st+offset[6]  , size_cel_union-offset[6]  , 4);
                MCR_shift_string_backward(cel_buf->st+offset[3]-4, size_cel_union-offset[3]+4, 12);
            }
        }
        n_written = GP_write_file( fds, cel_buf->st, *eof_ptr, size_cel_buf);

        if ( n_written != size_cel_buf )
        {
            nefis_errcnt += 1;
            nefis_errno   = 5011;
            sprintf(error_text,
            "Error writing cel \'%s\' to definition file.\n", cel_name);
            return nefis_errno;
        }

        prv_pnt_hash = *eof_ptr;
        next_pnt_hash = prv_pnt_hash + n_written;
        if (next_pnt_hash < prv_pnt_hash)
        {
            nefis_errcnt += 1;
            nefis_errno   = 5026;
            if ( nefis[set].one_file == TRUE )
            {
                sprintf(error_text,
                "Maximum size reached in definition file\"%s\"\n",nefis[set].daf_name);
            }
            else
            {
                sprintf(error_text,
                "Maximum size reached in definition file\"%s\"\n",nefis[set].def_name);
            }
            return nefis_errno;
        }
        *eof_ptr = next_pnt_hash;
        *(hashtb+LHSHEL+pnt_bucket) = pnt_hash;
    }
    free(cel_buf->st);
    free(cel_buf);
    return nefis_errno;
}
/*==========================================================================*/
/*     * * * * * * * * * * * * * DESCRIPTION * * * * * * * * * * * * *
 *
 *    - This function defines a new group, identified by its
 *      group name, on a NEFIS definition file by specifying
 *      the name of the cel to be used in this group, the number
 *      of dimensions and the length of each dimension
 */
/*
 *      return: 0    error occured
 *              !=0  error occured
 */
/*
 *  grp_buf.s[  0-  7] = grp_buf.p[ 0] = next_pointer
 *  grp_buf.s[  8- 15] = grp_buf.p[ 1] = rest_length      (.. = ..-16 bytes)
 *  grp_buf.s[ 16- 23] = "       3"
 *  grp_buf.s[ 24- 39] = group   name                     (16 bytes)
 *  grp_buf.s[ 40- 55] = cel name used in this group      (16 bytes)
 *  grp_buf.s[ 56- 59] = grp_buf.i[11] = grp_num_dim      (N<=5)
 *  grp_buf.s[ 60- 63] = grp_buf.i[12] = grp_dimens[0]
 *  grp_buf.s[ 64- 67] = grp_buf.i[13] = grp_dimens[1]
 *  grp_buf.s[ 68- 71] = grp_buf.i[14] = grp_dimens[2]
 *  grp_buf.s[ 72- 75] = grp_buf.i[15] = grp_dimens[3]
 *  grp_buf.s[ 76- 79] = grp_buf.i[16] = grp_dimens[4]
 *  grp_buf.s[ 80- 83] = grp_buf.i[17] = grp_order[0]
 *  grp_buf.s[ 84- 87] = grp_buf.i[18] = grp_order[1]
 *  grp_buf.s[ 88- 91] = grp_buf.i[19] = grp_order[2]
 *  grp_buf.s[ 92- 95] = grp_buf.i[20] = grp_order[3]
 *  grp_buf.s[ 96- 99] = grp_buf.i[21] = grp_order[4]
 */
/*==========================================================================*/
BInt4 Define_group ( BInt4   set         ,
                     BText   grp_name    ,
                     BText   cel_name    ,
                     BUInt4  grp_num_dim ,
                     BInt4 * grp_dimens  ,
                     BInt4 * grp_order   )
{
  BText    cp           ;
  BUInt8 * eof_ptr      ;
  BInt4    fds          ;
  BInt4    from_xdr     ;
  BUInt8 * hashtb       ;
  BUInt4   i            ;
  BUInt4   index        ;
  BUInt4   max_copy     ;
  BUInt8   n_written=0  ;
  BUInt4   new_entry    ;
  BUInt8   prv_pnt_hash ;
  BUInt8   next_pnt_hash;
  BUInt4   offset[6]    ;
  BUInt4   pnt_bucket   ;
  BUInt8   pnt_hash     ;
  BUInt8   rest_length  ;
  BUInt8   size_grp_buf = SIZE_GRP_BUF;
  BData    vp;
/*
 *  number of dimensions must be > 0 and < 6
 */
    if ( ( grp_num_dim < 1       ) &&
         ( grp_num_dim > MAX_DIM )    )
    {
        nefis_errcnt += 1;
        nefis_errno   = 5016;
        sprintf(error_text,
        "Number of dimensions not within the range [1,MAX_DIM]\n Group \'%s\'",
        grp_name);
        return nefis_errno;
    }
/*
 * Remark: Character string does not contain \0 at end of grp_buf.st
 */
    for ( i=0; i<SIZE_GRP_BUF; i++ )
    {
        grp_buf.st[i]= ' ';
    }

    offset[ 0] = 0;
    offset[ 1] = offset[ 0] + SIZE_BINT8;
    offset[ 2] = offset[ 1] + SIZE_BINT8;
    offset[ 3] = offset[ 2] + 8;
    offset[ 4] = offset[ 3] + MAX_NAME;
    offset[ 5] = offset[ 4] + MAX_NAME;

    if ( nefis[set].one_file == TRUE )
    {
        fds     =  nefis[set].daf_fds;
        eof_ptr = &nefis[set].daf.fds[1];
        hashtb  = &nefis[set].daf.fds[2];
    }
    else
    {
        fds     =  nefis[set].def_fds;
        eof_ptr = &nefis[set].def.fds[1];
        hashtb  = &nefis[set].def.fds[2];
    }
    if ( nefis[set].file_version == Version_1 )
    {
        size_grp_buf -= 3*SIZE_BINT4; /* 3 pointers */
    }

/*
 * Remark: Character string does not contain \0 at end of grp_buf.st
 */
    for ( i=0; i<SIZE_GRP_BUF; i++ )
    {
        grp_buf.st[i]= ' ';
    }

    grp_buf.ptr[0] = NIL;
    grp_buf.ptr[1] = NIL;

    strncpy(grp_buf.st+offset[2], "       3"   , 8);

    max_copy = min( MAX_NAME, (BInt4)strlen(grp_name) );
    strncpy(grp_buf.st+offset[3], grp_name , max_copy );

    max_copy = min( MAX_NAME, (BInt4)strlen(cel_name) );
    strncpy(grp_buf.st+offset[4], cel_name , max_copy );

    index = offset[5]/SIZE_BINT4;
    grp_buf.in[index] = grp_num_dim;
    for ( i=0; i<grp_num_dim; i++ )
    {
        grp_buf.in[index+1        +i] = grp_dimens[i];
        grp_buf.in[index+1+MAX_DIM+i] = grp_order[i];
    }
    for ( i=grp_num_dim; i<MAX_DIM; i++ )
    {
        grp_buf.in[index+1        +i] = 1;
        grp_buf.in[index+1+MAX_DIM+i] = i+1;
    }
/*
 *  check existence of cel definition
 */
    nefis_errno = HS_check_ecg (set   , fds      , cel_name   , NIL       , hashtb+LHSHEL       ,
                                LHSHCL, &pnt_hash, &pnt_bucket, &new_entry);
    if ( new_entry == 1 )
    {
      nefis_errcnt += 1;
      nefis_errno   = 5013;
      sprintf(error_text,
        "Cel \'%s\' does not exist on file \'%s\'.\n",
        cel_name, nefis[set].def_name);
    }
    if ( nefis_errno != 0 )
    {
      return nefis_errno;
    }
/*
 *  Check for group name
 */
    nefis_errno = HS_check_ecg (set   , fds      , grp_name   , NIL       , hashtb+LHSHEL+LHSHCL ,
                                LHSHGR, &pnt_hash, &pnt_bucket, &new_entry);
    if ( new_entry == 0 )
    {
      nefis_errcnt += 1;
      nefis_errno   = 5014;
      sprintf(error_text,
        "Group \'%s\' already exists on file \'%s\'.\n",
         grp_name, nefis[set].def_name );
    }
    if ( nefis_errno != 0 )
    {
      return nefis_errno;
    }
    if ( pnt_hash == NIL )
    {
        /*
         *    New group, point to end of file
         */
        pnt_hash      = *eof_ptr;
        next_pnt_hash = *(hashtb+LHSHEL+LHSHCL+pnt_bucket);
        if ( nefis[set].file_version == Version_1 )
        {
            rest_length   = (BUInt8) ( size_grp_buf - 2*SIZE_BINT4 );
        }
        else
        {
            rest_length   = (BUInt8) ( size_grp_buf - 2*SIZE_BINT8 );
        }
        grp_buf.ptr[0] = next_pnt_hash;
        grp_buf.ptr[1] = rest_length ;
        /*
        *    Set pointer to end-of-file and write buffer
        */
        if ( nefis[set].def_neutral == TRUE ||
             nefis[set].daf_neutral == TRUE    )
        {
            from_xdr = 0;
            cp = (BText) malloc ( sizeof(grp_buf.st) );

            vp = (BData) &grp_buf.ptr[ 0];
            nefis_errno = convert_ieee( &vp, &cp, 2*SIZE_BINT8, SIZE_BINT8, "INTEGER", from_xdr);
            for ( i=0; i<2; i++ )
            {
                grp_buf.ptr[   i] = *( (BUInt8 *) cp + i);
            }

            index = offset[5]/SIZE_BINT4;
            vp = (BData) &grp_buf.in[index];
            nefis_errno = convert_ieee( &vp, &cp, 11*SIZE_BINT4, SIZE_BINT4, "INTEGER", from_xdr);
            for ( i=0; i<11; i++ )
            {
                grp_buf.in[index+i] = *((BUInt4 *)cp + (BUInt4)i);
            }
            free( (BData) cp );
        }

        if ( nefis[set].file_version == Version_1 )
        {
            if ( nefis[set].daf_neutral == TRUE ||
                 nefis[set].def_neutral == TRUE    )
            {
                MCR_B_from_int8_to_int4 ((BInt4 *) grp_buf.st, 2);
                MCR_shift_string_backward(grp_buf.st+offset[3]-4, sizeof(grp_buf.st)-offset[3]+4, 12);
            }
            else
            {
                MCR_L_from_int8_to_int4 ((BInt4 *) grp_buf.st, 2);
                MCR_shift_string_backward(grp_buf.st+offset[3]-4, sizeof(grp_buf.st)-offset[3]+4, 12);
            }
        }
        n_written = GP_write_file( fds, grp_buf.st, *eof_ptr, size_grp_buf);

        if ( n_written != (BUInt8) size_grp_buf )
        {
            nefis_errcnt += 1;
            nefis_errno   = 5015;
            sprintf(error_text,
            "Error on writing group \'%s\' to definition file.\n", grp_name);
            return nefis_errno;
        }

        prv_pnt_hash = *eof_ptr;
        next_pnt_hash = prv_pnt_hash + n_written;
        if ( next_pnt_hash < prv_pnt_hash  )
        {
            nefis_errcnt += 1;
            nefis_errno   = 5028;
            if ( nefis[set].one_file == TRUE )
            {
                sprintf(error_text,
                "Maximum size reached in definition file \'%s\'\n",nefis[set].daf_name);
            }
            else
            {
                sprintf(error_text,
                "Maximum size reached in definition file\"%s\"\n",nefis[set].def_name);
            }
            return nefis_errno;
        }
        *eof_ptr = next_pnt_hash;
        *(hashtb+LHSHEL+LHSHCL+pnt_bucket) = pnt_hash;
    }
    return nefis_errno;
}
/*==========================================================================*/
/*     * * * * * * * * * * * * * DESCRIPTION * * * * * * * * * * * * *
 *
 *    Claim space for a data group on a NEFIS data file
 */
/*
 *      return: 0    error occured
 *              !=0  error occured
 */
/*
 *  Fix dimensions:
 *  dat_buf.s[  0-  7] = dat_buf.p[ 0] = next_pnt_hash
 *  dat_buf.s[  8- 15] = dat_buf.p[ 1] = rest_length ( . -16= . )
 *  dat_buf.s[ 16- 23] = "       4"   vaste dimensies
 *  dat_buf.s[ 24- 39] = group name        (16 bytes)
 *  dat_buf.s[ 40- 55] = group definition  (16 bytes)
 *  dat_buf.s[ 56-135] = integer attribute names  (80 bytes = 5*16)
 *  dat_buf.s[136-155] = dat_buf.in[31] ... [35]
 *  dat_buf.s[156-275] = real    attribute names  (80 bytes)
 *  dat_buf.s[276-295] = dat_buf.re[56] ... [60]
 *  dat_buf.s[296-375] = string  attribute names  (80 bytes)
 *  dat_buf.s[376-455] = string  attribute values (80 bytes)
 *
 *  Variable dimensions:
 *  dat_buf.s[  0-  7] = dat_buf.p[ 0] = next_pnt_hash
 *  dat_buf.s[  8- 15] = dat_buf.p[ 1] = rest_length ( . -16= . )
 *  dat_buf.s[ 16- 23] = "       5"   variable dimensions
 *  dat_buf.s[ 24- 39] = group name        (16 bytes)
 *  dat_buf.s[ 40- 55] = group definition  (16 bytes)
 *  dat_buf.s[ 56-135] = integer attribute names  (80 bytes = 5*16)
 *  dat_buf.s[136-155] = dat_buf.in[31] ... [35]
 *  dat_buf.s[156-275] = real    attribute names  (80 bytes)
 *  dat_buf.s[276-295] = dat_buf.re[56] ... [60]
 *  dat_buf.s[296-375] = string  attribute names  (80 bytes)
 *  dat_buf.s[376-455] = string  attribute values (80 bytes)
 *  dat_buf.s[456-463] = dat_buf.p[101] =
 *  dat_buf.s[464-469] = dat_buf.p[102] = pointer_1
 *  .........
 *  dat_buf.s[2040-2047] = dat_buf.p[267] = pointer_256
 */
/*==========================================================================*/
BInt4 Define_data ( BInt4   set         ,
                    BText   grp_name    ,
                    BText   grp_defined )
{
    BChar   cel_name  [MAX_NAME+1];
    BText   elm_names    ;
    BUInt8  cel_num      ;
    BUInt8  cel_num_bytes;
    BUInt4  cel_num_dim  ;
    BText   cp           ;
    BInt4   dat_fds      ;
    BInt4   def_fds      ;
    BUInt8 * dat_eof_ptr;
    BUInt8 * def_eof_ptr;
    BInt4   from_xdr     ;
    BUInt4  grp_dimens[MAX_DIM   ];
    BUInt8  grp_num_bytes;
    BUInt4  grp_num_dim  ;
    BUInt4  grp_order [MAX_DIM   ];
    BUInt8 * dat_hashtb;
    BUInt8 * def_hashtb;
    BUInt4  i            ;
    BUInt4  index        ;
    BUInt4  max_copy     ;
    BUInt8  n_written = 0;
    BUInt4  new_entry    ;
    BUInt8  next_pointer = 0;
    BUInt8  prv_pnt_hash ;
    BUInt8  next_pnt_hash;
    BUInt4  offset[8]   ;
    BUInt4  pnt_bucket   ;
    BUInt8  pnt_hash     ;
    BUInt8  rest_length  ;
    BUInt4  size_dat_buf = SIZE_DAT_BUF;
    BUInt8  var_bytes_cel = 0;
    BInt4   var_dim = FALSE;
    BData   vp;

    for ( i=0; i<SIZE_DAT_BUF; i++ )
    {
        dat_buf.st[i]= ' ';
    }

    if ( nefis[set].one_file == TRUE )
    {
        dat_fds =  nefis[set].daf_fds;
        def_fds =  nefis[set].daf_fds;
        dat_eof_ptr = &nefis[set].daf.fds[1];
        def_eof_ptr = &nefis[set].daf.fds[1];
        dat_hashtb  = &nefis[set].daf.fds[2+LHSHEL+LHSHCL+LHSHGR];
        def_hashtb  = &nefis[set].daf.fds[2];
    }
    else
    {
        dat_fds =  nefis[set].dat_fds;
        def_fds =  nefis[set].def_fds;
        dat_eof_ptr = &nefis[set].dat.fds[1];
        def_eof_ptr = &nefis[set].def.fds[1];
        dat_hashtb  = &nefis[set].dat.fds[2];
        def_hashtb  = &nefis[set].def.fds[2];
    }
    if ( nefis[set].file_version == Version_1 )
    {
        size_dat_buf -= 3*SIZE_BINT4;
    }

    for ( i=0; i<MAX_NAME; i++ )
    {
        cel_name[i]=' ';
    }
    cel_name[MAX_NAME] = '\0';

    offset[ 0] = 0;
    offset[ 1] = offset[ 0] + SIZE_BINT8;
    offset[ 2] = offset[ 1] + SIZE_BINT8;
    offset[ 3] = offset[ 2] + 8;
    offset[ 4] = offset[ 3] + MAX_NAME;
    offset[ 5] = offset[ 4] + MAX_NAME;
    offset[ 6] = offset[ 5] + MAX_DIM*(4*MAX_NAME+SIZE_BINT4+SIZE_REAL4);
    offset[ 7] = offset[ 6] + (1+256)*SIZE_BINT8; /* pointer table for variable dimensions */

/*
 *  Check for group name on definition file
 */
    nefis_errno = HS_check_ecg (set   , def_fds  , grp_defined, NIL       , def_hashtb+LHSHEL+LHSHCL  ,
                                LHSHGR, &pnt_hash, &pnt_bucket, &new_entry);

    if ( new_entry == 1 )
    {
        nefis_errcnt += 1;
        nefis_errno   = 5017;
        sprintf(error_text,
          "Group definition \'%s\' does not exist on file \'%s\'.\n",
           grp_defined, nefis[set].def_name);
    }
    if ( nefis_errno != 0 )
    {
        return nefis_errno;
    }
    nefis_errno = HS_get_cont_grp( set        , pnt_hash ,&next_pointer,
                                   grp_defined, cel_name ,&grp_num_dim ,
                                   grp_dimens , grp_order);
    if ( nefis_errno != 0 )
    {
        return nefis_errno;
    }
/*
 *  Check for cel name
 */
    nefis_errno = HS_check_ecg (set   , def_fds  , cel_name   , NIL       , def_hashtb+LHSHEL,
                                LHSHCL, &pnt_hash, &pnt_bucket, &new_entry);
    if ( new_entry == 1 )
    {
        nefis_errcnt += 1;
        nefis_errno   = 5018;
        sprintf(error_text,
          "Cel \'%s\' does not exist on file \'%s\'.\n",
           cel_name, nefis[set].def_name);
    }
    if ( nefis_errno != 0 )
    {
        return nefis_errno;
    }
    elm_names = NULL;
    nefis_errno = HS_get_cont_cel ( set          , pnt_hash   ,&next_pointer,
                                    cel_name     , &elm_names  ,&cel_num_dim ,
                                   &cel_num_bytes);

    if ( nefis_errno != 0 )
    {
        return nefis_errno;
    }

/*
 *  Compute group size in bytes
 */
    cel_num = 1;
    for (i=0; i<grp_num_dim; i++ )
    {
        if ( grp_dimens[i] > 0 )
        {
            cel_num = cel_num * (BUInt8) grp_dimens[i];
        }
        else
        {
            var_dim     =  TRUE;
        }
    }

    grp_num_bytes = cel_num * cel_num_bytes;
    var_bytes_cel = (BUInt8) -1;
    if ( var_dim == TRUE )
    {
        grp_num_bytes = SIZE_BINT8*(1+256);
        var_bytes_cel = cel_num * cel_num_bytes;
        strncpy(dat_buf.st+offset[2], "       5"      , 8        );
    }
    else
    {
        strncpy(dat_buf.st+offset[2], "       4"      , 8        );
    }

    max_copy = min( MAX_NAME, (BUInt4) strlen(grp_name) );
    strncpy(dat_buf.st+offset[3], grp_name    , max_copy );

    max_copy = min( MAX_NAME, (BUInt4) strlen(grp_defined) );
    strncpy(dat_buf.st+offset[4], grp_defined , max_copy );

/*
 *  Check for group name, on data file
 */
    nefis_errno = HS_check_ecg (set   , dat_fds  , grp_name   , NIL       , dat_hashtb               ,
                                LHSHDT, &pnt_hash, &pnt_bucket, &new_entry);
    if ( new_entry == 0 )
    {
        pnt_hash      = 0;
        nefis_errcnt += 1;
        nefis_errno   = 5019;
        sprintf(error_text,
          "Data group \'%s\' already exists in data file \'%s\'\n",
           grp_name, nefis[set].dat_name);
    }
    if ( nefis_errno != 0 )
    {
        return nefis_errno;
    }
    if ( pnt_hash == NIL )
    {
/*
 *  New group will be created at end-of-file, point to end of file
 */
        pnt_hash      = *dat_eof_ptr;
        next_pnt_hash = *(dat_hashtb+pnt_bucket);
        rest_length   = (BUInt8) size_dat_buf + grp_num_bytes; /*  - 2*SIZE_BINT8 consistency????; */
        if ( nefis[set].file_version == Version_1 )
        {
            rest_length   = (BUInt8) size_dat_buf + SIZE_BINT4*(1+256);
        }

        dat_buf.ptr[0] = next_pnt_hash;
        dat_buf.ptr[1] = rest_length ;

        if ( nefis[set].dat_neutral == TRUE ||
             nefis[set].daf_neutral == TRUE    )
        {
            from_xdr = 0;
            cp = (BText) malloc ( sizeof(dat_buf.st) );

            vp = (BData) &dat_buf.ptr[ 0];
            nefis_errno = convert_ieee( &vp, &cp, 2*SIZE_BINT8, SIZE_BINT8, "INTEGER", from_xdr);
            for (i=0; i<2; i++ )
            {
                dat_buf.ptr[ 0+i] = *((BUInt8 *)cp + i);
            }
            free( (BData) cp );
        }

        if ( var_dim == TRUE )
        {
            index = SIZE_DAT_BUF/SIZE_BINT8;
            for (i=0; i<1+256; i++ )
            {
                dat_buf.ptr[index+i] = NIL;
            }
            dat_buf.ptr[index] = var_bytes_cel;

            if ( nefis[set].dat_neutral == TRUE ||
                 nefis[set].daf_neutral == TRUE    )
            {
                from_xdr = 0;
                cp = (BText) malloc ( SIZE_BINT8 * (1+256) );

                vp = (BData) &dat_buf.ptr[index];
                nefis_errno = convert_ieee( &vp, &cp, (1+256)*SIZE_BINT8, SIZE_BINT8, "INTEGER", from_xdr);
                for (i=0; i<1+256; i++ )
                {
                    dat_buf.ptr[index+i] = *( (BUInt8 *)cp + (BUInt8) i );
                }
                free( (BData) cp );
            }
        }

/*
 *  Set pointer to end-of-file and write buffer
 */
/*
 *  To save time not the total number of bytes is written
 *  but only those that are strictly necessary. Possibly also space.
 *  Data field does not not need to be described, that is usually large.
 *  Possibly it is better to put in a default value here.
 *  The file pointer dat.fds[1] is increased with the right amount.
 *  Also for non-variable dimension, the statement should have been the following:
 *     write( fd_abs, dat_buf.st, SIZE_DAT_BUF + grp_num_bytes  );
 */
        if ( nefis[set].file_version == Version_1 )
        {
            if ( nefis[set].daf_neutral == TRUE ||
                 nefis[set].def_neutral == TRUE    )
            {
                MCR_B_from_int8_to_int4 ((BInt4 *) dat_buf.st, 2);
                if ( var_dim == TRUE )
                {
                    MCR_B_from_int8_to_int4 ((BInt4 *) (dat_buf.st+offset[6]),1+256);
                }
                MCR_shift_string_backward(dat_buf.st+offset[3]-4, sizeof(dat_buf.st)-offset[3]+4, 12);
            }
            else
            {
                MCR_L_from_int8_to_int4 ((BInt4 *) dat_buf.st, 2);
                if ( var_dim == TRUE )
                {
                    MCR_L_from_int8_to_int4 ((BInt4 *) (dat_buf.st+offset[6]), 1+256);
                }
                MCR_shift_string_backward(dat_buf.st+offset[3]-4, sizeof(dat_buf.st)-offset[3]+4, 12);
            }
        }

        n_written = GP_write_file( dat_fds, dat_buf.st, *dat_eof_ptr, size_dat_buf);
        if ( n_written != size_dat_buf )
        {
            nefis_errcnt += 1;
            nefis_errno   = 5023;
            sprintf(error_text,
                "Error on writing fixed part of group \'%s\' \n",
                grp_name);
            return nefis_errno;
        }
        if ( var_dim == TRUE )
        {
            n_written = GP_write_file_pointers( dat_fds, dat_buf.st+size_dat_buf, *dat_eof_ptr+size_dat_buf, grp_num_bytes, set);
            if ( n_written != grp_num_bytes )
            {
                nefis_errcnt += 1;
                nefis_errno   = 5022;
                sprintf(error_text,
                    "Error on writing variable part of group \'%s\' (first pointer table).\n",
                    grp_name);
                return nefis_errno;
            }
            if ( nefis[set].file_version == Version_1)
            {
                grp_num_bytes = n_written/2;
            }
        }
        else
        {
            /* Nothing for group with fixed dimensions */
        }

        prv_pnt_hash = *dat_eof_ptr;
        next_pnt_hash = prv_pnt_hash + size_dat_buf+grp_num_bytes;
        if ( next_pnt_hash < prv_pnt_hash )
        {
            nefis_errcnt += 1;
            nefis_errno   = 5030;
            if ( nefis[set].one_file == TRUE )
            {
                sprintf(error_text,
                "Maximum size reached in DataDefinition file\"%s\"\n",nefis[set].daf_name);
            }
            else
            {
                sprintf(error_text,
                "Maximum size reached in definition file\"%s\"\n",nefis[set].def_name);
            }
            return nefis_errno;
        }
        *dat_eof_ptr = next_pnt_hash;
        *(dat_hashtb+pnt_bucket) = pnt_hash;
    }
    return nefis_errno;
}


/*==========================================================================*/
/*     * * * * * * * * * * * * * DESCRIPTION * * * * * * * * * * * * *
 *
 *    Determine the total bytes needed for the element
 */
/*
 *      return: 0    error occured
 *              !=0  error occured
 */
/*==========================================================================*/
BInt4 DF_check_bytes_elm(BUInt8 * total_bytes,
                         BText    elm_type,
                         BUInt4   elm_single_bytes,
                         BUInt4   elm_num_dim)
{
  BUInt8 num_bytes;

  elm_num_dim = elm_num_dim;
/*
 *  length of element in bytes
 */
  *total_bytes   = 0;
  if ( strncmp( elm_type, "REAL", 4 ) == 0 )
  {
      num_bytes = (BUInt8) sizeof( BRea4 );
      if ( num_bytes != elm_single_bytes )
      {
        num_bytes = (BUInt8) sizeof( BRea8 );
        if ( num_bytes != elm_single_bytes )
        {
          nefis_errcnt += 1;
          nefis_errno   = 5002;
          sprintf(error_text,
            "This size of real (%ld) is not supported\n",
            elm_single_bytes);
          return nefis_errno;
        }
      }
  }
  else if ( strncmp( elm_type, "INTEGER", 7 ) == 0 )
  {
      num_bytes = (BUInt8) sizeof( BInt4 );
      if ( num_bytes != elm_single_bytes )
      {
        num_bytes = (BUInt8) sizeof( BInt2 );
        if ( num_bytes != elm_single_bytes )
        {
          num_bytes = (BUInt8) sizeof( BInt8 );
          if ( num_bytes != elm_single_bytes )
          {
            nefis_errcnt += 1;
            nefis_errno   = 5003;
            sprintf(error_text,
              "This size of integer (%ld) is not supported\n",
            elm_single_bytes);
            return nefis_errno;
          }
        }
      }
  }
  else if ( strncmp( elm_type, "CHARACTE", 8 ) == 0 )
  {
      num_bytes = (BUInt8) sizeof( BChar ) * elm_single_bytes;
  }
  else if ( strncmp( elm_type, "COMPLEX", 7 ) == 0 )
  {
      num_bytes = 2 * (BUInt8) sizeof( BRea4 );
      if ( num_bytes != elm_single_bytes )
      {
        num_bytes = 2 * (BUInt8) sizeof( BRea8 );
        if ( num_bytes != elm_single_bytes )
        {
          nefis_errcnt += 1;
          nefis_errno   = 5004;
          sprintf(error_text,
            "This size of complex (%ld) is not supported\n",
            elm_single_bytes);
          return nefis_errno;
        }
      }
  }
  else if ( strncmp( elm_type, "LOGICAL", 7 ) == 0 )
  {
      num_bytes = (BUInt8) sizeof( BInt4 );
      if ( num_bytes != elm_single_bytes )
      {
        num_bytes = (BUInt8) sizeof( BInt2 );
        if ( num_bytes != elm_single_bytes )
        {
          nefis_errcnt += 1;
          nefis_errno   = 5005;
          sprintf(error_text,
            "This size of logical (%ld) is not supported\n",
            elm_single_bytes);
          return nefis_errno;
        }
      }
  }
  else
  {
    nefis_errcnt += 1;
    nefis_errno   = 5006;
    sprintf(error_text,
            "This element type is not supported \'%s\'\n",
            elm_type);
    return nefis_errno;
  }

  *total_bytes = num_bytes;
  return nefis_errno;
}
