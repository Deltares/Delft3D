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
 *   <pt.c> - Write data to the NEFIS data file
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
#include <limits.h>

#include "btps.h"
#include "nefis.h" /* needed for definition of LENGTH_ERROR_MESSAGE */
#include "nef-def.h"
#include "pt.h"

#define A2D(m,n)  ((n) + DEFINE_STEPS*(m))

/*==========================================================================*/
/*     * * * * * * * * * * * * * DESCRIPTION * * * * * * * * * * * * *
 *
 *    - This function writes data to elements in one ore more
 *      cells of a data group of a NEFIS data file, in the order the
 *      cells are stored on the file (so in sequential order)
 */
/*==========================================================================*/
BInt4 Put_element ( BInt4   set           ,
                    BText   grp_name      ,
                    BText   elm_name      ,
                    BInt4 * usr_index     ,
                    BInt4 * usr_order     ,
                    BData   getal         )
{
  BUInt4  array_offset[MAX_DIM];
  BUInt8  cel_num_bytes ;
  BUInt8  cel_pointer   ;
  BUInt8  conv_bytes    ;
  BText   cp          ;
  BInt4   dat_fds       ;
  BUInt4  element_offset = 0;
  BInt4   elm_dimens  [ MAX_DIM    ];
  BUInt4  elm_num_dim   ;
  BUInt4  elm_single_bytes;
  BChar   elm_type    [ MAX_TYPE+1 ];
  BUInt4  file_offset[MAX_DIM];
  BInt4   from_xdr    ;
  BInt4   grp_dimens [MAX_DIM];
  BUInt4  grp_num_dim         ;
  BInt4   grp_order  [MAX_DIM];
  BUInt8  grp_pointer   ;
  BUInt4  i, j, k, l, m, n, v;
  BUInt4  j1=1, j2=1, j3=1;
  BUInt8  j_array_offset;
  BUInt8  j_file_offset ;
  BUInt4  k1=1, k2=1, k3=1;
  BUInt8  k_array_offset;
  BUInt8  k_file_offset ;
  BUInt4  l1=1, l2=1, l3=1;
  BUInt8  l_array_offset;
  BUInt8  l_file_offset ;
  BUInt8  l_pointer     ;
  BUInt4  m1=1, m2=1, m3=1;
  BUInt8  m_array_offset;
  BUInt8  m_file_offset ;
  BUInt4  n1=1, n2=1, n3=1;
  BUInt8  n_array_offset = BUINT8_MAX;
  BUInt8  n_file_offset ;
  BUInt8  next_eof = 0;
  BUInt8  prev_eof = 0;
  BInt4   size_dat_buf_on_file = SIZE_DAT_BUF;
  BInt4   usr_array  [MAX_DIM];
  BUInt4  usr_offset [MAX_DIM];
  BUInt4  v1=1, v2=1, v3=1;
  BInt4   var_dim = FALSE ;
  BUInt8  var_file_offset = 0U;
  BInt4   var_num_dim = -1;
  BData   vp          ;
  BText   cp_cont = NULL;
  BUInt8  cp_cont_size;
  BUInt8  write_bytes   ;
  BUInt8  write_bytes_seq;

  BRea4  step_size_elm;
  BUInt4 n_elems_j;
  BUInt4 elems_per_loop;
  BUInt4 elems_to_write;
  BUInt4 elems_written_loop;
  BUInt4 zz;
  BUInt8 bytes_read;

  elm_type[MAX_TYPE]='\0';

  if ( nefis[set].one_file == TRUE )
  {
    dat_fds = nefis[set].daf_fds;
  }
  else
  {
    dat_fds = nefis[set].dat_fds;
  }

  if ( nefis[set].file_version == Version_1 )
  {
      size_dat_buf_on_file -= 3 * SIZE_BINT4;
  }
/*
 *  Retrieve information about group and element
 */
  nefis_errno = RT_retrieve ( set            , grp_name        , elm_name     ,
                             &cel_num_bytes  , (BUInt4 *) elm_dimens      ,&elm_num_dim  ,
                             &element_offset ,&elm_single_bytes, elm_type     ,
                              (BUInt4 *) grp_dimens     ,&grp_num_dim     , (BUInt4 *) grp_order    ,
                             &grp_pointer    ,&write_bytes     );

  if ( nefis_errno != 0 )
  {
    return nefis_errno;
  }

/*
 *  Check if user supplied indexes are within the range (group)
 */
  for ( i=0; i<grp_num_dim; i++ )
  {
    if ( usr_index[A2D(i,0)] > usr_index[A2D(i,1)] )
    {
      nefis_errcnt += 1;
      nefis_errno   = 4001;
      sprintf(error_text,
         "Start value user index [%ld,2]=%ld should be smaller than [%ld,2]=%ld\n",
            i,usr_index[A2D(i,0)], i,usr_index[A2D(i,1)] );
    }
    if ( usr_index[A2D(i,2)] < 1 )
    {
      nefis_errcnt += 1;
      nefis_errno   = 4002;
      sprintf(error_text,
         "Increment value user index [%ld,2]=%ld should be greater than 0\n",
          i,usr_index[A2D(i,2)] );
    }
    if ( usr_index[A2D(i,0)] < 1 )
    {
      nefis_errcnt += 1;
      nefis_errno = 4003;
      sprintf(error_text,
         "Start value user index [%ld,0]=%ld should be greater than zero\n",
          i,usr_index[A2D(i,0)] );
    }
    if ( grp_dimens[ usr_order[i]-1 ] > 0 )
    {
      if ( usr_index[A2D(i,1)] > grp_dimens[ usr_order[i]-1 ] )
      {
        nefis_errcnt += 1;
        nefis_errno = 4004;
        sprintf(error_text,
                "Stop value user index [%ld,1]=%ld should be smaller than %ld\n",
                 i, usr_index[A2D(i,1)], grp_dimens[ usr_order[i]-1 ] );
      }
    }
  }
  for ( i=grp_num_dim; i<MAX_DIM; i++ )
  {
    usr_index [A2D(i,0)] = 1;
    usr_index [A2D(i,1)] = 1;
    usr_index [A2D(i,2)] = 1;
    grp_dimens[i]    = 1;
    usr_order [i]    = i+1;
    grp_order [i]    = i+1;
  }

  for ( i=0; i<grp_num_dim; i++ )
  {
    if ( grp_dimens[ usr_order[i]-1 ] == 0 )
    {
      var_dim     = TRUE;
      var_num_dim = i   ;
      grp_dimens[ usr_order[i]-1 ] = 1;
    }
  }

  if ( nefis_errno != 0 )
  {
    return nefis_errno;
  }

  for ( i=0; i<MAX_DIM; i++ )
  {
    usr_array[ usr_order[i]-1 ] = i+1;
  }

  usr_offset[0] = 1;
  for ( i=1; i<MAX_DIM; i++ )
  {
    usr_offset[i] = (BUInt4)(
                    (usr_index[A2D(i-1,1)] -  usr_index[A2D(i-1,0)] +
                     usr_index[A2D(i-1,2)]) / usr_index[A2D(i-1,2)]) *
                     usr_offset[i-1];
  }

  for ( i=0; i<MAX_DIM; i++ )
  {
    array_offset[i] = usr_offset[ usr_array[ grp_order[i]-1 ]-1 ];
  }

  file_offset[0] = 1;
  for ( i=1; i<MAX_DIM; i++ )
  {
    file_offset[i] = (BUInt4)grp_dimens[ grp_order[i-1]-1 ] * file_offset[i-1];
  }
/*
 * Write elements to data file
 *
 * Usr_order: which dimension runs fastest in view of data
 * Grp_order: which dimension runs fastest on data file
 */

  if ( var_dim == TRUE )
  {
    v1          =  usr_index[A2D(var_num_dim,0)];
    v2          =  usr_index[A2D(var_num_dim,1)];
    v3          =  usr_index[A2D(var_num_dim,2)];
    usr_index [A2D(var_num_dim,0)] = 1;
    usr_index [A2D(var_num_dim,1)] = 1;
    usr_index [A2D(var_num_dim,2)] = 1;
  }

  if ( grp_num_dim >= 5 )
  {
    n1 = usr_index[ A2D(usr_array[ grp_order[4]-1 ]-1,0)];
    n2 = usr_index[ A2D(usr_array[ grp_order[4]-1 ]-1,1)];
    n3 = usr_index[ A2D(usr_array[ grp_order[4]-1 ]-1,2)];
  }
  if ( grp_num_dim >= 4 )
  {
    m1 = usr_index[ A2D(usr_array[ grp_order[3]-1 ]-1,0)];
    m2 = usr_index[ A2D(usr_array[ grp_order[3]-1 ]-1,1)];
    m3 = usr_index[ A2D(usr_array[ grp_order[3]-1 ]-1,2)];
  }
  if ( grp_num_dim >= 3 )
  {
    l1 = usr_index[ A2D(usr_array[ grp_order[2]-1 ]-1,0)];
    l2 = usr_index[ A2D(usr_array[ grp_order[2]-1 ]-1,1)];
    l3 = usr_index[ A2D(usr_array[ grp_order[2]-1 ]-1,2)];
  }
  if ( grp_num_dim >= 2 )
  {
    k1 = usr_index[ A2D(usr_array[ grp_order[1]-1 ]-1,0)];
    k2 = usr_index[ A2D(usr_array[ grp_order[1]-1 ]-1,1)];
    k3 = usr_index[ A2D(usr_array[ grp_order[1]-1 ]-1,2)];
  }
  if ( grp_num_dim >= 1 )
  {
    j1 = usr_index[ A2D(usr_array[ grp_order[0]-1 ]-1,0)];
    j2 = usr_index[ A2D(usr_array[ grp_order[0]-1 ]-1,1)];
    j3 = usr_index[ A2D(usr_array[ grp_order[0]-1 ]-1,2)];
  }

  conv_bytes = write_bytes   *
               (j2-j1+j3)/j3 *
               (k2-k1+k3)/k3 *
               (l2-l1+l3)/l3 *
               (m2-m1+m3)/m3 *
               (n2-n1+n3)/n3 *
               (v2-v1+v3)/v3 ;

/*
 * Create character array to write data to file
 */
  if ( nefis[set].daf_neutral == TRUE ||
       nefis[set].dat_neutral == TRUE    )
  {
    from_xdr = 0;
    vp       = (BData) getal;
    cp       = (BText) malloc ( (size_t) conv_bytes );
    nefis_errno =
      convert_ieee ( &vp, &cp, conv_bytes, elm_single_bytes, elm_type ,
                      from_xdr);
  }
  else
  {
    cp = (BText) getal;
  }

  for ( v=v1-1; v<v2; v=v+v3 )
  {

    if ( var_dim == TRUE )
    {
      nefis_errno = GP_variable_pointer( set   , &grp_pointer    ,
                                         v     , &var_file_offset);

      if ( nefis_errno != 0 )
      {
        return nefis_errno;
      }
      if ( nefis[set].one_file == TRUE )
      {
        prev_eof = nefis[set].daf.fds[1];
      }
      else
      {
        prev_eof = nefis[set].dat.fds[1];
      }

      if ( prev_eof == var_file_offset)
      {
        next_eof = prev_eof
                 +         cel_num_bytes *
                   (BUInt8)grp_dimens[0] *
                   (BUInt8)grp_dimens[1] *
                   (BUInt8)grp_dimens[2] *
                   (BUInt8)grp_dimens[3] *
                   (BUInt8)grp_dimens[4] ;
        if ( nefis[set].one_file == TRUE )
        {
          nefis[set].daf.fds[1] = next_eof;
        }
        else
        {
          nefis[set].dat.fds[1] = next_eof;
        }
      }
    }

    n_array_offset = (BUInt8)((v-v1+v3)/v3)*usr_offset[var_num_dim];
    for ( n=n1-1; n<n2; n=n+n3 )
    {
      n_file_offset  = (BUInt8)n*file_offset[4];
      m_array_offset = n_array_offset;
      for ( m=m1-1; m<m2; m=m+m3 )
      {
        m_file_offset  = n_file_offset + (BUInt8)m * file_offset[3];
        l_array_offset = m_array_offset;
        for ( l=l1-1; l<l2; l=l+l3 )
        {
          l_file_offset  = m_file_offset + (BUInt8)l * file_offset[2];
          k_array_offset = l_array_offset;
          for ( k=k1-1; k<k2; k=k+k3 )
          {
            k_file_offset  = l_file_offset + (BUInt8)k * file_offset[1];
            j_array_offset = k_array_offset;
            for ( j=j1-1; j<j2; j=j+j3 )
            {
              j_file_offset  = k_file_offset + (BUInt8)j;    /*  file_offset[0] = 1 */
              if ( var_dim == TRUE )
              {
                cel_pointer = var_file_offset +
                              cel_num_bytes * j_file_offset;
              }
              else
              {
                cel_pointer = grp_pointer + (BUInt8)size_dat_buf_on_file +
                              cel_num_bytes * j_file_offset;
              }

              /*
               *    j_file_offset = j +
               *                    k * jmax +
               *                    l * kmax * jmax +
               *                    m * lmax * kmax * jmax +
               *                    n * mmax * lmax * kmax * jmax
               */
              l_pointer = cel_pointer + (BUInt8) element_offset;

              // Output file step size. Note that this is different from
              // skipping values from the array of values passed in, which is
              // array_offset[*]. Note that write_bytes could be e.g. 12 with
              // a cell size of 16, which is why we use a float
              step_size_elm  = ((BRea4) cel_num_bytes) / write_bytes * j3;
              n_elems_j = (j2-j1+j3)/j3;

              // Check for possible shortcuts we can take.
              if ( cel_num_bytes == write_bytes && j3 == 1 && element_offset == 0 && array_offset[0] == 1 )
              {
                // We can do a sequential write instead of doing each value individually
                write_bytes_seq = write_bytes * (j2-j1+1);
                (BVoid) GP_write_file ( dat_fds  ,
                                        cp + j_array_offset*write_bytes,
                                        l_pointer, write_bytes_seq  );
                j += (j2-j1+1);
              }
              // FIXME: Magic factor where we think reading + writing is
              // faster than just writing + seeking. This number currently
              // says how many elements from the passed array are in one
              // single aggregrated write. Do some proper testing instead.
              else if ( ((BRea4) n_elems_j) / step_size_elm > 5.f )
              {
                // Try to write sequentially if the step size is not too large

                // Allocate at least a megabyte as the temporary buffer
                // TODO: Avoid unnecessary free/malloc calls if we have
                // multiple dimensions, and instead just reuse the same buffer
                // over and over.
                cp_cont_size = max( (size_t) conv_bytes, 1<<20 );
                cp_cont = malloc( cp_cont_size );

                // TODO: Make it work for multiple dimensions
                elems_per_loop = cp_cont_size / cel_num_bytes;
                elems_to_write = n_elems_j;
                elems_written_loop = 0;
                while ( elems_to_write > 0 )
                {
                  memset( cp_cont, 0, cp_cont_size );

                  // Check if we are beyond end of file, so that we do not
                  // have to read and also do not get an error (=0) back from
                  // read_file
                  bytes_read = GP_read_file( dat_fds, cp_cont, l_pointer, cp_cont_size );
                  elems_written_loop = 0;
                  for ( i = 0; i < elems_per_loop && elems_to_write > 0; i++, elems_to_write-- )
                  {
                    for ( zz = 0; zz < write_bytes; zz++ )
                    {
                      cp_cont[i*cel_num_bytes*j3+zz] = ((char *)cp)[j_array_offset*write_bytes+zz];
                    }
                    j_array_offset += array_offset[0];
                    elems_written_loop += 1;
                  }

                  GP_write_file( dat_fds, cp_cont, l_pointer, ((elems_written_loop-1)*j3+1)*cel_num_bytes);
                  l_pointer += elems_written_loop*cel_num_bytes;
                }

                free( cp_cont );
                j += (j2-j1+1)*j3;
              }
              else
              {
                (BVoid) GP_write_file ( dat_fds  ,
                                        cp + j_array_offset*write_bytes,
                                        l_pointer, write_bytes  );
                j_array_offset += array_offset[0];
              }
            }
            k_array_offset += array_offset[1];
          }
          l_array_offset += array_offset[2];
        }
        m_array_offset += array_offset[3];
      }
      n_array_offset += array_offset[4];
    }
  } /* end variable dimension loop */

  if ( nefis[set].daf_neutral == TRUE ||
       nefis[set].dat_neutral == TRUE    )
  {
    free( (BVoid  *) cp );
  }

  return nefis_errno;
}
/*==========================================================================*/

