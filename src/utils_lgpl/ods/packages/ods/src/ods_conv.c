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
/*
 *  ods_conv.c -  Routines for converting data in ODS (especially for
 *                the generic NEFIS files)
 *
 *  Arjen Markus
 */

/*
 *  General information:
 *  This file contains the following functions:
 *  - ODS_CopyArray():                Copy the data from one array to another
 *  - ODS_AverageOverDepth():         Average an array over depth
 *  - ODS_AccumulateData():           Accumulate data in one array
 *  - ODS_AccumulateOverTime():       Accumulate data over time
 *  - ODS_ConvertVectorComponents():  Convert vector components to
 *                                    desired output
 *  - ODS_ConvertKCS():               Convert a KCS array to a proper grid matrix
 *  - ODS_ConstructTrivialGrid():     Construct a trivial grid matrix
 *  - ODS_GetReferenceTime():         Get the reference date/time and scale factor
 */

/*
 *  $Author: Markus $
 *  $Date: 11/04/05 11:58a $
 *  $Source$
*/

/*
 * Include files and definitions
 * Note:
 * The file welcome.h defines the version number
 */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>
#include "ods.h"
#include "nefis.h"
#include "ods_gnf.h"
#include "ods_conv.h"

/*
 * Macros - none
 */

/*
 * Static variables and functions - none
 */


/* -------------------------------------------------------------------
    Function: ODS_CopyArray()
    Author:   Arjen Markus
    Purpose:  Copy the data from one array to the next
    Context:  Used by ODS_GetMat()
    Pseudo Code:
              In a loop, copy the data
    Note:
              I could have used memcpy(), but that uses chars. I feel
              uncomfortable with that
------------------------------------------------------------------- */

TVoid
ODS_CopyArray(
      TReal4            * data_in,       /* I incoming data array */
      TReal4            * data_out,      /* O outgoing data array */
      TInt4               nodata )       /* I number of data to be copied */
{
   TInt4  i       ;

   for ( i = 0 ; i < nodata ; i ++ )
   {
      data_out[i] = data_in[i] ;
   }

   return ;
}

/* -------------------------------------------------------------------
    Function: ODS_AverageOverDepth()
    Author:   Arjen Markus
    Purpose:  Average a 3D array over depth (using the z-coordinate)
    Context:  Used by various routines
    Pseudo Code:
              Use the information on the underlying grid and the
              relative thickness of the layers to determine an
              average over depth.
    Note:
              This routine uses the z-coordinate for information about
              the vertical distribution, so that both sigma-layers and
              z-layers are covered.
              It does assume, however, that the z-coordinate is
              given over the same times as the data themselves.
    Note:
              The LGRID array is a mask only, it is not used
              (right now) for positioning the data within the grid.
              (This is not completely trivial)
------------------------------------------------------------------- */

TVoid
ODS_AverageOverDepth(
      TInt4             * ndim,          /* I array with dimensions */
      TInt4             * lgrid,         /* I mask array */
      TReal4            * data3d,        /* I 3D data array */
      TReal4            * zcrd,          /* I z-coordinate */
      TReal4              misval,        /* I missing value */
      TReal4            * data,          /* O averaged data */
      TInt4             * nodata )       /* O number of data in output */
{
   TInt4  i       ;
   TInt4  k       ;
   TInt4  i3      ;
   TInt4  i4      ;
   TInt4  i5      ;
   TInt4  t       ;
   TInt4  nosegl  ;
   TInt4  nolay   ;
   TInt4  notimes ;
   TReal4 weight  ;

   nolay   = ndim[2]           ;
   nosegl  = ndim[3]           ;
   notimes = ndim[4]           ;
   *nodata = nosegl  * notimes ;

   i4      = 0                 ;
   for ( t = 0 ; t < notimes ; t ++ )
   {
      for ( i = 0 ; i < nosegl ; i ++ )
      {
         data[i4] = 0.0 ;
         if ( lgrid[i] <= 0 )
         {
            data[i4] = misval ;
         }
         i4 ++ ;
      }
   }

   for ( t = 0 ; t < notimes ; t ++ )
   {
      for ( i = 0 ; i < nosegl ; i ++ )
      {
         i3     = i + t * nosegl * nolay     ;
         i4     = i + t * nosegl             ;
         i5     = i + t * nosegl * (nolay+1) ;
         weight = 0.0                        ;
         if ( lgrid[i] > 0 )
         {
            for ( k = 0 ; k < nolay ; k ++ )
            {
               if ( data3d[i3] != misval && data3d[i3] != -999.0 )
               {
                  data[i4] = data[i4] + (zcrd[i5+nosegl]-zcrd[i5])*data3d[i3] ;
                  weight   = weight   + (zcrd[i5+nosegl]-zcrd[i5])            ;
                  i3       = i3       + nosegl                                ;
                  i5       = i5       + nosegl                                ;
               }
            }

            if ( weight != 0.0 )
            {
               data[i4] = data[i4] / weight ;
            }
            else
            {
               data[i4] = misval ;
            }
         }

/* ---
         fprintf( stderr, "%d %d %g %g %g %g\n", t, i, data[i4], weight,
            zcrd[i3], data3d[i3-nosegl] ) ;
--- */
      }
   }

   return ;
}

/* -------------------------------------------------------------------
    Function: ODS_AccumulateData()
    Author:   Arjen Markus
    Purpose:  Accumulate data from different parameters
    Context:  Used by various routines
    Pseudo Code:
              Simply add the data in the second array to the
              data in the first array. No assumption is made
              about the structure (i.e. masks of active/inactive
              elements etc.)
------------------------------------------------------------------- */

TVoid
ODS_AccumulateData(
      TReal4              misval,        /* I  missing value */
      TInt4             * ndim,          /* I  array with dimensions */
      TReal4            * data,          /* IO accumulative array */
      TReal4            * data_to_add,   /* I  data to be added */
      TReal4              factor      )  /* I  factor to multiply the added data by */
{
   TInt4 i       ;
   TInt4 k       ;
   TInt4 t       ;
   TInt4 i3      ;
   TInt4 nosegl  ;
   TInt4 nolay   ;
   TInt4 notimes ;

   nosegl  = ndim[0] * ndim[1] ; /* Perhaps ndim[3] is better ... */
   nolay   = ndim[2]           ;
   notimes = ndim[4]           ;
   if ( nolay == 0 ) nolay = 1 ; /* Take care of 2D parameters! */

   for ( t = 0 ; t < notimes ; t ++ )
   {
      for ( k = 0 ; k < nolay ; k ++ )
      {
         i3 = t * nosegl * nolay + k * nosegl ;
         for ( i = 0 ; i < nosegl ; i ++ )
         {
            if ( data[i3] != misval && data_to_add[i3] != misval )
            {
               data[i3] = data[i3] + data_to_add[i3] * factor ;
            }
            else
            {
               data[i3] = misval ;
            }
            i3 ++ ;
         }
      }
   }

   return ;
}

/* -------------------------------------------------------------------
    Function: ODS_AccumulateOverTime()
    Author:   Arjen Markus
    Purpose:  Accumulate data over time
    Context:  Used by various routines
    Pseudo Code:
              Simply add the data in the second array to the
              data in the first array, multiplied by the timestep
------------------------------------------------------------------- */

TVoid
ODS_AccumulateOverTime(
      TInt4             * ndim,          /* I  array with dimensions */
      TReal4            * data,          /* IO accumulative array */
      TReal4            * data_to_add,   /* I  data to be added */
      TReal4              deltat      )  /* I  time step */
{
   TInt4 i      ;
   TInt4 k      ;
   TInt4 i3     ;
   TInt4 nosegl ;
   TInt4 nolay  ;

   nosegl  = ndim[0] * ndim[1] ;
   nolay   = ndim[2]           ;
   if ( nolay == 0 ) nolay = 1 ; /* Take care of 2D parameters! */

   for ( k = 0 ; k < nolay ; k ++ )
   {
      i3 = k * nosegl ;
      for ( i = 0 ; i < nosegl ; i ++ )
      {
         data[i3] = data[i3] + deltat * data_to_add[i3] ;
         i3 ++ ;
      }
   }

   return ;
}

/* -------------------------------------------------------------------
    Function: ODS_ConvertVectorComponents()
    Author:   Arjen Markus
    Purpose:  Convert vector components to desired output
    Context:  Used by various routines
    Pseudo Code:
              Use the orientation of the local grid to convert to
              cartesian or polar components. Keep in mind:
              - average to the grid cell centre (note the presence of
                inactive cells!)
              - return desired cartesian or polar component
                (depending on the selected output)
              The parameter "truegrid" indicates whether we are
              dealing with grid-based data or with a timeseries
              (ndim[5] contains the number of times in that case)
    Note:
              The result index is:
              0 - x-component of the vector
              1 - y-component of the vector
              2 - direction (angle with positive x-axis)
              3 - magnitude (length of vector)
------------------------------------------------------------------- */

TVoid
ODS_ConvertVectorComponents(
      TInt4             * ndim,          /* I array with ALL dimensions */
      TInt4             * lgrid,         /* I mask array */
      TReal4            * data_u,        /* I data array "U" */
      TReal4            * data_v,        /* I data array "V" */
      TReal4            * angle,         /* I grid orientation */
      TInt4               truegrid,      /* I is it a "true" grid? */
      TReal4              misval,        /* I missing value */
      TInt4               result_idx,    /* I index of result (U or V) */
      TReal4            * result,        /* O result data */
      TInt4             * nodata )       /* O number of data in output */
{
   TInt4   i       ;
   TInt4   j       ;
   TInt4   k       ;
   TInt4   t       ;
   TInt4   imin    ;
   TInt4   imax    ;
   TInt4   jmin    ;
   TInt4   jmax    ;
   TInt4   idx     ;
   TInt4   idxl    ;
   TInt4   nx      ;
   TInt4   ny      ;
   TInt4   nu      ;
   TInt4   nv      ;
   TInt4   noseg   ;
   TInt4   nosegl  ;
   TInt4   notimes ;
   TInt4   nolay   ;
   TReal4  uterm   ;
   TReal4  vterm   ;
   TReal4  cosv    ;
   TReal4  sinv    ;

   nx      = ndim[0]                     ;
   ny      = ndim[1]                     ;
   if ( truegrid == 2 )
   {
      nx = ndim[1] ;
      ny = ndim[0] ;
   }

   nosegl  = ndim[3]                     ; /* The caller is responsible here! */
   notimes = ndim[4]                     ; /* The caller is responsible here! */
   nolay   = MAX(1,ndim[2])              ;
   noseg   = ndim[0] * ndim[1] * nolay   ;
   *nodata = noseg * notimes             ;

   if ( truegrid == 0 )
   {
      imin = 0    ;
      imax = nx   ;
      jmin = 0    ;
      jmax = ny   ;
   }
   if ( truegrid == 1 || truegrid == 2 )
   {
      imin = 1    ;
      imax = nx-1 ;
      jmin = 1    ;
      jmax = ny-1 ;
   }

   for ( t = 0 ; t < notimes ; t ++ )
   {
      for ( k = 0 ; k < nolay ; k ++ )
      {
         /* Make sure every array element is set
         */
         for ( j = 0 ; j < ny ; j ++ )
         {
            for ( i = 0 ; i < nx ; i ++ )
            {
               idx = i + j * nx + k * nosegl + t * noseg ;
               result[idx] = misval ;
            }
         }

         for ( j = jmin ; j < jmax ; j ++ )
         {
            for ( i = imin ; i < imax ; i ++ )
            {
               idx   = i + j * nx + k * nosegl + t * noseg ;
               idxl  = i + j * nx                          ;
               if ( lgrid[idxl] > 0 )
               {
                  uterm = 0.0 ;
                  vterm = 0.0 ;
                  nu    = 0   ;
                  nv    = 0   ;

                  if ( data_u[idx] != misval )
                  {
                     uterm = data_u[idx] ;
                     nu    = 1           ;
                  }
                  if ( data_v[idx] != misval )
                  {
                     vterm = data_v[idx] ;
                     nv    = 1           ;
                  }

                  if ( truegrid )
                  {
                     if ( lgrid[idxl-1] > 0 && data_u[idx-1] != misval )
                     {
                        uterm = uterm + data_u[idx-1] ;
                        nu    ++                      ;
                     }
                     if ( lgrid[idxl-nx] > 0 && data_v[idx-nx] != misval )
                     {
                        vterm = vterm + data_v[idx-nx] ;
                        nv    ++                       ;
                     }
                  }
                  if ( nu > 0 )
                  {
                     uterm = uterm / (float)nu ;
                  }
                  if ( nv > 0 )
                  {
                     vterm = vterm / (float)nv ;
                  }
                  switch ( result_idx )
                  {
                  case 0:
                     cosv        = cos(angle[idxl]/57.29578) ;
                     sinv        = sin(angle[idxl]/57.29578) ;
                     result[idx] = uterm * cosv - vterm * sinv ;
                     break ;
                  case 1:
                     cosv        = cos(angle[idxl]/57.29578) ;
                     sinv        = sin(angle[idxl]/57.29578) ;
                     result[idx] = uterm * sinv + vterm * cosv ;

                     /* ----
                     fprintf( stderr, "\ni,j: %d %d - %d %d\n", i,j, idx, idxl ) ;
                     fprintf( stderr, "cosv, sinv: %g %g\n", cosv, sinv ) ;
                     fprintf( stderr, "term u,v: %g %g - %g\n", uterm,
                         vterm, result[idx] ) ;
                     fprintf( stderr, "data u,v,angle: %g %g %g %d\n",
                        data_u[idx], data_v[idx], angle[idxl], lgrid[idxl] ) ;

                     if ( lgrid[idxl-1] > 0 )
                     {
                        fprintf( stderr, "data u,v (idx-1): %g %g %d\n",
                        data_u[idx-1], data_v[idx-1], lgrid[idxl-1] ) ;
                     }
                     if ( lgrid[idxl-nx] > 0 )
                     {
                        fprintf( stderr, "data u,v (idx-nx): %g %g %d\n",
                        data_u[idx-nx], data_v[idx-nx], lgrid[idxl-nx] ) ;
                     }
                     ---- */

                     break ;
                  case 2:
                     /* Note: nautical convention required
                        Note: atan2(u,v) gives different results for the
                              case u,v = 0,0 than the one used here
                              (copied from old routines)
                        Note: it appears that the old Fortran routines
                              return pi/4 for this case
                     */
                     result[idx] = 45.0 ;
                     if ( uterm != 0.0 || vterm != 0.0 )
                     {
                        result[idx] = 90.0 - (float) (57.29578 * atan2(vterm,uterm))
                                      - angle[idxl] ;
                     }
                     if ( result[idx] < 0.0 )
                     {
                        result[idx] = result[idx] + 360.0 ;
                     }
                     if ( result[idx] > 360.0 )
                     {
                        result[idx] = result[idx] - 360.0 ;
                     }
                     break ;
                  case 3:
                     result[idx] = sqrt(uterm*uterm+vterm*vterm) ;
                     break ;
                  default:
                     result[idx] = misval ;
                  }
               }
               else
               {
                  result[idx] = misval ;
               }

               /*
               fprintf( stderr, "\ni,j: %d %d - %d %d\n", i,j, idx, idxl ) ;
               fprintf( stderr, "cosv, sinv: %g %g\n", cosv, sinv ) ;
               fprintf( stderr, "term u,v: %g %g - %g\n", uterm,
                   vterm, result[idx] ) ;
               fprintf( stderr, "data u,v,angle: %g %g %g %d\n",
                  data_u[idx], data_v[idx], angle[idxl], lgrid[idxl] ) ;

               if ( lgrid[idxl-1] > 0 )
               {
                  fprintf( stderr, "data u,v (idx-1): %g %g %d\n",
                  data_u[idx-1], data_v[idx-1], lgrid[idxl-1] ) ;
               }
               if ( lgrid[idxl-nx] > 0 )
               {
                  fprintf( stderr, "data u,v (idx-nx): %g %g %d\n",
                  data_u[idx-nx], data_v[idx-nx], lgrid[idxl-nx] ) ;
               }
               */
            }
         }
      }
   }

   return ;
}

/* -------------------------------------------------------------------
    Function: ODS_ConvertKCS()
    Author:   Arjen Markus
    Purpose:  Convert a KCS array to a proper grid matrix
    Context:  Used by the GNF routines
    Pseudo Code:
              Use the orientation of the local grid to convert to
              cartesian or polar components. Keep in mind:
              - average to the grid cell centre (note the presence of
                inactive cells!)
              - return desired cartesian or polar component
                (depending on the selected output)
    Note:
              The result index is:
              0 - x-component of the vector
              1 - y-component of the vector
              2 - direction (angle with positive x-axis)
              3 - magnitude (length of vector)
------------------------------------------------------------------- */

TVoid
ODS_ConvertKCS(
   TInt4             * dims,           /* I Grid dimensions */
   TInt4             * ibuffer,        /* I Grid according to KCS encoding */
   TInt4             * indx,           /* O Grid according to GPP encoding */
   TInt4               grid_type       /* I Type of grid (reversed indices?) */
   )
{
   TInt4       i    ;
   TInt4       j    ;
   TInt4       k    ;
   TInt4       iv   ;
   TInt4       nmax ;
   TInt4       mmax ;
   TInt4       n0_size ;
   TInt4       n1_size ;

   mmax    = dims[0] ;
   nmax    = dims[1] ;
   n0_size = 1       ;
   n1_size = dims[0] ;

   if ( grid_type == GRID_KCSINV )
   {
      n0_size = dims[1] ;
      n1_size = 1       ;
   }

   for ( j=0 ; j<nmax ; j++ )
   {
      for ( i=0 ; i<mmax ; i++ )
      {
         k  =  i * n0_size + j * n1_size ;
         iv =  i           + j * mmax    ;
         switch ( ibuffer[iv] )
         {
         case 0L :
            indx[k] = 0 ; /* inactive point */
            break ;

         case 1L :
            /* Note: counting starts at 1 as in FORTRAN */
            indx[k] = k + 1 ; /* active computational point */

            break ;

         case 2L :
            indx[k] = -1 ; /* open boundary point */

            break ;

         default :
            indx[k] = 0 ; /* inactive point */

            break ;
         }
         k ++ ;
      }
   }
}

/* -------------------------------------------------------------------
    Function: ODS_ConstructTrivialGrid()
    Author:   Arjen Markus
    Purpose:  Convert a trivial grid matrix
    Context:  Used by the GNF routines
    Pseudo Code:
              Fill the given array with grid cell numbers
------------------------------------------------------------------- */

TVoid
ODS_ConstructTrivialGrid(
   TInt4             * dims,           /* I Grid dimensions */
   TInt4             * indx            /* O Grid according to GPP encoding */
   )
{
   TInt4       i    ;
   TInt4       j    ;
   TInt4       iv   ;
   TInt4       nmax ;
   TInt4       mmax ;

   nmax = dims[0] ;
   mmax = dims[1] ;

   for ( i=0 ; i<nmax ; i++ )
   {
      for ( j=0 ; j<mmax ; j++ )
      {
         iv =  i * mmax + j ;
         /* Note: counting starts at 1 as in FORTRAN */
         indx[iv] = iv + 1 ; /* active computational point */

         if ( i == 0 || i == (nmax-1) ||
              j == 0 || j == (mmax-1)    )
         {
            indx[iv] = 0 ;
         }
      }
   }
}

/* -------------------------------------------------------------------
    Function: ODS_GetReferenceTime()
    Author:   Arjen Markus
    Purpose:  Get the reference date/time and scale factor
    Context:  Used by GNF_CheckTimes()
    Pseudo Code:
              Deal with the various ways that the information is
              stored in the files, convert the date/time to julian
              and return the proper scale factor so that the
              raw numbers can be converted to seconds.
------------------------------------------------------------------- */

TVoid
ODS_GetReferenceTime(
      NefisFileInfoPtr    file_info,     /* I file information pointer */
      DateTimeInfoPtr     datetime_info, /* I date information pointer */
      TReal8            * refdate,       /* O reference date/time */
      TReal8            * tscale )       /* O scale factor */
{
   TInt4             idxref           ;
   TInt4             cell_index[5][3] ;
   TReal4          * rbuffer          ;
   TReal8          * dbuffer          ;
   TInt4             elem_size        ;
   TInt4             number_cells     ;
   TInt4             iyear            ;
   TInt4             imonth           ;
   TInt4             iday             ;
   TInt4             ihour            ;
   TInt4             imin             ;
   TInt4             isec             ;
   TInt4           * it_item          ;
   TInt4             itdate[2]        ;
   TChar             elem_name[80]    ;
   TString           pstr             ;

   switch ( datetime_info->type )
   {
   case ITNONE : /* No reference date/time stored - use 1 january 1900 */
      itdate[0] = 19000101 ;
      itdate[1] =        0 ;
      break ;

   case IT0102 : /* Date and time stored separately :( */
      strcpy( elem_name, datetime_info->refdate_elem ) ;
      pstr  = strchr( elem_name, '|' ) ;
      *pstr = '\0' ;

      (void) GNF_FindParameter(
                datetime_info->refdate_group, elem_name, 0,
                file_info->no_items, file_info->items, &idxref ) ;

      cell_index[0][0] = 0         ;
      cell_index[0][1] = ALL_CELLS ;
      cell_index[0][2] = 1         ;
      GNF_GetIntBuffer( file_info, idxref, cell_index, &it_item, &elem_size,
         &number_cells )           ;
      itdate[0] = it_item[0]       ;
      free( it_item )              ;

      pstr  = strchr( datetime_info->refdate_elem, '|' ) ;
      pstr ++ ;
      strcpy( elem_name, pstr ) ;

      (void) GNF_FindParameter(
                datetime_info->refdate_group, elem_name, 0,
                file_info->no_items, file_info->items, &idxref ) ;

      cell_index[0][0] = 0         ;
      cell_index[0][1] = ALL_CELLS ;
      cell_index[0][2] = 1         ;
      GNF_GetIntBuffer( file_info, idxref, cell_index, &it_item, &elem_size,
         &number_cells )           ;
      itdate[1] = it_item[0]       ;
      free( it_item )              ;

      break ;

   case ITDATE :
      (void) GNF_FindParameter(
                datetime_info->refdate_group, datetime_info->refdate_elem, 0,
                file_info->no_items, file_info->items, &idxref ) ;
      cell_index[0][0] = 0         ;
      cell_index[0][1] = ALL_CELLS ;
      cell_index[0][2] = 1         ;
      GNF_GetIntBuffer( file_info, idxref, cell_index, &it_item, &elem_size,
         &number_cells )           ;
      itdate[0] = it_item[0]       ;
      itdate[1] = it_item[1]       ;
      free( it_item )              ;

      break ;

   default :
      fprintf( stderr, "Unknown case in ODS_GetReferenceTime!\n" ) ;
      exit(1) ;
   }

   iyear  =   itdate[0]          / 10000  ;
   imonth = ( itdate[0]% 10000 ) / 100    ;
   iday   = ( itdate[0]%   100 ) / 1      ;
   ihour  =   itdate[1]          / 10000  ;
   imin   = ( itdate[1]% 10000 ) / 100    ;
   isec   = ( itdate[1]%   100 ) / 1      ;
   julian( &iyear, &imonth, &iday, &ihour, &imin, &isec, refdate ) ;

   if ( strcmp(datetime_info->timestep_elem, "" ) != 0 )
   {
      (void) GNF_FindParameter(
                datetime_info->timestep_group, datetime_info->timestep_elem, 0,
                file_info->no_items, file_info->items, &idxref ) ;
      cell_index[0][0] = 0         ;
      cell_index[0][1] = ALL_CELLS ;
      cell_index[0][2] = 1         ;
      GNF_GetRealBuffer( file_info, idxref, cell_index, &rbuffer, &elem_size,
         &number_cells )           ;

      /* We need to reckon with the possibility of a double-precision time step */
      if ( rbuffer != NULL )
      {
         (*tscale) = rbuffer[0] ;
         free( rbuffer ) ;
      }
      else
      {
         GNF_GetDoubleBuffer( file_info, idxref, cell_index, &dbuffer, &elem_size,
            &number_cells ) ;

         (*tscale) = dbuffer[0] ;
         free( dbuffer ) ;
      }
   }
   else
   {
      (*tscale) = 1.0              ;
   }

   if ( strcmp(datetime_info->scale_elem, "" ) != 0 )
   {
      (void) GNF_FindParameter(
                datetime_info->scale_group, datetime_info->scale_elem, 0,
                file_info->no_items, file_info->items, &idxref ) ;
      cell_index[0][0] = 0         ;
      cell_index[0][1] = ALL_CELLS ;
      cell_index[0][2] = 1         ;
      GNF_GetRealBuffer( file_info, idxref, cell_index, &rbuffer, &elem_size,
         &number_cells )           ;

      /* We need to reckon with the possibility of a double-precision time step */
      if ( rbuffer != NULL )
      {
         (*tscale) = (*tscale) * rbuffer[0] / 86400.0f ;

         free( rbuffer ) ;
      }
      else
      {
         GNF_GetDoubleBuffer( file_info, idxref, cell_index, &dbuffer, &elem_size,
            &number_cells ) ;

         (*tscale) = (*tscale) * dbuffer[0] / 86400.0 ;

         free( dbuffer ) ;
      }
   }
   else
   {
      (*tscale) = (*tscale) * datetime_info->default_scale / 86400.0f ;
   }

   return ;
}

/* -------------------------------------------------------------------
    Function: ODS_GetISODateTime()
    Author:   Arjen Markus
    Purpose:  Get the date/time from an ISO-conformant string
    Context:  Used by GNF_CheckTimes()
    Pseudo Code:
              Read an ISO date/time string and convert it to julian.
              No error checks made, as the strings are supposed
              to be conforming.
------------------------------------------------------------------- */

TVoid
ODS_GetISODateTime(
      TString             string,        /* I date/time string */
      TReal8            * datetime )     /* O julian date/time */
{
   TInt4  year   ;
   TInt4  month  ;
   TInt4  day    ;
   TInt4  hour   ;
   TInt4  minute ;
   TInt4  _sec0_ ;
   TReal8 second ;

   sscanf( string, "%d-%d-%d %d:%d:%lf", &year, &month, &day,
      &hour, &minute, &second ) ;

   _sec0_ = 0 ;
   julian( &year, &month, &day, &hour, &minute, &_sec0_, datetime ) ;

   if ( *datetime > -1.0 )
   {
      *datetime = *datetime + second / 86400.0 ;
   }
}


/* Test driver for these routines:
   Use the following grid:
      0  0  0  0  0
      0  1  2  3  0
      0  4  0  0  0
      0  7  8  9  0
      0 10 11 12  0
      0 13 14 15  0
      0  0  0  0  0
   Averaging over depth:
      layer 1: 0.5 - values 10
      layer 2: 0.3 - values 3
      layer 3: 0.2 - values lgrid[i,j]
      Result: 5.9+0.2*lgrid[i,j] - 6.1 ... 7.3
   Vector components:
      angle = 45 degrees
      u     = 1
      v     = -1
      Result: u1 = sqrt(2), v1 = 0, dir = 0, mag = sqrt(2)
*/
#ifdef TEST2
void PrintData( TInt4 *ndim, TReal4 *data, TInt4 nodims )
{
   TInt4  i                  ;
   TInt4  j                  ;
   TInt4  k                  ;
   TInt4  idx                ;
   TInt4  nx                 ;
   TInt4  ny                 ;
   TInt4  nz                 ;

   nx = ndim[0] ;
   ny = ndim[1] ;
   nz = ndim[2] ;
   if ( nodims != 3 ) nz = 1 ;

   idx = 0 ;
   for ( k = 0 ; k < nz ; k ++ )
   {
      for ( j = 0 ; j < ny ; j ++ )
      {
         for ( i = 0 ; i < nx ; i ++ )
         {
            printf( "%10.3f", data[idx] ) ;
            idx ++ ;
         }
         printf( "\n" ) ;
      }
      printf( "\n" ) ;
   }
}

int main( int argc, char *argv[] )
{
#define NLAY 3
#define NX   5
#define NY   7
   TInt4  i                  ;
   TInt4  j                  ;
   TInt4  k                  ;
   TInt4  idx                ;
   TInt4  nodata             ;
   TReal4 data3d[NX*NY*NLAY] ;
   TReal4 data2d[NX*NY]      ;
   TReal4 angle[NX*NY]       ;
   TInt4  lgrid[NX*NY]       ;
   TReal4 data_u[NX*NY*NLAY] ;
   TReal4 data_v[NX*NY*NLAY] ;
   TReal4 misval             ;
   TReal4 thickn[NLAY]       ;
   TInt4  ndim[3]            ;
   TInt4  result_idx         ;

   idx = 0 ;
   k   = 0 ;
   for ( j = 0 ; j < NY ; j ++ )
   {
      for ( i = 0 ; i < NX ; i ++ )
      {
         lgrid[idx]        = 0 ;
         if ( i > 0 && i < NX-1 && j > 0 && j < NY-1 )
         {
            k ++ ;
            lgrid[idx] = k ;
         }

         data3d[idx]         = 10.0 ;
         data3d[idx+NX*NY]   =  3.0 ;
         data3d[idx+2*NX*NY] = (float) lgrid[idx] ;
         data_u[idx]         = sqrt(2.0)          ;
         data_u[idx+NX*NY]   = sqrt(2.0)          ;
         data_u[idx+2*NX*NY] = sqrt(2.0)          ;
         data_v[idx]         = -sqrt(2.0)         ;
         data_v[idx+NX*NY]   = -sqrt(2.0)         ;
         data_v[idx+2*NX*NY] = -sqrt(2.0)         ;
         angle[idx]          = 45.0               ;

         idx ++ ;
      }
   }
   lgrid[2+NX*2] = 0 ;
   lgrid[3+NX*2] = 0 ;

   ndim[0]       = NX   ;
   ndim[1]       = NY   ;
   ndim[2]       = NLAY ;
   thickn[0]     = 0.5  ;
   thickn[1]     = 0.3  ;
   thickn[2]     = 0.2  ;

   misval        = -999.0 ;

   ODS_AverageOverSigmaLayers(
      ndim, lgrid, thickn, data3d, misval, data2d, &nodata ) ;

   printf( "Averaging over depth\n" ) ;
   printf( "Input\n" ) ;
   PrintData( ndim, data3d, 3 ) ;
   printf( "Result\n" ) ;
   PrintData( ndim, data2d, 2 ) ;

   printf( "Converting vectors\n" ) ;
   for ( i = 0 ; i < 4 ; i ++ )
   {
      printf( "   Output: %d\n", i ) ;

      result_idx = i ;
      ODS_ConvertVectorComponents(
         ndim, lgrid, data_u, data_v, angle, misval, result_idx,
         data3d, &nodata ) ;

      PrintData( ndim, data3d, 3 ) ;
   }

   return 0 ;
}
#endif
