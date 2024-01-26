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
 *  ods_spcf.c -  Routines that are more or less specific to particular
 *                file types
 *
 *  Copyright (C) 2002-2024 Deltares
 *
 *  Arjen Markus
 */

/*
 *  General information:
 *  This file contains the following functions:
 *  - ODS_DetermineDepthInCentre():  Determine the depth in the cell centres
 *  - ODS_DetermineZcoordinate():    Determine the z-coordinate
 *  - ODS_GetGrdIndices():           Get the grid indices (convenience function)
 *  - ODS_EncodeDryWetInformation(): Encode the dry/wet information according to Delft-GPP
 */

/*
 *  $Author: Markus $
 *  $Date: 23/08/07 11:44 $
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
#include "ods_spcf.h"

/*
 * Macros - none
 */

/*
 * Static variables and functions
 */

/* -------------------------------------------------------------------
    Function: ODS_DetermineDepthInCellCentre()
    Author:   Arjen Markus
    Purpose:  Determine the depth in the cell centres
    Context:  Used by GNF_GetMat
    Pseudo Code:
              The depth at the four vertices together with the
              drying/flooding option determines the effective depth
              in the centre of a cell.
    Note:
              Necessary for Delft3D-FLOW map files (ADD_DPS_TO_PREV)
------------------------------------------------------------------- */

TVoid
ODS_DetermineDepthInCellCentre(
      NefisFileInfoPtr    file_info,     /* I Contents of the file */
      TInt4             * ndim,          /* I array with dimensions */
      TInt4             * lgrid,         /* I mask array */
      TReal4            * depth,         /* I depth at vertices */
      TReal4              misval,        /* I missing value */
      TReal4            * dps    )       /* O depth in centre */
{
   TInt4     i1     ;
   TInt4     i2     ;
   TInt4     k      ;
   TInt4     kl     ;
   TInt4     k1     ;
   TInt4     k2     ;
   TInt4     k3     ;
   TInt4     n1max  ;
   TInt4     n2max  ;
   TInt4     idxflg ;
   TInt4     flag   ;
   TInt4     string_length  ;
   TInt4     number_strings ;
   TString   dryflg ;

   /* Delft3D-FLOW specific:
   */
  (void) GNF_FindParameter(
            "map-const", "DRYFLP", 0,
            file_info->no_items, file_info->items, &idxflg ) ;

   /* TODO: No check on idxflg - assume always present */

   GNF_GetStringBuffer( file_info, idxflg, &dryflg, &string_length,
      &number_strings ) ;

   flag = 0 ;
   if ( strncmp( dryflg, "MAX", 3 ) == 0 )
   {
      flag = 0 ;
   }
   if ( strncmp( dryflg, "MIN", 3 ) == 0 )
   {
      flag = 1 ;
   }
   if ( strncmp( dryflg, "MEAN", 4 ) == 0 )
   {
      flag = 2 ;
   }
   free( dryflg ) ;

   n1max   = ndim[1]          ;
   n2max   = ndim[0]          ;

   for ( i2 = 0 ; i2 < n2max ; i2 ++ )
   {
      for ( i1 = 0 ; i1 < n1max ; i1 ++ )
      {
         kl =  i1    +  i2    * n1max ;
         k  =  i1    +  i2    * n1max ;
         k1 = (i1-1) +  i2    * n1max ;
         k2 =  i1    + (i2-1) * n1max ;
         k3 = (i1-1) + (i2-1) * n1max ;

         if ( i1 == 0 )
         {
            k1 = k ;
            k3 = k ;
         }
         if ( i2 == 0 )
         {
            k2 = k ;
            k3 = k ;
         }

         dps[k] = misval ;
         if ( lgrid[kl] > 0 )
         {
            switch ( flag )
            {
            case 0:
               dps[k] = MAX( MAX(depth[k],depth[k1]),
                             MAX(depth[k2],depth[k3]) ) ;
               break ;

            case 1:
               dps[k] = MIN( MIN(depth[k],depth[k1]),
                             MIN(depth[k2],depth[k3]) ) ;
               break ;

            case 2:
               dps[k] = 0.25 *( depth[k]+depth[k1]+depth[k2]+depth[k3] ) ;
               break ;

            default:
               fprintf( stderr, "Unknown drying/flooding option! Panic!\n" ) ;
               exit(1) ;
            }
         }
      }
   }

   return ;
}

/* -------------------------------------------------------------------
    Function: ODS_DetermineZcoordinate()
    Author:   Arjen Markus
    Purpose:  Determine the z-coordinate
    Context:  Used by GNF_GetMat
    Pseudo Code:
              Get the necessary information, such as the layer
              thickness (sigma) or the position of the interfaces
              (z-layers).
              Then calculate the z-coordinate for the interfaces
              based on the relevant method.
    Note:
              Necessary for Delft3D-FLOW map files (COORD_DATA_3D)
------------------------------------------------------------------- */

TVoid
ODS_DetermineZcoordinate(
      NefisFileInfoPtr    file_info,         /* I Contents of the file */
      ParameterInfoPtr    wl_info,           /* I Parameter: water level */
      ParameterInfoPtr    depth_info,        /* I Parameter: depth */
      TInt4               cell_index[5][3],  /* I Cell indexes to retrieve */
      TInt4             * loc,               /* I Details about locations */
      TInt4             * ndim,              /* I Array dimensions */
      TInt4             * lgrid,             /* I Grid indices */
      TReal4              misval,            /* I missing value */
      TReal4           ** zcrd_ptr     )     /* IO z-coordinate (will be allocated
                                                if NULL on entry!) */
{
   TInt4             i                 ;
   TInt4             i2                ;
   TInt4             i3                ;
   TInt4             i4                ;
   TInt4             k                 ;
   TInt4             t                 ;
   TInt4             nosegl            ;
   TInt4             nolay             ;
   TInt4             idxpar            ;
   TInt4             idxzcrd           ;
   TInt4             idxsigma          ;
   TInt4             notimes           ;
   TInt4             nodata            ;
   TInt4             nodummy           ;
   TInt4             elem_size         ;
   TInt4             number_cells      ;
   TInt4             cell_index2[5][3] ;
   TReal4          * water_level       ;
   TReal4          * depth             ;
   TReal4          * thickn            ;
   TReal4          * zlevel            ;
   TReal4          * th_factor         ;
   TReal4          * z_coordinate      ;
   LocationInfoPtr   loc_info          ;


   /* Get the data we always need
      TODO: time-dependent bottom!
   */
   loc_info = wl_info->location_info ;

   notimes  = cell_index[0][1] - cell_index[0][0] + 1 ;
   nolay    = ndim[2] - 1       ;
   nodata   = ndim[0] * ndim[1] ;

   water_level  = (TReal4 *) malloc( nodata*notimes*sizeof(TReal4) ) ;
   depth        = (TReal4 *) malloc( nodata*sizeof(TReal4) ) ;
   thickn       = NULL;
   zlevel       = NULL;
   th_factor    = NULL;
   z_coordinate = NULL;

   (void) GNF_FindParameter(
             wl_info->group_name, wl_info->elem_name, 1,
             file_info->no_items, file_info->items, &idxpar ) ;

   GNF_GetActualData( file_info, wl_info, idxpar, cell_index,
      loc, misval, water_level, &nodummy ) ;

   (void) GNF_FindParameter(
             depth_info->group_name, depth_info->elem_name, 1,
             file_info->no_items, file_info->items, &idxpar ) ;

   cell_index2[0][0] = 0 ;
   cell_index2[0][1] = 0 ;
   cell_index2[0][2] = 1 ;

   GNF_GetActualData( file_info, depth_info, idxpar, cell_index2,
      loc, misval, depth, &nodummy ) ;

   nosegl = ndim[0]*ndim[1] ;

   /* Delft3D-FLOW specific (somewhat):
      Decide on the method
   */
   idxsigma = -1 ;
   (void) GNF_FindParameter(
             loc_info->zcoord_group, loc_info->zcoord_elem, 1,
             file_info->no_items, file_info->items, &idxzcrd ) ;
   if ( idxzcrd == -1 || file_info->items->elem_dims[0] < 1 )
   {
      idxzcrd = -1 ;
      (void) GNF_FindParameter(
                loc_info->sigmalayer_group, loc_info->sigmalayer_elem, 1,
                file_info->no_items, file_info->items, &idxsigma ) ;
      if ( idxsigma == -1 )
      {
         idxsigma = -2 ;
      }
   }

   /* Use z-layers */
   if ( idxzcrd != -1 )
   {
      cell_index2[0][0] = 0 ;
      cell_index2[0][1] = 0 ;
      cell_index2[0][2] = 1 ;
      GNF_GetRealBuffer( file_info, idxzcrd, &cell_index2[0][0], &zlevel, &elem_size,
         &number_cells ) ;

      /* Make sure we only use as much of the array as is allowed
         -- deep problems with the vertical direction
         ** TODO **
      */
      if ( nolay > elem_size-1 )
      {
         nolay = elem_size-1 ;
      }

      /* Allocate the z-coordinates array? */
      if ( (*zcrd_ptr) == NULL )
      {
         (*zcrd_ptr) = (TReal4 *) malloc( (nolay+1)*nosegl*notimes*sizeof(TReal4) ) ;
      }
      z_coordinate = *zcrd_ptr ;

#define MISSING(x) (fabs(x-misval)<0.0001)

      i4 = 0 ;
      for ( t = 0 ; t < notimes ; t ++ )
      {
         for ( k = 0 ; k < nolay+1 ; k ++ )
         {
            i2 = 0 ;
            i3 = t * nosegl ;
            for ( i = 0 ; i < nosegl ; i ++ )
            {
               /* Keep the bottom depth fixed for the moment! */
               z_coordinate[i4] = zlevel[k] ;

               if ( zlevel[k] < -depth[i2] )
               {
                  z_coordinate[i4] = -depth[i2] ;
               }
               if ( zlevel[k] > water_level[i3] )
               {
                  z_coordinate[i4] = water_level[i3] ;
               }
               if ( lgrid[i] <= 0 || MISSING(depth[i2]) || MISSING(water_level[i3]) )
               {
                  z_coordinate[i4] = misval ;
               }

               /* --
               if ( z_coordinate[i4] > -10.0 &&
                    z_coordinate[i4] < 1200.0 )
               printf( "i,k: %d %d - %g %g %g %g\n", i,k,
                  z_coordinate[i4], zlevel[k], water_level[i3], depth[i2] ) ;
               -- */

               i2 ++ ;
               i3 ++ ;
               i4 ++ ;
            }
         }
      }

      free( zlevel    ) ;
   }

   /* Use sigma-layers */
   if ( idxsigma != -1 )
   {
      if ( idxsigma != -2 )
      {
         cell_index2[0][0] = 0 ;
         cell_index2[0][1] = 0 ;
         cell_index2[0][2] = 1 ;
         GNF_GetRealBuffer( file_info, idxsigma, &cell_index2[0][0], &thickn, &elem_size,
            &number_cells ) ;
      }
      else
      {
         /* Assume a uniform distribution */
         elem_size = nolay ;
         thickn    = (TReal4 *) malloc( (nolay+1)*sizeof(TReal4) ) ;
         for ( i = 1 ; i < nolay+1 ; i ++ )
         {
            thickn[i] = 1.0/nolay ;
         }
      }

      /* Make sure we only use as much of the array as is allowed
         -- deep problems with the vertical direction
         ** TODO **
      */
      if ( nolay > elem_size )
      {
         nolay = elem_size ;
      }

      /* Two-dimensional quantities? */
      if ( nolay == 1 )
      {
         thickn[0] = 1.0 ;
      }

      /* Allocate the z-coordinates array? */
      if ( (*zcrd_ptr) == NULL )
      {
         (*zcrd_ptr) = (TReal4 *) malloc( (nolay+1)*nosegl*notimes*sizeof(TReal4) ) ;
      }
      z_coordinate = *zcrd_ptr ;

      th_factor = (TReal4 *) malloc( (nolay+1)*sizeof(TReal4) ) ;

      th_factor[0] = 0.0 ;
      for ( i = 1 ; i < nolay+1 ; i ++ )
      {
         th_factor[i] = th_factor[i-1] + thickn[i-1] ;
      }

      i4 = 0 ;
      for ( t = 0 ; t < notimes ; t ++ )
      {
         for ( k = 0 ; k < nolay+1 ; k ++ )
         {
            i2 = 0 ;
            i3 = t * nosegl ;
            for ( i = 0 ; i < nosegl ; i ++ )
            {
               /* Keep the bottom depth fixed for the moment! */
               z_coordinate[i4] = water_level[i3] - th_factor[k] *
                                  ( water_level[i3] + depth[i2] ) ;
               if ( lgrid[i] <= 0 )
               {
                  z_coordinate[i4] = misval ;
               }

               i2 ++ ;
               i3 ++ ;
               i4 ++ ;
            }
         }
      }

      free( thickn    ) ;
      free( th_factor ) ;
   }

   free( water_level ) ;
   free( depth       ) ;

   return ;
}

/* -------------------------------------------------------------------
    Function: ODS_GetGrdIndices()
    Author:   Arjen Markus
    Purpose:  Get the grid indices (convenience function)
    Context:  Used by GNF_GetMat
    Pseudo Code:
              Either get the grid indices from file or construct them
------------------------------------------------------------------- */

TVoid
ODS_GetGrdIndices(
      TString             filename,          /* I File name */
      TInt4             * ftype,             /* I File type */
      TInt4               loc_type,          /* I Named locations? */
      TInt4             * ndim,              /* IO Array with dimensions */
      TInt4            ** indx,              /* O Index array */
      TInt4             * truegrid )          /* O True grid or not */
{
   TInt4    indloc[1] ;
   TInt4    igisty    ;
   TInt4    nocell    ;
   TInt4    ierror    ;
   TInt4    i         ;

   if ( loc_type == LOC_NAMES ||
        loc_type == BAL_NAMES )
   {
      *truegrid = 0         ;
      ndim[3]   = 1         ; /* Cells per layer */
      *indx = (TInt4 *) malloc( sizeof(TInt4) * (ndim[0]*ndim[1]) ) ;
      for ( i = 0 ; i < ndim[0]*ndim[1] ; i ++ )
      {
         (*indx)[i] = i+1 ; /* Construct a horizontal "grid" */
      }
   }
   else
   {
      *truegrid = 1               ;
      ndim[3]   = ndim[0]*ndim[1] ; /* Cells per layer */
      *indx     = (TInt4 *) malloc( sizeof(TInt4) * (ndim[0]*ndim[1]) ) ;
      indloc[0] = 1 ;
      GNF_GetGrd( filename, ftype, indloc, *indx, &nocell, &igisty, &ierror ) ;
   }

}

/* -------------------------------------------------------------------
    Function: ODS_EncodeDryWetInformation()
    Author:   Arjen Markus
    Purpose:  Encode the dry/wet information according to Delft-GPP
    Context:  Used by GNF_GetMat()
    Pseudo Code:
              Use the integer arrays that indicate dry/wet cell
              boundaries to determine a single, encoded value for
              a cell.
    Note:
              Specific for Delft3D-FLOW, due to the transposition
              of the raw data. As Delft3D-FLOW is the only model
              providing this information, this currently does not
              pose problems.
------------------------------------------------------------------- */

TVoid
ODS_EncodeDryWetInformation(
      TInt4             * ndim,          /* I array with dimensions */
      TInt4             * lgrid,         /* I mask array */
      TInt4             * udam,          /* I dry/wet in U-direction */
      TInt4             * vdam,          /* I dry/wet in V-direction */
      TReal4            * data,          /* O encoded information */
      TInt4               ordering)      /* I ordering of the grid */
{
   TInt4   i         ;
   TInt4   j         ;
   TInt4   k         ;
   TInt4   kl        ;

   /* Unfortunately, lgrid[] is in ordinary orientation and udam/vdam
      in transposed orientation, as they come directly from the file
      (and the ndim[] array represents the transposition too)
   */

   for ( j = 0 ; j < ndim[1] ; j ++ )
   {
      for ( i = 0 ; i < ndim[0] ; i ++ )
      {
         k  = i + ndim[0] * j ;
         kl = i + ndim[0] * j ;
         if ( ordering == GRID_KCSINV )
         {
            kl = ndim[1] * i + j ;
         }
         if ( lgrid[kl] <= 0 )
         {
            data[kl] = 0.0 ;
         }
         else
         {
            if ( ordering == GRID_KCSINV )
            {
               data[kl] = 1.0 * udam[k] + 2.0 * vdam[k] ;
            }
            else
            {
               data[kl] = 2.0 * udam[k] + 1.0 * vdam[k] ;
            }
         }
      }
   }

   return ;
}

/* -------------------------------------------------------------------
    Function: ODS_HeuristicCheckAccept()
    Author:   Arjen Markus
    Purpose:  Check if the parameter is really available in the file
    Context:  Used by GNF_CheckParameters()
    Pseudo Code:
              For Delft3D-FLOW, history files:
              - Check for the constituents in ZTUR that they really
                are there
              - Check for parameters defined on cross sections
                that there is a cross section
------------------------------------------------------------------- */

TInt4
ODS_HeuristicCheckAccept(
      NefisFileInfoPtr    file_info,         /* I Contents of the file */
      ParameterInfoPtr    param,             /* I Parameter to check */
      TInt4               idx )              /* I Expected index */
{
   TInt4  idxl             ;
   TInt4 *ints             ;
   TInt4  elem_size        ;
   TInt4  number_ints      ;
   TInt4  accept           ;
   TInt4  cell_index[5][3] ;

   /* Delft3D-FLOW, history files */
   if ( file_info->filetype != ODS_TRISULA_HIS_NEFIS )
   {
      return 1 ;
   }

   /* AM: this can be compressed into a single loop
          and a generic routine
   */
   if ( strcmp( param->elem_name, "ZTUR" )             != 0 &&
        strcmp( param->elem_name, "GRO" )              != 0 &&
        strcmp( param->location_id, "FLOW-HIS-TRANS" ) != 0    )
   {
      return 1 ;
   }

   /* Ordinary quantities ...
   */
   if ( strcmp( param->elem_name, "GRO" ) == 0 )
   {
      (void) GNF_FindParameter(
                 "his-const", "LSTCI", 0,
                 file_info->no_items, file_info->items, &idxl ) ;

      cell_index[0][0] = 0 ;
      cell_index[0][1] = 0 ;
      cell_index[0][2] = 1 ;
      GNF_GetIntBuffer( file_info, idxl, &cell_index[0][0],
          &ints, &elem_size, &number_ints ) ;

      accept = 0 ;
      if ( idx <= ints[0] && idx >= 0 )
      {
         accept = 1 ;
      }
      free( ints ) ;
      return accept ;
   }

   /* Turbulence quantities ...
   */
   if ( strcmp( param->elem_name, "ZTUR" ) == 0 )
   {
      (void) GNF_FindParameter(
                 "his-const", "LTUR", 0,
                 file_info->no_items, file_info->items, &idxl ) ;

      cell_index[0][0] = 0 ;
      cell_index[0][1] = 0 ;
      cell_index[0][2] = 1 ;
      GNF_GetIntBuffer( file_info, idxl, &cell_index[0][0],
          &ints, &elem_size, &number_ints ) ;

      accept = 0 ;
      if ( idx < ints[0] && idx >= 0 )
      {
         accept = 1 ;
      }
      free( ints ) ;
      return accept ;
   }

   /* Flux quantities ...
   */
   if ( strcmp( param->location_id, "FLOW-HIS-TRANS" ) == 0 )
   {
      (void) GNF_FindParameter(
                 "his-const", "NTRUV", 0,
                 file_info->no_items, file_info->items, &idxl ) ;

      cell_index[0][0] = 0 ;
      cell_index[0][1] = 0 ;
      cell_index[0][2] = 1 ;
      GNF_GetIntBuffer( file_info, idxl, &cell_index[0][0],
          &ints, &elem_size, &number_ints ) ;

      accept = 0 ;
      if ( 0 < ints[0] )
      {
         accept = 1 ;
      }
      free( ints ) ;
      return accept ;
   }

   return 1; /* Accept in any case ... */
}

/* -------------------------------------------------------------------
    Function: ODS_ConstructBalanceNames()
    Author:   Arjen Markus
    Purpose:  Check if the parameter is really available in the file
    Context:  Used by GNF_CheckLocations()
    Pseudo Code:
              Read the connectivity matrix
              Construct the combined names
------------------------------------------------------------------- */

TVoid
ODS_ConstructBalanceNames(
      NefisFileInfoPtr    file_info,         /* I Contents of the file */
      LocationInfoPtr     location,          /* I Location information */
      TString             name,              /* O Array of names */
      TInt4             * id,                /* O Array of location IDs */
      TInt4             * number )           /* I/O Number of locations */
{
   TInt4   i            ;
   TInt4   from         ;
   TInt4   to           ;
   TInt4   ierror       ;
   BInt4   elem_ndim    ;
   BInt4   elem_dims[5] ;
   TInt4   size         ;
   BInt4   uindex[5][3] ;
   BInt4   usrord[5]    ;
   BInt4   buflen       ;
   TString polygon_name ;
   BInt4   nbytsg       ;
   BInt4   nbyt_name    ;
   TInt4 * ibuffer      ;

   TChar   elem_type[NFNAMELEN]  ;
   TChar   elem_qty [NFNAMELEN]  ;
   TChar   elem_unit[NFNAMELEN]  ;
   TChar   elem_desc[5*NFNAMELEN]  ;

   for ( i = 0 ; i < 5 ; i ++ )
   {
      uindex[i][0] = 0 ;
      uindex[i][1] = 0 ;
      uindex[i][2] = 0 ;
   }

   elem_ndim    = 5 ;
   elem_dims[0] = 0 ;
   elem_dims[1] = 0 ;
   elem_dims[2] = 0 ;
   elem_dims[3] = 0 ;
   elem_dims[4] = 0 ;

   /* The names of the polygons */
   ierror = Inqelm( file_info->deffds, location->elem_name, elem_type,
                    &nbyt_name, elem_qty, elem_unit, elem_desc,
                    &elem_ndim, elem_dims ) ;

   uindex[0][0] = 1 ;
   uindex[0][1] = 1 ; // Just one group
   uindex[0][2] = 1 ;

   usrord[0]    = 1 ;
   usrord[1]    = 2 ;
   usrord[2]    = 3 ;
   usrord[3]    = 4 ;
   usrord[4]    = 5 ;
   buflen       = elem_dims[0] * nbyt_name * sizeof( TChar ) ; /* AM: Why is factor 4 required? */

   polygon_name = (TString) malloc( buflen + 1) ;

   ierror = Getelt( file_info->datfds,
                    location->group_name, location->elem_name,
                    uindex, usrord, &buflen, polygon_name ) ;
   polygon_name[buflen] = '\0' ;

   /* The connectivity matrix */
   elem_ndim    = 5 ; // reset
   ierror = Inqelm( file_info->deffds, "BALNEIGHB", elem_type,
                    &nbytsg, elem_qty, elem_unit, elem_desc,
                    &elem_ndim, elem_dims ) ;

   size = 1 ;
   for ( i = 0 ; i < 5 ; i ++ )
   {
      if ( elem_dims[i] > 0 )
      {
         size = size * elem_dims[i] ;
      }
   }

   ibuffer = (int *) malloc( size * sizeof( TInt4 ) ) ;

   uindex[0][0] = 1 ;
   uindex[0][1] = 1 ; // Just one group
   uindex[0][2] = 1 ;

   usrord[0]    = 1 ;
   usrord[1]    = 2 ;
   usrord[2]    = 3 ;
   usrord[3]    = 4 ;
   usrord[4]    = 5 ;
   buflen       = size * sizeof( TInt4 ) ;

   ierror = Getelt( file_info->datfds,
                    location->group_name, "BALNEIGHB",
                    uindex, usrord, &buflen, ibuffer ) ;

   if ( *number > 0 )
   {
      for ( i = 0 ; i < elem_dims[1] ; i ++ ) {
         from = ibuffer[0+i*2] - 1 ;
         to   = ibuffer[1+i*2] - 1 ;
         sprintf( &name[21*i], "%9.9s->%9.9s", &polygon_name[nbyt_name*from], &polygon_name[nbyt_name*to] );
         id[i] = i + 1 ;
      }
   }
   else
   {
      *number = elem_dims[1] ;
   }

   free( ibuffer ) ;
   free( polygon_name ) ;
}

