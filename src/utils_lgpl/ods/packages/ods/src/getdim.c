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
 *  getdim.c  -  ODS Get list of parameters from file
 *
 *  copied from getpar.c EAV
 *
 *  Marc Kool
 */

/*
 *  $Author: Markus $
 *  $Date: 11/04/05 11:58a $
 *  $Source: /u/cvsroot/gpp/libsrc/ods/getdim.c,v $
*/
/*
 *
 */


/*   Date:       28 Feb 1994                                          */
/*   Time:       15:55                                                */
/*   Program:    GETDIM.C                                             */
/*   Version:    1.03                                                 */
/*   Programmer: Andr‚ Hendriks                                       */
/*   Copyright (C)  Stichting Deltares, 2011-2024.                    */
/*   Previous version(s):                                             */
/*   1.02 -- 6 Dec 1993 -- 13:00 -- Operating System: DOS             */
/*   1.01 -- 5 Aug 1993 -- 16:21 -- Operating System: DOS             */
/*   1.00 -- 5 Aug 1993 -- 12:31 -- Operating System: DOS             */
/*   1.00 -- 4 Aug 1993 -- 13:32 -- Operating System: DOS             */
/*   0.00 -- 4 Aug 1993 -- 07:44 -- Operating System: DOS             */
/*   Project:    Open Data Structuur                                  */
/*   Module:     GetParameter                                         */
/*   Function:                                                        */
/*   Comment:    1.03: added WAS and WS1 files                        */
/*   Reference:                                                       */
/*   Review:                                                          */

#include "portable.h"
#include "ods.h"

#include <string.h>
#include <stdio.h>

#ifndef NOPROT

#include "odsmodel.h"

TInt4 iftype ( char *fname) ;

#endif

/* the function to get dimensions from 'any' file:   */

void DllExport getdim ( char  *fname,  TInt4 *itype,  char  *dim,
                        TInt4 *pardep, TInt4 *timdep, TInt4 *locdep,
                        TInt4 *ndim,   TInt4 *ierror, char  *option)

/*************************************************************************/
/*                                                                       */
/*     Arguments:                                                        */
/*                                                                       */
/*        Name    Size      I/O  Description                             */
/*        ------  --------  ---  ------------------------------------    */
/*        dim     -          I   Dimension required.                     */
/*        fname   3          I   Full filename, including extension      */
/*        ierror  -          O   Errorcode, see errors.inc for definitio */
/*        itype   -         I/O  Filetype, see types.inc for definitions */
/*        len_..  -          I   UNIX only: length of CHARACTER variable */
/*        locdep  -          I   location dependency of dimension        */
/*        ndim    -          O   dimension.                              */
/*        option  -         I/O  option (reserved for future extensions) */
/*        pardep  -          I   parameter dependency of dimension       */
/*        timdep  -          I   time dependency of dimension            */
/*                                                                       */
/*************************************************************************/

{
   int i, inum, length ;
   char *fn;
   char dim_copy[4];

#define DEBUG 0
#if DEBUG
   FILE *dbgout;
#endif

   strncpy( dim_copy, dim, 3 );
   dim_copy[3] = '\0';

   *ierror = IEOK;

   /* strip trailing blanks and add terminating 0 to all strings,
      just to make sure:
   */

   for (inum = 0 ; inum < ODS_FNAME_DIM ; inum++)
   {
       fn = &fname[inum*ODS_FILNAMLEN] ;

       fn[ODS_FILNAMLEN-1] = '\0' ;
       length = strlen( fn ) ;
       for ( i = length-1 ; i >= 0 ; i -- )
       {
          if ( fn[i] == ' ')
          {
             fn[i] = '\0' ;
          }
          else
          {
             break ;
          }
       }
   }


#if DEBUG
   dbgout = fopen( "ods_debug.txt", "a" ) ;
   fprintf( dbgout, "File name: %s\n", fname  ); fflush( dbgout );
   fprintf( dbgout, "File type: %d\n", *itype ); fflush( dbgout );
   fprintf( dbgout, "Dimension pointer: %p\n", dim   ); fflush( dbgout );
   fprintf( dbgout, "Dimension: %s\n", dim    ); fflush( dbgout );
   fprintf( dbgout, "Parameter: %d\n", *pardep); fflush( dbgout );
   fclose( dbgout ) ;
#endif

   /*  Get filetype, if we don't know, first check extension:          */

   if (*itype == ITUNDE)
      *itype = iftype ( fname) ;

   /*  Now proceed:                                                    */

   switch ( *itype)
      {
      case ITUNDE :

         /* Unable to determine filetype                               */

         *ierror = IEUNDE ;
         break ;

      case ODS_DELWAQ_HIS_NEFIS :
      case ODS_DELWAQ_MAP_NEFIS :
      case ODS_DELPAR_HIS_NEFIS :
      case ODS_DELPAR_MAP_NEFIS :
      case ODS_DELPAR_PLO_NEFIS :

         /* DELWAQ/DELPAR NEFIS filetype */

         DlwGetNefisDim ( fname, itype, dim, pardep, timdep, locdep,
                          ndim, ierror, option );

         break ;


#ifdef PC
      case ITDBF3 :

         /* DBASE 3 filetype                                           */

         break ;

      case ITDBF4 :

         /* DBASE 4 filetype                                           */

         break ;
#endif

      case ODS_DELWAQ_MAP_BIN:
      case ODS_DELWAQ_HIS_BIN:
      case ODS_DELPAR_PLO_BIN:
      case ODS_DELWAQ_BAL_BIN:

        /* DELWAQ MAP file */

         /*ODSGetDimDlwm ( fname, itype, dim, ndim, ierror) ;*/
         ODSGetDimDlwq ( fname, itype, dim, pardep, ndim, ierror) ;
         break ;

    /*case ODS_DELWAQ_HIS_BIN:*/

        /* DELWAQ HIS file */

    /*   ODSGetDimDlwh ( fname, itype, dim, ndim, ierror) ;
         break ; */

      case ITWAS  :

         /* WASPRO WAS file                                            */

         break ;

      case ITWS1  :

         /* WASPRO WS1 file                                            */

         break ;

      case ITMPX :

         /* MAPPIX file                                                */

         break ;

      case ODS_TRISULA_HIS_NEFIS :

         /* TRISULA NEFIS HIS file                                     */
         GNF_GetDim( fname, itype, dim, pardep, timdep, locdep, ndim,
                     ierror, option ) ;

         break ;

      case ODS_TRISULA_MAP_NEFIS :

         /* TRISULA NEFIS MAP file                                     */
         GNF_GetDim( fname, itype, dim, pardep, timdep, locdep, ndim,
                     ierror, option ) ;

         break ;

      case ODS_TRISULA_DRO_NEFIS :
      case ODS_DELPAR_TRK_NEFIS  :

         /* TRISULA NEFIS drogues file                                 */

         TriDroGetDim( fname, itype, dim, pardep, timdep, locdep,
                       (TInt4 *) ndim, (TInt4 *) ierror,  option );
         break ;


      case ODS_SAMPLES_2D    :
      case ODS_SAMPLES_TABLE :
      case ODS_SAMPLES_TIME  :

         /* Samples file (2D or table)                                 */

         SamplesGetDim( fname, itype, dim, pardep, timdep, locdep,
                        (TInt4 *) ndim, (TInt4 *) ierror,  option );
         break ;

      case ODS_MORSYS_MAP_NEFIS :

         /* MORSYS NEFIS COM file                                     */

         GNF_GetDim( fname, itype, dim, pardep, timdep, locdep, ndim,
                     ierror, option ) ;

         break ;


      case ODS_MORSYS_TRAM_NEFIS :

         /* MORSYS NEFIS TRAM file */

         GNF_GetDim( fname, itype, dim, pardep, timdep, locdep, ndim,
                     ierror, option ) ;

         break ;


      case ODS_MORSYS_SWAN_NEFIS :

         /* MORSYS NEFIS SWAN file */

         GNF_GetDim( fname, itype, dim, pardep, timdep, locdep, ndim,
                     ierror, option ) ;

         break ;


      case ODS_MORSYS_HWBOD :

         /* MORSYS NEFIS COM file                                     */

         ODS_MORSYS_NEFIS_COMDIM( fname, 256, 3, *itype,
                                   dim, *pardep, *timdep, *locdep,
                                   (TInt4 *) ndim, (TInt4 *) ierror,
                                  option );
         break ;

      case ODS_MORSYS_HWGXY :

         /* MORSYS NEFIS COM file                                     */

         ODS_MORSYS_NEFIS_COMDIM( fname, 256, 3, *itype,
                                   dim, *pardep, *timdep, *locdep,
                                   (TInt4 *) ndim, (TInt4 *) ierror,
                                  option );
         break ;

      case ODS_MORSYS_TRAH_NEFIS :

         /* MORSYS NEFIS HIS files                                    */

         GNF_GetDim( fname, itype, dim, pardep, timdep, locdep, ndim,
                     ierror, option ) ;

         break ;

      case ODS_MORSYS_BAGR_NEFIS :

         /* MORSYS NEFIS BAGR file (dredging)                          */

         MorOdsGetDim ( fname, itype, dim, pardep, timdep, locdep,
                        ndim, ierror, option );
         break ;

      case ODS_ANY_TEKAL_ASCII_1D :
      case ODS_ANY_TEKAL_ASCII_1DE :

    /* TEKAL ASCII 1D files                                       */

         ODSGetDimAnyTekAscHis      (
           fname , itype, dim, *pardep,*timdep,
                *locdep, ndim, ierror, option ) ;

         break ;

      case ODS_ANY_TEKAL_ASCII_2D :
      case ODS_ANY_TEKAL_ASCII_VEC :

    /* TEKAL ASCII 2D files (scalar and vector)                   */

         ODSGetDimAnyTekAscMap      (
           fname , itype, dim, *pardep,*timdep,
                *locdep, ndim, ierror, option ) ;

         break ;

      case ODS_ANY_TEKAL_ASCII_ANNO :

    /* TEKAL ASCII Annotation files (text at specified location)          */

         ODSGetDimTekAnno (
           fname , itype, dim, *pardep,*timdep,
                *locdep, ndim, ierror, option ) ;

         break ;

      case ODS_ANY_JSPOST:

    /* JSPost files                                              */
         ODSGetDimJSP( fname, itype, dim, *pardep, *timdep,
              *locdep, ndim, ierror, option ) ;
    break ;

      case ODS_DELWAQ_GRID_UNF :
      case ODS_DELWAQ_TELEMAC  :

    /* DELWAQ/DELPAR grid files                                   */

         ODSGetDimDlwg( fname , itype, dim, *pardep,*timdep,
              *locdep, ndim, ierror , option ) ;

         break ;

      case ODS_SHYFEM_GRID :

    /* SHYFEM grid files                                          */

         ODSGetDimShyf( fname , (TInt4 *) itype, dim, *pardep, *timdep,
              *locdep, (TInt4 *) ndim, (TInt4 *) ierror,
                        option ) ;

         break ;

      case ODS_PHAROS_AMP_NEFIS:

    /* PHAROS admin file (seiches amplifications)                */

    ODSGetDimPharAmp( fname, (TInt4 *) itype, dim, *pardep, *timdep,
            *locdep, (TInt4 *) ndim, (TInt4 *) ierror,
            option ) ;
    break ;

      case ODS_PHAROS_MAP_NEFIS:

         /* PHAROS NEFIS file                                         */

         ODSGetDimPharMap( fname, (TInt4 *) itype, dim, *pardep, *timdep,
                           *locdep, (TInt4 *) ndim, (TInt4 *) ierror,
                           option ) ;
         break ;


      case ODS_PHIDIAS_HISTORY:
      case ODS_PHIDIAS_MAP:

         /* PHIDIAS NEFIS map file                             */

         ODSGetDimPhiMap( fname, 256, 3, *itype,
                                   dim, *pardep, *timdep, *locdep,
                                   (TInt4 *) ndim, (TInt4 *) ierror,
                                  option );
         break;

      case ODS_PHIDIAS_SPECTRAL:

         /* PHIDIAS NEFIS spectrum file                             */

         ODSGetDimPhiSpec( fname, 256, 3, *itype,
                                   dim, *pardep, *timdep, *locdep,
                                   (TInt4 *) ndim, (TInt4 *) ierror,
                                  option );
         break;


      case ODS_GEOGR_BNA:
      case ODS_GEOGR_DXF:
      case ODS_GEOGR_TEKAL:

         /* BNA, DXF and TEKAL landboundary files                     */

         ODS_BNA_getdim( fname,  itype, dim, pardep, timdep,
                         locdep, ndim,  ierror, option     ) ;
         break ;

      default :

         /* Filetype not implemented:                                  */

         *ierror = IEUNKN;
      }

#if DEBUG
   dbgout = fopen( "ods_debug.txt", "a" ) ;
   fprintf( dbgout, "Result (dimensions): %d %d %d %d\n",
        ndim[0], ndim[1], ndim[2], ndim[3] );
   fclose( dbgout );
#endif
}

