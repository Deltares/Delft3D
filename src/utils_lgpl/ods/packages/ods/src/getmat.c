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
 *  getmat.c  -  Read ODS 3-D matrix data from file
 *
 *  copied from getval.c EAV
 *
 *  Marc Kool
 */

/*
 *  $Author: Markus $
 *  $Date: 11/04/05 11:58a $
 *  $Source: /u/cvsroot/gpp/libsrc/ods/getmat.c,v $
*/
/*
 *
 */


/*   Date:       28 Feb 1993                                          */
/*   Time:       11:52                                                */
/*   Program:    GETMAT.C                                             */
/*   Version:    1.03                                                 */
/*   Programmer: Andr‚ Hendriks                                       */
/*   Copyright (C)  Stichting Deltares, 2011-2024.                    */
/*   Previous version(s):                                             */
/*   1.02 -- 6 Dec 1993 -- 13:00 -- Operating System: DOS             */
/*   1.01 -- 5 Aug 1993 -- 16:31 -- Operating System: DOS             */
/*   1.00 -- 5 Aug 1993 -- 16:21 -- Operating System: DOS             */
/*   0.00 -- 5 Aug 1993 -- 12:31 -- Operating System: DOS             */
/*   Project:    Open Data Structuur                                  */
/*   Module:     GetValue                                             */
/*   Function:                                                        */
/*   Comment: 1.03: Support for WASPRO files                          */
/*   Reference:                                                       */
/*   Review:                                                          */

#include "portable.h"
#include "ods.h"

#include <stdio.h>

#ifndef NOPROT

#include "odsmodel.h"

TInt4 iftype ( char *fname) ;

#endif


/* the function to get values from 'any' file:   */

void DllExport getmat ( char  *fname,  TInt4  *itype,  TInt4 *parcod,
                        TInt4 *loc,    double *tim,    float *misval,
                        TInt4 *i3gl,   TInt4  *maxdim, float *data,
                        TInt4 *ierror, char   *option)

/*************************************************************************/
/*                                                                       */
/*     Arguments:                                                        */
/*                                                                       */
/*        Name    Size      I/O  Description                             */
/*        ------  --------  ---  ------------------------------------    */
/*        data    maxdim     O   Retrieved data.                         */
/*        fname   -          I   Full filename, including extension      */
/*        ierror  -          O   Errorcode, see errors.inc for definitio */
/*        itype   -         I/O  Filetype, see types.inc for definitions */
/*        i3gl    -          I   Code for storage type of data.          */
/*        len_..  -          I   UNIX only: length of CHARACTER variable */
/*        loc     -          I   List of indices of locations wanted.    */
/*        maxdim  -          I   dimensions of data array.               */
/*        misval  -          I   Real to use if no data found.           */
/*        option  -         I/O  option (reserved for future extensions) */
/*        parcod  -          I   Parameter identifying data wanted.      */
/*        tim     -          I   List of times (Julian dates).           */
/*************************************************************************/

{
   int     i,
           inum , length ;
   char   *fn;
   TReal4 *zbuffs;
   TReal4 *rbuffs;
   TInt4  *ibuffs;
   TInt4   fortran_loc[9];

   *ierror = IEOK;

   /* locations are indexed from 0, so correct them for Fortran */
   /* HACK/WARNING:
      This is only true for those files that do not contain named locations
      Otherwise the location index is used and no correction is necessary!
   */
   fortran_loc[0] = loc[0] + 1;
   fortran_loc[1] = loc[1] + 1;
   fortran_loc[2] = loc[2];
   fortran_loc[3] = loc[3] + 1;
   fortran_loc[4] = loc[4] + 1;
   fortran_loc[5] = loc[5];
   fortran_loc[6] = loc[6] + 1;
   fortran_loc[7] = loc[7] + 1;
   fortran_loc[8] = loc[8];


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

   /*  Get filetype, if we don't know, first check extension:          */

   if ( *itype == ITUNDE)
      *itype = iftype ( fname) ;

   /*  Now proceed:                                                    */

   switch ( *itype)
   {
      case ITUNDE :

         /* Unable to determine filetype                               */

         *ierror = IEUNDE ;
         break ;

#ifdef PC
      case ITDBF3 :
      case ITDBF4 :

         /* DBASE 3 filetype                                           */

         break ;
#endif

      case ODS_DELWAQ_MAP_BIN :
      case ODS_DELWAQ_HIS_BIN :
      case ODS_DELWAQ_BAL_BIN :
      case ODS_DELPAR_PLO_BIN :

         /* DELWAQ 4 MAP/HIS file, DELPAR PLO file                     */

         ODSGetMatDlwq (  fname, itype, parcod , tim , loc , misval,
                         maxdim, data, ierror) ;
         break ;

      case ODS_DELWAQ_MAP_NEFIS :
      case ODS_DELWAQ_HIS_NEFIS :
      case ODS_DELPAR_MAP_NEFIS :
      case ODS_DELPAR_HIS_NEFIS :
      case ODS_DELPAR_PLO_NEFIS :

         /* DELWAQ/DELPAR NEFIS filetype */

         DlwGetNefisMat ( fname, itype, parcod, tim , loc , misval,
                          maxdim, data, ierror, option );
         break ;

      case ITWS1  :

         /* WASPRO WS1 file                                            */

/*         ODSGetValWs1  ( fname , locin , parin , timin , *maxilo,
                         *maxipa, *maxiti, *misval, loc   , par   ,
                         tim   , values, *maxolo, *maxopa, *maxoti,
                         nrloc , nrpar , nrtim , ierror) ;
*/
         break ;

      case ITWAS  :

         /* WASPRO WAS file                                            */

/*         ODSGetMatWas  ( fname , locin , parin , timin , *maxilo,
                         *maxipa, *maxiti, *misval, loc   , par   ,
                         tim   , values, *maxolo, *maxopa, *maxoti,
                         nrloc , nrpar , nrtim , ierror) ;
*/
         break ;

      case ITMPX  :

         /* MAPPIX file                                                */

/*         ODSGetMatMPX ( fname , locin , parin , timin , *maxilo,
                        *maxipa, *maxiti, *misval, loc   , par   ,
                        tim   , values, *maxolo, *maxopa, *maxoti,
                        nrloc , nrpar , nrtim , ierror) ;
*/
         break ;

      case ODS_TRISULA_HIS_NEFIS  :
         /* Trisula Nefis history file                                 */
         GNF_GetMat( fname, itype, parcod, loc, tim, misval, i3gl,
            maxdim, data, ierror, option ) ;

         break ;

      case ODS_TRISULA_MAP_NEFIS  :

         /* Trisula Nefis map file                                     */
         GNF_GetMat( fname, itype, parcod, loc, tim, misval, i3gl,
            maxdim, data, ierror, option ) ;

         break ;

      case ODS_TRISULA_DRO_NEFIS  :
      case ODS_DELPAR_TRK_NEFIS   :

         /* Trisula Nefis drogues file                                     */

         TriDroGetMat( fname,  itype,  parcod, loc,    tim,    misval,
                       i3gl,   maxdim, data,   ierror, option        ) ;

         break ;

      case ODS_SAMPLES_2D    :
      case ODS_SAMPLES_TABLE :
      case ODS_SAMPLES_TIME  :

         /* Samples file (2D or table)                                 */

         SamplesGetMat( fname,  itype,  parcod, loc,    tim,    misval,
                        i3gl,   maxdim, data,   ierror, option        ) ;

         break ;


      case ODS_MORSYS_MAP_NEFIS  :

         /* Morsys Nefis com file                                     */
         GNF_GetMat( fname, itype, parcod, loc, tim, misval, i3gl,
            maxdim, data, ierror, option ) ;

         break ;

      case ODS_MORSYS_TRAM_NEFIS  :

         /* Morsys Nefis tram file                                    */
         GNF_GetMat( fname, itype, parcod, loc, tim, misval, i3gl,
            maxdim, data, ierror, option ) ;

         break ;

      case ODS_MORSYS_HWBOD  :

         /* Morsys Nefis com file                                     */

         ODS_MORSYS_NEFIS_commat_buffer( fname, parcod,*itype,
                                          &ibuffs, &rbuffs, ierror );
        if (*ierror != 0)
             break;

         ODS_MORSYS_NEFIS_COMMAT( fname, 256, 3, *itype,
                                   *parcod, (TInt4 *) fortran_loc,
                                   (TReal8 *)  tim,
                                   *misval, *i3gl, *maxdim,
                                   (TReal4 *) data, (TInt4 *) ierror,
                                   option,
                                   (TInt4 *) ibuffs,
                                   (TReal4 *) rbuffs );
         free( ibuffs );
         free( rbuffs );

         break ;

      case ODS_MORSYS_HWGXY  :

         /* Morsys Nefis com file                                     */

         ODS_MORSYS_NEFIS_commat_buffer( fname, parcod,*itype,
                                          &ibuffs, &rbuffs, ierror );
        if (*ierror != 0)
             break;

         ODS_MORSYS_NEFIS_COMMAT( fname, 256, 3, *itype,
                                   *parcod, (TInt4 *) fortran_loc,
                                   (TReal8 *)  tim,
                                   *misval, *i3gl, *maxdim,
                                   (TReal4 *) data, (TInt4 *) ierror,
                                   option,
                                   (TInt4 *) ibuffs,
                                   (TReal4 *) rbuffs );
         free( ibuffs );
         free( rbuffs );

         break ;

      case ODS_MORSYS_SWAN_NEFIS :

         /* Morsys Nefis SWAN file */
         GNF_GetMat( fname, itype, parcod, loc, tim, misval, i3gl,
            maxdim, data, ierror, option ) ;

         break ;

      case ODS_MORSYS_TRAH_NEFIS :

         /* Morsys Nefis His file                                     */

         GNF_GetMat( fname, itype, parcod, loc, tim, misval, i3gl,
            maxdim, data, ierror, option ) ;

         break ;

      case ODS_MORSYS_BAGR_NEFIS :

         /* Morsys Nefis BAGR file (dredging)                         */

         MorOdsGetMat ( fname, itype, parcod, tim , loc , misval,
                        maxdim, data, ierror, option );
         break ;


      case ODS_PHAROS_MAP_NEFIS  :
      case ODS_PHAROS_AMP_NEFIS  :

         /* Pharos Nefis admin file                                    */

         ODSGetMatPharMap(  fname, *itype, *parcod, (TReal8 *) tim,
                            (TInt4 *) loc, *misval, *maxdim, data,
                            (TInt4 *) ierror ) ;

         break ;
      case ODS_PHIDIAS_MAP  :
      case ODS_PHIDIAS_HISTORY :

         /* Phidias Nefis map file                                     */

         ODS_PHIDIAS_commat_buffer( fname, parcod,*itype,
                                    &ibuffs, &rbuffs, ierror );
        if (*ierror != 0)
             break;
         ODSGetMatPhiMap( fname, 256, 3, *itype,
                                   *parcod, (TInt4 *) fortran_loc,
                                   (TReal8 *)  tim,
                                   *misval, *i3gl, *maxdim,
                                   (TReal4 *) data, (TInt4 *) ierror,
                                   option,
                                   (TInt4 *) ibuffs,
                                   (TReal4 *) rbuffs );

         free( ibuffs );
         free( rbuffs );

         break ;

      case ODS_PHIDIAS_SPECTRAL  :

         /* Phidias Nefis spectral file                                     */

         ODS_PHIDIAS_commat_buffer( fname, parcod,*itype,
                                    &ibuffs, &rbuffs, ierror );
         if (*ierror != 0)
             break;
         ODSGetMatPhiSpec( fname, 256, 3, *itype,
                                   *parcod, (TInt4 *) fortran_loc,
                                   (TReal8 *)  tim,
                                   *misval, *i3gl, *maxdim,
                                   (TReal4 *) data, (TInt4 *) ierror,
                                   option,
                                   (TInt4 *) ibuffs,
                                   (TReal4 *) rbuffs );

         free( ibuffs );
         free( rbuffs );

         break ;

      case ODS_ANY_TEKAL_ASCII_1D :
      case ODS_ANY_TEKAL_ASCII_1DE :

         /* TEKAL ASCII 1D files                                       */

         ODSGetMatAnyTekAscHis      (
            fname , itype, *parcod, tim,
            loc, *misval, *maxdim, data, ierror ) ;

         break ;

      case ODS_ANY_TEKAL_ASCII_2D :
      case ODS_ANY_TEKAL_ASCII_VEC :

         /* TEKAL ASCII 2D files                                       */

         ODSGetMatAnyTekAscMap      (
            fname , itype, *parcod, tim,
            loc, *misval, *maxdim, data, ierror ) ;

         break ;

      case ODS_ANY_TEKAL_ASCII_ANNO :

    /* TEKAL ASCII Annotation files (text at specified location)          */

         ODSGetMatTekAnno (
            fname , itype, *parcod, tim,
            loc, *misval, *maxdim, data, ierror ) ;

         break ;

      case ODS_ANY_JSPOST :

         /* JSPost files                                               */

    ODSGetMatJSP( fname, itype, parcod, tim, loc, misval, maxdim,
             data,  ierror ) ;

         break ;

      case ODS_DELWAQ_GRID_UNF :

         /* DELWAQ/DELPAR grid file                                    */

         ODS_DELWAQ_UNF_telmac( fname, 256 , 3 , (TInt4) *itype,
                                (TInt4) *parcod ,
                                (TReal8 *) tim , (TInt4 *) fortran_loc ,
                                (TReal4) *misval , (TInt4) *maxdim ,
                                (TReal4 *) data , (TInt4 *) ierror ) ;

         break ;

      case ODS_DELWAQ_TELEMAC  :

         /* TELEMAC grid file                                          */

         ODS_Delwaq_Telemac_Coords( fname, *itype, *parcod, loc, tim, *misval, *i3gl,
            *maxdim, data, ierror, option ) ;
         break ;

      case ODS_SHYFEM_GRID :

         /* SHYFEM grid file                                    */

         ODSGetMatShyf( (TString) fname, (TInt4) *itype,
                               (TInt4) *parcod ,
                               (TReal8 *) tim , (TInt4  *) loc ,
                               (TReal4) *misval , (TInt4) *maxdim ,
                               (TReal4 *) data , (TInt4  *) ierror ) ;
         break ;

      case ODS_GEOGR_BNA:
      case ODS_GEOGR_DXF:
      case ODS_GEOGR_TEKAL:

         /* BNA, DXF and TEKAL landboundary files                     */

         ODS_BNA_getmat( fname,  itype, parcod, loc,   tim, misval, i3gl,
                         maxdim, data,  ierror, option                  ) ;
         break ;

      default :

         /* Filetype not implemented:                                  */

         *ierror = IEUNKN ;
   }
}
