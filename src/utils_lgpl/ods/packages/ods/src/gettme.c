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
 *  gettme.c  -  ODS get list of time steps from file
 *
 *  Marc Kool
 */

/*
 *  $Author: Markus $
 *  $Date: 11/04/05 11:58a $
 *  $Source: /u/cvsroot/gpp/libsrc/ods/gettme.c,v $
*/
/*
 *
 */


/*   Date:       28 Feb 1993                                           */
/*   Time:       15:20                                                */
/*   Program:    GETTME.C                                             */
/*   Version:    1.03                                                 */
/*   Programmer: Andre Hendriks                                       */
/*   Copyright (C)  Stichting Deltares, 2011-2024.                    */
/*   Previous version(s):                                             */
/*   1.02 -- 6 Dec 1993 -- 13:00 -- Operating System: DOS             */
/*   1.01 -- 5 Aug 1993 -- 16:31 -- Operating System: DOS             */
/*   1.00 -- 5 Aug 1993 -- 16:21 -- Operating System: DOS             */
/*   0.00 -- 5 Aug 1993 -- 12:31 -- Operating System: DOS             */
/*   Project:    Open Data Structuur                                  */
/*   Module:     GetTime                                              */
/*   Function:                                                        */
/*   Comment:    Added WAS and WS1 files                              */
/*   Reference:                                                       */
/*   Review:                                                          */

#include "portable.h"
#include "ods.h"

#include <stdio.h>

#ifndef NOPROT

/* function prototypes */

#include "odsmodel.h"

TInt4 iftype ( char *fname) ;

#endif


/* the function to get times from 'any' file:   */

void DllExport gettme ( char  *fname,  TInt4  *itype,  double *timdef,
                        TInt4 *maxdef, TInt4  *pardep, TInt4  *locdep,
                        TInt4 *maxlst, double *timlst, TInt4  *timtyp,
                        TInt4 *nrlst,  TInt4  *ierror, char   *option)

/*************************************************************************/
/*                                                                       */
/*     Arguments:                                                        */
/*                                                                       */
/*        Name    Size      I/O  Description                             */
/*        ------  --------  ---  ------------------------------------    */
/*        fname   -          I   Full filename, including extension      */
/*        ierror  -          O   Errorcode, see errors.inc for definitio */
/*        itype   -         I/O  Filetype, see types.inc for definitions */
/*        len.    -          I   UNIX only: length of CHARACTER variable */
/*        locdep  -          I   location dependency of times.           */
/*        maxdef  -          I   Max. nr of times wanted.                */
/*        maxlst  -          I   Max. nr of times to return.             */
/*        nrlst   -          O   Nr of times returned.                   */
/*        option  -         I/O  option (reserved for future extensions) */
/*        pardep  -          I   parameter dependency of timesteps       */
/*        timdef  2,maxdef   I   List of times wanted.                   */
/*        timtyp  maxlst     O   list of time types.                     */
/*        timlst  maxlst     O   List of times found.                    */
/*                                                                       */
/*************************************************************************/

   {
   int i, inum , length ;
   char *fn;

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

   *nrlst = 0 ;

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

#if 0
      case ITDBF3 :
      case ITDBF4 :

         /* DBASE 3/4 filetype                                         */

         ODSGetTmeDbf3 ( fname , timdef, *maxdef, timlst,
                         *maxlst, nrlst , ierror) ;
         break ;
#endif

      case ODS_DELWAQ_MAP_BIN :
      case ODS_DELWAQ_HIS_BIN :
      case ODS_DELWAQ_BAL_BIN :
      case ODS_DELPAR_PLO_BIN :

         /* DELWAQ 3 MAP/HIS file, DELPAR PLO file                     */

         ODSGetTmeDlwq ( fname, itype, timdef, *maxdef, *pardep, timlst,
                         *maxlst, nrlst , ierror) ;
         break ;

      case ODS_DELWAQ_HIS_NEFIS :
      case ODS_DELWAQ_MAP_NEFIS :
      case ODS_DELPAR_HIS_NEFIS :
      case ODS_DELPAR_MAP_NEFIS :
      case ODS_DELPAR_PLO_NEFIS :

         /* DELWAQ/DELPAR NEFIS filetype */

         DlwGetNefisTme ( fname,  itype, timdef, maxdef, timlst,
                          maxlst, nrlst, ierror, option );

         break ;

      case ITWAS  :

         /* WASPRO WAS file                                            */

         ODSGetTmeWas  ( fname , timdef, *maxdef, timlst,
                         *maxlst, nrlst , ierror) ;
         break ;

      case ITWS1  :

         /* WASPRO WS1 file                                            */

         ODSGetTmeWs1  ( fname , timdef, *maxdef, timlst,
                  *maxlst, nrlst , ierror) ;
         break ;

      case ITMPX  :

         /* MAPPIX file                                                */

         ODSGetTmeMPX  ( fname , timdef, *maxdef, timlst,
                  *maxlst, nrlst , ierror) ;
         break ;

      case ODS_TRISULA_HIS_NEFIS  :

         /* Trisula Nefis history file                                 */
         GNF_GetTme( fname, itype, timdef, maxdef, pardep, locdep,
            maxlst, timlst, timtyp, nrlst, ierror, option ) ;

         break ;

      case ODS_TRISULA_MAP_NEFIS  :

         /* Trisula Nefis map file                                     */

         GNF_GetTme( fname, itype, timdef, maxdef, pardep, locdep,
            maxlst, timlst, timtyp, nrlst, ierror, option ) ;

         break ;

      case ODS_TRISULA_DRO_NEFIS  :
      case ODS_DELPAR_TRK_NEFIS   :

         /* Trisula Nefis drogues file                                     */

         TriDroGetTme( fname,  itype,  timdef, maxdef, pardep,
                       locdep, maxlst, timlst, timtyp, nrlst,
                       ierror, option                        ) ;


         break ;

      case ODS_SAMPLES_2D    :
      case ODS_SAMPLES_TABLE :
      case ODS_SAMPLES_TIME  :

         /* Samples file (2D or table)                                 */

         SamplesGetTme( fname,  itype,  timdef, maxdef, pardep,
                        locdep, maxlst, timlst, timtyp, nrlst,
                        ierror, option                        ) ;

         break ;

      case ODS_MORSYS_MAP_NEFIS  :

         /* Morsys Nefis com file                                     */
         GNF_GetTme( fname, itype, timdef, maxdef, pardep, locdep,
            maxlst, timlst, timtyp, nrlst, ierror, option ) ;

         break ;

      case ODS_MORSYS_TRAM_NEFIS  :

         /* Morsys Nefis tram file */
         GNF_GetTme( fname, itype, timdef, maxdef, pardep, locdep,
            maxlst, timlst, timtyp, nrlst, ierror, option ) ;

         break ;


      case ODS_MORSYS_HWBOD  :

         /* Morsys Nefis com file                                     */

         ODS_MORSYS_NEFIS_COMTME( fname, 256, 3, *itype,
                                   (TReal8 *) timdef, *maxdef, *pardep,
                                   *locdep, *maxlst, (TReal8 *) timlst,
                                   (TInt4 *) timtyp, (TInt4 *) nrlst,
                                   (TInt4 *) ierror, option );

         break ;

      case ODS_MORSYS_HWGXY  :

         /* Morsys Nefis com file                                     */

         ODS_MORSYS_NEFIS_COMTME( fname, 256, 3, *itype,
                                   (TReal8 *) timdef, *maxdef, *pardep,
                                   *locdep, *maxlst, (TReal8 *) timlst,
                                   (TInt4 *) timtyp, (TInt4 *) nrlst,
                                   (TInt4 *) ierror, option );

         break ;

      case ODS_MORSYS_SWAN_NEFIS :

         GNF_GetTme( fname, itype, timdef, maxdef, pardep, locdep,
            maxlst, timlst, timtyp, nrlst, ierror, option ) ;

         break ;

      case ODS_MORSYS_TRAH_NEFIS  :

         GNF_GetTme( fname, itype, timdef, maxdef, pardep, locdep,
            maxlst, timlst, timtyp, nrlst, ierror, option ) ;

         break ;

      case ODS_MORSYS_BAGR_NEFIS :

         /* Morsys Nefis BAGR file (dredging)                         */

         MorOdsGetTme ( fname,  itype, timdef, maxdef, timlst,
                        maxlst, nrlst, ierror, option );

         break ;

      case ODS_ANY_TEKAL_ASCII_1D :
      case ODS_ANY_TEKAL_ASCII_1DE :

         /* TEKAL ASCII 1D files                                       */

         ODSGetTmeAnyTekAscHis      (
                          fname , itype, timdef, *maxdef, *pardep,
           *locdep, timlst, *maxlst,
                          nrlst, ierror, option ) ;

         break ;

      case ODS_ANY_TEKAL_ASCII_2D :
      case ODS_ANY_TEKAL_ASCII_VEC :

         /* TEKAL ASCII 2D files ( scalar and vector )                 */

         ODSGetTmeAnyTekAscMap      (
                          fname , itype, timdef, *maxdef, *pardep,
           *locdep, timlst, *maxlst,
                          nrlst, ierror, option ) ;

         break ;

      case ODS_ANY_TEKAL_ASCII_ANNO :

    /* TEKAL ASCII Annotation files (text at specified location)          */

         ODSGetTmeTekAnno (
            fname , itype, timdef, *maxdef, *pardep,
            *locdep, timlst, *maxlst,
            nrlst, ierror, option ) ;

         break ;

      case ODS_ANY_JSPOST :

         /* JSPost files                                               */

    ODSGetTmeJSP ( fname, timdef, *maxdef, timlst, *maxlst, nrlst,
         timtyp, ierror, option ) ;

    break ;


      case ODS_DELWAQ_GRID_UNF :
      case ODS_DELWAQ_TELEMAC  :

         /* DELWAQ/DELPAR grid files (List will have length zero)           */

         ODSGetTmeDlwg ( fname, itype, *timdef, *maxdef, *pardep, *locdep,
                         timlst, *maxlst, nrlst , ierror, option ) ;
         break ;

      case ODS_SHYFEM_GRID :

         /* SHYFEM grid files (List will have length zero)           */

         ODSGetTmeShyf ( fname, (TInt4 *) itype, *timdef, *maxdef, *pardep,
                         *locdep, timlst, *maxlst, (TInt4 *) nrlst ,
                         (TInt4 *) ierror, option ) ;
         break ;

      case ODS_PHAROS_MAP_NEFIS :
      case ODS_PHAROS_AMP_NEFIS :

         /* Pharos Nefis admin file                                  */

         ODSGetTmePharAmp( fname, (TInt4 *) itype, timdef, *maxdef, *pardep,
                           *locdep, timlst, *maxlst, (TInt4 *) nrlst,
                           (TInt4 *) ierror, option ) ;

         break ;

      case ODS_PHIDIAS_MAP  :
      case ODS_PHIDIAS_HISTORY :

         /* Phidias Nefis map file                                     */

         ODSGetTmePhiMap( fname, 256, 3, *itype,
                                   (TReal8 *) timdef, *maxdef, *pardep,
                                   *locdep, *maxlst, (TReal8 *) timlst,
                                   (TInt4 *) timtyp, (TInt4 *) nrlst,
                                   (TInt4 *) ierror, option );

         break ;

      case ODS_PHIDIAS_SPECTRAL:

         /* Phidias Nefis spectral file                                     */

         ODSGetTmePhiSpec( fname, 256, 3, *itype,
                           (TReal8 *) timdef, *maxdef, *pardep,
                           *locdep, *maxlst, (TReal8 *) timlst,
                           (TInt4 *) timtyp, (TInt4 *) nrlst,
                           (TInt4 *) ierror, option );

         break ;

      case ODS_GEOGR_BNA:
      case ODS_GEOGR_DXF:
      case ODS_GEOGR_TEKAL:

         /* BNA, DXF and TEKAL landboundary files                     */

         ODS_BNA_gettme( fname,  itype,  timdef, maxdef, pardep,
                         locdep, maxlst, timlst, timtyp, nrlst,
                         ierror, option                         ) ;
         break ;
      default :

         /* Filetype not implemented:                                  */

         *ierror = IEUNKN ;
      }
   if ( *ierror == IEPMNY) *ierror = IETMNY ;
   return ;
   }
