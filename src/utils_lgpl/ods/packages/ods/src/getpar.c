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
 *  getpar.c  -  ODS Get list of parameters from file
 *
 *  Marc Kool
 */

/*
 *  $Author: Markus $
 *  $Date: 11/04/05 11:58a $
 *  $Source: /u/cvsroot/gpp/libsrc/ods/getpar.c,v $
 */


/*   Date:       28 Feb 1994                                          */
/*   Time:       15:55                                                */
/*   Program:    GETPAR.C                                             */
/*   Version:    1.03                                                 */
/*   Programmer: Andre Hendriks                                       */
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

#include <stdio.h>

#ifndef NOPROT

/* function prototypes */

#include "odsmodel.h"

TInt4 iftype ( char *fname) ;

#endif


/* the function to get parameters from 'any' file:   */

void DllExport getpar ( char  *fname,  TInt4 *itype,  char  *pardef,
                        TInt4 *maxdef, TInt4 *timdep, TInt4 *locdep,
                        TInt4 *maxlst, TInt4 *lang,   char  *parlst,
                        char  *paruni, TInt4 *partyp, TInt4 *parcod,
                        TInt4 *nrlst,  TInt4 *ierror, char  *option)

/*************************************************************************/
/*                                                                       */
/*     Arguments:                                                        */
/*                                                                       */
/*        Name    Size      I/O  Description                             */
/*        ------  --------  ---  ------------------------------------    */
/*        fname   3          I   Full filename, including extension      */
/*        ierror  -          O   Errorcode, see errors.inc for definitio */
/*        itype   -         I/O  Filetype, see types.inc for definitions */
/*        lang    -          I   code for language                       */
/*        len_..  -          I   UNIX only: length of CHARACTER variable */
/*        locdep  -          I   location dependency parameters          */
/*        nrlst   -          O   Nr of parameters returned.              */
/*        maxdef  -          I   Max. nr of parameters wanted.           */
/*        maxlst  -          I   Max. nr of parameters to return.        */
/*        option  -         I/O  option (reserved for future extensions) */
/*        parcod  maxlst     O   List of parameter indices.              */
/*        pardef  maxdef     I   List of parameters wanted.              */
/*        parlst  maxlst     O   List of parameters found.               */
/*        partyp  maxlst     O   List of parameter types.                */
/*        paruni  maxlst     O   List of units for parameters found.     */
/*        timdep  -          I   time dependency parameters              */
/*                                                                       */
/*************************************************************************/

   {
   int i , j , inum , length ;
   char *fn;

   *ierror = IEOK;

   /* Make sure the list of parameter names is clean
      - this helps with possible trailing blanks
   */
   memset( parlst, '\0', (PARLEN+1)*(*maxlst) ) ;

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

      case ODS_DELWAQ_MAP_NEFIS :
      case ODS_DELWAQ_HIS_NEFIS :
      case ODS_DELPAR_MAP_NEFIS :
      case ODS_DELPAR_HIS_NEFIS :
      case ODS_DELPAR_PLO_NEFIS :

         /* DELWAQ/DELPAR NEFIS filetype */

         DlwGetNefisPar ( fname, itype,  pardef, maxdef, parlst, paruni,
                          maxlst, nrlst , partyp, parcod, ierror,
                          option)
;

         break;

#if 0
      case ITDBF3 :

         /* DBASE 3 filetype                                           */

         ODSGetParDbf3 ( fname , pardef, *maxdef, parlst, paruni,
                         *maxlst, nrlst , ierror) ;
         break ;

      case ITDBF4 :

         /* DBASE 4 filetype                                           */

         ODSGetParDbf4 ( fname , pardef, *maxdef, parlst, paruni,
                         *maxlst, nrlst , ierror) ;
         break ;
#endif

      case ODS_DELWAQ_MAP_BIN :
      case ODS_DELWAQ_HIS_BIN :
      case ODS_DELWAQ_BAL_BIN :
      case ODS_DELPAR_PLO_BIN :

         /* DELWAQ 3 MAP or HIS file, DELPAR PLO file                  */

         ODSGetParDlwq ( fname , itype, pardef, *maxdef, parlst, paruni,
                         *maxlst, nrlst , partyp, parcod, ierror) ;
         break ;

      case ITWAS  :

         /* WASPRO WAS file                                            */

         ODSGetParWas  ( fname , pardef, *maxdef, parlst, paruni,
                         *maxlst, nrlst , ierror) ;
         break ;

      case ITWS1  :

         /* WASPRO WS1 file                                            */

         ODSGetParWs1  ( fname , pardef, *maxdef, parlst, paruni,
                         *maxlst, nrlst , ierror) ;
         break ;

      case ITMPX :

         /* MAPPIX file                                                */

         ODSGetParMPX ( fname , pardef, *maxdef, parlst, paruni,
                        *maxlst, nrlst , ierror) ;
         break ;

      case ODS_TRISULA_HIS_NEFIS  :

         /* Trisula Nefis history file                                 */

         GNF_GetPar( fname, itype, pardef, maxdef, timdep, locdep,
            maxlst, lang, parlst, paruni, partyp, parcod, nrlst,
            ierror, option ) ;

         break ;

      case ODS_TRISULA_MAP_NEFIS  :

         /* Trisula Nefis map file                                     */

         GNF_GetPar( fname, itype, pardef, maxdef, timdep, locdep,
            maxlst, lang, parlst, paruni, partyp, parcod, nrlst,
            ierror, option ) ;

         break ;

      case ODS_TRISULA_DRO_NEFIS  :
      case ODS_DELPAR_TRK_NEFIS   :

         /* Trisula Nefis drogues file                                  */

         TriDroGetPar( fname,  itype,  pardef, maxdef, timdep, locdep,
                       maxlst, lang,   parlst, paruni, partyp, parcod,
                       nrlst,  ierror, option                        ) ;

         break ;

      case ODS_SAMPLES_2D    :
      case ODS_SAMPLES_TABLE :
      case ODS_SAMPLES_TIME  :

         /* Samples file (2D or table)                                 */

         SamplesGetPar( fname,  itype,  pardef, maxdef, timdep, locdep,
                        maxlst, lang,   parlst, paruni, partyp, parcod,
                        nrlst,  ierror, option                        ) ;

         break ;

      case ODS_MORSYS_MAP_NEFIS  :

         /* Morsys Nefis com file                                     */

         GNF_GetPar( fname, itype, pardef, maxdef, timdep, locdep,
            maxlst, lang, parlst, paruni, partyp, parcod, nrlst,
            ierror, option ) ;

         break ;

      case ODS_MORSYS_TRAM_NEFIS  :

         /* Morsys Nefis tram file */

         GNF_GetPar( fname, itype, pardef, maxdef, timdep, locdep,
            maxlst, lang, parlst, paruni, partyp, parcod, nrlst,
            ierror, option ) ;

         break ;

      case ODS_MORSYS_HWBOD  :

         /* Morsys Nefis HWBOD file                                     */

         ODS_MORSYS_NEFIS_COMPAR( fname, 256, 3, *itype,
                                   pardef, strlen(pardef), 1, /* HACK */
                                  *maxdef, *timdep, *locdep, *maxlst, *lang,
                                  parlst, 21, *maxlst, paruni, 21, *maxlst,
                                  (TInt4 *) partyp,
                                   (TInt4 *) parcod, (TInt4 *) nrlst,
                                   (TInt4 *) ierror, option );

         break ;


      case ODS_MORSYS_HWGXY  :

         /* Morsys Nefis HWGXY file                                     */

         ODS_MORSYS_NEFIS_COMPAR( fname, 256, 3, *itype,
                                   pardef, strlen(pardef), 1, /* HACK */
                                  *maxdef, *timdep, *locdep, *maxlst, *lang,
                                  parlst, 21, *maxlst, paruni, 21, *maxlst,
                                  (TInt4 *) partyp,
                                   (TInt4 *) parcod, (TInt4 *) nrlst,
                                   (TInt4 *) ierror, option );

         break ;

      case ODS_MORSYS_SWAN_NEFIS :

         /* Morsys Nefis SWAN file */

         GNF_GetPar( fname, itype, pardef, maxdef, timdep, locdep,
            maxlst, lang, parlst, paruni, partyp, parcod, nrlst,
            ierror, option ) ;

         break ;

      case ODS_MORSYS_TRAH_NEFIS  :

         /* Morsys Nefis history files                                */

         GNF_GetPar( fname, itype, pardef, maxdef, timdep, locdep,
            maxlst, lang, parlst, paruni, partyp, parcod, nrlst,
            ierror, option ) ;

         break ;

      case ODS_MORSYS_BAGR_NEFIS  :

         /* Morsys Nefis BAGR files (dredging)                     */
         MorOdsGetPar ( fname, itype,  pardef, maxdef, parlst, paruni,
                        maxlst, nrlst , partyp, parcod, ierror,
                        option) ;
         break ;

      case ODS_PHIDIAS_MAP  :
      case ODS_PHIDIAS_HISTORY  :

         /* Phidias Nefis MAP and HISTORY file                   */

         ODSGetParPhiMap( fname, 256, 3, *itype,
                                   pardef, strlen(pardef), 1, /* HACK */
                                  *maxdef, *timdep, *locdep, *maxlst, *lang,
                                  parlst, 21, *maxlst, paruni, 21, *maxlst,
                                  (TInt4 *) partyp,
                                   (TInt4 *) parcod, (TInt4 *) nrlst,
                                   (TInt4 *) ierror, option );

         break ;

      case ODS_PHIDIAS_SPECTRAL :

         /* Phidias Nefis Spectrum file                   */

         ODSGetParPhiSpec( fname, 256, 3, *itype,
                                   pardef, strlen(pardef), 1, /* HACK */
                                  *maxdef, *timdep, *locdep, *maxlst, *lang,
                                  parlst, 21, *maxlst, paruni, 21, *maxlst,
                                  (TInt4 *) partyp,
                                   (TInt4 *) parcod, (TInt4 *) nrlst,
                                   (TInt4 *) ierror, option );

         break ;

      case ODS_ANY_TEKAL_ASCII_1D :
      case ODS_ANY_TEKAL_ASCII_1DE :

         /* TEKAL ASCII 1D files                                       */

         ODSGetParAnyTekAscHis      (
                          fname , itype, pardef, *maxdef, *timdep,
           *locdep, *maxlst, *lang, parlst, paruni,
                     partyp, parcod, nrlst, ierror, option ) ;

         break ;

      case ODS_ANY_TEKAL_ASCII_2D :
      case ODS_ANY_TEKAL_ASCII_VEC :

         /* TEKAL ASCII 2D files  ( scalar and vector )    */

         ODSGetParAnyTekAscMap      (
                          fname , itype, pardef, *maxdef, *timdep,
           *locdep, *maxlst, *lang, parlst, paruni,
                     partyp, parcod, nrlst, ierror, option ) ;

         break ;

      case ODS_ANY_TEKAL_ASCII_ANNO :

    /* TEKAL ASCII Annotation files (text at specified location)          */

         ODSGetParTekAnno (
                          fname , itype, pardef, *maxdef, *timdep,
           *locdep, *maxlst, *lang, parlst, paruni,
                     partyp, parcod, nrlst, ierror, option ) ;

         break ;

      case ODS_ANY_JSPOST :

         /* JSPost files                                               */
         ODSGetParJSP( fname, pardef, *maxdef, parlst, paruni, *maxlst,
             nrlst, partyp, parcod, ierror, option ) ;
    break ;

      case ODS_DELWAQ_GRID_UNF :
      case ODS_DELWAQ_TELEMAC  :

         /* DELWAQ/DELPAR grid files (No difference as far as          */
         /* parameters are concerned) :                                */

         ODSGetParDlwg ( fname , itype, pardef, *maxdef, parlst, paruni,
                         *maxlst, nrlst , partyp, parcod, ierror, option) ;
         break ;

      case ODS_SHYFEM_GRID :

         /* SHYFEM grid files                                          */

         ODSGetParShyf ( fname , (TInt4 *) itype, pardef, *maxdef, parlst,
                         paruni, *maxlst, (TInt4 *) nrlst ,
                         (TInt4 *) partyp, (TInt4 *) parcod,
                         (TInt4 *) ierror) ;
         break ;

      case ODS_PHAROS_AMP_NEFIS :

    /* PHAROS admin file (seiches amplifications)                */

    ODSGetParPharAmp ( fname, (TInt4 *) itype, pardef, *maxdef, parlst,
             paruni, *maxlst, (TInt4 *) nrlst,
             (TInt4 *) partyp, (TInt4 *) parcod,
             (TInt4 *) ierror) ;

    break ;
      case ODS_PHAROS_MAP_NEFIS :

    /* PHAROS Nefis file                                         */

    ODSGetParPharMap ( fname, (TInt4 *) itype, pardef, *maxdef, parlst,
             paruni, *maxlst, (TInt4 *) nrlst,
             (TInt4 *) partyp, (TInt4 *) parcod,
             (TInt4 *) ierror) ;

    break ;


      case ODS_GEOGR_BNA:
      case ODS_GEOGR_DXF:
      case ODS_GEOGR_TEKAL:

         /* BNA, DXF and TEKAL landboundary files                     */

         ODS_BNA_getpar( fname,  itype,  pardef, maxdef, timdep, locdep,
                         maxlst, lang,   parlst, paruni, partyp, parcod,
                         nrlst,  ierror, option                        ) ;
         break ;

      default :

         /* Filetype not implemented:                                  */

         *ierror = IEUNKN ;
      }


/* A patch/hack: make sure that no trailing blanks occur in the names
   Note: unfortunately the current routines work with a string length
   of PARLEN+1, instead of PARLEN.
   Note:
   Avoid strlen(), as this will not work if nulls have not been
   added properly. With the construct below some superfluous work
   is done, but it works.
*/
   for ( i = 0 ; i < *nrlst ; i ++ )
   {
      for ( j = PARLEN-1 ; j >= 0 ; j -- )
      {
         if ( parlst[j+(PARLEN+1)*i] == ' '  ||
              parlst[j+(PARLEN+1)*i] == '\0'    )
         {
            parlst[j+(PARLEN+1)*i] = '\0' ;
         }
         else
         {
            break ;
         }
      }
   }

   return ;
}
